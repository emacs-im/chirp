;;; chirp-timeline.el --- Timeline views for chirp -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Fetch, merge, paginate, refresh, and render Chirp timeline views.

;;; Code:

(require 'cl-lib)
(require 'appkit-projection)
(require 'appkit-invalidation)
(require 'appkit-position)
(require 'chirp-core)
(require 'chirp-backend)
(require 'chirp-media)
(require 'chirp-render)

(defun chirp-timeline--set-kind (buffer kind)
  "Record timeline KIND in BUFFER."
  (with-current-buffer buffer
    (setq-local chirp--timeline-kind kind)))

(defun chirp-timeline--title (kind)
  "Return the buffer title for timeline KIND."
  (pcase kind
    ('home "For You")
    ('following "Following")
    (_ "Timeline")))

(defconst chirp-timeline--primary-view-id 'primary-timeline
  "Stable Appkit view identity shared by Home and Following.")

(defconst chirp-timeline--request-key 'primary-timeline
  "View request-table key for the active primary timeline transport.")

(cl-defstruct (chirp-timeline--generation
               (:constructor chirp-timeline--generation-create))
  "One logical primary timeline request generation."
  id
  phase
  settled-p)


(define-derived-mode chirp-timeline--mode chirp-view-mode "Chirp-Timeline"
  "Major mode for Appkit-owned primary timeline buffers."
  (setq-local chirp--refresh-function #'chirp-timeline--refresh-primary)
  (setq-local chirp--entry-wrap-navigation nil)
  (setq-local header-line-format nil))

(defun chirp-timeline--make-state (kind limit)
  "Return canonical state for primary timeline KIND and LIMIT."
  (list :type 'timeline
        :query (list :kind kind :limit limit)
        :items nil
        :page (list :next-cursor nil :exhausted-p nil)
        :status (list :phase 'initial :message nil)
        :generation nil
        :loaded-p nil
        :position nil
        :expanded-tweet-ids (make-hash-table :test #'equal)))

(defun chirp-timeline--feed-state (kind)
  "Return the session-owned canonical primary feed state for KIND."
  (unless (memq kind '(home following))
    (error "Invalid Chirp primary feed kind: %S" kind))
  (let ((states
         (chirp--session-primary-feed-states (chirp--session))))
    (or (gethash kind states)
        (puthash kind
                 (chirp-timeline--make-state
                  kind chirp-default-max-results)
                 states))))

(defun chirp-timeline--view-state (view)
  "Return VIEW's validated primary timeline state."
  (let ((state (appkit-view-state view)))
    (unless (and (listp state)
                 (eq (plist-get state :type) 'timeline)
                 (memq (plist-get (plist-get state :query) :kind)
                       '(home following)))
      (error "Invalid Chirp timeline view state"))
    state))

(defun chirp-timeline--current-view ()
  "Return the current live primary timeline view, or nil."
  (when-let* ((view (appkit-current-view))
              ((appkit-view-live-p view))
              (state (appkit-view-state view))
              ((eq (plist-get state :type) 'timeline)))
    view))

(defun chirp-timeline--row-key (tweet)
  "Return the stable projection key for TWEET."
  (when-let* ((id (plist-get tweet :id)))
    (list 'tweet id)))

(defun chirp-timeline--project-rows (tweets)
  "Project normalized TWEETS into keyed Appkit rows."
  (appkit-projection-project
   tweets #'chirp-timeline--row-key
   :context-function (lambda (previous _tweet) previous)
   :dependencies-function #'chirp-media-resource-keys-for-tweet))

(defun chirp-timeline--print-row (row)
  "Insert one projected timeline ROW at point."
  (chirp-render-insert-tweet-row
   (appkit-projection-row-payload row)
   (appkit-projection-row-context row)))

(defun chirp-timeline--frame-text (state)
  "Return header text representing timeline STATE."
  (let* ((status (plist-get state :status))
         (phase (plist-get status :phase))
         (message (plist-get status :message)))
    (pcase phase
      ('initial "Loading timeline...\n\n")
      ('refresh "Refreshing timeline...\n\n")
      ('older "Loading older posts...\n\n")
      ('error (format "Unable to load data.\n\n%s\n\n" message))
      (_ (and (null (plist-get state :items)) "No posts returned.\n")))))

(defun chirp-timeline--remember-position ()
  "Remember durable primary feed positions before their buffer is killed."
  (when-let* ((view (appkit-current-view))
              ((appkit-view-live-p view)))
    (let ((state (chirp-timeline--view-state view)))
      (when (plist-get state :items)
        (setf (plist-get state :position)
              (appkit-position-capture
               :anchor-property 'chirp-entry-id
               :preserve-window-start t)))
      ;; Live window references cannot restore a later projection buffer.  The
      ;; same snapshots retain semantic point and legacy viewport anchors.
      (dolist (feed-state
               (chirp--primary-feed-state-values state))
        (when-let* ((position (plist-get feed-state :position)))
          (setf (appkit-position-snapshot-window-snapshots position) nil))))))

(defun chirp-timeline--setup-view (view)
  "Initialize VIEW's EWOC and first projection."
  (let* ((state (chirp-timeline--view-state view))
         (kind (plist-get (plist-get state :query) :kind))
         (buffer (appkit-view-buffer view)))
    (setq-local chirp--view-title (chirp-timeline--title kind))
    (setq-local chirp--refresh-function #'chirp-timeline--refresh-primary)
    (setq-local chirp--rerender-function nil)
    (add-hook 'kill-buffer-hook #'chirp-timeline--remember-position nil t)
    (chirp--apply-buffer-name buffer chirp--view-title)
    (appkit-projection-ensure
     view
     :printer #'chirp-timeline--print-row
     :anchor-property 'chirp-entry-id)
    (appkit-view-enqueue-event
     view (list :position (or (plist-get state :position) 'first)))
    (appkit-invalidate view :structure t :part 'frame :position t)
    (appkit-sync-invalidations view)))



(defun chirp-timeline--position-intent (events)
  "Return the effective semantic position intent from EVENTS."
  (or (cl-loop for event in events
               when (eq (plist-get event :position) 'first)
               return 'first)
      (cl-loop for event in (reverse events)
               for position = (plist-get event :position)
               when position return position)
      'preserve))


(defun chirp-timeline--sync (view invalidations)
  "Synchronize VIEW from coalesced INVALIDATIONS."
  (let* ((state (chirp-timeline--view-state view))
         (events (appkit-view-pending-events-snapshot view))
         (event-count (length events))
         (position-intent (chirp-timeline--position-intent events))
         (resources (appkit-invalidations-resource-keys invalidations))
         (all-resources-p (memq 'all resources))
         (reconcile-p
          (or (appkit-invalidations-structure-p invalidations)
              (appkit-invalidations-entry-keys invalidations)
              resources))
         (rows
          (and reconcile-p
               (chirp-timeline--project-rows (plist-get state :items))))
         (force-keys
          (append
           (appkit-invalidations-entry-keys invalidations)
           (and all-resources-p
                (mapcar #'appkit-projection-row-key rows)))))
    (appkit-projection-sync
     view rows
     :header (or (chirp-timeline--frame-text state) "")
     :force-keys force-keys
     :changed-dependencies (and (not all-resources-p) resources)
     :position position-intent
     :reconcile-p reconcile-p)
    (appkit-view-acknowledge-events view event-count)))

(defun chirp-timeline--generation-current-p (view state generation)
  "Return non-nil when GENERATION may still update STATE in VIEW."
  (and (appkit-view-live-p view)
       (eq state (appkit-view-state view))
       (eq generation (plist-get state :generation))))

(defun chirp-timeline--fetch-count (state phase)
  "Return request size for STATE and request PHASE."
  (let ((limit (plist-get (plist-get state :query) :limit)))
    (pcase phase
      ('older (max 1 chirp-timeline-load-more-step))
      ('refresh
       (min limit
            (or (and chirp-timeline-refresh-max-results
                     (max 1 chirp-timeline-refresh-max-results))
                limit)))
      (_ limit))))

(defun chirp-timeline--settle-success
    (view state generation tweets envelope)
  "Settle GENERATION in VIEW and merge TWEETS from ENVELOPE into STATE."
  (setf (chirp-timeline--generation-settled-p generation) t)
  (when (chirp-timeline--generation-current-p view state generation)
    (let* ((phase (chirp-timeline--generation-phase generation))
           (query (plist-get state :query))
           (page (plist-get state :page))
           (status (plist-get state :status))
           (current (plist-get state :items))
           (next-cursor (chirp-backend-envelope-next-cursor envelope))
           (position-intent 'preserve)
           new-count)
      (pcase phase
        ('older
         (let ((merged (chirp-append-unique-tweets current tweets)))
           (unless (> (length merged) (length current))
             (message "No older posts."))
           (setf (plist-get state :items) merged
                 (plist-get page :next-cursor) next-cursor
                 (plist-get page :exhausted-p) (not next-cursor))))
        ('refresh
         (let ((merged (chirp-timeline--merge-refreshed-tweets current tweets)))
           (setq new-count (plist-get merged :new-count))
           (setf (plist-get state :items) (plist-get merged :tweets)
                 (plist-get page :next-cursor)
                 (if (> (length current) (plist-get query :limit))
                     (plist-get page :next-cursor)
                   (or next-cursor (plist-get page :next-cursor))))
           (when (or (null current) (> new-count 0))
             (setq position-intent 'first))))
        (_
         (setf (plist-get state :items) tweets
               (plist-get page :next-cursor) next-cursor
               (plist-get page :exhausted-p) (not next-cursor))
         (setq position-intent 'first)))
      (setf (plist-get status :phase) 'idle
            (plist-get status :message) nil
            (plist-get state :generation) nil
            (plist-get state :loaded-p) t)
      (appkit-view-enqueue-event
       view (list :position position-intent))
      (appkit-request-sync
       view :structure t :part 'frame :position t)
      (chirp-media-prefetch-tweets tweets (appkit-view-buffer view))
      (chirp-enrich-quoted-tweets tweets (appkit-view-buffer view))
      (when new-count
        (message "%s" (chirp-timeline--refresh-message new-count))))))

(defun chirp-timeline--settle-error (view state generation message)
  "Settle GENERATION in VIEW and STATE with error MESSAGE."
  (setf (chirp-timeline--generation-settled-p generation) t)
  (when (chirp-timeline--generation-current-p view state generation)
    (let ((status (plist-get state :status)))
      (setf (plist-get status :phase) 'error
            (plist-get status :message) message
            (plist-get state :generation) nil)
      (appkit-request-sync view :part 'frame :position t)
      (message "%s" (replace-regexp-in-string "[\r\n]+" "  " message)))))

(defun chirp-timeline--cancel-request (view)
  "Cancel VIEW's superseded primary timeline transport, if any."
  (let* ((table (appkit-view-request-table view))
         (request (gethash chirp-timeline--request-key table)))
    (remhash chirp-timeline--request-key table)
    (when request
      (chirp-backend-cancel-request request))))

(defun chirp-timeline--interrupt-state-request (state)
  "Retire STATE's interrupted request generation, if any."
  (when-let* ((generation (plist-get state :generation)))
    (setf (chirp-timeline--generation-settled-p generation) t
          (plist-get state :generation) nil)
    (let ((status (plist-get state :status)))
      (setf (plist-get status :phase)
            (if (plist-get state :loaded-p) 'idle 'initial)
            (plist-get status :message) nil))))

(defun chirp-timeline--retire-request (view state generation)
  "Retire VIEW's transport when GENERATION still owns STATE."
  (when (chirp-timeline--generation-current-p view state generation)
    (remhash chirp-timeline--request-key
             (appkit-view-request-table view))))

(defun chirp-timeline--request (view phase)
  "Start one logical PHASE request owned by VIEW."
  (let* ((state (chirp-timeline--view-state view))
         (query (plist-get state :query))
         (page (plist-get state :page))
         (status (plist-get state :status))
         (generation
          (chirp-timeline--generation-create
           :id (gensym "chirp-timeline-generation-")
           :phase phase))
         callback-ran-p
         request)
    ;; Revoke the previous generation before cancellation can synchronously
    ;; deliver its errback at the callback boundary.
    (chirp-timeline--interrupt-state-request state)
    (setf (plist-get state :generation) generation
          (plist-get status :phase) phase
          (plist-get status :message) nil)
    (chirp-timeline--cancel-request view)
    (appkit-request-sync view :part 'frame :position t)
    (setq request
          (chirp-backend-feed
           (lambda (tweets envelope)
             (setq callback-ran-p t)
             (chirp-timeline--retire-request view state generation)
             (chirp-timeline--settle-success
              view state generation tweets envelope))
           (eq (plist-get query :kind) 'following)
           (lambda (message)
             (setq callback-ran-p t)
             (chirp-timeline--retire-request view state generation)
             (chirp-timeline--settle-error
              view state generation message))
           (chirp-timeline--fetch-count state phase)
           (and (eq phase 'older) (plist-get page :next-cursor))
           view))
    (cond
     ((and (not callback-ran-p)
           request
           (chirp-timeline--generation-current-p view state generation))
      (puthash chirp-timeline--request-key request
               (appkit-view-request-table view)))
     ((and (not callback-ran-p)
           (null request)
           (chirp-timeline--generation-current-p view state generation))
      (chirp-timeline--settle-error
       view state generation "X timeline request did not start")))
    request))

(defun chirp-timeline--ensure-initial-request (view)
  "Start VIEW's initial request when its feed has never settled."
  (let* ((state (chirp-timeline--view-state view))
         (phase (plist-get (plist-get state :status) :phase)))
    (when (and (eq phase 'initial)
               (not (plist-get state :loaded-p))
               (null (plist-get state :generation)))
      (chirp-timeline--request view 'initial))))

(defun chirp-timeline--open-primary (kind)
  "Open or reuse the Appkit-owned primary timeline for KIND."
  (let* ((app (chirp-app))
         (view (appkit-view-for-id app chirp-timeline--primary-view-id)))
    (if view
        (let* ((state (chirp-timeline--view-state view))
               (current-kind
                (plist-get (plist-get state :query) :kind))
               (buffer (appkit-view-buffer view)))
          (unless (eq current-kind kind)
            (chirp-timeline--switch-primary view kind))
          (pop-to-buffer buffer)
          buffer)
      (let* ((state (chirp-timeline--feed-state kind)))
        ;; A dead view may have been killed while this session-owned state had
        ;; an in-flight transport.  Its callbacks no longer own a live view.
        (chirp-timeline--interrupt-state-request state)
        (let* ((view
                (appkit-open-view
                 :app app
                 :id chirp-timeline--primary-view-id
                 :mode 'chirp-timeline--mode
                 :buffer-name (chirp--format-buffer-name
                               (chirp-timeline--title kind))
                 :state state
                 :sync-function #'chirp-timeline--sync
                 :parts '(frame entries)
                 :position-policy 'chirp-entry-id
                 :setup #'chirp-timeline--setup-view
                 :select t))
               (buffer (appkit-view-buffer view)))
          ;; SETUP projects before SELECT; restore once more now that a
          ;; recreated buffer has a live window for its durable viewport.
          (when-let* ((position (plist-get state :position)))
            (with-current-buffer buffer
              (appkit-position-restore position)))
          (chirp-timeline--ensure-initial-request view)
          buffer)))))

(defun chirp-timeline--switch-primary (view kind)
  "Switch primary timeline VIEW in place to cached feed KIND."
  (let* ((state (chirp-timeline--view-state view))
         (current-kind (plist-get (plist-get state :query) :kind)))
    (unless (eq current-kind kind)
      (let* ((buffer (appkit-view-buffer view))
             (target (chirp-timeline--feed-state kind)))
        (when (plist-get state :items)
          (with-current-buffer buffer
            (setf (plist-get state :position)
                  (appkit-position-capture
                   :anchor-property 'chirp-entry-id
                   :preserve-window-start t))))
        (chirp-timeline--interrupt-state-request state)
        (chirp-timeline--cancel-request view)
        (chirp-timeline--interrupt-state-request target)
        (setf (appkit-view-state view) target
              (appkit-view-pending-events view) nil)
        (with-current-buffer buffer
          (setq-local chirp--view-title (chirp-timeline--title kind))
          (chirp--apply-buffer-name buffer chirp--view-title))
        (appkit-view-enqueue-event
         view (list :position (or (plist-get target :position) 'first)))
        (appkit-request-sync view :structure t :part 'frame :position t)
        (chirp-timeline--ensure-initial-request view)))))

(defun chirp-timeline--refresh-primary ()
  "Refresh the current Appkit-owned primary timeline."
  (if-let* ((view (chirp-timeline--current-view)))
      (chirp-timeline--request view 'refresh)
    (user-error "Current view is not a primary timeline")))

(defun chirp-timeline--likes-title (handle)
  "Return the buffer title for liked tweets by HANDLE."
  (if (and handle (not (string-empty-p handle)))
      (format "Liked: @%s" handle)
    "Liked"))

(defun chirp-timeline--list-id (target)
  "Return a display-friendly list id extracted from TARGET."
  (let ((text (string-trim (format "%s" target))))
    (if (string-match "/lists?/\\([0-9]+\\)" text)
        (match-string 1 text)
      text)))

(defun chirp-timeline--list-title (target)
  "Return the buffer title for list TARGET."
  (format "List: %s" (chirp-timeline--list-id target)))

(defun chirp-timeline--list-source-labels (list-info)
  "Return human-readable source labels for LIST-INFO."
  (mapcar (lambda (source)
            (pcase source
              ("owned" "owned")
              ("subscribed" "subscribed")
              ("member" "member")
              (_ source)))
          (or (chirp-get list-info "sources") '())))

(defun chirp-timeline--list-candidate (list-info)
  "Return a minibuffer display candidate for LIST-INFO."
  (let* ((name (or (chirp-get list-info "name")
                   (chirp-get list-info "fullName")
                   (chirp-get list-info "id")))
         (owner (chirp-get-in list-info '("owner" "screenName")))
         (mode (chirp-get list-info "mode"))
         (sources (chirp-timeline--list-source-labels list-info))
         (list-id (chirp-get list-info "id"))
         (parts (delq nil
                      (list (and owner (format "@%s" owner))
                            (and mode (not (string-empty-p mode)) mode)
                            (and sources (string-join sources ", "))
                            list-id))))
    (if parts
        (format "%s (%s)" name (string-join parts " · "))
      name)))

(defun chirp-timeline--read-list-target (lists)
  "Prompt for one accessible list from LISTS and return its id."
  (let ((choices (mapcar (lambda (list-info)
                            (cons (chirp-timeline--list-candidate list-info)
                                  (chirp-get list-info "id")))
                         lists)))
    (unless choices
      (user-error "No accessible lists found"))
    (cdr (assoc (completing-read
                 (format "List (%d): " (length choices))
                 choices
                 nil
                 t)
                choices))))

(defun chirp-timeline--refresh-function (kind buffer)
  "Return a refresh function for timeline KIND in BUFFER."
  (lambda ()
    (chirp-timeline--open
     kind
     :limit (or chirp--timeline-limit chirp-default-max-results)
     :anchor-id (chirp-capture-point-anchor)
     :buffer buffer
     :refreshing t)))

(defun chirp-timeline--current-count (buffer)
  "Return the current timeline post count for BUFFER."
  (with-current-buffer buffer
    (if-let* ((view (chirp-timeline--current-view)))
        (length (plist-get (chirp-timeline--view-state view) :items))
      (or chirp--timeline-count
          (let ((count 0)
                (pos (chirp--entry-position-forward (point-min))))
            (while pos
              (setq count (1+ count)
                    pos (chirp--entry-position-forward
                         (min (point-max) (1+ pos)))))
            count)))))

(defun chirp-timeline--buffer-tweets (buffer)
  "Return the canonical tweets represented by BUFFER."
  (with-current-buffer buffer
    (if-let* ((view (chirp-timeline--current-view)))
        (plist-get (chirp-timeline--view-state view) :items)
      (let (tweets)
        (chirp--map-buffer-tweets
         buffer
         (lambda (tweet)
           (push tweet tweets)))
        (nreverse tweets)))))

(defun chirp-timeline--prepended-new-count (current fetched)
  "Return how many unique FETCHED tweets are not already present in CURRENT.

For algorithmic timelines, especially \"For You\", the top recommendation can
stay fixed while newer posts are inserted below it.  Count every unseen tweet in
the refreshed head page so refresh feedback matches what the user will actually
see after the merge."
  (if (null current)
      0
    (let ((current-keys (make-hash-table :test #'equal))
          (seen-keys (make-hash-table :test #'equal))
          (count 0))
      (dolist (tweet current)
        (puthash (chirp-tweet-key tweet) t current-keys))
      (dolist (tweet fetched count)
        (let ((key (chirp-tweet-key tweet)))
          (unless (gethash key seen-keys)
            (puthash key t seen-keys)
            (unless (gethash key current-keys)
              (setq count (1+ count)))))))))

(defun chirp-timeline--merge-refreshed-tweets (current fetched)
  "Return a plist describing how FETCHED should merge over CURRENT."
  (let ((current-keys (make-hash-table :test #'equal))
        (merged nil)
        (merged-keys (make-hash-table :test #'equal))
        (new-count (chirp-timeline--prepended-new-count current fetched)))
    (dolist (tweet current)
      (puthash (chirp-tweet-key tweet) t current-keys))
    (dolist (tweet fetched)
      (let ((key (chirp-tweet-key tweet)))
        (unless (gethash key merged-keys)
          (puthash key t merged-keys)
          (push tweet merged))))
    (dolist (tweet current)
      (let ((key (chirp-tweet-key tweet)))
        (unless (gethash key merged-keys)
          (puthash key t merged-keys)
          (push tweet merged))))
    (list :tweets (nreverse merged)
          :new-count new-count)))

(defun chirp-timeline--refresh-message (new-count)
  "Return a status message for NEW-COUNT refreshed tweets."
  (if (zerop new-count)
      "No new posts."
    (format "%d new post%s."
            new-count
            (if (= new-count 1) "" "s"))))

(defun chirp-timeline--refresh-anchor-id (new-count anchor-id)
  "Return the entry id to anchor after a refresh.

When NEW-COUNT is positive, return nil so the refreshed view shows the
newly inserted posts at the top.  Otherwise preserve ANCHOR-ID."
  (and (zerop new-count) anchor-id))

(cl-defun chirp-timeline--render
    (buffer title refresh tweets
            &key kind limit anchor-id exhausted-p display-p next-cursor)
  "Render TWEETS into BUFFER with TITLE and REFRESH metadata.

KIND and LIMIT describe the timeline.  ANCHOR-ID restores point.  EXHAUSTED-P,
DISPLAY-P, and NEXT-CURSOR control pagination and presentation."
  (let ((tweet-count (length tweets)))
    (chirp-render-into-buffer
     buffer title refresh
     (lambda ()
       (if tweets
           (chirp-render-insert-tweet-list tweets)
         (chirp-render-insert-empty "No posts returned."))))
    (with-current-buffer buffer
      (setq-local chirp--timeline-kind kind)
      (setq-local chirp--timeline-limit (and kind limit))
      (setq-local chirp--timeline-count (and kind tweet-count))
      (setq-local chirp--timeline-next-cursor (and (memq kind '(home following))
                                                   next-cursor))
      (setq-local chirp--timeline-load-more-function
                  (and (memq kind '(home following))
                       #'chirp-load-more))
      (setq-local chirp--timeline-exhausted-p (and (memq kind '(home following))
                                                   exhausted-p))
      (setq-local chirp--rerender-function
                  (let ((saved-tweets tweets)
                        (saved-title title)
                        (saved-refresh refresh)
                        (saved-kind kind)
                        (saved-limit limit)
                        (saved-exhausted exhausted-p)
                        (saved-next-cursor next-cursor))
                    (lambda ()
                      (chirp-timeline--render
                       buffer
                       saved-title
                       saved-refresh
                       saved-tweets
                       :kind saved-kind
                       :limit saved-limit
                       :anchor-id (chirp-capture-point-anchor)
                       :exhausted-p saved-exhausted
                       :next-cursor saved-next-cursor))))
      (setq-local chirp--timeline-loading-more nil)
      (or (and anchor-id
               (chirp-restore-point-anchor anchor-id))
          (chirp-move-point-to-first-entry)))
    (chirp-clear-status buffer)
    (when display-p
      (chirp-display-buffer buffer))
    (chirp-media-prefetch-tweets tweets buffer)
    (chirp-enrich-quoted-tweets tweets buffer)))

(cl-defun chirp-timeline--handle-feed-success
    (buffer title refresh tweets
            &key kind limit anchor-id loading-more refreshing previous-count
            previous-tweets previous-exhausted-p previous-next-cursor envelope)
  "Handle a successful feed response for BUFFER with TITLE and REFRESH.

TWEETS, KIND, LIMIT, and ANCHOR-ID describe the new view.  LOADING-MORE and
REFRESHING select merge behavior.  PREVIOUS-COUNT, PREVIOUS-TWEETS,
PREVIOUS-EXHAUSTED-P, and PREVIOUS-NEXT-CURSOR describe the old view.  ENVELOPE
contains response pagination metadata."
  (ignore previous-count)
  (with-current-buffer buffer
    (setq-local chirp--request-token nil))
  (let ((next-cursor (chirp-backend-envelope-next-cursor envelope)))
    (cond
     (loading-more
      (let* ((current (or previous-tweets
                          (chirp-timeline--buffer-tweets buffer)))
             (merged-tweets (chirp-append-unique-tweets current tweets))
             (new-items-added (> (length merged-tweets) (length current)))
             (exhausted-p (not next-cursor)))
        (with-current-buffer buffer
          (setq-local chirp--timeline-loading-more nil)
          (setq-local chirp--request-token nil)
          (setq-local chirp--timeline-exhausted-p exhausted-p)
          (setq-local chirp--timeline-next-cursor next-cursor))
        (if new-items-added
            (chirp-timeline--render
             buffer
             title
             refresh
             merged-tweets
             :kind kind
             :limit limit
             :anchor-id anchor-id
             :exhausted-p exhausted-p
             :display-p t
             :next-cursor next-cursor)
          (when exhausted-p
            (message "No older posts.")))
        (chirp-clear-status buffer)))
     (refreshing
      (let* ((merged (chirp-timeline--merge-refreshed-tweets previous-tweets tweets))
             (merged-tweets (plist-get merged :tweets))
             (new-count (plist-get merged :new-count))
             (effective-next-cursor
              (if (> (length previous-tweets) limit)
                  previous-next-cursor
                (or next-cursor previous-next-cursor)))
             (render-needed (not (equal previous-tweets merged-tweets))))
        (if render-needed
            (chirp-timeline--render
             buffer
             title
             refresh
             merged-tweets
             :kind kind
             :limit limit
             :anchor-id (chirp-timeline--refresh-anchor-id new-count anchor-id)
             :exhausted-p previous-exhausted-p
             :display-p t
             :next-cursor effective-next-cursor)
          (with-current-buffer buffer
            (setq-local chirp--timeline-loading-more nil)
            (setq-local chirp--timeline-exhausted-p previous-exhausted-p)
            (setq-local chirp--timeline-next-cursor effective-next-cursor)
            (setq-local chirp--timeline-count (length previous-tweets))))
        (chirp-clear-status buffer)
        (message "%s" (chirp-timeline--refresh-message new-count))))
     (t
      (chirp-timeline--render
       buffer
       title
       refresh
       tweets
       :kind kind
       :limit limit
       :anchor-id anchor-id
       :exhausted-p (and (memq kind '(home following))
                         (not next-cursor))
       :display-p t
       :next-cursor next-cursor)
      (when (and loading-more
                 (not next-cursor))
        (message "No older posts."))))))

(cl-defun chirp-timeline--open
    (kind &key limit anchor-id buffer loading-more refreshing cursor)
  "Open timeline KIND with LIMIT posts.

When ANCHOR-ID is non-nil, restore point to that entry after rendering.
When BUFFER is non-nil, render into that existing buffer.
When LOADING-MORE is non-nil, keep the current buffer visible while fetching.
When REFRESHING is non-nil, merge newer tweets at the top on success.  CURSOR
requests a specific pagination page."
  (let* ((buffer (or buffer (chirp-buffer)))
         (title (chirp-timeline--title kind))
         (limit (or limit chirp-default-max-results))
         (refresh-count (and refreshing
                             (or (and chirp-timeline-refresh-max-results
                                      (max 1 chirp-timeline-refresh-max-results))
                                 limit)))
         (fetch-count (cond
                       (loading-more
                        (max 1 chirp-timeline-load-more-step))
                       (refreshing
                        (min limit refresh-count))
                       (t
                        limit)))
         (refresh (chirp-timeline--refresh-function kind buffer))
         (previous-count (and (or loading-more refreshing)
                              (chirp-timeline--current-count buffer)))
         (previous-tweets (and (or loading-more refreshing)
                               (chirp-timeline--buffer-tweets buffer)))
         (previous-exhausted-p (and refreshing
                                    (with-current-buffer buffer
                                      chirp--timeline-exhausted-p)))
         (previous-next-cursor (and (or loading-more refreshing)
                                    (with-current-buffer buffer
                                      chirp--timeline-next-cursor)))
         (token (if (or loading-more refreshing)
                    (progn
                      (with-current-buffer buffer
                        (setq-local chirp--timeline-loading-more t))
                      (chirp-begin-request buffer))
                  (chirp-begin-background-request buffer title))))
    (cond
     (loading-more
      (chirp-set-status buffer "Loading older posts...")
      (message "Loading older posts..."))
     (refreshing
      (chirp-set-status buffer "Refreshing timeline...")
      (message "Refreshing timeline...")))
    (chirp-timeline--set-kind buffer kind)
    (chirp-backend-feed
     (lambda (tweets envelope)
       (when (chirp-request-current-p buffer token)
         (chirp-timeline--handle-feed-success
          buffer title refresh tweets
          :kind kind
          :limit limit
          :anchor-id anchor-id
          :loading-more loading-more
          :refreshing refreshing
          :previous-count previous-count
          :previous-tweets previous-tweets
          :previous-exhausted-p previous-exhausted-p
          :previous-next-cursor previous-next-cursor
          :envelope envelope)))
     (eq kind 'following)
     (lambda (message)
       (when (chirp-request-current-p buffer token)
         (with-current-buffer buffer
           (setq-local chirp--timeline-loading-more nil)
           (setq-local chirp--request-token nil))
         (chirp-timeline--set-kind buffer kind)
         (if (or loading-more refreshing)
             (progn
               (chirp-set-status
                buffer
                (if refreshing
                    "Refresh failed"
                  "Load more failed")
                'error)
               (message "%s" (replace-regexp-in-string "[\r\n]+" "  " message)))
           (chirp-show-error buffer title refresh message))))
     fetch-count
     cursor)))

(defun chirp-timeline-open-home ()
  "Open Chirp's unique Appkit-owned home timeline."
  (interactive)
  (chirp-timeline--open-primary 'home))

(defun chirp-timeline-open-following ()
  "Open Chirp's unique Appkit-owned following timeline."
  (interactive)
  (chirp-timeline--open-primary 'following))

(defun chirp-timeline--load-more-primary (view)
  "Load an older page for primary timeline VIEW."
  (let* ((state (chirp-timeline--view-state view))
         (page (plist-get state :page))
         (phase (plist-get (plist-get state :status) :phase)))
    (cond
     ((memq phase '(initial refresh older))
      (message "Timeline request already in progress..."))
     ((or (plist-get page :exhausted-p)
          (not (plist-get page :next-cursor)))
      (message "No older posts."))
     (t
      (chirp-timeline--request view 'older)))))

(defun chirp-load-more (&optional anchor-id)
  "Load older posts, preserving the current semantic position.

ANCHOR-ID remains supported by legacy timeline views."
  (interactive)
  (if-let* ((view (chirp-timeline--current-view)))
      (chirp-timeline--load-more-primary view)
    (unless (memq chirp--timeline-kind '(home following))
      (user-error "Current view does not support loading more posts"))
    (cond
     (chirp--timeline-loading-more
      (message "Already loading older posts..."))
     (chirp--timeline-exhausted-p
      (message "No older posts."))
     ((not chirp--timeline-next-cursor)
      (message "No older posts."))
     (t
      (chirp-timeline--open
       chirp--timeline-kind
       :limit (or chirp--timeline-limit chirp-default-max-results)
       :anchor-id (or anchor-id (chirp-capture-point-anchor))
       :buffer (current-buffer)
       :loading-more t
       :cursor chirp--timeline-next-cursor)))))

(defun chirp-timeline-open-bookmarks (&optional buffer)
  "Open bookmarks in BUFFER."
  (interactive)
  (let* ((buffer (or buffer (chirp-buffer)))
         (refresh (lambda () (chirp-timeline-open-bookmarks buffer))))
    (let ((token (chirp-begin-background-request buffer "Bookmarks")))
      (chirp-timeline--set-kind buffer nil)
      (chirp-backend-bookmarks
       (lambda (tweets _envelope)
         (when (chirp-request-current-p buffer token)
           (chirp-timeline--render
            buffer "Bookmarks" refresh tweets :display-p t)))
       (lambda (message)
         (when (chirp-request-current-p buffer token)
           (chirp-timeline--set-kind buffer nil)
           (chirp-show-error buffer "Bookmarks" refresh message)))))))

(defun chirp-timeline-open-likes (&optional handle buffer)
  "Open liked tweets for HANDLE in BUFFER.

When HANDLE is nil, resolve the currently authenticated account first."
  (interactive)
  (let* ((buffer (or buffer (chirp-buffer)))
         (clean-handle (and handle
                            (string-remove-prefix "@"
                                                  (string-trim (format "%s" handle)))))
         (title (chirp-timeline--likes-title clean-handle)))
    (let ((token (chirp-begin-background-request buffer title)))
      (chirp-timeline--set-kind buffer nil)
      (if clean-handle
          (let ((refresh (lambda () (chirp-timeline-open-likes clean-handle buffer))))
            (chirp-backend-likes
             clean-handle
             (lambda (tweets _envelope)
               (when (chirp-request-current-p buffer token)
                 (chirp-timeline--render
                  buffer title refresh tweets :display-p t)))
             (lambda (message)
               (when (chirp-request-current-p buffer token)
                 (chirp-timeline--set-kind buffer nil)
                 (chirp-show-error buffer title refresh message)))))
        (chirp-backend-whoami
         (lambda (user _envelope)
           (when (chirp-request-current-p buffer token)
             (if-let* ((resolved-handle (plist-get user :handle)))
                 (let* ((resolved-title (chirp-timeline--likes-title resolved-handle))
                        (refresh (lambda ()
                                   (chirp-timeline-open-likes resolved-handle buffer))))
                   (chirp-backend-likes
                    resolved-handle
                    (lambda (tweets _likes-envelope)
                      (when (chirp-request-current-p buffer token)
                        (chirp-timeline--render
                         buffer resolved-title refresh tweets :display-p t)))
                    (lambda (message)
                      (when (chirp-request-current-p buffer token)
                        (chirp-timeline--set-kind buffer nil)
                        (chirp-show-error buffer resolved-title refresh message)))))
               (chirp-show-error
                buffer
                title
                (lambda () (chirp-timeline-open-likes nil buffer))
                "X returned a whoami payload Chirp could not parse."))))
         (lambda (message)
           (when (chirp-request-current-p buffer token)
             (chirp-timeline--set-kind buffer nil)
             (chirp-show-error
              buffer
              title
              (lambda () (chirp-timeline-open-likes nil buffer))
              message))))))))

(defun chirp-timeline-open-list (&optional list-target buffer)
  "Open the timeline for LIST-TARGET in BUFFER.

LIST-TARGET may be a numeric list id or a full list URL."
  (interactive)
  (let ((buffer (or buffer (chirp-buffer))))
    (if (null list-target)
        (let ((token (chirp-begin-request buffer)))
          (message "Loading X lists...")
          (chirp-backend-lists
           (lambda (lists _envelope)
             (when (chirp-request-current-p buffer token)
               (condition-case err
                   (chirp-timeline-open-list
                    (chirp-timeline--read-list-target lists) buffer)
                 (quit nil)
                 (user-error (message "%s" (error-message-string err))))))
           (lambda (message)
             (when (chirp-request-current-p buffer token)
               (message "Chirp list lookup failed: %s" message)))))
      (let* ((clean-target (string-trim (format "%s" list-target)))
             (title (chirp-timeline--list-title clean-target))
             (refresh
              (lambda () (chirp-timeline-open-list clean-target buffer))))
        (when (string-empty-p clean-target)
          (user-error "List ID or URL cannot be empty"))
        (let ((token (chirp-begin-background-request buffer title)))
          (chirp-timeline--set-kind buffer nil)
          (chirp-backend-list
           clean-target
           (lambda (tweets _envelope)
             (when (chirp-request-current-p buffer token)
               (chirp-timeline--render
                buffer title refresh tweets :display-p t)))
           (lambda (message)
             (when (chirp-request-current-p buffer token)
               (chirp-timeline--set-kind buffer nil)
               (chirp-show-error buffer title refresh message)))))))))

(defun chirp-timeline-open-search (query &optional buffer)
  "Open search results for QUERY in BUFFER."
  (interactive "sSearch X: ")
  (let* ((title (format "Search: %s" query))
         (buffer (or buffer (chirp-buffer)))
         (refresh (lambda () (chirp-timeline-open-search query buffer))))
    (let ((token (chirp-begin-background-request buffer title)))
      (chirp-timeline--set-kind buffer nil)
      (chirp-backend-search
       query
       (lambda (tweets _envelope)
         (when (chirp-request-current-p buffer token)
           (chirp-timeline--render
            buffer title refresh tweets :display-p t)))
       (lambda (message)
         (when (chirp-request-current-p buffer token)
           (chirp-timeline--set-kind buffer nil)
           (chirp-show-error buffer title refresh message)))))))

(defun chirp-toggle-home-following ()
  "Toggle between primary Chirp subviews.

On Home/Following, switch between the two timelines.  In profile buffers that
expose subviews, cycle the current profile mode."
  (interactive)
  (cond
   ((functionp chirp--profile-switch-mode-function)
    (funcall chirp--profile-switch-mode-function :next))
   ((chirp-timeline--current-view)
    (let* ((view (chirp-timeline--current-view))
           (state (chirp-timeline--view-state view))
           (kind (plist-get (plist-get state :query) :kind)))
      (chirp-timeline--switch-primary
       view (if (eq kind 'home) 'following 'home))))
   (t
    (pcase chirp--timeline-kind
      ('home
       (chirp-timeline--open
        'following
        :limit chirp-default-max-results
        :buffer (current-buffer)))
      ('following
       (chirp-timeline--open
        'home
        :limit chirp-default-max-results
        :buffer (current-buffer)))
      (_
       (user-error "Current view does not support TAB switching"))))))

(provide 'chirp-timeline)

;;; chirp-timeline.el ends here
