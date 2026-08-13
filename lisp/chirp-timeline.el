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
(require 'appkit-view)
(require 'chirp-core)
(require 'chirp-backend)
(require 'chirp-media)
(require 'chirp-render)

(declare-function chirp-profile-load-more "chirp-profile" (&optional anchor-id))

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

(defun chirp-timeline--list-state (view)
  "Return VIEW's validated tweet-list projection state."
  (let ((state (appkit-view-state view)))
    (unless (and (listp state)
                 (memq (plist-get state :type) '(timeline collection))
                 (plist-get (plist-get state :query) :kind))
      (error "Invalid Chirp tweet-list view state"))
    state))

(defun chirp-timeline--current-view ()
  "Return the current live primary timeline view, or nil."
  (when-let* ((view (appkit-current-view))
              ((appkit-view-live-p view))
              (state (appkit-view-state view))
              ((eq (plist-get state :type) 'timeline))
              ((memq (plist-get (plist-get state :query) :kind)
                     '(home following))))
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
         (message (plist-get status :message))
         (title (downcase (or (plist-get state :title) "timeline"))))
    (pcase phase
      ('initial (format "Loading %s...\n\n" title))
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
     :anchor-property 'chirp-entry-id
     :no-separator-p t)
    (appkit-view-enqueue-event
     view (list :position (or (plist-get state :position) 'first)))
    (appkit-invalidate view :structure t :part 'frame :position t)
    (appkit-sync-invalidations view)))

(defun chirp-timeline--sync (view invalidations)
  "Synchronize VIEW from coalesced INVALIDATIONS."
  (let ((state (chirp-timeline--list-state view)))
    (chirp-sync-projection
     view invalidations
     (chirp-timeline--project-rows (plist-get state :items))
     (chirp-timeline--frame-text state))))

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
                 :parts '(frame entries geometry)
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

(defun chirp-timeline--collection-state (kind title refresh)
  "Return canonical state for collection KIND titled TITLE.
REFRESH reloads the collection."
  (list :type 'collection
        :query (list :kind kind)
        :items nil
        :title title
        :refresh refresh
        :status (list :phase 'initial :message nil)
        :expanded-tweet-ids (make-hash-table :test #'equal)
        :loaded-p nil))

(defun chirp-timeline--ensure-collection (kind title refresh)
  "Open or reuse collection KIND titled TITLE with REFRESH."
  (chirp-open-projection-view
   :id (list 'collection kind title)
   :title title
   :state (chirp-timeline--collection-state kind title refresh)
   :sync-function #'chirp-timeline--sync
   :printer #'chirp-timeline--print-row
   :select t))

(defun chirp-timeline--install-tweets (view tweets)
  "Install TWEETS into collection VIEW and request a projection sync."
  (let ((state (appkit-view-state view))
        (buffer (appkit-view-buffer view)))
    (setf (plist-get state :items) tweets
          (plist-get state :loaded-p) t
          (plist-get (plist-get state :status) :phase) 'idle
          (plist-get (plist-get state :status) :message) nil)
    (appkit-view-enqueue-event view (list :position 'first))
    (appkit-invalidate view :structure t :part 'frame :position t)
    (appkit-sync-invalidations view)
    (chirp-clear-status buffer)
    (chirp-media-prefetch-tweets tweets buffer)
    (chirp-enrich-quoted-tweets tweets buffer)))

(defun chirp-timeline--fetch-collection (view title refresh fetch-fn)
  "Run FETCH-FN for collection VIEW titled TITLE.
REFRESH retries the request after failure."
  (let* ((buffer (appkit-view-buffer view))
         (token (chirp-begin-background-request buffer title)))
    (funcall
     fetch-fn
     (lambda (tweets _envelope)
       (when (chirp-request-current-p buffer token)
         (chirp-timeline--install-tweets view tweets)))
     (lambda (message)
       (when (chirp-request-current-p buffer token)
         (chirp-show-error buffer title refresh message))))
    buffer))

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

(defun chirp-load-more (&optional _anchor-id)
  "Load older posts, preserving the current semantic position."
  (interactive)
  (cond
   ((chirp-timeline--current-view)
    (chirp-timeline--load-more-primary (chirp-timeline--current-view)))
   ((and (appkit-current-view)
         (eq (plist-get (appkit-view-state (appkit-current-view)) :type)
             'profile))
    (chirp-profile-load-more))
   (t
    (user-error "Current view does not support loading more posts"))))

(defun chirp-timeline-open-bookmarks (&optional _buffer)
  "Open bookmarks."
  (interactive)
  (let ((refresh (lambda () (chirp-timeline-open-bookmarks))))
    (chirp-timeline--fetch-collection
     (chirp-timeline--ensure-collection 'bookmarks "Bookmarks" refresh)
     "Bookmarks" refresh #'chirp-backend-bookmarks)))

(defun chirp-timeline--open-likes-for (handle)
  "Open liked tweets for HANDLE."
  (let* ((title (chirp-timeline--likes-title handle))
         (refresh (lambda () (chirp-timeline-open-likes handle)))
         (view (chirp-timeline--ensure-collection 'likes title refresh)))
    (chirp-timeline--fetch-collection
     view title refresh
     (lambda (success errback)
       (chirp-backend-likes handle success errback)))))

(defun chirp-timeline-open-likes (&optional handle _buffer)
  "Open liked tweets for HANDLE.

When HANDLE is nil, resolve the currently authenticated account first."
  (interactive)
  (let ((clean-handle
         (and handle
              (string-remove-prefix "@" (string-trim (format "%s" handle))))))
    (if clean-handle
        (chirp-timeline--open-likes-for clean-handle)
      (let* ((title "Liked")
             (refresh (lambda () (chirp-timeline-open-likes)))
             (view (chirp-timeline--ensure-collection 'likes title refresh))
             (buffer (appkit-view-buffer view))
             (token (chirp-begin-background-request buffer title)))
        (chirp-backend-whoami
         (lambda (user _envelope)
           (when (chirp-request-current-p buffer token)
             (if-let* ((resolved (plist-get user :handle)))
                 (chirp-timeline--open-likes-for resolved)
               (chirp-show-error
                buffer title refresh
                "X returned a whoami payload Chirp could not parse."))))
         (lambda (message)
           (when (chirp-request-current-p buffer token)
             (chirp-show-error buffer title refresh message))))
        buffer))))

(defun chirp-timeline-open-list (&optional list-target _buffer)
  "Open the timeline for LIST-TARGET.

LIST-TARGET may be a numeric list id or a full list URL."
  (interactive)
  (if (null list-target)
      (let* ((buffer (chirp-buffer))
             (token (chirp-begin-request buffer)))
        (message "Loading X lists...")
        (chirp-backend-lists
         (lambda (lists _envelope)
           (when (chirp-request-current-p buffer token)
             (condition-case err
                 (progn
                   (when (buffer-live-p buffer)
                     (kill-buffer buffer))
                   (chirp-timeline-open-list
                    (chirp-timeline--read-list-target lists)))
               (quit nil)
               (user-error (message "%s" (error-message-string err))))))
         (lambda (message)
           (when (chirp-request-current-p buffer token)
             (when (buffer-live-p buffer)
               (kill-buffer buffer))
             (message "Chirp list lookup failed: %s" message))))
        buffer)
    (let* ((clean-target (string-trim (format "%s" list-target)))
           (title (chirp-timeline--list-title clean-target))
           (refresh (lambda () (chirp-timeline-open-list clean-target))))
      (when (string-empty-p clean-target)
        (user-error "List ID or URL cannot be empty"))
      (chirp-timeline--fetch-collection
       (chirp-timeline--ensure-collection 'list title refresh)
       title refresh
       (lambda (success errback)
         (chirp-backend-list clean-target success errback))))))

(defun chirp-timeline-open-search (query &optional _buffer)
  "Open search results for QUERY."
  (interactive "sSearch X: ")
  (let* ((title (format "Search: %s" query))
         (refresh (lambda () (chirp-timeline-open-search query))))
    (chirp-timeline--fetch-collection
     (chirp-timeline--ensure-collection 'search title refresh)
     title refresh
     (lambda (success errback)
       (chirp-backend-search query success errback)))))

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
    (user-error "Current view does not support TAB switching"))))

(provide 'chirp-timeline)

;;; chirp-timeline.el ends here
