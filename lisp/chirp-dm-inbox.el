;;; chirp-dm-inbox.el --- XChat inbox lifecycle -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Appkit directory projection, pagination, and activation for XChat inboxes.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'appkit-core)
(require 'appkit-directory)
(require 'appkit-name-color)
(require 'appkit-invalidation)
(require 'appkit-ui)
(require 'appkit-view)
(require 'chirp-backend)
(require 'chirp-core)
(require 'chirp-media)
(require 'chirp-dm-state)
(require 'chirp-dm-conversation)
(require 'chirp-time)
(require 'chirp-x)

(defcustom chirp-dm-inbox-page-size 20
  "Number of XChat conversations requested per inbox page, up to 100."
  :type '(integer 1 100)
  :group 'chirp)

(defvar chirp-dm-inbox--next-instance 0
  "Monotonic identity source for fresh direct-message inbox views.")

(defconst chirp-dm-inbox--request-key 'dm-inbox
  "Operation key for one inbox view's active transport.")

;;; Implementation
(defconst chirp-dm-inbox--icon-slot-width 4
  "Columns reserved for one direct-message inbox avatar.")

;;; Inbox

(defun chirp-dm-inbox--one-line (text)
  "Return TEXT collapsed into one trimmed display line."
  (string-trim
   (replace-regexp-in-string "[[:space:]\n\r]+" " " (or text ""))))

(defun chirp-dm-inbox--format-time (milliseconds)
  "Return a compact local timestamp for MILLISECONDS, or an empty string."
  (if (and (stringp milliseconds)
           (string-match-p "\\`[0-9]+\\'" milliseconds))
      (chirp-time-format-compact
       (seconds-to-time (/ (string-to-number milliseconds) 1000)))
    ""))

(defun chirp-dm-inbox--status (state)
  "Return canonical status plist from direct-message STATE."
  (or (plist-get state :status)
      (error "Direct-message view state has no status")))


(defun chirp-dm-inbox--participant-label (participant)
  "Return PARTICIPANT's display label, or nil."
  (or (plist-get participant :name)
      (and-let* ((handle (plist-get participant :handle)))
        (concat "@" handle))))

(defun chirp-dm-inbox--participant (conversation sender-id)
  "Return SENDER-ID's participant from CONVERSATION."
  (cl-find sender-id (plist-get conversation :participants)
           :key (lambda (participant) (plist-get participant :id))
           :test #'equal))

(defun chirp-dm-inbox--view-user-id (view)
  "Return VIEW's current XChat user ID, or nil."
  (when (appkit-view-live-p view)
    (chirp--session-xchat-user-id
     (appkit-app-state (appkit-view-app view)))))

(defun chirp-dm-inbox--participant-for-row (view conversation)
  "Return the direct peer representing CONVERSATION in VIEW."
  (when (eq (plist-get conversation :type) 'direct)
    (let ((participants (plist-get conversation :participants))
          (self-id (chirp-dm-inbox--view-user-id view)))
      (or (cl-find-if
           (lambda (participant)
             (not (equal (plist-get participant :id) self-id)))
           participants)
          (car participants)))))

(defun chirp-dm-inbox--conversation-title (view conversation)
  "Return CONVERSATION's activity title as presented in VIEW."
  (let ((fallback (chirp-dm-inbox--one-line (plist-get conversation :title))))
    (or (and (not (string-empty-p fallback)) fallback)
        (and-let* ((participant
                    (chirp-dm-inbox--participant-for-row view conversation)))
          (chirp-dm-inbox--participant-label participant))
        (if (eq (plist-get conversation :type) 'group)
            "Group conversation"
          "Direct message"))))

(defun chirp-dm-inbox--avatar-key (view conversation)
  "Return the avatar resource key representing CONVERSATION in VIEW."
  (when-let* ((participant
               (chirp-dm-inbox--participant-for-row view conversation)))
    (chirp-media-xchat-avatar-resource-key
     (plist-get participant :id)
     (plist-get participant :avatar-url))))

(defun chirp-dm-inbox--state (view)
  "Return VIEW's validated XChat inbox state."
  (let ((state (appkit-view-state view)))
    (unless (and (listp state)
                 (eq (plist-get state :type) 'dm-inbox)
                 (plist-get state :instance))
      (error "Invalid Chirp direct-message inbox state"))
    state))

(defun chirp-dm-inbox--current-view ()
  "Return the current live direct-message inbox view, or nil."
  (when-let* ((view (appkit-current-view))
              ((appkit-view-live-p view))
              (state (appkit-view-state view))
              ((eq (plist-get state :type) 'dm-inbox)))
    view))



(defun chirp-dm-inbox--make-state (instance)
  "Return canonical inbox state for INSTANCE."
  (list :type 'dm-inbox
        :instance instance
        :items nil
        :page (list :next-cursor nil :exhausted-p nil)
        :status (list :phase 'initial :message nil)
        :generation nil))

;;;; Projection

(defun chirp-dm-inbox--status-entry (state)
  "Return the passive status directory entry for inbox STATE, or nil."
  (let* ((status (chirp-dm-inbox--status state))
         (phase (plist-get status :phase))
         (message (plist-get status :message))
         (items (plist-get state :items))
         text face)
    (pcase phase
      ('initial (setq text "Loading conversations…"))
      ('refresh (setq text "Refreshing conversations…"))
      ('older (setq text "Loading older conversations…"))
      ('error (setq text (format "Unable to load conversations: %s" message)
                    face 'error))
      (_ (when (null items)
           (setq text "No conversations returned."))))
    (when text
      (appkit-directory-entry-create
       :key '(dm-inbox status)
       :role 'note
       :label text
       :face face
       :stamp (list phase message (null items))))))

(defun chirp-dm-inbox--conversation-entry (view conversation)
  "Adapt normalized CONVERSATION to an Appkit directory entry for VIEW."
  (when-let* ((participant
               (chirp-dm-inbox--participant-for-row view conversation)))
    (chirp-media-request-xchat-avatar-resource
     view
     (plist-get participant :id)
     (plist-get participant :avatar-url)))
  (appkit-directory-entry-create
   :key (list 'dm-conversation (plist-get conversation :id))
   :role 'item
   :section-key '(dm-inbox recent)
   :label (chirp-dm-inbox--conversation-title view conversation)
   :primary-action 'item
   :item-p t
   :payload conversation
   :stamp (list conversation (chirp-dm-inbox--view-user-id view))
   :help-echo "Open this conversation"
   :mouse-face 'highlight))

(defun chirp-dm-inbox--project (view state)
  "Project canonical inbox STATE into recent-session entries for VIEW."
  (let* ((items (plist-get state :items))
         (count (length items))
         (requests
          (cl-count-if
           (lambda (item) (plist-get item :message-request-p))
           items))
         (muted
          (cl-count-if
           (lambda (item) (plist-get item :muted-p))
           items)))
    (append
     (when items
       (list
        (appkit-directory-entry-create
         :key '(dm-inbox summary)
         :role 'note
         :label
         (format "%d conversation%s · %d request%s · %d muted"
                 count (if (= count 1) "" "s")
                 requests (if (= requests 1) "" "s")
                 muted)
         :face 'font-lock-doc-face
         :stamp (list count requests muted))
        (appkit-directory-entry-create
         :key '(dm-inbox recent)
         :role 'section
         :label "Recent Conversations"
         :face 'bold)))
     (mapcar
      (lambda (conversation)
        (chirp-dm-inbox--conversation-entry view conversation))
      items)
     (when-let* ((status-entry (chirp-dm-inbox--status-entry state)))
       (list status-entry)))))

(defun chirp-dm-inbox--preview-model (view conversation)
  "Return an activity-style one-line preview for CONVERSATION in VIEW."
  (let* ((latest (plist-get conversation :latest-event))
         (sender-id (and latest (plist-get latest :sender-id)))
         (sender
          (and sender-id
               (chirp-dm-inbox--participant conversation sender-id)))
         (self-id (chirp-dm-inbox--view-user-id view))
         (label
          (when (and latest
                     (eq (plist-get latest :kind) 'message))
            (cond
             ((and self-id (equal sender-id self-id)) "You")
             ((eq (plist-get conversation :type) 'group)
              (or (chirp-dm-inbox--participant-label sender)
                  "Unknown sender")))))
         (preview (chirp-dm-inbox--one-line (plist-get conversation :preview))))
    (appkit-ui-one-line-preview-create
     :label label
     :separator (and label ":")
     :label-face (and label (or (appkit-name-color-face sender-id) 'bold))
     :text preview)))

(defun chirp-dm-inbox--context-trail (conversation)
  "Return status trail displayed beside CONVERSATION's inbox title."
  (string-join
   (delq nil
         (list
          (when (plist-get conversation :message-request-p)
            (propertize "request" 'face 'warning))
          (when (plist-get conversation :muted-p)
            (propertize "muted" 'face 'shadow))))
   " "))

(defun chirp-dm-inbox--insert-avatar (view conversation)
  "Insert CONVERSATION's avatar or type fallback for inbox VIEW."
  (let* ((resource-key
          (and view (chirp-dm-inbox--avatar-key view conversation)))
         (image
          (and resource-key
               (chirp-media-avatar-resource-image view resource-key)))
         (fallback (if (eq (plist-get conversation :type) 'group) "#" "@"))
         (start (point)))
    (if image
        (insert-image image fallback)
      (insert fallback))
    (add-text-properties
     start (point)
     (list 'face (if resource-key 'default 'shadow)
           'help-echo
           (if resource-key "Participant avatar"
             (if (eq (plist-get conversation :type) 'group)
                 "Group conversation"
               "Direct conversation"))))))

(defun chirp-dm-inbox--insert-item (_surface entry)
  "Insert one XChat inbox directory ENTRY."
  (let* ((conversation (appkit-directory-entry-payload entry))
         (view (appkit-current-view)))
    (appkit-view-insert-one-line-row
     (appkit-view-one-line-row-create
      :icon-inserter
      (lambda () (chirp-dm-inbox--insert-avatar view conversation))
      :context (appkit-directory-entry-label entry)
      :context-trail (chirp-dm-inbox--context-trail conversation)
      :preview (chirp-dm-inbox--preview-model view conversation)
      :time (chirp-dm-inbox--format-time
             (plist-get conversation :updated-at-msec))
      :time-face 'shadow
      :time-tail-face nil
      :line-properties
      (list 'chirp-dm-conversation-id (plist-get conversation :id)
            'chirp-dm-message-request-p
            (and (plist-get conversation :message-request-p) t)
            'chirp-dm-muted-p (and (plist-get conversation :muted-p) t)))
     :indent 2
     :width (chirp--view-width)
     :icon-slot-width chirp-dm-inbox--icon-slot-width
     :context-width-spec '(0.34 18 36))))

(defun chirp-dm-inbox--activate-item (_surface entry)
  "Open the conversation carried by inbox directory ENTRY."
  (let ((conversation
         (copy-sequence (appkit-directory-entry-payload entry))))
    (setf (plist-get conversation :title)
          (or (appkit-directory-entry-label entry)
              (plist-get conversation :title)))
    (chirp-dm-conversation-open conversation :refresh-p t)))

(defun chirp-dm-inbox--sync (view invalidations)
  "Synchronize inbox VIEW for pending INVALIDATIONS."
  (let* ((state (chirp-dm-inbox--state view))
         (entries (chirp-dm-inbox--project view state))
         (resources (appkit-invalidations-resource-keys invalidations))
         (resource-keys
          (cl-loop
           for conversation in (plist-get state :items)
           for avatar-key = (chirp-dm-inbox--avatar-key view conversation)
           when (and avatar-key
                     (cl-member avatar-key resources :test #'equal))
           collect (list 'dm-conversation (plist-get conversation :id))))
         (force-keys
          (delete-dups
           (append
            resource-keys
            (when (memq 'geometry
                        (appkit-invalidations-parts invalidations))
              (mapcar #'appkit-directory-entry-key entries))))))
    (appkit-directory-reconcile
     (appkit-directory-surface) entries :force-keys force-keys)))

(defun chirp-dm-inbox--setup (view)
  "Initialize Appkit directory adapters for inbox VIEW."
  (appkit-view-enable-responsive-geometry view)
  (setq-local chirp--view-title "Direct Messages")
  (setq-local header-line-format nil)
  (appkit-directory-configure
   (appkit-directory-surface)
   :item-inserter #'chirp-dm-inbox--insert-item
   :activate-function #'chirp-dm-inbox--activate-item
   :action-rows-p t)
  (appkit-invalidate view :structure t :part 'entries :position t)
  (appkit-sync-invalidations view))

;;;; Requests

(defun chirp-dm-inbox--append-unique (current fetched key-function)
  "Append unique FETCHED values to CURRENT using KEY-FUNCTION."
  (let ((seen (make-hash-table :test #'equal))
        additions)
    (dolist (item current)
      (puthash (funcall key-function item) t seen))
    (dolist (item fetched)
      (let ((key (funcall key-function item)))
        (unless (gethash key seen)
          (puthash key t seen)
          (push item additions))))
    (append current (nreverse additions))))

(defun chirp-dm-inbox--settle-success
    (view state generation phase conversations envelope)
  "Settle inbox GENERATION and PHASE with CONVERSATIONS and ENVELOPE.

VIEW and STATE identify the inbox whose request is completing."
  (when (chirp-view-state-token-current-p view state generation)
    (let* ((page (plist-get state :page))
           (status (chirp-dm-inbox--status state))
           (canonical
            (mapcar #'chirp-dm-state-acquire conversations))
           (next-cursor (chirp-backend-envelope-next-cursor envelope)))
      (setf (plist-get state :items)
            (if (eq phase 'older)
                (chirp-dm-inbox--append-unique
                 (plist-get state :items) canonical
                 (lambda (item) (plist-get item :id)))
              canonical)
            (plist-get page :next-cursor) next-cursor
            (plist-get page :exhausted-p) (not next-cursor)
            (plist-get status :phase) 'idle
            (plist-get status :message) nil
            (plist-get state :generation) nil)
      (appkit-request-sync view :structure t :part 'entries :position t)
      (dolist (conversation canonical)
        (chirp-dm-state-publish conversation)))))

(defun chirp-dm-inbox--settle-error (view state generation message)
  "Settle inbox GENERATION in VIEW and STATE with error MESSAGE."
  (when (chirp-view-state-token-current-p view state generation)
    (let ((status (chirp-dm-inbox--status state)))
      (setf (plist-get status :phase) 'error
            (plist-get status :message) message
            (plist-get state :generation) nil)
      (appkit-request-sync view :structure t :part 'entries :position t)
      (message "%s" (replace-regexp-in-string "[\r\n]+" "  " message)))))

(defun chirp-dm-inbox--operation-current-p
    (view state generation operation)
  "Return non-nil when GENERATION and OPERATION may update VIEW and STATE."
  (and (chirp-view-state-token-current-p view state generation)
       (appkit-view-operation-current-p operation)))

(defun chirp-dm-inbox--request (view phase)
  "Start inbox request PHASE owned by VIEW."
  (let* ((state (chirp-dm-inbox--state view))
         (page (plist-get state :page))
         (status (chirp-dm-inbox--status state))
         (generation (list 'dm-inbox-generation))
         (operation
          (appkit-view-operation-begin
           view chirp-dm-inbox--request-key
           :cancel-function #'chirp-x-cancel-request))
         request)
    (setf (plist-get state :generation) generation
          (plist-get status :phase) phase
          (plist-get status :message) nil)
    (appkit-request-sync view :part 'entries :position t)
    (setq request
          (chirp-backend-dm-inbox
           (lambda (conversations envelope)
             (when (chirp-dm-inbox--operation-current-p
                    view state generation operation)
               (appkit-view-operation-finish operation)
               (chirp-dm-inbox--settle-success
                view state generation phase conversations envelope)))
           :cursor (and (eq phase 'older)
                        (plist-get page :next-cursor))
           :max-results chirp-dm-inbox-page-size
           :errback
           (lambda (text)
             (when (chirp-dm-inbox--operation-current-p
                    view state generation operation)
               (appkit-view-operation-finish operation)
               (chirp-dm-inbox--settle-error
                view state generation text)))
           :owner view))
    (appkit-view-operation-bind operation request)
    (when (and (null request)
               (chirp-dm-inbox--operation-current-p
                view state generation operation))
      (appkit-view-operation-finish operation)
      (chirp-dm-inbox--settle-error
       view state generation "XChat inbox request did not start"))
    request))

(defun chirp-dm-inbox-refresh-live-view (view)
  "Start a fallback live refresh for inbox VIEW when it is idle.

Return non-nil when the refresh was accepted."
  (when (and (appkit-view-live-p view)
             (eq (plist-get (appkit-view-state view) :type) 'dm-inbox)
             (null (plist-get (appkit-view-state view) :generation)))
    (chirp-dm-inbox--request view 'refresh)
    t))

(defun chirp-dm-refresh-inbox ()
  "Refresh the current XChat inbox without sending a read acknowledgment."
  (interactive)
  (if-let* ((view (chirp-dm-inbox--current-view)))
      (chirp-dm-inbox--request view 'refresh)
    (user-error "Current view is not a direct-message inbox")))

(defun chirp-dm-load-more-inbox ()
  "Load one older page in the current XChat inbox."
  (interactive)
  (if-let* ((view (chirp-dm-inbox--current-view)))
      (let* ((state (chirp-dm-inbox--state view))
             (page (plist-get state :page)))
        (cond
         ((plist-get state :generation)
          (user-error "A direct-message inbox request is already running"))
         ((plist-get page :exhausted-p)
          (user-error "No older conversations available"))
         ((null (plist-get page :next-cursor))
          (user-error "Direct-message inbox cursor is unavailable"))
         (t
          (chirp-dm-inbox--request view 'older))))
    (user-error "Current view is not a direct-message inbox")))

;;; Inbox Mode

(defvar-keymap chirp-dm-inbox--mode-map
  :doc "Keymap for `chirp-dm-inbox--mode'."
  :parent appkit-directory-mode-map
  "g" #'chirp-dm-refresh-inbox
  "N" #'chirp-dm-load-more-inbox
  "q" #'chirp-quit-current-buffer)

(define-derived-mode chirp-dm-inbox--mode appkit-directory-mode "Chirp-DMs"
  "Major mode for Chirp's Appkit-owned XChat inbox."
  (setq-local revert-buffer-function
              (lambda (&rest _ignored)
                (chirp-dm-refresh-inbox))))

(defun chirp-dm-inbox-open ()
  "Open an unlocked read-only XChat inbox and return its buffer."
  (let* ((instance (cl-incf chirp-dm-inbox--next-instance))
         (view
          (appkit-open-view
           :app (chirp-app)
           :id (list 'dm-inbox instance)
           :mode 'chirp-dm-inbox--mode
           :buffer-name (chirp--format-buffer-name "Direct Messages")
           :state (chirp-dm-inbox--make-state instance)
           :sync-function #'chirp-dm-inbox--sync
           :parts '(entries geometry)
           :position-policy appkit-directory-key-property
           :setup #'chirp-dm-inbox--setup
           :select t)))
    (chirp-dm-inbox--request view 'initial)
    (appkit-view-buffer view)))

(provide 'chirp-dm-inbox)

;;; chirp-dm-inbox.el ends here
