;;; chirp-dm-conversation.el --- XChat conversation lifecycle -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Appkit history, decryption, composer, and send lifecycle for XChat conversations.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'appkit-core)
(require 'appkit-compose)
(require 'appkit-chatbuf)
(require 'appkit-media)
(require 'appkit-ui)
(require 'appkit-chat-history)
(require 'appkit-chat-timeline)
(require 'appkit-scroll)
(require 'appkit-evil)
(require 'appkit-invalidation)
(require 'appkit-markup-codec)
(require 'appkit-markup-codecs)
(require 'appkit-markup-compose)
(require 'appkit-view)
(require 'chirp-backend)
(require 'chirp-core)
(require 'chirp-dm-render)
(require 'chirp-dm-state)
(require 'chirp-x)
(require 'chirp-xchat)

(declare-function chirp-xchat-native-decrypt-events
                  "chirp-xchat-native" (conversation-id events signing-keys))

;;; Implementation
(defcustom chirp-dm-history-page-size 200
  "Number of older XChat events requested per conversation page, up to 200."
  :type '(integer 1 200)
  :group 'chirp)

(defcustom chirp-dm-history-auto-load-threshold 2000
  "Character distance from the visible history start that loads older events.

Set this to nil to disable automatic pagination.  Manual loading with
`chirp-dm-load-older-messages' remains available."
  :type '(choice (const :tag "Disable automatic pagination" nil)
                 integer)
  :group 'chirp)

(defcustom chirp-dm-attach-commands
  '(("photo" . chirp-dm-attach-photo)
    ("video" . chirp-dm-attach-video)
    ("audio" . chirp-dm-attach-audio)
    ("file" . chirp-dm-attach-file)
    ("gif" . chirp-dm-attach-gif))
  "Attachment type candidates offered by `chirp-dm-attach'."
  :type '(alist :key-type string :value-type function)
  :group 'chirp)

;;; Variables

(defvar chirp-dm-conversation--next-instance 0
  "Monotonic identity source for fresh direct-message views.")

;;; Constants


(defconst chirp-dm-conversation--decrypt-request-key 'dm-decrypt
  "Operation key for one conversation view's signing-key retrieval.")

(defconst chirp-dm-conversation--refresh-bridge-page-limit 10
  "Maximum history pages fetched to join one focused refresh fragment.")

(defun chirp-dm-conversation--one-line (text)
  "Return TEXT collapsed into one trimmed display line."
  (string-trim
   (replace-regexp-in-string "[[:space:]\n\r]+" " " (or text ""))))

(defun chirp-dm-conversation--status (state)
  "Return canonical status plist from conversation STATE."
  (or (plist-get state :status)
      (error "Direct-message conversation state has no status")))

(defun chirp-dm-conversation--title (conversation fallback)
  "Return CONVERSATION's normalized title, or FALLBACK when empty."
  (let ((title
         (chirp-dm-conversation--one-line
          (plist-get conversation :title))))
    (if (string-empty-p title) fallback title)))

(defun chirp-dm-conversation--conversation (state)
  "Return canonical conversation referenced by view STATE."
  (let ((conversation (plist-get state :conversation)))
    (unless (and (listp conversation)
                 (stringp (plist-get conversation :id)))
      (error "Invalid Chirp canonical direct-message conversation"))
    conversation))

(defun chirp-dm-conversation--id (state)
  "Return canonical conversation identity from view STATE."
  (plist-get (chirp-dm-conversation--conversation state) :id))

(defun chirp-dm-conversation--events (state)
  "Return canonical ordered events from view STATE."
  (plist-get (chirp-dm-conversation--conversation state) :events))

(defun chirp-dm-conversation--state (view)
  "Return VIEW's validated XChat conversation state."
  (let ((state (appkit-view-state view)))
    (unless (and (listp state)
                 (eq (plist-get state :type) 'dm-conversation)
                 (plist-get state :instance)
                 (listp (plist-get state :conversation)))
      (error "Invalid Chirp direct-message conversation state"))
    (chirp-dm-conversation--conversation state)
    state))

(defun chirp-dm-conversation--current-view ()
  "Return the current live direct-message conversation view, or nil."
  (when-let* ((view (appkit-current-view))
              ((appkit-view-live-p view))
              (state (appkit-view-state view))
              ((eq (plist-get state :type) 'dm-conversation)))
    view))

(defun chirp-dm-conversation--append-unique-events (current fetched)
  "Append unique FETCHED events to CURRENT by event identity."
  (let ((seen (make-hash-table :test #'equal))
        additions)
    (dolist (event current)
      (puthash (or (plist-get event :id)
                   (plist-get event :encoded-event))
               t seen))
    (dolist (event fetched)
      (let ((identity
             (or (plist-get event :id)
                 (plist-get event :encoded-event))))
        (unless (gethash identity seen)
          (puthash identity t seen)
          (push event additions))))
    (append current (nreverse additions))))


(defun chirp-dm-conversation--events-overlap-p (left right)
  "Return non-nil when event lists LEFT and RIGHT share an identity."
  (let ((ids (make-hash-table :test #'equal)))
    (dolist (event left)
      (puthash (plist-get event :id) t ids))
    (cl-some (lambda (event)
               (gethash (plist-get event :id) ids))
             right)))

(defun chirp-dm-conversation--history-key-events (envelope)
  "Return normalized recovery key events carried by history ENVELOPE."
  (or (chirp-get envelope "keyEvents")
      (mapcar
       (lambda (encoded)
         (list :id encoded :encoded-event encoded))
       (or (chirp-get envelope "encodedKeyEvents") nil))))


;;; Decryption and Recovery

(defun chirp-dm-conversation--native-key-epoch ()
  "Return the current native session epoch used for key ingestion."
  (or (chirp--session-xchat-native-epoch (chirp--session))
      'test-native-session))

(defun chirp-dm-conversation--pending-key-event-p (event epoch)
  "Return non-nil when key EVENT has not been ingested for EPOCH."
  (not (eql (plist-get event :native-key-epoch) epoch)))

(defun chirp-dm-conversation--decrypt-input (state)
  "Return encoded events and signing-key user IDs from conversation STATE."
  (let* ((conversation (chirp-dm-conversation--conversation state))
         (events (plist-get conversation :events))
         (recovery-events (plist-get state :recovery-key-events))
         (epoch (chirp-dm-conversation--native-key-epoch))
         (encoded
          (delete-dups
           (delq nil
                 (append
                  (cl-loop
                   for event in recovery-events
                   when
                   (chirp-dm-conversation--pending-key-event-p event epoch)
                   collect (plist-get event :encoded-event))
                  (cl-loop
                   for event in events
                   when (or (plist-get event :encrypted-p)
                            (and
                             (eq (plist-get event :kind)
                                 'conversation-key-change)
                             (chirp-dm-conversation--pending-key-event-p
                              event epoch)))
                   collect (plist-get event :encoded-event))))))
         (user-ids
          (delete-dups
           (delq nil
                 (append
                  (mapcar (lambda (participant)
                            (plist-get participant :id))
                          (plist-get conversation :participants))
                  (mapcar (lambda (event)
                            (plist-get event :sender-id))
                          (append recovery-events events)))))))
    (list :events encoded
          :user-ids
          (cl-remove-if-not
           (lambda (id)
             (and (stringp id)
                  (string-match-p "\\`[0-9]+\\'" id)))
           user-ids))))

(defun chirp-dm-conversation--decryption-needed-p (state)
  "Return non-nil when conversation STATE has pending ciphertext or keys."
  (let ((epoch (chirp-dm-conversation--native-key-epoch)))
    (or
     (cl-some
      (lambda (event)
        (or (plist-get event :encrypted-p)
            (and
             (eq (plist-get event :kind) 'conversation-key-change)
             (chirp-dm-conversation--pending-key-event-p event epoch))))
      (chirp-dm-conversation--events state))
     (cl-some
      (lambda (event)
        (chirp-dm-conversation--pending-key-event-p event epoch))
      (plist-get state :recovery-key-events)))))

(defun chirp-dm-conversation--decode-plain-text (text)
  "Decode plain TEXT into an immutable Appkit semantic document."
  (when (stringp text)
    (appkit-markup-parse-result-document
     (appkit-markup-parse 'plain text))))

(defun chirp-dm-conversation--verified-attachments
    (attachments conversation-id message-id)
  "Add stable resource identities to verified ATTACHMENTS.

CONVERSATION-ID and MESSAGE-ID identify their owning message."
  (cl-loop for attachment in attachments
           for index from 0
           collect
           (let ((copy (copy-sequence attachment)))
             (setf (plist-get copy :resource-key)
                   (list 'xchat-media conversation-id message-id index))
             copy)))

(defun chirp-dm-conversation--apply-verified-messages (state messages)
  "Apply decoded verified MESSAGES to canonical conversation STATE."
  (let* ((conversation (chirp-dm-conversation--conversation state))
         (conversation-id (plist-get conversation :id))
         (by-id (make-hash-table :test #'equal))
         (updated 0))
    (dolist (message messages)
      (when (equal (plist-get message :conversation-id) conversation-id)
        (dolist (id (list (plist-get message :sequence-id)
                          (plist-get message :message-id)))
          (when id
            (puthash id message by-id)))))
    (chirp-dm-state-set-events
     conversation
     (mapcar
      (lambda (event)
        (if-let* ((message
                   (or (gethash (plist-get event :sequence-id) by-id)
                       (gethash (plist-get event :message-id) by-id)))
                  ((or (null (plist-get message :sender-id))
                       (equal (plist-get message :sender-id)
                              (plist-get event :sender-id)))))
            (let* ((copy (copy-sequence event))
                   (content-kind (plist-get message :content-kind))
                   (text (plist-get message :text))
                   (reply-text (plist-get message :reply-text))
                   (message-id
                    (or (plist-get message :message-id)
                        (plist-get message :sequence-id)))
                   (attachments
                    (chirp-dm-conversation--verified-attachments
                     (plist-get message :attachments)
                     conversation-id message-id)))
              (setf (plist-get copy :kind)
                    (pcase content-kind
                      ('mark-read 'mark-read)
                      ('mark-unread 'mark-unread)
                      (_ (plist-get copy :kind)))
                    (plist-get copy :content-kind) content-kind
                    (plist-get copy :text) text
                    (plist-get copy :target-message-id)
                    (plist-get message :target-message-id)
                    (plist-get copy :document)
                    (chirp-dm-conversation--decode-plain-text text)
                    (plist-get copy :attachments) attachments
                    (plist-get copy :attachment-count) (length attachments)
                    (plist-get copy :reply-p)
                    (plist-get message :reply-p)
                    (plist-get copy :reply-text) reply-text
                    (plist-get copy :reply-document)
                    (chirp-dm-conversation--decode-plain-text reply-text)
                    (plist-get copy :reply-attachment-count)
                    (plist-get message :reply-attachment-count)
                    (plist-get copy :key-version)
                    (plist-get message :key-version)
                    (plist-get copy :encrypted-p) nil
                    (plist-get copy :decrypted-p) t)
              (cl-incf updated)
              copy)
          event))
      (plist-get conversation :events)))
    updated))

(defun chirp-dm-conversation--mark-key-events-processed
    (state encoded epoch)
  "Mark key events from STATE present in ENCODED as ingested for EPOCH."
  (setf
   (plist-get state :recovery-key-events)
   (mapcar
    (lambda (event)
      (if (member (plist-get event :encoded-event) encoded)
          (plist-put event :native-key-epoch epoch)
        event))
    (plist-get state :recovery-key-events)))
  (let ((conversation (chirp-dm-conversation--conversation state)))
    (chirp-dm-state-set-events
     conversation
     (mapcar
      (lambda (event)
        (if (and (eq (plist-get event :kind) 'conversation-key-change)
                 (member (plist-get event :encoded-event) encoded))
            (plist-put event :native-key-epoch epoch)
          event))
      (plist-get conversation :events)))))

(defun chirp-dm-conversation--settle-decrypt-error (state message)
  "Settle STATE's decryption request with error MESSAGE."
  (setf (plist-get state :decrypt-loading-p) nil)
  (display-warning 'chirp message :warning))

(defun chirp-dm-conversation--settle-decrypt-success
    (view state encoded signing-keys)
  "Decrypt ENCODED events with SIGNING-KEYS for VIEW STATE."
  (condition-case err
      (let ((messages
             (cl-loop for batch in (seq-partition encoded 200)
                      append
                      (chirp-xchat-native-decrypt-events
                       (chirp-dm-conversation--id state)
                       batch signing-keys))))
        (setf (plist-get state :decrypt-loading-p) nil)
        (chirp-dm-conversation--mark-key-events-processed
         state encoded (chirp-dm-conversation--native-key-epoch))
        (let ((updated
               (chirp-dm-conversation--apply-verified-messages
                state messages)))
          (when (> updated 0)
            (chirp-dm-state-publish
             (chirp-dm-conversation--conversation state))
            (message "Decrypted %d verified XChat message%s"
                     updated (if (= updated 1) "" "s"))))
        (let ((remaining
               (plist-get
                (chirp-dm-conversation--decrypt-input state) :events)))
          (when (cl-set-difference remaining encoded :test #'equal)
            (chirp-dm-conversation--decrypt-view view t))))
    (error
     (chirp-dm-conversation--settle-decrypt-error
      state (error-message-string err)))))


(defun chirp-dm-conversation--decrypt-view (view &optional key-history-loaded-p)
  "Decrypt encrypted events in conversation VIEW.

Unless KEY-HISTORY-LOADED-P is non-nil, fetch one older page first when the
view has no conversation-key event."
  (when (appkit-view-live-p view)
    (let* ((state (chirp-dm-conversation--state view))
           (input (chirp-dm-conversation--decrypt-input state))
           (encoded (plist-get input :events))
           (user-ids (plist-get input :user-ids))
           (key-event-p
            (cl-some
             (lambda (event)
               (eq (plist-get event :kind) 'conversation-key-change))
             (append (plist-get state :recovery-key-events)
                     (chirp-dm-conversation--events state)))))
      (cond
       ((null encoded)
        (message "This conversation has no encoded XChat events"))
       ((and (not key-event-p)
             (not key-history-loaded-p)
             (plist-get state :older-available-p)
             (plist-get state :older-cursor)
             (not (with-current-buffer (appkit-view-buffer view)
                    (appkit-chat-history-loading-p))))
        (message "Loading XChat conversation-key history...")
        (chirp-dm-conversation--request view 'older))
       ((null user-ids)
        (display-warning 'chirp "XChat signing-key users are unavailable" :warning))
       ((> (length user-ids) 100)
        (display-warning 'chirp "XChat conversation has too many signing-key users" :warning))
       (t
        (let ((operation
               (appkit-view-operation-begin
                view chirp-dm-conversation--decrypt-request-key)))
          (setf (plist-get state :decrypt-loading-p) t)
          (chirp-backend-dm-signing-keys
           user-ids
           (lambda (signing-keys _envelope)
             (when (appkit-view-operation-finish operation)
               (chirp-dm-conversation--settle-decrypt-success
                view state encoded signing-keys)))
           :errback
           (lambda (message)
             (when (appkit-view-operation-finish operation)
               (chirp-dm-conversation--settle-decrypt-error
                state message)))
           :owner operation)))))))

(defun chirp-dm-conversation-accept-live-event (view conversation)
  "Process a canonical live update for CONVERSATION through matching VIEW.

Return non-nil when VIEW represents CONVERSATION.  At most one decryption
request remains active; a later live event is picked up after it settles."
  (when (appkit-view-live-p view)
    (let ((state (appkit-view-state view)))
      (when (and (eq (plist-get state :type) 'dm-conversation)
                 (eq (plist-get state :conversation) conversation))
        (when (and (chirp-dm-conversation--decryption-needed-p state)
                   (not (plist-get state :decrypt-loading-p)))
          (chirp-dm-conversation--decrypt-view view t))
        t))))

(defun chirp-dm-conversation-refresh-live-view (view)
  "Start a fallback live refresh for conversation VIEW when it is idle.

Return non-nil when the refresh was accepted."
  (when (and (appkit-view-live-p view)
             (eq (plist-get (appkit-view-state view) :type) 'dm-conversation)
             (with-current-buffer (appkit-view-buffer view)
               (not (appkit-chat-history-loading-p))))
    (chirp-dm-conversation--request view 'refresh)
    t))

;;; Conversation

(defun chirp-dm-conversation--make-state (instance conversation)
  "Return view state for INSTANCE and normalized CONVERSATION."
  (let ((canonical (chirp-dm-state-acquire conversation)))
    (list :type 'dm-conversation
          :instance instance
          :conversation canonical
          :recovery-key-events nil
          :older-cursor (copy-tree (plist-get canonical :older-cursor))
          :older-available-p (plist-get canonical :has-more)
          :older-stalled-p nil
          :status (list :phase 'idle :message nil)
          :decrypt-loading-p nil
          :send-error nil)))

;;;; Rendering


;;;; Composer

(defun chirp-dm-conversation--composer-prompt ()
  "Return the Appkit composer prompt for the current operation."
  (if (appkit-compose-operation-active-p) "…> " ">>> "))

(defun chirp-dm-conversation--bind-composer (_state)
  "Create the trailing Appkit composer."
  (appkit-chatbuf-bind-input-region
   :visible-p t
   :prompt (chirp-dm-conversation--composer-prompt)
   :input-text (appkit-chatbuf-input-state)))

(defun chirp-dm-conversation--sync-composer-state (_state)
  "Project Appkit's compose operation without rebuilding input."
  (setq buffer-read-only nil)
  (appkit-chatbuf-prompt-update (chirp-dm-conversation--composer-prompt))
  (appkit-chatbuf-input-apply-text-properties)
  (when (appkit-compose-operation-active-p)
    (setq buffer-read-only t)))

(defun chirp-dm-conversation--message-by-id (state message-id)
  "Return MESSAGE-ID from canonical conversation STATE, or nil."
  (cl-find-if
   (lambda (event)
     (or (equal (plist-get event :sequence-id) message-id)
         (equal (plist-get event :id) message-id)))
   (chirp-dm-conversation--events state)))

(defun chirp-dm-conversation--reply-target (state)
  "Return STATE's current Appkit reply target event, or nil."
  (let ((aux (appkit-chatbuf-aux-state)))
    (when (eq (plist-get aux :aux-type) 'reply)
      (chirp-dm-conversation--message-by-id
       state (plist-get aux :message-id)))))

(defun chirp-dm-conversation--event-sender-label (conversation event)
  "Return EVENT's sender label in CONVERSATION."
  (let ((participant
         (cl-find (plist-get event :sender-id)
                  (plist-get conversation :participants)
                  :key (lambda (item) (plist-get item :id))
                  :test #'equal)))
    (or (plist-get participant :name)
        (and-let* ((handle (plist-get participant :handle)))
          (concat "@" handle))
        (and (equal (plist-get event :sender-id)
                    (chirp--session-xchat-user-id (chirp--session)))
             "You")
        "message")))

(defun chirp-dm-conversation--reply-context-text (state)
  "Return Appkit reply context chrome for conversation STATE."
  (if-let* ((target (chirp-dm-conversation--reply-target state)))
      (let* ((conversation (chirp-dm-conversation--conversation state))
             (document (plist-get target :document))
             (preview
              (chirp-dm-conversation--one-line
               (if document (appkit-markup-plain-text document) ""))))
        (appkit-chatbuf-aux-render
         :title
         (format "Reply to %s"
                 (chirp-dm-conversation--event-sender-label
                  conversation target))
         :preview
         (appkit-ui-one-line-preview-create
          :text
          (cond
           ((not (string-empty-p preview))
            (truncate-string-to-width preview 80 nil nil "…"))
           ((> (or (plist-get target :attachment-count) 0) 0)
            "[Attachment message]")
           (t "[Message preview unavailable]")))
         :cancel-action #'chirp-dm-cancel-reply
         :cancel-help "Cancel reply (C-c C-k)"
         :width (max 20 (min 80 (window-width)))))
    ""))

(defun chirp-dm-conversation--reply-key-events (state target)
  "Return bounded raw key events from STATE needed to validate TARGET."
  (let* ((version
          (or (plist-get target :conversation-key-version)
              (plist-get target :key-version)))
         (events
          (append (plist-get state :recovery-key-events)
                  (cl-remove-if-not
                   (lambda (event)
                     (eq (plist-get event :kind)
                         'conversation-key-change))
                   (chirp-dm-conversation--events state))))
         (ordered
          (append
           (cl-remove-if-not
            (lambda (event)
              (and version
                   (equal (plist-get event :conversation-key-version)
                          version)))
            events)
           events)))
    (seq-take
     (delete-dups
      (delq nil
            (mapcar (lambda (event)
                      (plist-get event :encoded-event))
                    ordered)))
     64)))

(defun chirp-dm-conversation--maybe-auto-load-older
    (view window position start)
  "Load older events for VIEW when WINDOW's POSITION approaches START."
  (ignore window)
  (when (appkit-view-live-p view)
    (with-current-buffer (appkit-view-buffer view)
      (let* ((state (chirp-dm-conversation--state view))
             (status (chirp-dm-conversation--status state)))
        (when (and (eq (plist-get status :phase) 'idle)
                   (plist-get state :older-cursor)
                   (not (plist-get state :older-stalled-p))
                   (appkit-chat-history-autoload-older-p
                    position start chirp-dm-history-auto-load-threshold))
          (chirp-dm-conversation--request view 'older))))))

(defun chirp-dm-conversation--sync (view invalidations _events)
  "Synchronize conversation VIEW for pending INVALIDATIONS."
  (let* ((state (chirp-dm-conversation--state view))
         (conversation (chirp-dm-conversation--conversation state))
         (title
          (chirp-dm-conversation--one-line
           (plist-get conversation :title)))
         (diff
          (appkit-projection-diff-derive
           invalidations
           :existing-keys
           (and (appkit-chat-timeline-live-p)
                (appkit-chat-timeline-keys))
           :reconcile-parts '(timeline)))
         (slice
          (appkit-chat-history-window-slice
           (plist-get conversation :events)
           (lambda (event) (plist-get event :id))))
         (rows
          (and (appkit-projection-diff-reconcile-p diff)
               (plist-get slice :valid-p)
               (chirp-dm-render-project-events
                view conversation (plist-get slice :entries)))))
    (unless (plist-get slice :valid-p)
      (error "Invalid XChat history window: %s" (plist-get slice :reason)))
    (setq-local chirp--view-title
                (format "DM: %s"
                        (if (string-empty-p title) "Conversation" title)))
    (appkit-chat-timeline-run-preserving-position
     (lambda ()
       (when (appkit-projection-diff-reconcile-p diff)
         (appkit-chat-timeline-sync
          rows
          :force-keys (appkit-projection-diff-force-keys diff)
          :changed-resources
          (appkit-projection-diff-changed-dependencies diff)))
       (appkit-chat-timeline-set-frame
        (chirp-dm-render-header conversation)
        (concat (chirp-dm-render-footer state)
                (chirp-dm-conversation--reply-context-text state))
        :bind-input-function
        (lambda () (chirp-dm-conversation--bind-composer state))
        :composer-visible-p t)
       (chirp-dm-conversation--sync-composer-state state)))
    (when-let* ((observer (appkit-chat-timeline-scroll-observer)))
      (appkit-scroll-observer-check observer))))

(defun chirp-dm-conversation--establish-window (state)
  "Establish Appkit's exact history window from conversation STATE."
  (let ((events (chirp-dm-conversation--events state)))
    (if events
        (progn
          (appkit-chat-history-window-set
           (plist-get (car events) :id) nil)
          (appkit-chat-history-older-loaded-set
           (not (plist-get state :older-available-p))))
      (appkit-chat-history-window-establish-empty)
      (when (plist-get state :older-available-p)
        (let ((status (chirp-dm-conversation--status state)))
          (setf (plist-get status :phase) 'error
                (plist-get status :message)
                "XChat did not provide a history cursor"))))))

(defun chirp-dm-conversation--ensure-timeline ()
  "Ensure the current conversation owns one Appkit chat timeline."
  (appkit-chat-timeline-ensure
   :printer #'chirp-dm-render-print-event-row
   :anchor-property 'chirp-dm-message-id
   :header ""
   :footer ""))

(defun chirp-dm-conversation--setup (view)
  "Initialize Appkit chat controllers for conversation VIEW."
  (appkit-view-enable-responsive-geometry view)
  (let ((state (chirp-dm-conversation--state view)))
    (appkit-chatbuf-reset-state)
    (appkit-chat-history-reset-state)
    (chirp-dm-conversation--establish-window state)
    (chirp-dm-conversation--ensure-timeline)
    (appkit-chat-timeline-install-history-observer
     view
     :start-function
     (lambda (window position start)
       (chirp-dm-conversation--maybe-auto-load-older
        view window position start)))
    (appkit-invalidate view :structure t :parts '(frame timeline) :position t)
    (appkit-sync-invalidations view)))

;;;; Requests

(defun chirp-dm-conversation--history-current-p (view owner)
  "Return non-nil when OWNER owns VIEW's active history request."
  (and (appkit-view-live-p view)
       (with-current-buffer (appkit-view-buffer view)
         (appkit-chat-history-request-current-p owner))))



(defun chirp-dm-conversation--settle-older-success
    (view state owner events envelope)
  "Settle OWNER's older request with EVENTS and ENVELOPE in VIEW STATE."
  (when (with-current-buffer (appkit-view-buffer view)
          (appkit-chat-history-request-end owner))
    (let* ((conversation (chirp-dm-conversation--conversation state))
           (current (plist-get conversation :events))
           (old-first (and current (plist-get (car current) :id)))
           (old-cursor (plist-get state :older-cursor))
           (merged (chirp-dm-state-merge-events current events))
           (new-first (and merged (plist-get (car merged) :id)))
           (next-cursor (chirp-backend-envelope-next-cursor envelope))
           (recovery-key-events
            (chirp-dm-conversation--history-key-events envelope))
           (complete (and (chirp-get-in envelope
                                        '("pagination" "complete"))
                          t))
           (progressed (or (not (equal old-first new-first))
                           (and next-cursor
                                (not (equal old-cursor next-cursor)))))
           (status (chirp-dm-conversation--status state)))
      (chirp-dm-state-set-events conversation merged)
      (setf (plist-get state :recovery-key-events)
            (chirp-dm-conversation--append-unique-events
             (plist-get state :recovery-key-events)
             recovery-key-events)
            (plist-get status :phase) 'idle
            (plist-get status :message) nil)
      (when next-cursor
        (setf (plist-get state :older-cursor) next-cursor
              (plist-get conversation :older-cursor)
              (copy-tree next-cursor)
              (plist-get conversation :has-more) t))
      (when (and new-first (not (equal old-first new-first)))
        (with-current-buffer (appkit-view-buffer view)
          (appkit-chat-history-window-set new-first nil)))
      (with-current-buffer (appkit-view-buffer view)
        (when complete
          (setf (plist-get conversation :older-cursor) nil
                (plist-get conversation :has-more) nil)
          (appkit-chat-history-older-loaded-set t)))
      (setf (plist-get state :older-stalled-p)
            (and (not complete) (not progressed)))
      (chirp-dm-state-publish conversation)
      (when (chirp-dm-conversation--decryption-needed-p state)
        (chirp-dm-conversation--decrypt-view view t)))))

(cl-defun chirp-dm-conversation--settle-refresh-success
    (view state owner conversation
          &key recovery-key-events history-first-key history-cursor
          older-complete-p)
  "Settle refreshed CONVERSATION for OWNER in VIEW STATE.

RECOVERY-KEY-EVENTS came from any history pages used to prove continuity.
HISTORY-FIRST-KEY and HISTORY-CURSOR describe that bridge's older edge.
OLDER-COMPLETE-P means those pages also reached the oldest remote edge."
  (when (with-current-buffer (appkit-view-buffer view)
          (appkit-chat-history-request-end owner))
    (let* ((canonical (chirp-dm-conversation--conversation state))
           (current (plist-get canonical :events))
           (merged
            (chirp-dm-state-merge-events
             current (plist-get conversation :events)))
           (first (and merged (plist-get (car merged) :id)))
           (status (chirp-dm-conversation--status state)))
      (setf (plist-get conversation :title)
            (chirp-dm-conversation--title
             conversation (plist-get canonical :title)))
      (chirp-dm-state-merge-snapshot
       canonical conversation :events merged)
      (setf (plist-get state :recovery-key-events)
            (chirp-dm-conversation--append-unique-events
             (plist-get state :recovery-key-events)
             recovery-key-events)
            (plist-get status :phase) 'idle
            (plist-get status :message) nil)
      (cond
       (older-complete-p
        (setf (plist-get state :older-cursor) nil
              (plist-get state :older-available-p) nil
              (plist-get state :older-stalled-p) nil
              (plist-get canonical :older-cursor) nil
              (plist-get canonical :has-more) nil)
        (with-current-buffer (appkit-view-buffer view)
          (appkit-chat-history-window-set first nil)
          (appkit-chat-history-older-loaded-set t)))
       ((and history-first-key (equal first history-first-key))
        (setf (plist-get state :older-cursor) (copy-tree history-cursor)
              (plist-get state :older-available-p) (and history-cursor t)
              (plist-get state :older-stalled-p) nil
              (plist-get canonical :older-cursor) (copy-tree history-cursor)
              (plist-get canonical :has-more) (and history-cursor t))
        (with-current-buffer (appkit-view-buffer view)
          (appkit-chat-history-window-set first nil)
          (appkit-chat-history-older-loaded-set (null history-cursor))))
       ((and (null current) first)
        (setf (plist-get state :older-cursor)
              (copy-tree (plist-get conversation :older-cursor))
              (plist-get state :older-available-p)
              (plist-get conversation :has-more)
              (plist-get state :older-stalled-p) nil)
        (with-current-buffer (appkit-view-buffer view)
          (unless (appkit-chat-history-window-seed-live first)
            (appkit-chat-history-window-set first nil))
          (appkit-chat-history-older-loaded-set
           (not (plist-get conversation :has-more))))))
      (chirp-dm-state-publish canonical)
      (when (chirp-dm-conversation--decryption-needed-p state)
        (chirp-dm-conversation--decrypt-view view)))))

(defun chirp-dm-conversation--settle-error
    (view state owner phase message)
  "Settle OWNER for PHASE with error MESSAGE in VIEW STATE."
  (when (with-current-buffer (appkit-view-buffer view)
          (appkit-chat-history-request-end owner))
    (let ((status (chirp-dm-conversation--status state)))
      (setf (plist-get status :phase) 'error
            (plist-get status :message) message)
      (appkit-request-sync view :part 'frame :position t)
      (message "%s" (replace-regexp-in-string "[\r\n]+" "  " message))
      (when (and (eq phase 'refresh)
                 (chirp-dm-conversation--decryption-needed-p state))
        (chirp-dm-conversation--decrypt-view view)))))

(cl-defun chirp-dm-conversation--request-refresh-bridge
    (view state owner conversation
          &key cursor recovery-key-events
          (remaining chirp-dm-conversation--refresh-bridge-page-limit))
  "Bridge refreshed CONVERSATION for OWNER to VIEW STATE's event window.

CURSOR selects the next older history page, RECOVERY-KEY-EVENTS accumulates
its key history, and REMAINING bounds the automatic page count."
  (when (chirp-dm-conversation--history-current-p view owner)
    (chirp-backend-dm-history
     (chirp-dm-conversation--id state)
     cursor
     (lambda (events envelope)
       (when (chirp-dm-conversation--history-current-p view owner)
         (let* ((bridged (copy-sequence conversation))
                (bridged-events
                 (chirp-dm-state-merge-events
                  (plist-get conversation :events) events))
                (key-events
                 (chirp-dm-conversation--append-unique-events
                  recovery-key-events
                  (chirp-dm-conversation--history-key-events envelope)))
                (next-cursor
                 (chirp-backend-envelope-next-cursor envelope))
                (complete
                 (and (chirp-get-in envelope
                                    '("pagination" "complete"))
                      t)))
           (setf (plist-get bridged :events) bridged-events)
           (cond
            ((chirp-dm-conversation--events-overlap-p
              (chirp-dm-conversation--events state) bridged-events)
             (chirp-dm-conversation--settle-refresh-success
              view state owner bridged
              :recovery-key-events key-events
              :history-first-key
              (plist-get (car bridged-events) :id)
              :history-cursor next-cursor
              :older-complete-p complete))
            ((and (not complete)
                  next-cursor
                  (not (equal cursor next-cursor))
                  (> remaining 1))
             (chirp-dm-conversation--request-refresh-bridge
              view state owner bridged
              :cursor next-cursor
              :recovery-key-events key-events
              :remaining (1- remaining)))
            (t
             (chirp-dm-conversation--settle-error
              view state owner 'refresh
              "XChat history could not bridge the visible timeline"))))))
     :max-results chirp-dm-history-page-size
     :errback
     (lambda (text)
       (chirp-dm-conversation--settle-error
        view state owner 'refresh text))
     :owner owner)))

(defun chirp-dm-conversation--accept-refresh-success
    (view state owner conversation)
  "Accept refreshed CONVERSATION for OWNER in VIEW STATE.

Disjoint focused fragments are bridged through older history before merging."
  (when (chirp-dm-conversation--history-current-p view owner)
    (let ((current (chirp-dm-conversation--events state))
          (refreshed (plist-get conversation :events)))
      (if (and current refreshed
               (not (chirp-dm-conversation--events-overlap-p
                     current refreshed)))
          (if-let* ((cursor (plist-get conversation :older-cursor)))
              (chirp-dm-conversation--request-refresh-bridge
               view state owner conversation :cursor cursor)
            (chirp-dm-conversation--settle-error
             view state owner 'refresh
             "XChat refresh has no cursor to bridge the visible timeline"))
        (chirp-dm-conversation--settle-refresh-success
         view state owner conversation)))))

(defun chirp-dm-conversation--request (view phase)
  "Start VIEW's conversation request PHASE under its history controller."
  (unless (memq phase '(older refresh))
    (error "Unknown XChat conversation request phase: %S" phase))
  (let* ((state (chirp-dm-conversation--state view))
         (status (chirp-dm-conversation--status state))
         owner)
    (setf (plist-get status :phase) 'idle
          (plist-get status :message) nil)
    (with-current-buffer (appkit-view-buffer view)
      (setq owner (appkit-chat-history-request-start view phase)))
    (appkit-request-sync view :part 'frame :position t)
    (pcase phase
      ('older
       (chirp-backend-dm-history
        (chirp-dm-conversation--id state)
        (plist-get state :older-cursor)
        (lambda (events envelope)
          (chirp-dm-conversation--settle-older-success
           view state owner events envelope))
        :max-results chirp-dm-history-page-size
        :errback
        (lambda (text)
          (chirp-dm-conversation--settle-error
           view state owner phase text))
        :owner owner))
      ('refresh
       (chirp-backend-dm-conversation-data
        (chirp-dm-conversation--id state)
        (lambda (conversation _envelope)
          (chirp-dm-conversation--accept-refresh-success
           view state owner conversation))
        :errback
        (lambda (text)
          (chirp-dm-conversation--settle-error
           view state owner phase text))
        :owner owner)))))

;;;; Sending

(defun chirp-dm-conversation--settle-send-error (view state owner message)
  "Settle Appkit send OWNER in VIEW and STATE with error MESSAGE."
  (when
      (appkit-with-live-view view
        (when (and (bound-and-true-p appkit-compose-session-mode)
                   (appkit-compose-operation-finish owner))
          (setq buffer-read-only nil)
          (setf (plist-get state :send-error) message)
          t))
    (appkit-request-sync view :part 'frame :position t)
    (message "%s" (replace-regexp-in-string "[\r\n]+" "  " message))))

(defun chirp-dm-conversation--settle-send-success
    (view state owner text reply-p)
  "Settle acknowledged Appkit send OWNER for TEXT in VIEW and STATE.

When REPLY-P is non-nil, clear the reply context owned by the acknowledged
composer capture."
  (when
      (appkit-with-live-view view
        (when (and (bound-and-true-p appkit-compose-session-mode)
                   (appkit-compose-operation-finish owner))
          (setq buffer-read-only nil)
          (setf (plist-get state :send-error) nil)
          (unless (string-empty-p (string-trim text))
            (appkit-chatbuf-input-history-push text))
          (appkit-chatbuf-input-set-text "")
          (when reply-p
            (appkit-chatbuf-aux-reset))
          t))
    (appkit-request-sync view :part 'frame :position t)
    (chirp-dm-conversation--request view 'refresh)
    (message "%s sent" (if reply-p "Direct-message reply" "Direct message"))))

;;;; Commands

(defconst chirp-dm-conversation--attachment-object-kind 'dm-attachment
  "Structured composer object kind for one local XChat attachment.")

(defconst chirp-dm-conversation--attachment-kinds
  '(photo video audio file gif)
  "Supported typed XChat composer attachment kinds.")

(defun chirp-dm-conversation--attachment-object-p (value)
  "Return non-nil when VALUE is a typed local XChat attachment object."
  (and (listp value)
       (eq (plist-get value :kind)
           chirp-dm-conversation--attachment-object-kind)
       (memq (plist-get value :attachment-kind)
             chirp-dm-conversation--attachment-kinds)
       (stringp (plist-get value :path))))

(defun chirp-dm-conversation--classify-compose-object (value _text)
  "Classify structured compose VALUE for XChat output."
  (if (chirp-dm-conversation--attachment-object-p value)
      '(side-channel . attachments)
    '(reject . unsupported-xchat-compose-object)))

(defun chirp-dm-conversation--capture-attachments (capture)
  "Return ordered typed attachment objects frozen in CAPTURE."
  (let* ((parse-result
          (appkit-markup-compose-capture-parse-result capture))
         (occurrences
          (alist-get
           'attachments
           (appkit-markup-parse-result-side-channels parse-result))))
    (mapcar
     (lambda (occurrence)
       (let ((value
              (copy-tree
               (appkit-markup-object-occurrence-value occurrence))))
         (unless (chirp-dm-conversation--attachment-object-p value)
           (error "XChat composer captured an invalid attachment"))
         value))
     occurrences)))

(defun chirp-dm-conversation--attachment-display-text (attachment)
  "Return typed composer preview text for ATTACHMENT."
  (let* ((kind (plist-get attachment :attachment-kind))
         (path (plist-get attachment :path))
         (filename
          (or (plist-get attachment :filename)
              (file-name-nondirectory path)))
         (image-p (memq kind '(photo gif)))
         (image
          (and image-p
               (appkit-media-file-present-p path)
               (appkit-media-one-line-preview-image-from-file path)))
         (preview
          (and image
               (appkit-media-one-line-image-display-string image "[image]")))
         (size
          (and (appkit-media-file-present-p path)
               (file-size-human-readable
                (file-attribute-size (file-attributes path)))))
         (label
          (pcase kind
            ('photo "[photo]")
            ('gif "[GIF]")
            ('video "[video]")
            ('audio "[audio]")
            (_ "[file]"))))
    (concat label " "
            (if preview (concat preview " ") "")
            (propertize filename 'help-echo path)
            (if size (format " (%s)" size) ""))))

(defun chirp-dm-conversation--queue-attachment (file attachment-kind)
  "Queue FILE with typed ATTACHMENT-KIND in the current XChat composer."
  (unless (chirp-dm-conversation--current-view)
    (user-error "Current view is not a direct-message conversation"))
  (when (appkit-compose-operation-active-p)
    (user-error "A direct message is already being sent"))
  (unless (memq attachment-kind
                chirp-dm-conversation--attachment-kinds)
    (user-error "Unsupported XChat attachment kind: %s" attachment-kind))
  (let* ((path (expand-file-name file))
         (attributes (and (file-regular-p path) (file-attributes path)))
         (size (and attributes (file-attribute-size attributes))))
    (unless (and attributes (file-readable-p path)
                 (integerp size) (> size 0)
                 (<= size (* 50 1024 1024)))
      (user-error
       "XChat attachment must be a readable file between 1 byte and 50 MiB"))
    (let* ((capture (appkit-markup-compose-capture))
           (attachments
            (chirp-dm-conversation--capture-attachments capture))
           (multi-p (memq attachment-kind '(photo gif video))))
      (when (>= (length attachments) 10)
        (user-error "XChat messages support at most 10 attachments"))
      (when (or (and attachments (not multi-p))
                (cl-some
                 (lambda (attachment)
                   (not
                    (memq (plist-get attachment :attachment-kind)
                          '(photo gif video))))
                 attachments))
        (user-error
         "Files and audio must be the only attachment in an XChat message")))
    (let ((object
           (list :kind chirp-dm-conversation--attachment-object-kind
                 :attachment-kind attachment-kind
                 :path path
                 :filename (file-name-nondirectory path))))
      (goto-char
       (or (appkit-chatbuf-input-logical-end-position) (point-max)))
      (appkit-chatbuf-input-insert
       (chirp-dm-conversation--attachment-display-text object)
       :object object)
      (message "XChat %s queued: %s"
               attachment-kind (file-name-nondirectory path))
      object)))

(defun chirp-dm-attach-file (file)
  "Queue local FILE as a document in the current XChat composer."
  (interactive (list (read-file-name "Attach file: " nil nil t)))
  (chirp-dm-conversation--queue-attachment file 'file))

(defun chirp-dm-attach-photo (file)
  "Queue local image FILE as a photo in the current XChat composer."
  (interactive (list (read-file-name "Attach photo: " nil nil t)))
  (chirp-dm-conversation--queue-attachment file 'photo))

(defun chirp-dm-attach-video (file)
  "Queue local video FILE as a video in the current XChat composer."
  (interactive (list (read-file-name "Attach video: " nil nil t)))
  (chirp-dm-conversation--queue-attachment file 'video))

(defun chirp-dm-attach-audio (file)
  "Queue local audio FILE as audio in the current XChat composer."
  (interactive (list (read-file-name "Attach audio: " nil nil t)))
  (chirp-dm-conversation--queue-attachment file 'audio))

(defun chirp-dm-attach-gif (file)
  "Queue local GIF FILE as an animation in the current XChat composer."
  (interactive (list (read-file-name "Attach GIF: " nil nil t)))
  (chirp-dm-conversation--queue-attachment file 'gif))

(defun chirp-dm-attach (attach-type)
  "Choose ATTACH-TYPE and invoke its configured XChat attachment command."
  (interactive
   (list
    (completing-read
     "Attachment type: "
     (mapcar #'car chirp-dm-attach-commands)
     nil t)))
  (let ((command (cdr (assoc-string
                       attach-type chirp-dm-attach-commands t))))
    (unless (commandp command)
      (user-error "Invalid XChat attachment type: %s" attach-type))
    (call-interactively command)))

(defun chirp-dm-submit ()
  "Encrypt and send the current XChat composer input once."
  (interactive)
  (if-let* ((view (chirp-dm-conversation--current-view)))
      (let* ((state (chirp-dm-conversation--state view))
             (aux (appkit-chatbuf-aux-state))
             (reply-p (eq (plist-get aux :aux-type) 'reply))
             (reply-target (and reply-p
                                (chirp-dm-conversation--reply-target state)))
             request capture owner transport-operation text attachments)
        (unless (appkit-chatbuf-point-in-input-p)
          (user-error "Point is not in the direct-message composer"))
        (when (appkit-compose-operation-active-p)
          (user-error "A direct message is already being sent"))
        (when (and reply-p (null reply-target))
          (user-error "Direct-message reply target is no longer available"))
        (when (and reply-target
                   (not (stringp (plist-get reply-target :encoded-event))))
          (user-error "Direct-message reply target has no raw XChat event"))
        (setq capture
              (condition-case nil
                  (appkit-markup-compose-capture)
                (appkit-markup-object-rejected
                 (user-error "XChat composer contains an unsupported object")))
              text
              (appkit-markup-compose-output-source
               (appkit-markup-compose-output capture 'plain))
              attachments
              (chirp-dm-conversation--capture-attachments capture))
        (when (and (string-empty-p (string-trim text))
                   (null attachments))
          (user-error "Direct message is empty"))
        (setq owner
              (appkit-compose-operation-begin
               (if reply-p 'dm-reply 'dm-send)
               :generation
               (appkit-markup-compose-capture-generation capture)
               :label
               (cond
                ((and reply-p attachments)
                 "Sending direct-message reply with attachments")
                (reply-p "Sending direct-message reply")
                (attachments "Sending direct message with attachments")
                (t "Sending direct message"))
               :cancel-function
               (lambda ()
                 (when transport-operation
                   (appkit-view-operation-cancel view 'dm-send))
                 (chirp-dm-conversation--settle-send-error
                  view state owner "Direct message send canceled"))))
        (setq transport-operation
              (appkit-view-operation-begin view 'dm-send))
        (setf (plist-get state :send-error) nil)
        (setq buffer-read-only t)
        (appkit-request-sync view :part 'frame :position t)
        (let ((success
               (lambda (_event _envelope)
                 (when (appkit-view-operation-finish transport-operation)
                   (chirp-dm-conversation--settle-send-success
                    view state owner text reply-p))))
              (failure
               (lambda (message)
                 (when (appkit-view-operation-finish transport-operation)
                   (chirp-dm-conversation--settle-send-error
                    view state owner message)))))
          (condition-case err
              (setq request
                    (cond
                     (attachments
                      (chirp-backend-dm-send-attachments
                       (chirp-dm-conversation--id state)
                       text attachments success
                       :target-event
                       (and reply-p
                            (plist-get reply-target :encoded-event))
                       :key-events
                       (and reply-p
                            (chirp-dm-conversation--reply-key-events
                             state reply-target))
                       :errback failure
                       :owner transport-operation))
                     (reply-p
                      (chirp-backend-dm-send-reply
                       (chirp-dm-conversation--id state)
                       text
                       (plist-get reply-target :encoded-event)
                       (chirp-dm-conversation--reply-key-events
                        state reply-target)
                       success
                       :errback failure
                       :owner transport-operation))
                     (t
                      (chirp-backend-dm-send-text
                       (chirp-dm-conversation--id state) text success
                       :errback failure
                       :owner transport-operation))))
            ((error quit)
             (when (appkit-view-operation-finish transport-operation)
               (chirp-dm-conversation--settle-send-error
                view state owner (error-message-string err)))
             (signal (car err) (cdr err)))))
        (when (appkit-view-operation-current-p transport-operation)
          (message "%s..."
                   (if reply-p
                       "Sending direct-message reply"
                     "Sending direct message")))
        request)
    (user-error "Current view is not a direct-message conversation")))

(defun chirp-dm-return-dwim (arg)
  "Move to the composer, send its text, or insert a newline with ARG."
  (interactive "P")
  (if (not (appkit-chatbuf-point-in-input-p))
      (goto-char (or (appkit-chatbuf-input-logical-end-position) (point-max)))
    (if arg
        (insert "\n")
      (chirp-dm-submit))))

(defun chirp-dm-conversation--event-at-point (state)
  "Return canonical message event at point in conversation STATE."
  (let ((projected (get-text-property (point) 'chirp-dm-event))
        (key (appkit-chat-timeline-key-at-point)))
    (chirp-dm-conversation--message-by-id
     state
     (or (plist-get projected :sequence-id)
         (plist-get projected :id)
         key))))

(defun chirp-dm-conversation--target-event-at-point (state action)
  "Return actionable message event at point in STATE for ACTION."
  (let ((event (chirp-dm-conversation--event-at-point state)))
    (unless (and (listp event)
                 (eq (plist-get event :kind) 'message)
                 (stringp (plist-get event :encoded-event))
                 (not (string-empty-p
                       (plist-get event :encoded-event))))
      (user-error "Point is not on a message that can %s" action))
    event))

(defun chirp-dm-reply-to-message ()
  "Set the message at point as the next direct-message reply target."
  (interactive)
  (if-let* ((view (chirp-dm-conversation--current-view)))
      (let* ((state (chirp-dm-conversation--state view))
             (event
              (chirp-dm-conversation--target-event-at-point
               state "be replied to"))
             (message-id
              (or (plist-get event :sequence-id)
                  (plist-get event :id))))
        (when (appkit-compose-operation-active-p)
          (user-error "A direct message is already being sent"))
        (appkit-chatbuf-aux-set
         (list :aux-type 'reply
               :aux-msg event
               :message-id message-id))
        (appkit-request-sync view :part 'frame :position t)
        (appkit-chatbuf-focus-input)
        (message "Next direct message will reply to %s" message-id))
    (user-error "Current view is not a direct-message conversation")))

(defun chirp-dm-cancel-reply ()
  "Cancel the current direct-message reply context."
  (interactive)
  (if-let* ((view (chirp-dm-conversation--current-view)))
      (progn
        (when (appkit-compose-operation-active-p)
          (user-error "A direct message is already being sent"))
        (if (eq (appkit-chatbuf-aux-type) 'reply)
            (progn
              (appkit-chatbuf-aux-reset)
              (appkit-request-sync view :part 'frame :position t)
              (message "Direct-message reply cancelled"))
          (message "No direct-message reply is active")))
    (user-error "Current view is not a direct-message conversation")))

(defun chirp-dm-conversation--reaction-selected-p (event emoji)
  "Return non-nil when the current user selected EMOJI on EVENT."
  (when-let* ((user-id
               (chirp--session-xchat-user-id (chirp--session)))
              (reaction
               (cl-find emoji (plist-get event :reactions)
                        :key (lambda (item) (plist-get item :emoji))
                        :test #'equal)))
    (member user-id (plist-get reaction :senders))))

(defun chirp-dm-conversation--default-reaction (event)
  "Return the best default reaction for EVENT."
  (or (when-let* ((user-id
                   (chirp--session-xchat-user-id (chirp--session)))
                  (selected
                   (cl-find-if
                    (lambda (reaction)
                      (member user-id (plist-get reaction :senders)))
                    (plist-get event :reactions))))
        (plist-get selected :emoji))
      (plist-get (car (plist-get event :reactions)) :emoji)
      "👍"))

(defun chirp-dm-conversation--settle-reaction-error (operation message)
  "Settle reaction OPERATION with error MESSAGE."
  (when (appkit-view-operation-finish operation)
    (message "%s" (replace-regexp-in-string "[\r\n]+" "  " message))))

(defun chirp-dm-conversation--settle-reaction-success
    (view state operation event emoji remove-p)
  "Settle acknowledged reaction OPERATION with EVENT in VIEW and STATE.

EMOJI and REMOVE-P describe the acknowledged operation."
  (when (appkit-view-operation-finish operation)
    (let ((conversation (chirp-dm-conversation--conversation state)))
      (chirp-dm-state-set-events
       conversation
       (chirp-dm-state-merge-events
        (plist-get conversation :events) (list event)))
      (chirp-dm-state-publish conversation)
      (when (and (chirp-dm-conversation--decryption-needed-p state)
                 (not (plist-get state :decrypt-loading-p)))
        (chirp-dm-conversation--decrypt-view view t)))
    (message "Reaction %s: %s"
             (if remove-p "removed" "added") emoji)))

(defun chirp-dm-toggle-reaction (&optional emoji)
  "Toggle the current user's EMOJI reaction on the message at point."
  (interactive)
  (if-let* ((view (chirp-dm-conversation--current-view)))
      (let* ((state (chirp-dm-conversation--state view))
             (event
              (chirp-dm-conversation--target-event-at-point
               state "receive a reaction"))
             (default (chirp-dm-conversation--default-reaction event))
             (emoji
              (string-trim
               (or emoji
                   (read-string (format-prompt "Reaction" default)
                                nil nil default))))
             (target-id
              (or (plist-get event :sequence-id)
                  (plist-get event :id)))
             (key (list 'dm-reaction target-id emoji))
             (remove-p
              (and (chirp-dm-conversation--reaction-selected-p event emoji)
                   t))
             operation request)
        (when (string-empty-p emoji)
          (user-error "Reaction emoji cannot be empty"))
        (when (gethash key (appkit-view-request-table view))
          (user-error "This reaction operation is already running"))
        (setq operation (appkit-view-operation-begin view key))
        (condition-case err
            (setq request
                  (chirp-backend-dm-send-reaction
                   (chirp-dm-conversation--id state)
                   (plist-get event :encoded-event)
                   emoji remove-p
                   (lambda (acknowledged _envelope)
                     (chirp-dm-conversation--settle-reaction-success
                      view state operation acknowledged emoji remove-p))
                   :errback
                   (lambda (text)
                     (chirp-dm-conversation--settle-reaction-error
                      operation text))
                   :owner operation))
          ((error quit)
           (chirp-dm-conversation--settle-reaction-error
            operation (error-message-string err))
           (signal (car err) (cdr err))))
        (when (appkit-view-operation-current-p operation)
          (message "%s reaction..."
                   (if remove-p "Removing" "Adding")))
        request)
    (user-error "Current view is not a direct-message conversation")))

(defun chirp-dm-refresh-conversation ()
  "Refresh the current XChat conversation without acknowledging reads."
  (interactive)
  (if-let* ((view (chirp-dm-conversation--current-view)))
      (chirp-dm-conversation--request view 'refresh)
    (user-error "Current view is not a direct-message conversation")))

(defun chirp-dm-load-older-messages ()
  "Load one older page in the current XChat conversation."
  (interactive)
  (if-let* ((view (chirp-dm-conversation--current-view)))
      (let ((state (chirp-dm-conversation--state view)))
        (cond
         ((appkit-chat-history-loading-p)
          (user-error "A direct-message history request is already running"))
         ((appkit-chat-history-older-loaded-p)
          (user-error "No older direct messages available"))
         ((plist-get state :older-stalled-p)
          (user-error "Older direct-message history made no progress"))
         ((null (plist-get state :older-cursor))
          (user-error "Direct-message history cursor is unavailable"))
         (t
          (chirp-dm-conversation--request view 'older))))
    (user-error "Current view is not a direct-message conversation")))

(defun chirp-dm-conversation--move-message (direction)
  "Move to the next message row in DIRECTION."
  (let* ((keys (appkit-chat-timeline-keys))
         (current (appkit-chat-timeline-key-at-point))
         (index (and current (seq-position keys current #'equal)))
         (target-index
          (if (> direction 0)
              (if index (1+ index) 0)
            (if index (1- index) (1- (length keys)))))
         (target (and (>= target-index 0) (nth target-index keys))))
    (if-let* ((position (and target
                             (appkit-chat-timeline-key-position target))))
        (goto-char position)
      (user-error "No %s direct message"
                  (if (> direction 0) "next" "previous")))))

(defun chirp-dm-next-message ()
  "Move to the next visible direct-message event."
  (interactive)
  (chirp-dm-conversation--move-message 1))

(defun chirp-dm-previous-message ()
  "Move to the previous visible direct-message event."
  (interactive)
  (chirp-dm-conversation--move-message -1))

;;; Conversation Modes

(defvar-keymap chirp-dm-conversation--timeline-mode-map
  :doc "Timeline-only keymap active outside the XChat composer."
  "!" #'chirp-dm-toggle-reaction
  "g" #'chirp-dm-refresh-conversation
  "N" #'chirp-dm-load-older-messages
  "n" #'chirp-dm-next-message
  "p" #'chirp-dm-previous-message
  "r" #'chirp-dm-reply-to-message
  "q" #'chirp-quit-current-buffer)

(define-minor-mode chirp-dm-conversation--timeline-mode
  "Enable direct-message navigation keys outside the XChat composer."
  :init-value nil
  :lighter nil
  :keymap chirp-dm-conversation--timeline-mode-map)

(defvar-keymap chirp-dm-conversation--mode-map
  :doc "Keymap for `chirp-dm-conversation--mode'."
  "RET" #'chirp-dm-return-dwim
  "C-c C-a" #'chirp-dm-attach
  "C-c C-f" #'chirp-dm-attach-file
  "C-c C-c" #'chirp-dm-submit
  "C-c C-k" #'chirp-dm-cancel-reply
  "C-c C-r" #'chirp-dm-refresh-conversation
  "C-c C-n" #'chirp-dm-load-older-messages)

(defun chirp-dm-conversation--setup-evil ()
  "Install optional Evil bindings for XChat conversation views."
  (when appkit-evil-enable-integration
    (appkit-evil-set-initial-states
     '(chirp-dm-conversation--mode) 'normal)
    (appkit-evil-map
      (:map chirp-dm-conversation--mode-map
       :nm
       "RET" #'chirp-dm-return-dwim
       "g r" #'chirp-dm-refresh-conversation)
      (:map chirp-dm-conversation--timeline-mode-map
       :nm
       "!" #'chirp-dm-toggle-reaction
       "q" #'chirp-quit-current-buffer
       "r" #'chirp-dm-reply-to-message
       "i" #'appkit-evil-chatbuf-enter-input
       "g j" #'chirp-dm-next-message
       "g k" #'chirp-dm-previous-message
       "g n" #'chirp-dm-load-older-messages))
    (appkit-evil-normalize-buffers '(chirp-dm-conversation--mode))))

(with-eval-after-load 'evil
  (chirp-dm-conversation--setup-evil))

(define-derived-mode chirp-dm-conversation--mode appkit-chatbuf-mode "Chirp-DM"
  "Major mode for one XChat conversation with structured attachments."
  (setq-local line-spacing 0)
  (appkit-compose-setup
   :snapshot-function #'appkit-chatbuf-input-string
   :source-bounds-function #'appkit-chatbuf-input-region-bounds)
  (appkit-markup-compose-setup
   :codecs '(plain)
   :object-classifier #'chirp-dm-conversation--classify-compose-object)
  (add-hook 'chirp-dm-conversation--timeline-mode-hook
            #'appkit-evil-normalize-keymaps nil t)
  (appkit-chatbuf-use-timeline-mode #'chirp-dm-conversation--timeline-mode))

(cl-defun chirp-dm-conversation-open (conversation &key refresh-p)
  "Open a fresh chat view for normalized CONVERSATION.

When REFRESH-P is non-nil, fetch focused conversation data before decryption."
  (unless (and (listp conversation)
               (stringp (plist-get conversation :id)))
    (user-error "Direct-message conversation is invalid"))
  (let* ((instance (cl-incf chirp-dm-conversation--next-instance))
         (title
          (chirp-dm-conversation--one-line
           (plist-get conversation :title)))
         (view
          (appkit-open-view
           :app (chirp-app)
           :id (list 'dm-conversation instance)
           :mode 'chirp-dm-conversation--mode
           :buffer-name
           (chirp--format-buffer-name
            (format "DM: %s"
                    (if (string-empty-p title) "Conversation" title)))
           :state
           (chirp-dm-conversation--make-state instance conversation)
           :sync-function #'chirp-dm-conversation--sync
           :parts '(frame timeline composer geometry)
           :position-policy 'chirp-dm-message-id
           :setup #'chirp-dm-conversation--setup
           :select t)))
    (if refresh-p
        (chirp-dm-conversation--request view 'refresh)
      (when (chirp-dm-conversation--decryption-needed-p
             (appkit-view-state view))
        (chirp-dm-conversation--decrypt-view view)))
    (appkit-view-buffer view)))

(provide 'chirp-dm-conversation)

;;; chirp-dm-conversation.el ends here
