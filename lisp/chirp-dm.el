;;; chirp-dm.el --- XChat direct messages for Chirp -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Appkit-owned XChat inbox and conversation views with a trailing composer.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'url-parse)
(require 'appkit-core)
(require 'appkit-directory)
(require 'appkit-evil)
(require 'appkit-invalidation)
(require 'appkit-chatbuf)
(require 'appkit-chat-history)
(require 'appkit-chat-timeline)
(require 'chirp-core)
(require 'chirp-backend)
(require 'chirp-media)

(declare-function chirp-xchat-native-load "chirp-xchat-native" ())
(declare-function chirp-xchat-native-recovery-active-p
                  "chirp-xchat-native" ())
(declare-function chirp-xchat-native-unlocked-p
                  "chirp-xchat-native" ())
(declare-function chirp-xchat-native-decrypt-events
                  "chirp-xchat-native" (events signing-keys))
(declare-function chirp-xchat-native-recover
                  "chirp-xchat-native" (pin input callback &rest keys))
(declare-function chirp-xchat-native-discard-recovery-input
                  "chirp-xchat-native" (input))
(declare-function chirp-xchat-native-cancel-recovery
                  "chirp-xchat-native" ())

(defcustom chirp-dm-inbox-page-size 20
  "Number of XChat conversations requested per inbox page, up to 100."
  :type '(integer 1 100)
  :group 'chirp)

(defcustom chirp-dm-history-page-size 200
  "Number of older XChat events requested per conversation page, up to 200."
  :type '(integer 1 200)
  :group 'chirp)

(defvar chirp-dm--next-instance 0
  "Monotonic identity source for fresh direct-message views.")

(defconst chirp-dm--inbox-request-key 'dm-inbox
  "Request-table key for one inbox view's active transport.")

(defconst chirp-dm--conversation-request-key 'dm-conversation
  "Request-table key for one conversation view's active transport.")

(defconst chirp-dm--decrypt-request-key 'dm-decrypt
  "Request-table key for one conversation view's signing-key retrieval.")

(defconst chirp-dm--send-request-key 'dm-send
  "Request-table key for one conversation view's active message write.")

(defconst chirp-dm--refresh-bridge-page-limit 10
  "Maximum history pages fetched to join one focused refresh fragment.")

(defun chirp-dm--one-line (text)
  "Return TEXT collapsed into one trimmed display line."
  (string-trim
   (replace-regexp-in-string "[[:space:]\n\r]+" " " (or text ""))))

(defun chirp-dm--format-time (milliseconds)
  "Return a compact local timestamp for MILLISECONDS, or an empty string."
  (if (and (stringp milliseconds)
           (string-match-p "\\`[0-9]+\\'" milliseconds))
      (format-time-string
       "%m-%d %H:%M"
       (seconds-to-time (/ (string-to-number milliseconds) 1000)))
    ""))

(defun chirp-dm--status (state)
  "Return canonical status plist from direct-message STATE."
  (or (plist-get state :status)
      (error "Direct-message view state has no status")))

(defun chirp-dm--inbox-state (view)
  "Return VIEW's validated XChat inbox state."
  (let ((state (appkit-view-state view)))
    (unless (and (listp state)
                 (eq (plist-get state :type) 'dm-inbox)
                 (plist-get state :instance))
      (error "Invalid Chirp direct-message inbox state"))
    state))

(defun chirp-dm--conversation-state (view)
  "Return VIEW's validated XChat conversation state."
  (let ((state (appkit-view-state view)))
    (unless (and (listp state)
                 (eq (plist-get state :type) 'dm-conversation)
                 (plist-get state :instance)
                 (stringp (plist-get state :conversation-id)))
      (error "Invalid Chirp direct-message conversation state"))
    state))

(defun chirp-dm--current-view (type)
  "Return current live direct-message view of TYPE, or nil."
  (when-let* ((view (appkit-current-view))
              ((appkit-view-live-p view))
              (state (appkit-view-state view))
              ((eq (plist-get state :type) type)))
    view))

(defun chirp-dm--generation-current-p (view state generation)
  "Return non-nil when GENERATION may still update STATE in VIEW."
  (and (appkit-view-live-p view)
       (eq state (appkit-view-state view))
       (eq generation (plist-get state :generation))))

(defun chirp-dm--retire-inbox-request (view state generation)
  "Retire GENERATION's inbox transport when it still owns VIEW and STATE."
  (when (chirp-dm--generation-current-p view state generation)
    (remhash chirp-dm--inbox-request-key
             (appkit-view-request-table view))))

(defun chirp-dm--cancel-request (view request-key)
  "Cancel VIEW's active transport under REQUEST-KEY, if any."
  (let* ((table (appkit-view-request-table view))
         (request (gethash request-key table)))
    (remhash request-key table)
    (when request
      (chirp-backend-cancel-request request))))

(defun chirp-dm--make-inbox-state (instance)
  "Return canonical inbox state for INSTANCE."
  (list :type 'dm-inbox
        :instance instance
        :items nil
        :page (list :next-cursor nil :exhausted-p nil)
        :status (list :phase 'initial :message nil)
        :generation nil))

(defun chirp-dm--inbox-status-entry (state)
  "Return the passive status directory entry for inbox STATE, or nil."
  (let* ((status (chirp-dm--status state))
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
       :section-key '(dm-inbox section)
       :label text
       :face face))))

(defun chirp-dm--project-inbox (state)
  "Project canonical inbox STATE into Appkit directory entries."
  (append
   (list
    (appkit-directory-entry-create
     :key '(dm-inbox section)
     :role 'section
     :label "Direct Messages"
     :face 'bold))
   (mapcar
    (lambda (conversation)
      (appkit-directory-entry-create
       :key (list 'dm-conversation (plist-get conversation :id))
       :role 'item
       :section-key '(dm-inbox section)
       :label (plist-get conversation :title)
       :primary-action 'item
       :item-p t
       :payload conversation
       :help-echo "Open this conversation"))
    (plist-get state :items))
   (when-let* ((status-entry (chirp-dm--inbox-status-entry state)))
     (list status-entry))))

(defun chirp-dm--insert-inbox-item (_surface entry)
  "Insert one XChat inbox directory ENTRY."
  (let* ((conversation (appkit-directory-entry-payload entry))
         (title (chirp-dm--one-line (plist-get conversation :title)))
         (preview (chirp-dm--one-line (plist-get conversation :preview)))
         (time (chirp-dm--format-time
                (plist-get conversation :updated-at-msec))))
    (insert (propertize (if (string-empty-p title) "Direct message" title)
                        'face 'bold))
    (when (plist-get conversation :message-request-p)
      (insert (propertize "  [request]" 'face 'warning)))
    (unless (string-empty-p preview)
      (insert "  " (propertize preview 'face 'shadow)))
    (unless (string-empty-p time)
      (insert "  " (propertize time 'face 'shadow)))
    (insert "\n")))

(defun chirp-dm--activate-inbox-item (_surface entry)
  "Open the conversation carried by inbox directory ENTRY."
  (chirp-dm--open-conversation
   (appkit-directory-entry-payload entry) :refresh-p t))

(defun chirp-dm--sync-inbox (view _invalidations)
  "Synchronize inbox VIEW from canonical state."
  (appkit-directory-reconcile
   (appkit-directory-surface)
   (chirp-dm--project-inbox (chirp-dm--inbox-state view))))

(defun chirp-dm--setup-inbox (view)
  "Initialize Appkit directory adapters for inbox VIEW."
  (setq-local chirp--view-title "Direct Messages")
  (setq-local header-line-format nil)
  (appkit-directory-configure
   (appkit-directory-surface)
   :item-inserter #'chirp-dm--insert-inbox-item
   :activate-function #'chirp-dm--activate-inbox-item)
  (appkit-invalidate view :structure t :part 'entries :position t)
  (appkit-sync-invalidations view))

(defun chirp-dm--append-unique (current fetched key-function)
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

(defun chirp-dm--merge-events (left right)
  "Merge ordered normalized event lists LEFT and RIGHT by sequence ID."
  (sort (chirp-dm--append-unique
         left right (lambda (event) (plist-get event :id)))
        (lambda (left-event right-event)
          (< (string-to-number (plist-get left-event :id))
             (string-to-number (plist-get right-event :id))))))

(defun chirp-dm--events-overlap-p (left right)
  "Return non-nil when event lists LEFT and RIGHT share an identity."
  (let ((ids (make-hash-table :test #'equal)))
    (dolist (event left)
      (puthash (plist-get event :id) t ids))
    (cl-some (lambda (event)
               (gethash (plist-get event :id) ids))
             right)))

(defun chirp-dm--history-key-events (envelope)
  "Return normalized recovery key events carried by history ENVELOPE."
  (or (chirp-get envelope "keyEvents")
      (mapcar
       (lambda (encoded)
         (list :id encoded :encoded-event encoded))
       (or (chirp-get envelope "encodedKeyEvents") nil))))

(defun chirp-dm--settle-inbox-success
    (view state generation phase conversations envelope)
  "Settle inbox GENERATION and PHASE with CONVERSATIONS and ENVELOPE.

VIEW and STATE identify the inbox whose request is completing."
  (when (chirp-dm--generation-current-p view state generation)
    (let* ((page (plist-get state :page))
           (status (chirp-dm--status state))
           (next-cursor (chirp-backend-envelope-next-cursor envelope)))
      (setf (plist-get state :items)
            (if (eq phase 'older)
                (chirp-dm--append-unique
                 (plist-get state :items) conversations
                 (lambda (item) (plist-get item :id)))
              conversations)
            (plist-get page :next-cursor) next-cursor
            (plist-get page :exhausted-p) (not next-cursor)
            (plist-get status :phase) 'idle
            (plist-get status :message) nil
            (plist-get state :generation) nil)
      (appkit-request-sync view :structure t :part 'entries :position t))))

(defun chirp-dm--settle-inbox-error (view state generation message)
  "Settle inbox GENERATION in VIEW and STATE with error MESSAGE."
  (when (chirp-dm--generation-current-p view state generation)
    (let ((status (chirp-dm--status state)))
      (setf (plist-get status :phase) 'error
            (plist-get status :message) message
            (plist-get state :generation) nil)
      (appkit-request-sync view :structure t :part 'entries :position t)
      (message "%s" (replace-regexp-in-string "[\r\n]+" "  " message)))))

(defun chirp-dm--request-inbox (view phase)
  "Start inbox request PHASE owned by VIEW."
  (let* ((state (chirp-dm--inbox-state view))
         (page (plist-get state :page))
         (status (chirp-dm--status state))
         (generation (list 'dm-inbox-generation))
         callback-ran-p
         request)
    (setf (plist-get state :generation) generation
          (plist-get status :phase) phase
          (plist-get status :message) nil)
    (chirp-dm--cancel-request view chirp-dm--inbox-request-key)
    (appkit-request-sync view :part 'entries :position t)
    (setq request
          (chirp-backend-dm-inbox
           (lambda (conversations envelope)
             (setq callback-ran-p t)
             (chirp-dm--retire-inbox-request view state generation)
             (chirp-dm--settle-inbox-success
              view state generation phase conversations envelope))
           :cursor (and (eq phase 'older)
                        (plist-get page :next-cursor))
           :max-results chirp-dm-inbox-page-size
           :errback
           (lambda (text)
             (setq callback-ran-p t)
             (chirp-dm--retire-inbox-request view state generation)
             (chirp-dm--settle-inbox-error view state generation text))
           :owner view))
    (cond
     ((and (not callback-ran-p)
           request
           (chirp-dm--generation-current-p view state generation))
      (puthash chirp-dm--inbox-request-key request
               (appkit-view-request-table view)))
     ((and (not callback-ran-p)
           (null request)
           (chirp-dm--generation-current-p view state generation))
      (chirp-dm--settle-inbox-error
       view state generation "XChat inbox request did not start")))
    request))

(defun chirp-dm-refresh-inbox ()
  "Refresh the current XChat inbox without sending a read acknowledgment."
  (interactive)
  (if-let* ((view (chirp-dm--current-view 'dm-inbox)))
      (chirp-dm--request-inbox view 'refresh)
    (user-error "Current view is not a direct-message inbox")))

(defun chirp-dm-load-more-inbox ()
  "Load one older page in the current XChat inbox."
  (interactive)
  (if-let* ((view (chirp-dm--current-view 'dm-inbox)))
      (let* ((state (chirp-dm--inbox-state view))
             (page (plist-get state :page)))
        (cond
         ((plist-get state :generation)
          (user-error "A direct-message inbox request is already running"))
         ((plist-get page :exhausted-p)
          (user-error "No older conversations available"))
         ((null (plist-get page :next-cursor))
          (user-error "Direct-message inbox cursor is unavailable"))
         (t
          (chirp-dm--request-inbox view 'older))))
    (user-error "Current view is not a direct-message inbox")))

(defun chirp-dm--decrypt-current-p (view state generation)
  "Return non-nil when GENERATION still owns decryption in VIEW and STATE."
  (and (appkit-view-live-p view)
       (eq state (appkit-view-state view))
       (eq generation (plist-get state :decrypt-generation))))

(defun chirp-dm--decrypt-input (state)
  "Return encoded events and signing-key user IDs from conversation STATE."
  (let* ((events (plist-get state :events))
         (recovery-events (plist-get state :recovery-key-events))
         (encoded
          (delete-dups
           (delq nil
                 (append
                  (mapcar (lambda (event)
                            (plist-get event :encoded-event))
                          recovery-events)
                  (mapcar (lambda (event)
                            (plist-get event :encoded-event))
                          events)))))
         (user-ids
          (delete-dups
           (delq nil
                 (append
                  (mapcar (lambda (participant)
                            (plist-get participant :id))
                          (plist-get state :participants))
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

(defun chirp-dm--decryption-needed-p (state)
  "Return non-nil when conversation STATE has encrypted messages."
  (cl-some (lambda (event) (plist-get event :encrypted-p))
           (plist-get state :events)))

(defun chirp-dm--trusted-media-url-p (value)
  "Return non-nil when VALUE is a trusted HTTPS X media URL."
  (and (stringp value)
       (<= (length value) 8192)
       (not (string-match-p "[[:cntrl:]]" value))
       (condition-case nil
           (let* ((parsed (url-generic-parse-url value))
                  (host (downcase (or (url-host parsed) ""))))
             (and (equal (url-type parsed) "https")
                  (null (url-user parsed))
                  (null (url-password parsed))
                  (memq (url-port parsed) '(nil 443))
                  (or (equal host "ton.twitter.com")
                      (string-suffix-p ".twimg.com" host))))
         (error nil))))

(defun chirp-dm--normalize-attachment (raw conversation-id message-id index)
  "Normalize attachment RAW for CONVERSATION-ID, MESSAGE-ID, and INDEX."
  (let* ((kind-name (alist-get 'kind raw))
         (kind (and (member kind-name
                            '("image" "gif" "video" "audio" "file" "svg"
                              "media" "url" "post" "unified-card" "money"))
                    (intern kind-name)))
         (url (and (chirp-dm--trusted-media-url-p (alist-get 'url raw))
                   (alist-get 'url raw)))
         (preview
          (and (chirp-dm--trusted-media-url-p (alist-get 'preview_url raw))
               (alist-get 'preview_url raw)))
         (name (alist-get 'name raw)))
    (when kind
      (list :kind kind
            :url url
            :preview-url preview
            :name (and (stringp name) (<= (length name) 1024) name)
            :resource-key
            (list 'xchat-media conversation-id message-id index)))))

(defun chirp-dm--normalize-attachments (message conversation-id)
  "Return verified native attachments from MESSAGE for CONVERSATION-ID."
  (let ((raw (alist-get 'attachments message))
        (message-id (or (alist-get 'id message)
                        (alist-get 'sequence_id message))))
    (when (listp raw)
      (cl-loop for attachment in raw
               for index from 0
               for normalized =
               (and (listp attachment)
                    message-id
                    (chirp-dm--normalize-attachment
                     attachment conversation-id message-id index))
               when normalized collect normalized))))

(defun chirp-dm--apply-plaintext (state messages)
  "Apply verified plaintext MESSAGES to canonical conversation STATE."
  (let ((conversation-id (plist-get state :conversation-id))
        (by-id (make-hash-table :test #'equal))
        (updated 0))
    (dolist (message messages)
      (when (and (eq (alist-get 'verified message) t)
                 (equal (alist-get 'conversation_id message)
                        conversation-id))
        (dolist (id (list (alist-get 'sequence_id message)
                          (alist-get 'id message)))
          (when (stringp id)
            (puthash id message by-id)))))
    (setf
     (plist-get state :events)
     (mapcar
      (lambda (event)
        (if-let* ((message
                   (or (gethash (plist-get event :sequence-id) by-id)
                       (gethash (plist-get event :message-id) by-id))))
            (let* ((copy (copy-sequence event))
                   (content-kind (alist-get 'content_kind message))
                   (attachments
                    (chirp-dm--normalize-attachments message conversation-id))
                   (reply-count
                    (alist-get 'reply_attachment_count message)))
              (setf (plist-get copy :kind)
                    (pcase content-kind
                      ("mark-read" 'mark-read)
                      ("mark-unread" 'mark-unread)
                      (_ (plist-get copy :kind)))
                    (plist-get copy :content-kind)
                    (and (member content-kind
                                 '("text" "reaction" "reaction-removed"
                                   "edit" "mark-read" "mark-unread"
                                   "unknown"))
                         (intern content-kind))
                    (plist-get copy :text)
                    (and (stringp (alist-get 'text message))
                         (alist-get 'text message))
                    (plist-get copy :attachments) attachments
                    (plist-get copy :attachment-count) (length attachments)
                    (plist-get copy :reply-p)
                    (eq (alist-get 'reply message) t)
                    (plist-get copy :reply-text)
                    (and (stringp (alist-get 'reply_text message))
                         (alist-get 'reply_text message))
                    (plist-get copy :reply-attachment-count)
                    (if (and (integerp reply-count) (>= reply-count 0))
                        reply-count
                      0)
                    (plist-get copy :encrypted-p) nil
                    (plist-get copy :decrypted-p) t)
              (cl-incf updated)
              copy)
          event))
      (plist-get state :events)))
    updated))

(defun chirp-dm--prefetch-event-media (view events)
  "Request image dependencies of verified XChat EVENTS for VIEW."
  (dolist (event events)
    (dolist (attachment (plist-get event :attachments))
      (when (memq (plist-get attachment :kind) '(image gif svg))
        (chirp-media-request-image-resource
         view
         (plist-get attachment :resource-key)
         (or (plist-get attachment :preview-url)
             (plist-get attachment :url))
         :name (plist-get attachment :name))))))

(defun chirp-dm--settle-decrypt-error (view state generation message)
  "Settle decryption GENERATION in VIEW and STATE with error MESSAGE."
  (when (chirp-dm--decrypt-current-p view state generation)
    (setf (plist-get state :decrypt-generation) nil)
    (remhash chirp-dm--decrypt-request-key
             (appkit-view-request-table view))
    (display-warning 'chirp message :warning)))

(defun chirp-dm--settle-decrypt-success
    (view state generation encoded signing-keys)
  "Decrypt ENCODED events with SIGNING-KEYS for GENERATION in VIEW and STATE."
  (when (chirp-dm--decrypt-current-p view state generation)
    (condition-case err
        (let ((messages
               (cl-loop for batch in (seq-partition encoded 200)
                        append
                        (alist-get
                         'messages
                         (chirp-xchat-native-decrypt-events
                          batch signing-keys)))))
          (setf (plist-get state :decrypt-generation) nil)
          (remhash chirp-dm--decrypt-request-key
                   (appkit-view-request-table view))
          (let ((updated (chirp-dm--apply-plaintext state messages)))
            (when (> updated 0)
              (chirp-dm--prefetch-event-media view (plist-get state :events))
              (appkit-request-sync view :part 'timeline :position t))
            (message "Decrypted %d verified XChat message%s"
                     updated (if (= updated 1) "" "s"))))
      (error
       (chirp-dm--settle-decrypt-error
        view state generation (error-message-string err))))))

(defun chirp-dm--decrypt-view (view &optional key-history-loaded-p)
  "Decrypt encrypted events in conversation VIEW.

Unless KEY-HISTORY-LOADED-P is non-nil, fetch one older page first when the
view has no conversation-key event."
  (when (appkit-view-live-p view)
    (let* ((state (chirp-dm--conversation-state view))
           (input (chirp-dm--decrypt-input state))
           (encoded (plist-get input :events))
           (user-ids (plist-get input :user-ids))
           (key-event-p
            (cl-some
             (lambda (event)
               (eq (plist-get event :kind) 'conversation-key-change))
             (append (plist-get state :recovery-key-events)
                     (plist-get state :events))))
           (generation (cons 'decrypt nil))
           callback-ran-p request)
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
        (chirp-dm--request-conversation view 'older))
       ((null user-ids)
        (display-warning 'chirp "XChat signing-key users are unavailable" :warning))
       ((> (length user-ids) 100)
        (display-warning 'chirp "XChat conversation has too many signing-key users" :warning))
       (t
        (setf (plist-get state :decrypt-generation) generation)
        (chirp-dm--cancel-request view chirp-dm--decrypt-request-key)
        (setq request
              (chirp-backend-dm-signing-keys
               user-ids
               (lambda (signing-keys _envelope)
                 (setq callback-ran-p t)
                 (when (chirp-dm--decrypt-current-p view state generation)
                   (chirp-dm--settle-decrypt-success
                    view state generation encoded signing-keys)))
               :errback
               (lambda (message)
                 (setq callback-ran-p t)
                 (chirp-dm--settle-decrypt-error
                  view state generation message))
               :owner view))
        (when (and (buffer-live-p request)
                   (chirp-dm--decrypt-current-p view state generation))
          (puthash chirp-dm--decrypt-request-key request
                   (appkit-view-request-table view)))
        (when (and (null request) (not callback-ran-p))
          (chirp-dm--settle-decrypt-error
           view state generation "XChat signing-key request did not start")))))))

(defun chirp-dm--unlock-result (result)
  "Report XChat unlock RESULT and open the inbox on success."
  (pcase (plist-get result :status)
    ('unlocked
     (message "XChat keys unlocked for this Chirp session")
     (chirp-dm--open-unlocked-inbox))
    ('incorrect-pin
     (if-let* ((remaining (plist-get result :guesses-remaining)))
         (display-warning
          'chirp
          (format "Incorrect XChat PIN; %d attempt%s reported remaining"
                  remaining (if (= remaining 1) "" "s"))
          :warning)
       (display-warning 'chirp "Incorrect XChat PIN" :warning)))
    ('not-registered
     (display-warning 'chirp "No XChat keys are registered for this PIN" :warning))
    ('invalid-auth
     (display-warning 'chirp "XChat Juicebox authorization was rejected" :warning))
    ('upgrade-required
     (display-warning 'chirp "The XChat cryptography module must be upgraded" :error))
    ('rate-limited
     (display-warning 'chirp "XChat key recovery is rate-limited" :warning))
    ('no-tokens
     (display-warning 'chirp "XChat recovery configuration has no realm tokens" :error))
    ('key-reconstruction-failed
     (display-warning 'chirp "XChat returned invalid recovered key material" :error))
    ('registered-key-mismatch
     (display-warning 'chirp "Recovered XChat keys do not match this account" :error))
    ('ambiguous-registered-key
     (display-warning 'chirp "Recovered XChat keys match multiple versions" :error))
    ('assertion-failed
     (display-warning 'chirp "The Juicebox recovery protocol rejected its state" :error))
    ((or 'uncertain 'cancelled)
     (display-warning
      'chirp
      "XChat recovery outcome is uncertain; do not retry automatically"
      :warning))
    (_
     (display-warning 'chirp "XChat key recovery failed" :error))))

(defun chirp-dm--unlock-error (message)
  "Report XChat unlock failure MESSAGE without placing it in a DM buffer."
  (display-warning 'chirp message :error))

(defun chirp-dm--prompt-and-unlock (app input)
  "Prompt using INPUT and recover XChat keys in APP."
  (require 'chirp-xchat-native)
  (let (pin)
    (unwind-protect
        (when (appkit-app-live-p app)
          (setq pin (read-passwd "XChat 4-digit PIN: "))
          (if (not (and (= (length pin) 4)
                        (string-match-p "\\`[0-9]+\\'" pin)))
              (display-warning
               'chirp "XChat PIN must contain exactly four digits" :warning)
            (when (chirp-xchat-native-recover
                   pin input
                   (lambda (result)
                     (chirp-dm--unlock-result result))
                   :errback #'chirp-dm--unlock-error)
              (message "Recovering XChat keys..."))))
      (when (stringp pin)
        (clear-string pin))
      (chirp-xchat-native-discard-recovery-input input))))

(defun chirp-dm--start-unlock ()
  "Start one explicit XChat unlock before opening its inbox."
  (let ((app (chirp-app)))
    (message "Fetching XChat key configuration...")
    (chirp-backend-whoami
     (lambda (user _envelope)
       (if-let* ((user-id (plist-get user :id)))
           (progn
             (when (appkit-app-live-p app)
               (setf (chirp--session-xchat-user-id (appkit-app-state app))
                     user-id))
             (chirp-backend-dm-recovery-input
              user-id
              (lambda (input _response-envelope)
                (chirp-dm--prompt-and-unlock app input))
              :errback #'chirp-dm--unlock-error
              :owner app))
         (chirp-dm--unlock-error
          "Authenticated X profile has no user identity")))
     #'chirp-dm--unlock-error)))

;;;###autoload
(defun chirp-dm-cancel-unlock ()
  "Cancel the active XChat recovery without automatically retrying it."
  (interactive)
  (require 'chirp-xchat-native)
  (if (chirp-xchat-native-cancel-recovery)
      (message "Canceling XChat recovery; its remote outcome may be uncertain")
    (user-error "No XChat key recovery is active")))

(defvar chirp-dm--inbox-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map appkit-directory-mode-map)
    (define-key map (kbd "g") #'chirp-dm-refresh-inbox)
    (define-key map (kbd "N") #'chirp-dm-load-more-inbox)
    (define-key map (kbd "q") #'chirp-quit-current-buffer)
    map)
  "Keymap for `chirp-dm--inbox-mode'.")

(define-derived-mode chirp-dm--inbox-mode appkit-directory-mode "Chirp-DMs"
  "Major mode for Chirp's Appkit-owned XChat inbox."
  (setq-local revert-buffer-function
              (lambda (&rest _ignored)
                (chirp-dm-refresh-inbox))))

(defun chirp-dm--open-inbox-view ()
  "Open an unlocked read-only XChat inbox and return its buffer."
  (let* ((instance (cl-incf chirp-dm--next-instance))
         (view
          (appkit-open-view
           :app (chirp-app)
           :id (list 'dm-inbox instance)
           :mode 'chirp-dm--inbox-mode
           :buffer-name (chirp--format-buffer-name "Direct Messages")
           :state (chirp-dm--make-inbox-state instance)
           :sync-function #'chirp-dm--sync-inbox
           :parts '(entries)
           :position-policy appkit-directory-key-property
           :setup #'chirp-dm--setup-inbox
           :select t)))
    (chirp-dm--request-inbox view 'initial)
    (appkit-view-buffer view)))

(defun chirp-dm--open-unlocked-inbox ()
  "Open an unlocked inbox after ensuring the current XChat user identity."
  (let* ((app (chirp-app))
         (state (appkit-app-state app))
         (user-id (chirp--session-xchat-user-id state)))
    (if (and (stringp user-id)
             (string-match-p "\\`[0-9]+\\'" user-id))
        (chirp-dm--open-inbox-view)
      (message "Resolving the authenticated XChat identity...")
      (chirp-backend-whoami
       (lambda (user _envelope)
         (if-let* ((resolved (plist-get user :id))
                   ((appkit-app-live-p app)))
             (progn
               (setf (chirp--session-xchat-user-id state) resolved)
               (chirp-dm--open-inbox-view))
           (chirp-dm--unlock-error
            "Authenticated X profile has no user identity")))
       #'chirp-dm--unlock-error))))

(defun chirp-dm-open-inbox ()
  "Unlock XChat, then open a fresh direct-message inbox."
  (require 'chirp-xchat-native)
  (chirp-xchat-native-load)
  (cond
   ((chirp-xchat-native-unlocked-p)
    (chirp-dm--open-unlocked-inbox))
   ((chirp-xchat-native-recovery-active-p)
    (user-error "XChat key recovery is already active"))
   (t
    (chirp-dm--start-unlock)
    nil)))

(defun chirp-dm--make-conversation-state (instance conversation)
  "Return canonical state for INSTANCE and normalized CONVERSATION."
  (list :type 'dm-conversation
        :instance instance
        :conversation-id (plist-get conversation :id)
        :title (plist-get conversation :title)
        :participants (copy-tree (plist-get conversation :participants))
        :events (copy-tree (plist-get conversation :events))
        :recovery-key-events nil
        :older-cursor (copy-tree (plist-get conversation :older-cursor))
        :older-available-p (plist-get conversation :has-more)
        :older-stalled-p nil
        :status (list :phase 'idle :message nil)
        :decrypt-generation nil
        :send-generation nil
        :send-error nil
        :composer-reset-p nil))

(defun chirp-dm--conversation-header (state)
  "Return generated header text for conversation STATE."
  (let ((title (chirp-dm--one-line (plist-get state :title))))
    (concat
     (propertize
      (format "Direct message: %s\n"
              (if (string-empty-p title) "Conversation" title))
      'face 'bold)
     "\n")))

(defun chirp-dm--conversation-footer (state)
  "Return generated footer text for conversation STATE."
  (let* ((status (chirp-dm--status state))
         (phase (plist-get status :phase))
         (message (plist-get status :message))
         (loading-text
          (pcase (appkit-chat-history-loading)
            ('older "loading older messages…")
            ('refresh "refreshing…")
            (_ nil))))
    (concat
     "\n"
     (appkit-chat-history-delimiter-string
      (max 20 (min 80 (window-width)))
      :loading-text loading-text)
     "\n"
     (when (plist-get state :older-stalled-p)
       (propertize "Older history made no progress.\n" 'face 'warning))
     (when (eq phase 'error)
       (propertize (format "Unable to load messages: %s\n" message)
                   'face 'error))
     (when-let* ((send-error (plist-get state :send-error)))
       (propertize (format "Unable to send message: %s\n" send-error)
                   'face 'error)))))

(defun chirp-dm--participant (state sender-id)
  "Return SENDER-ID's participant from conversation STATE."
  (cl-find sender-id (plist-get state :participants)
           :key (lambda (participant) (plist-get participant :id))
           :test #'equal))

(defun chirp-dm--event-sender-label (state event)
  "Return display sender label for EVENT in conversation STATE."
  (if-let* ((participant
             (chirp-dm--participant state (plist-get event :sender-id))))
      (or (plist-get participant :name)
          (and-let* ((handle (plist-get participant :handle)))
            (concat "@" handle))
          "Unknown sender")
    "Unknown sender"))

(defun chirp-dm--event-system-label (event)
  "Return a passive label for non-message XChat EVENT."
  (pcase (plist-get event :kind)
    ('message-delete "A message was deleted")
    ('conversation-key-change "Conversation encryption keys changed")
    ('group-change "Group details changed")
    ('conversation-metadata-change "Conversation details changed")
    ('conversation-delete "Conversation was deleted")
    ('mark-read "Conversation was marked read")
    ('mark-unread "Conversation was marked unread")
    ('pin-message "A message was pinned")
    ('unpin-message "A message was unpinned")
    (kind (format "XChat event: %s" kind))))

(defun chirp-dm--insert-reply-preview (reply)
  "Insert verified REPLY preview chrome."
  (let ((text (chirp-dm--one-line (plist-get reply :text))))
    (insert
     (propertize
      (cond
       ((not (string-empty-p text))
        (format "↪ %s" (truncate-string-to-width text 80 nil nil "…")))
       ((> (or (plist-get reply :attachment-count) 0) 0)
        "↪ [Attachment reply]")
       (t "↪ [Reply]"))
      'face 'shadow))))

(defun chirp-dm--attachment-label (attachment)
  "Return a passive fallback label for verified ATTACHMENT."
  (pcase (plist-get attachment :kind)
    ('image "[Image unavailable]")
    ('gif "[GIF unavailable]")
    ('video "[Video attachment]")
    ('audio "[Audio attachment]")
    ('file "[File attachment]")
    ('svg "[SVG unavailable]")
    ('url "[Link attachment]")
    ('post "[Post attachment]")
    ('unified-card "[Card attachment]")
    ('money "[Payment attachment]")
    (_ "[Attachment]")))

(defun chirp-dm--insert-attachment (view attachment)
  "Insert verified ATTACHMENT using VIEW's shared media workflow."
  (let ((status
         (when (memq (plist-get attachment :kind) '(image gif svg))
           (chirp-media-insert-image-resource
            view (plist-get attachment :resource-key)
            :alternate-text "[image]" :help-echo "Open image in Emacs"))))
    (pcase status
      ('rendered nil)
      ('pending
       (insert (propertize "[Image loading…]" 'face 'shadow)))
      (_
       (insert (propertize (chirp-dm--attachment-label attachment)
                           'face 'shadow))))))

(defun chirp-dm--insert-message-content (view event context)
  "Insert EVENT content and projected CONTEXT using Appkit VIEW resources."
  (let* ((text (plist-get event :text))
         (content-kind (plist-get event :content-kind))
         (body
          (pcase content-kind
            ('reaction
             (and (stringp text) (format "Reaction: %s" text)))
            ('reaction-removed
             (and (stringp text) (format "Reaction removed: %s" text)))
            ('edit
             (and (stringp text) (format "Edited message: %s" text)))
            (_ (and (stringp text) (not (string-empty-p text)) text))))
         (reply (plist-get context :reply))
         (attachments (plist-get event :attachments))
         inserted-p)
    (when reply
      (chirp-dm--insert-reply-preview reply)
      (setq inserted-p t))
    (when body
      (when inserted-p (insert "\n"))
      (insert body)
      (setq inserted-p t))
    (dolist (attachment attachments)
      (when inserted-p (insert "\n"))
      (chirp-dm--insert-attachment view attachment)
      (setq inserted-p t))
    (dotimes (_index (max 0 (- (or (plist-get event :attachment-count) 0)
                               (length attachments))))
      (when inserted-p (insert "\n"))
      (insert (propertize "[Attachment]" 'face 'shadow))
      (setq inserted-p t))
    (unless inserted-p
      (insert (if (plist-get event :decrypted-p)
                  "[Verified non-text message]"
                "[Message content unavailable]")))))

(defun chirp-dm--print-event-row (row)
  "Insert one projected XChat event ROW."
  (let* ((event (appkit-chat-timeline-row-payload row))
         (context (appkit-chat-timeline-row-context row))
         (start (point))
         (kind (plist-get event :kind))
         (timestamp (chirp-dm--format-time
                     (plist-get event :created-at-msec))))
    (if (eq kind 'message)
        (progn
          (insert (propertize (or (plist-get context :sender-label)
                                  "Unknown sender")
                              'face 'bold))
          (unless (string-empty-p timestamp)
            (insert "  " (propertize timestamp 'face 'shadow)))
          (insert "\n")
          (chirp-dm--insert-message-content
           (appkit-current-view) event context)
          (insert "\n\n"))
      (insert (propertize
               (format "— %s%s —\n\n"
                       (chirp-dm--event-system-label event)
                       (if (string-empty-p timestamp)
                           ""
                         (concat " · " timestamp)))
               'face 'shadow)))
    (add-text-properties
     start (point)
     (list 'read-only t
           'front-sticky '(read-only)
           'rear-nonsticky '(read-only)
           'chirp-dm-message-id (plist-get event :id)
           'chirp-dm-event event))))

(defun chirp-dm--project-conversation-events (state events)
  "Project ordered XChat EVENTS using participant metadata from STATE."
  (appkit-chat-timeline-project
   events
   (lambda (event) (plist-get event :id))
   :context-function
   (lambda (_previous event)
     (when (eq (plist-get event :kind) 'message)
       (list :sender-label (chirp-dm--event-sender-label state event)
             :reply
             (when (plist-get event :reply-p)
               (list :text (plist-get event :reply-text)
                     :attachment-count
                     (plist-get event :reply-attachment-count))))))
   :dependencies-function
   (lambda (event)
     (mapcar (lambda (attachment)
               (plist-get attachment :resource-key))
             (plist-get event :attachments)))))

(defun chirp-dm--composer-prompt (state)
  "Return the Appkit composer prompt for conversation STATE."
  (if (plist-get state :send-generation) "…> " ">>> "))

(defun chirp-dm--bind-composer (state)
  "Create the trailing Appkit composer from conversation STATE."
  (appkit-chatbuf-bind-input-region
   :visible-p t
   :prompt (chirp-dm--composer-prompt state)
   :input-text (appkit-chatbuf-input-state)))

(defun chirp-dm--sync-composer-state (state)
  "Project pending and reset fields from STATE without rebuilding input."
  (setq buffer-read-only nil)
  (appkit-chatbuf-prompt-update (chirp-dm--composer-prompt state))
  (when (plist-get state :composer-reset-p)
    (setf (plist-get state :composer-reset-p) nil)
    (appkit-chatbuf-input-set-text (appkit-chatbuf-input-state)))
  (appkit-chatbuf-input-apply-text-properties)
  (when (plist-get state :send-generation)
    (setq buffer-read-only t)))

(defun chirp-dm--sync-conversation (view invalidations)
  "Synchronize conversation VIEW for pending INVALIDATIONS."
  (let* ((state (chirp-dm--conversation-state view))
         (title (chirp-dm--one-line (plist-get state :title)))
         (slice
          (appkit-chat-history-window-slice
           (plist-get state :events)
           (lambda (event) (plist-get event :id)))))
    (unless (plist-get slice :valid-p)
      (error "Invalid XChat history window: %s" (plist-get slice :reason)))
    (setq-local chirp--view-title
                (format "DM: %s"
                        (if (string-empty-p title) "Conversation" title)))
    (appkit-chat-timeline-run-preserving-position
     (lambda ()
       (appkit-chat-timeline-sync
        (chirp-dm--project-conversation-events
         state (plist-get slice :entries))
        :changed-resources
        (appkit-invalidations-resource-keys invalidations))
       (appkit-chat-timeline-set-frame
        (chirp-dm--conversation-header state)
        (chirp-dm--conversation-footer state)
        :bind-input-function (lambda () (chirp-dm--bind-composer state))
        :composer-visible-p t)
       (chirp-dm--sync-composer-state state)))))

(defun chirp-dm--establish-conversation-window (state)
  "Establish Appkit's exact history window from conversation STATE."
  (let ((events (plist-get state :events)))
    (if events
        (progn
          (appkit-chat-history-window-set
           (plist-get (car events) :id) nil)
          (appkit-chat-history-older-loaded-set
           (not (plist-get state :older-available-p))))
      (appkit-chat-history-window-establish-empty)
      (when (plist-get state :older-available-p)
        (let ((status (chirp-dm--status state)))
          (setf (plist-get status :phase) 'error
                (plist-get status :message)
                "XChat did not provide a history cursor"))))))

(defun chirp-dm--ensure-conversation-timeline ()
  "Ensure the current conversation owns one Appkit chat timeline."
  (appkit-chat-timeline-ensure
   :printer #'chirp-dm--print-event-row
   :anchor-property 'chirp-dm-message-id
   :header ""
   :footer ""))

(defun chirp-dm--setup-conversation (view)
  "Initialize Appkit chat controllers for conversation VIEW."
  (let ((state (chirp-dm--conversation-state view)))
    (appkit-chatbuf-reset-state)
    (appkit-chat-history-reset-state)
    (chirp-dm--establish-conversation-window state)
    (chirp-dm--ensure-conversation-timeline)
    (chirp-dm--prefetch-event-media view (plist-get state :events))
    (appkit-invalidate view :structure t :parts '(frame timeline) :position t)
    (appkit-sync-invalidations view)))

(defun chirp-dm--conversation-owner-current-p (view state generation)
  "Return non-nil when GENERATION owns VIEW's history request for STATE."
  (and (appkit-view-live-p view)
       (eq state (appkit-view-state view))
       (buffer-live-p (appkit-view-buffer view))
       (with-current-buffer (appkit-view-buffer view)
         (appkit-chat-history-request-current-p generation))))

(defun chirp-dm--retire-conversation-transport (view state generation)
  "Retire GENERATION's transport when it still owns VIEW and STATE."
  (when (chirp-dm--conversation-owner-current-p view state generation)
    (remhash chirp-dm--conversation-request-key
             (appkit-view-request-table view))))

(defun chirp-dm--finish-conversation-request (view generation)
  "End GENERATION's Appkit history request in VIEW."
  (with-current-buffer (appkit-view-buffer view)
    (appkit-chat-history-request-end generation)))

(defun chirp-dm--settle-older-success
    (view state generation events envelope)
  "Settle older GENERATION in VIEW and STATE with EVENTS and ENVELOPE."
  (when (chirp-dm--conversation-owner-current-p view state generation)
    (let* ((current (plist-get state :events))
           (old-first (and current (plist-get (car current) :id)))
           (old-cursor (plist-get state :older-cursor))
           (merged (chirp-dm--merge-events current events))
           (new-first (and merged (plist-get (car merged) :id)))
           (next-cursor (chirp-backend-envelope-next-cursor envelope))
           (recovery-key-events
            (chirp-dm--history-key-events envelope))
           (complete (and (chirp-get-in envelope
                                        '("pagination" "complete"))
                          t))
           (progressed (or (not (equal old-first new-first))
                           (and next-cursor
                                (not (equal old-cursor next-cursor)))))
           (status (chirp-dm--status state)))
      (chirp-dm--finish-conversation-request view generation)
      (setf (plist-get state :events) merged
            (plist-get state :recovery-key-events)
            (chirp-dm--append-unique
             (plist-get state :recovery-key-events)
             recovery-key-events
             (lambda (event)
               (or (plist-get event :id)
                   (plist-get event :encoded-event))))
            (plist-get status :phase) 'idle
            (plist-get status :message) nil)
      (when next-cursor
        (setf (plist-get state :older-cursor) next-cursor))
      (when (and new-first (not (equal old-first new-first)))
        (with-current-buffer (appkit-view-buffer view)
          (appkit-chat-history-window-set new-first nil)))
      (with-current-buffer (appkit-view-buffer view)
        (when complete
          (appkit-chat-history-older-loaded-set t)))
      (setf (plist-get state :older-stalled-p)
            (and (not complete) (not progressed)))
      (appkit-request-sync
       view :structure t :parts '(frame timeline) :position t)
      (when (chirp-dm--decryption-needed-p state)
        (chirp-dm--decrypt-view view t)))))

(cl-defun chirp-dm--settle-refresh-success
    (view state generation conversation
          &key recovery-key-events history-first-key history-cursor
          older-complete-p)
  "Settle a continuous refreshed CONVERSATION for GENERATION.

VIEW and STATE identify the conversation.  RECOVERY-KEY-EVENTS came from any
history pages used to prove continuity.  HISTORY-FIRST-KEY and HISTORY-CURSOR
describe that bridge's older edge.  OLDER-COMPLETE-P means those pages also
reached the oldest remote edge."
  (when (chirp-dm--conversation-owner-current-p view state generation)
    (let* ((current (plist-get state :events))
           (merged
            (chirp-dm--merge-events
             current (plist-get conversation :events)))
           (first (and merged (plist-get (car merged) :id)))
           (status (chirp-dm--status state)))
      (chirp-dm--finish-conversation-request view generation)
      (setf (plist-get state :events) merged
            (plist-get state :title) (plist-get conversation :title)
            (plist-get state :participants)
            (copy-tree (plist-get conversation :participants))
            (plist-get state :recovery-key-events)
            (chirp-dm--append-unique
             (plist-get state :recovery-key-events)
             recovery-key-events
             (lambda (event)
               (or (plist-get event :id)
                   (plist-get event :encoded-event))))
            (plist-get status :phase) 'idle
            (plist-get status :message) nil)
      (cond
       (older-complete-p
        (setf (plist-get state :older-cursor) nil
              (plist-get state :older-available-p) nil
              (plist-get state :older-stalled-p) nil)
        (with-current-buffer (appkit-view-buffer view)
          (appkit-chat-history-window-set first nil)
          (appkit-chat-history-older-loaded-set t)))
       ((and history-first-key (equal first history-first-key))
        (setf (plist-get state :older-cursor) (copy-tree history-cursor)
              (plist-get state :older-available-p) (and history-cursor t)
              (plist-get state :older-stalled-p) nil)
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
      (appkit-request-sync
       view :structure t :parts '(frame timeline) :position t)
      (when (chirp-dm--decryption-needed-p state)
        (chirp-dm--decrypt-view view)))))

(defun chirp-dm--settle-conversation-error
    (view state generation phase message)
  "Settle conversation GENERATION and PHASE with error MESSAGE.

VIEW and STATE identify the conversation whose request failed."
  (when (chirp-dm--conversation-owner-current-p view state generation)
    (let ((status (chirp-dm--status state)))
      (chirp-dm--finish-conversation-request view generation)
      (setf (plist-get status :phase) 'error
            (plist-get status :message) message)
      (appkit-request-sync view :part 'frame :position t)
      (message "%s" (replace-regexp-in-string "[\r\n]+" "  " message))
      (when (and (eq phase 'refresh)
                 (chirp-dm--decryption-needed-p state))
        (chirp-dm--decrypt-view view)))))

(cl-defun chirp-dm--request-refresh-bridge
    (view state generation conversation
          &key cursor recovery-key-events
          (remaining chirp-dm--refresh-bridge-page-limit))
  "Bridge refreshed CONVERSATION back to STATE's visible event window.

GENERATION owns the multi-stage request in VIEW.  CURSOR selects the next
older history page, RECOVERY-KEY-EVENTS accumulates its key history, and
REMAINING bounds the automatic page count."
  (let (callback-ran-p request)
    (setq request
          (chirp-backend-dm-history
           (plist-get state :conversation-id)
           cursor
           (lambda (events envelope)
             (setq callback-ran-p t)
             (chirp-dm--retire-conversation-transport
              view state generation)
             (when (chirp-dm--conversation-owner-current-p
                    view state generation)
               (let* ((bridged (copy-sequence conversation))
                      (bridged-events
                       (chirp-dm--merge-events
                        (plist-get conversation :events) events))
                      (key-events
                       (chirp-dm--append-unique
                        recovery-key-events
                        (chirp-dm--history-key-events envelope)
                        (lambda (event)
                          (or (plist-get event :id)
                              (plist-get event :encoded-event)))))
                      (next-cursor
                       (chirp-backend-envelope-next-cursor envelope))
                      (complete
                       (and (chirp-get-in envelope
                                          '("pagination" "complete"))
                            t)))
                 (setf (plist-get bridged :events) bridged-events)
                 (cond
                  ((chirp-dm--events-overlap-p
                    (plist-get state :events) bridged-events)
                   (chirp-dm--settle-refresh-success
                    view state generation bridged
                    :recovery-key-events key-events
                    :history-first-key
                    (plist-get (car bridged-events) :id)
                    :history-cursor next-cursor
                    :older-complete-p complete))
                  ((and (not complete)
                        next-cursor
                        (not (equal cursor next-cursor))
                        (> remaining 1))
                   (chirp-dm--request-refresh-bridge
                    view state generation bridged
                    :cursor next-cursor
                    :recovery-key-events key-events
                    :remaining (1- remaining)))
                  (t
                   (chirp-dm--settle-conversation-error
                    view state generation 'refresh
                    "XChat history could not bridge the visible timeline"))))))
           :max-results chirp-dm-history-page-size
           :errback
           (lambda (text)
             (setq callback-ran-p t)
             (chirp-dm--retire-conversation-transport
              view state generation)
             (chirp-dm--settle-conversation-error
              view state generation 'refresh text))
           :owner view))
    (cond
     ((and (not callback-ran-p)
           request
           (chirp-dm--conversation-owner-current-p view state generation))
      (puthash chirp-dm--conversation-request-key request
               (appkit-view-request-table view)))
     ((and (not callback-ran-p)
           (null request)
           (chirp-dm--conversation-owner-current-p view state generation))
      (chirp-dm--settle-conversation-error
       view state generation 'refresh
       "XChat history bridge request did not start")))
    request))

(defun chirp-dm--accept-refresh-success
    (view state generation conversation)
  "Accept refreshed CONVERSATION for GENERATION in VIEW and STATE.

Disjoint focused fragments are bridged through older history before merging."
  (when (chirp-dm--conversation-owner-current-p view state generation)
    (let ((current (plist-get state :events))
          (refreshed (plist-get conversation :events)))
      (if (and current refreshed
               (not (chirp-dm--events-overlap-p current refreshed)))
          (if-let* ((cursor (plist-get conversation :older-cursor)))
              (chirp-dm--request-refresh-bridge
               view state generation conversation :cursor cursor)
            (chirp-dm--settle-conversation-error
             view state generation 'refresh
             "XChat refresh has no cursor to bridge the visible timeline"))
        (chirp-dm--settle-refresh-success
         view state generation conversation)))))

(defun chirp-dm--request-conversation (view phase)
  "Start conversation request PHASE owned by VIEW."
  (let* ((state (chirp-dm--conversation-state view))
         (status (chirp-dm--status state))
         (generation (list 'dm-conversation-generation))
         callback-ran-p
         request)
    (setf (plist-get status :phase) 'idle
          (plist-get status :message) nil)
    (with-current-buffer (appkit-view-buffer view)
      (appkit-chat-history-request-begin phase generation))
    (chirp-dm--cancel-request view chirp-dm--conversation-request-key)
    (appkit-request-sync view :part 'frame :position t)
    (setq request
          (pcase phase
            ('older
             (chirp-backend-dm-history
              (plist-get state :conversation-id)
              (plist-get state :older-cursor)
              (lambda (events envelope)
                (setq callback-ran-p t)
                (chirp-dm--retire-conversation-transport
                 view state generation)
                (chirp-dm--settle-older-success
                 view state generation events envelope))
              :max-results chirp-dm-history-page-size
              :errback
              (lambda (text)
                (setq callback-ran-p t)
                (chirp-dm--retire-conversation-transport
                 view state generation)
                (chirp-dm--settle-conversation-error
                 view state generation phase text))
              :owner view))
            ('refresh
             (chirp-backend-dm-conversation-data
              (plist-get state :conversation-id)
              (lambda (conversation _envelope)
                (setq callback-ran-p t)
                (chirp-dm--retire-conversation-transport
                 view state generation)
                (chirp-dm--accept-refresh-success
                 view state generation conversation))
              :errback
              (lambda (text)
                (setq callback-ran-p t)
                (chirp-dm--retire-conversation-transport
                 view state generation)
                (chirp-dm--settle-conversation-error
                 view state generation phase text))
              :owner view))
            (_ (error "Unknown XChat conversation request phase: %S" phase))))
    (cond
     ((and (not callback-ran-p)
           request
           (chirp-dm--conversation-owner-current-p view state generation))
      (puthash chirp-dm--conversation-request-key request
               (appkit-view-request-table view)))
     ((and (not callback-ran-p)
           (null request)
           (chirp-dm--conversation-owner-current-p view state generation))
      (chirp-dm--settle-conversation-error
       view state generation phase
       "XChat conversation request did not start")))
    request))

(defun chirp-dm--send-current-p (view state generation)
  "Return non-nil when GENERATION owns the active send in VIEW and STATE."
  (and (appkit-view-live-p view)
       (eq state (appkit-view-state view))
       (eq generation (plist-get state :send-generation))))

(defun chirp-dm--settle-send-error (view state generation message)
  "Settle send GENERATION in VIEW and STATE with error MESSAGE."
  (when (chirp-dm--send-current-p view state generation)
    (remhash chirp-dm--send-request-key (appkit-view-request-table view))
    (setf (plist-get state :send-generation) nil
          (plist-get state :send-error) message)
    (appkit-request-sync view :part 'frame :position t)
    (message "%s" (replace-regexp-in-string "[\r\n]+" "  " message))))

(defun chirp-dm--settle-send-success (view state generation text)
  "Settle acknowledged send GENERATION for TEXT in VIEW and STATE."
  (when (chirp-dm--send-current-p view state generation)
    (remhash chirp-dm--send-request-key (appkit-view-request-table view))
    (setf (plist-get state :send-generation) nil
          (plist-get state :send-error) nil
          (plist-get state :composer-reset-p) t)
    (appkit-with-live-view view
      (appkit-chatbuf-input-history-push text)
      (appkit-chatbuf-input-state-clear :reset-history-p t))
    (appkit-request-sync view :part 'frame :position t)
    (chirp-dm--request-conversation view 'refresh)
    (message "Direct message sent")))

(defun chirp-dm-submit ()
  "Encrypt and send the current plain-text XChat composer input once."
  (interactive)
  (if-let* ((view (chirp-dm--current-view 'dm-conversation)))
      (let* ((state (chirp-dm--conversation-state view))
             (generation (list 'dm-send-generation))
             callback-ran-p request text)
        (unless (appkit-chatbuf-point-in-input-p)
          (user-error "Point is not in the direct-message composer"))
        (when (plist-get state :send-generation)
          (user-error "A direct message is already being sent"))
        (let* ((input
                (plist-get (appkit-chatbuf-input-state-sync) :value))
               (plain (appkit-chatbuf-string-plain-text input)))
          (when (appkit-chatbuf-string-has-objects-p input)
            (user-error "XChat sending currently supports plain text only"))
          (when (appkit-chatbuf-composer-idle-p)
            (user-error "Direct message is empty"))
          (setq text (copy-sequence plain)))
        (setf (plist-get state :send-generation) generation
              (plist-get state :send-error) nil)
        (setq buffer-read-only t)
        (appkit-request-sync view :part 'frame :position t)
        (condition-case err
            (setq request
                  (chirp-backend-dm-send-text
                   (plist-get state :conversation-id) text
                   (lambda (_event _envelope)
                     (setq callback-ran-p t)
                     (chirp-dm--settle-send-success
                      view state generation text))
                   :errback
                   (lambda (message)
                     (setq callback-ran-p t)
                     (chirp-dm--settle-send-error
                      view state generation message))
                   :owner view))
          ((error quit)
           (chirp-dm--settle-send-error
            view state generation (error-message-string err))
           (signal (car err) (cdr err))))
        (when (and (not callback-ran-p)
                   (chirp-dm--send-current-p view state generation))
          (if (buffer-live-p request)
              (puthash chirp-dm--send-request-key request
                       (appkit-view-request-table view))
            (chirp-dm--settle-send-error
             view state generation "XChat message request did not start")))
        (when (chirp-dm--send-current-p view state generation)
          (message "Sending direct message..."))
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

(defun chirp-dm-refresh-conversation ()
  "Refresh the current XChat conversation without acknowledging reads."
  (interactive)
  (if-let* ((view (chirp-dm--current-view 'dm-conversation)))
      (chirp-dm--request-conversation view 'refresh)
    (user-error "Current view is not a direct-message conversation")))

(defun chirp-dm-load-older-messages ()
  "Load one older page in the current XChat conversation."
  (interactive)
  (if-let* ((view (chirp-dm--current-view 'dm-conversation)))
      (let ((state (chirp-dm--conversation-state view)))
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
          (chirp-dm--request-conversation view 'older))))
    (user-error "Current view is not a direct-message conversation")))

(defun chirp-dm--move-message (direction)
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
  (chirp-dm--move-message 1))

(defun chirp-dm-previous-message ()
  "Move to the previous visible direct-message event."
  (interactive)
  (chirp-dm--move-message -1))

(defvar chirp-dm--timeline-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'chirp-dm-refresh-conversation)
    (define-key map (kbd "N") #'chirp-dm-load-older-messages)
    (define-key map (kbd "n") #'chirp-dm-next-message)
    (define-key map (kbd "p") #'chirp-dm-previous-message)
    (define-key map (kbd "q") #'chirp-quit-current-buffer)
    map)
  "Timeline-only keymap active outside the XChat composer.")

(define-minor-mode chirp-dm--timeline-mode
  "Enable direct-message navigation keys outside the XChat composer."
  :init-value nil
  :lighter nil
  :keymap chirp-dm--timeline-mode-map)

(defvar chirp-dm--conversation-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'chirp-dm-return-dwim)
    (define-key map (kbd "C-c C-c") #'chirp-dm-submit)
    (define-key map (kbd "C-c C-r") #'chirp-dm-refresh-conversation)
    (define-key map (kbd "C-c C-n") #'chirp-dm-load-older-messages)
    map)
  "Keymap for `chirp-dm--conversation-mode'.")

(defun chirp-dm--setup-evil ()
  "Install optional Evil bindings for XChat conversation views."
  (when appkit-evil-enable-integration
    (appkit-evil-define-keys '(normal motion) 'chirp-dm--conversation-mode-map
      (kbd "RET") #'chirp-dm-return-dwim
      (kbd "g r") #'chirp-dm-refresh-conversation)
    (appkit-evil-define-keys '(normal motion) 'chirp-dm--timeline-mode-map
      (kbd "q") #'chirp-quit-current-buffer
      (kbd "n") #'chirp-dm-next-message
      (kbd "p") #'chirp-dm-previous-message
      (kbd "N") #'chirp-dm-load-older-messages)))

(chirp-dm--setup-evil)

(define-derived-mode chirp-dm--conversation-mode appkit-chatbuf-mode "Chirp-DM"
  "Major mode for one XChat conversation with a plain-text composer."
  (setq-local truncate-lines nil)
  (setq-local word-wrap t)
  (setq-local line-spacing 0)
  (add-hook 'chirp-dm--timeline-mode-hook
            #'appkit-evil-normalize-keymaps nil t)
  (appkit-chatbuf-use-timeline-mode #'chirp-dm--timeline-mode)
  (visual-line-mode 1))

(cl-defun chirp-dm--open-conversation (conversation &key refresh-p)
  "Open a fresh chat view for normalized CONVERSATION.

When REFRESH-P is non-nil, fetch focused conversation data before decryption."
  (unless (and (listp conversation)
               (stringp (plist-get conversation :id)))
    (user-error "Direct-message conversation is invalid"))
  (let* ((instance (cl-incf chirp-dm--next-instance))
         (title (chirp-dm--one-line (plist-get conversation :title)))
         (view
          (appkit-open-view
           :app (chirp-app)
           :id (list 'dm-conversation instance)
           :mode 'chirp-dm--conversation-mode
           :buffer-name
           (chirp--format-buffer-name
            (format "DM: %s"
                    (if (string-empty-p title) "Conversation" title)))
           :state (chirp-dm--make-conversation-state instance conversation)
           :sync-function #'chirp-dm--sync-conversation
           :parts '(frame timeline composer)
           :position-policy 'chirp-dm-message-id
           :setup #'chirp-dm--setup-conversation
           :select t)))
    (if refresh-p
        (chirp-dm--request-conversation view 'refresh)
      (when (chirp-dm--decryption-needed-p (appkit-view-state view))
        (chirp-dm--decrypt-view view)))
    (appkit-view-buffer view)))

(provide 'chirp-dm)

;;; chirp-dm.el ends here
