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
(require 'appkit-chat-history)
(require 'appkit-chat-timeline)
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

;;; Variables

(defvar chirp-dm-conversation--next-instance 0
  "Monotonic identity source for fresh direct-message views.")

;;; Constants

(defconst chirp-dm-conversation--request-key 'dm-conversation
  "Request-table key for one conversation view's active transport.")

(defconst chirp-dm-conversation--decrypt-request-key 'dm-decrypt
  "Request-table key for one conversation view's signing-key retrieval.")

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

(defun chirp-dm-conversation--decrypt-input (state)
  "Return encoded events and signing-key user IDs from conversation STATE."
  (let* ((conversation (chirp-dm-conversation--conversation state))
         (events (plist-get conversation :events))
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
  "Return non-nil when conversation STATE has encrypted messages."
  (cl-some (lambda (event) (plist-get event :encrypted-p))
           (chirp-dm-conversation--events state)))

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

(defun chirp-dm-conversation--settle-decrypt-error (view state generation message)
  "Settle decryption GENERATION in VIEW and STATE with error MESSAGE."
  (when (chirp-view-state-token-current-p view state generation :decrypt-generation)
    (setf (plist-get state :decrypt-generation) nil)
    (remhash chirp-dm-conversation--decrypt-request-key
             (appkit-view-request-table view))
    (display-warning 'chirp message :warning)))

(defun chirp-dm-conversation--settle-decrypt-success
    (view state generation encoded signing-keys)
  "Decrypt ENCODED events with SIGNING-KEYS for GENERATION in VIEW and STATE."
  (when (chirp-view-state-token-current-p view state generation :decrypt-generation)
    (condition-case err
        (let ((messages
               (cl-loop for batch in (seq-partition encoded 200)
                        append
                        (chirp-xchat-native-decrypt-events
                         (chirp-dm-conversation--id state)
                         batch signing-keys))))
          (setf (plist-get state :decrypt-generation) nil)
          (remhash chirp-dm-conversation--decrypt-request-key
                   (appkit-view-request-table view))
          (let ((updated
                 (chirp-dm-conversation--apply-verified-messages
                  state messages)))
            (when (> updated 0)
              (chirp-dm-state-publish
               (chirp-dm-conversation--conversation state)))
            (message "Decrypted %d verified XChat message%s"
                     updated (if (= updated 1) "" "s"))))
      (error
       (chirp-dm-conversation--settle-decrypt-error
        view state generation (error-message-string err))))))

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
                     (chirp-dm-conversation--events state))))
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
        (chirp-dm-conversation--request view 'older))
       ((null user-ids)
        (display-warning 'chirp "XChat signing-key users are unavailable" :warning))
       ((> (length user-ids) 100)
        (display-warning 'chirp "XChat conversation has too many signing-key users" :warning))
       (t
        (setf (plist-get state :decrypt-generation) generation)
        (chirp-cancel-view-request view chirp-dm-conversation--decrypt-request-key #'chirp-x-cancel-request)
        (setq request
              (chirp-backend-dm-signing-keys
               user-ids
               (lambda (signing-keys _envelope)
                 (setq callback-ran-p t)
                 (when (chirp-view-state-token-current-p view state generation :decrypt-generation)
                   (chirp-dm-conversation--settle-decrypt-success
                    view state generation encoded signing-keys)))
               :errback
               (lambda (message)
                 (setq callback-ran-p t)
                 (chirp-dm-conversation--settle-decrypt-error
                  view state generation message))
               :owner view))
        (when (and (buffer-live-p request)
                   (chirp-view-state-token-current-p view state generation :decrypt-generation))
          (puthash chirp-dm-conversation--decrypt-request-key request
                   (appkit-view-request-table view)))
        (when (and (null request) (not callback-ran-p))
          (chirp-dm-conversation--settle-decrypt-error
           view state generation "XChat signing-key request did not start")))))))

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
          :decrypt-generation nil
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

(defun chirp-dm-conversation--sync (view invalidations)
  "Synchronize conversation VIEW for pending INVALIDATIONS."
  (let* ((state (chirp-dm-conversation--state view))
         (conversation (chirp-dm-conversation--conversation state))
         (title
          (chirp-dm-conversation--one-line
           (plist-get conversation :title)))
         (slice
          (appkit-chat-history-window-slice
           (plist-get conversation :events)
           (lambda (event) (plist-get event :id))))
         (rows
          (and (plist-get slice :valid-p)
               (chirp-dm-render-project-events
                view conversation (plist-get slice :entries))))
         (force-keys
          (and (memq 'geometry
                     (appkit-invalidations-parts invalidations))
               (mapcar #'appkit-chat-timeline-row-key rows))))
    (unless (plist-get slice :valid-p)
      (error "Invalid XChat history window: %s" (plist-get slice :reason)))
    (setq-local chirp--view-title
                (format "DM: %s"
                        (if (string-empty-p title) "Conversation" title)))
    (appkit-chat-timeline-run-preserving-position
     (lambda ()
       (appkit-chat-timeline-sync
        rows
        :force-keys force-keys
        :changed-resources
        (appkit-invalidations-resource-keys invalidations))
       (appkit-chat-timeline-set-frame
        (chirp-dm-render-header conversation)
        (chirp-dm-render-footer state)
        :bind-input-function (lambda () (chirp-dm-conversation--bind-composer state))
        :composer-visible-p t)
       (chirp-dm-conversation--sync-composer-state state)))))

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
    (appkit-invalidate view :structure t :parts '(frame timeline) :position t)
    (appkit-sync-invalidations view)))

;;;; Requests

(defun chirp-dm-conversation--owner-current-p (view state generation)
  "Return non-nil when GENERATION owns VIEW's history request for STATE."
  (and (appkit-view-live-p view)
       (eq state (appkit-view-state view))
       (buffer-live-p (appkit-view-buffer view))
       (with-current-buffer (appkit-view-buffer view)
         (appkit-chat-history-request-current-p generation))))

(defun chirp-dm-conversation--retire-transport (view state generation)
  "Retire GENERATION's transport when it still owns VIEW and STATE."
  (when (chirp-dm-conversation--owner-current-p view state generation)
    (remhash chirp-dm-conversation--request-key
             (appkit-view-request-table view))))

(defun chirp-dm-conversation--finish-request (view generation)
  "End GENERATION's Appkit history request in VIEW."
  (with-current-buffer (appkit-view-buffer view)
    (appkit-chat-history-request-end generation)))

(defun chirp-dm-conversation--settle-older-success
    (view state generation events envelope)
  "Settle older GENERATION in VIEW and STATE with EVENTS and ENVELOPE."
  (when (chirp-dm-conversation--owner-current-p view state generation)
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
      (chirp-dm-conversation--finish-request view generation)
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
    (view state generation conversation
          &key recovery-key-events history-first-key history-cursor
          older-complete-p)
  "Settle a continuous refreshed CONVERSATION for GENERATION.

VIEW and STATE identify the conversation.  RECOVERY-KEY-EVENTS came from any
history pages used to prove continuity.  HISTORY-FIRST-KEY and HISTORY-CURSOR
describe that bridge's older edge.  OLDER-COMPLETE-P means those pages also
reached the oldest remote edge."
  (when (chirp-dm-conversation--owner-current-p view state generation)
    (let* ((canonical (chirp-dm-conversation--conversation state))
           (current (plist-get canonical :events))
           (merged
            (chirp-dm-state-merge-events
             current (plist-get conversation :events)))
           (first (and merged (plist-get (car merged) :id)))
           (status (chirp-dm-conversation--status state)))
      (chirp-dm-conversation--finish-request view generation)
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
    (view state generation phase message)
  "Settle conversation GENERATION and PHASE with error MESSAGE.

VIEW and STATE identify the conversation whose request failed."
  (when (chirp-dm-conversation--owner-current-p view state generation)
    (let ((status (chirp-dm-conversation--status state)))
      (chirp-dm-conversation--finish-request view generation)
      (setf (plist-get status :phase) 'error
            (plist-get status :message) message)
      (appkit-request-sync view :part 'frame :position t)
      (message "%s" (replace-regexp-in-string "[\r\n]+" "  " message))
      (when (and (eq phase 'refresh)
                 (chirp-dm-conversation--decryption-needed-p state))
        (chirp-dm-conversation--decrypt-view view)))))

(cl-defun chirp-dm-conversation--request-refresh-bridge
    (view state generation conversation
          &key cursor recovery-key-events
          (remaining chirp-dm-conversation--refresh-bridge-page-limit))
  "Bridge refreshed CONVERSATION back to STATE's visible event window.

GENERATION owns the multi-stage request in VIEW.  CURSOR selects the next
older history page, RECOVERY-KEY-EVENTS accumulates its key history, and
REMAINING bounds the automatic page count."
  (let (callback-ran-p request)
    (setq request
          (chirp-backend-dm-history
           (chirp-dm-conversation--id state)
           cursor
           (lambda (events envelope)
             (setq callback-ran-p t)
             (chirp-dm-conversation--retire-transport
              view state generation)
             (when (chirp-dm-conversation--owner-current-p
                    view state generation)
               (let* ((bridged (copy-sequence conversation))
                      (bridged-events
                       (chirp-dm-state-merge-events (plist-get conversation :events) events))
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
                   (chirp-dm-conversation--request-refresh-bridge
                    view state generation bridged
                    :cursor next-cursor
                    :recovery-key-events key-events
                    :remaining (1- remaining)))
                  (t
                   (chirp-dm-conversation--settle-error
                    view state generation 'refresh
                    "XChat history could not bridge the visible timeline"))))))
           :max-results chirp-dm-history-page-size
           :errback
           (lambda (text)
             (setq callback-ran-p t)
             (chirp-dm-conversation--retire-transport
              view state generation)
             (chirp-dm-conversation--settle-error
              view state generation 'refresh text))
           :owner view))
    (cond
     ((and (not callback-ran-p)
           request
           (chirp-dm-conversation--owner-current-p view state generation))
      (puthash chirp-dm-conversation--request-key request
               (appkit-view-request-table view)))
     ((and (not callback-ran-p)
           (null request)
           (chirp-dm-conversation--owner-current-p view state generation))
      (chirp-dm-conversation--settle-error
       view state generation 'refresh
       "XChat history bridge request did not start")))
    request))

(defun chirp-dm-conversation--accept-refresh-success
    (view state generation conversation)
  "Accept refreshed CONVERSATION for GENERATION in VIEW and STATE.

Disjoint focused fragments are bridged through older history before merging."
  (when (chirp-dm-conversation--owner-current-p view state generation)
    (let ((current (chirp-dm-conversation--events state))
          (refreshed (plist-get conversation :events)))
      (if (and current refreshed
               (not (chirp-dm-conversation--events-overlap-p current refreshed)))
          (if-let* ((cursor (plist-get conversation :older-cursor)))
              (chirp-dm-conversation--request-refresh-bridge
               view state generation conversation :cursor cursor)
            (chirp-dm-conversation--settle-error
             view state generation 'refresh
             "XChat refresh has no cursor to bridge the visible timeline"))
        (chirp-dm-conversation--settle-refresh-success
         view state generation conversation)))))

(defun chirp-dm-conversation--request (view phase)
  "Start conversation request PHASE owned by VIEW."
  (let* ((state (chirp-dm-conversation--state view))
         (status (chirp-dm-conversation--status state))
         (generation (list 'dm-conversation-generation))
         callback-ran-p
         request)
    (setf (plist-get status :phase) 'idle
          (plist-get status :message) nil)
    (with-current-buffer (appkit-view-buffer view)
      (appkit-chat-history-request-begin phase generation))
    (chirp-cancel-view-request view chirp-dm-conversation--request-key #'chirp-x-cancel-request)
    (appkit-request-sync view :part 'frame :position t)
    (setq request
          (pcase phase
            ('older
             (chirp-backend-dm-history
              (chirp-dm-conversation--id state)
              (plist-get state :older-cursor)
              (lambda (events envelope)
                (setq callback-ran-p t)
                (chirp-dm-conversation--retire-transport
                 view state generation)
                (chirp-dm-conversation--settle-older-success
                 view state generation events envelope))
              :max-results chirp-dm-history-page-size
              :errback
              (lambda (text)
                (setq callback-ran-p t)
                (chirp-dm-conversation--retire-transport
                 view state generation)
                (chirp-dm-conversation--settle-error
                 view state generation phase text))
              :owner view))
            ('refresh
             (chirp-backend-dm-conversation-data
              (chirp-dm-conversation--id state)
              (lambda (conversation _envelope)
                (setq callback-ran-p t)
                (chirp-dm-conversation--retire-transport
                 view state generation)
                (chirp-dm-conversation--accept-refresh-success
                 view state generation conversation))
              :errback
              (lambda (text)
                (setq callback-ran-p t)
                (chirp-dm-conversation--retire-transport
                 view state generation)
                (chirp-dm-conversation--settle-error
                 view state generation phase text))
              :owner view))
            (_ (error "Unknown XChat conversation request phase: %S" phase))))
    (cond
     ((and (not callback-ran-p)
           request
           (chirp-dm-conversation--owner-current-p view state generation))
      (puthash chirp-dm-conversation--request-key request
               (appkit-view-request-table view)))
     ((and (not callback-ran-p)
           (null request)
           (chirp-dm-conversation--owner-current-p view state generation))
      (chirp-dm-conversation--settle-error
       view state generation phase
       "XChat conversation request did not start")))
    request))

;;;; Sending

(defun chirp-dm-conversation--send-owner-current-p (view owner)
  "Return non-nil when OWNER is VIEW's current Appkit compose operation."
  (and (appkit-view-live-p view)
       (with-current-buffer (appkit-view-buffer view)
         (and (bound-and-true-p appkit-compose-session-mode)
              (appkit-compose-operation-current-p owner)))))

(defun chirp-dm-conversation--settle-send-error (view state owner message)
  "Settle Appkit send OWNER in VIEW and STATE with error MESSAGE."
  (when (chirp-dm-conversation--send-owner-current-p view owner)
    (appkit-with-live-view view
      (appkit-compose-operation-finish owner)
      (setq buffer-read-only nil)
      (setf (plist-get state :send-error) message))
    (appkit-request-sync view :part 'frame :position t)
    (message "%s" (replace-regexp-in-string "[\r\n]+" "  " message))))

(defun chirp-dm-conversation--settle-send-success (view state owner text)
  "Settle acknowledged Appkit send OWNER for TEXT in VIEW and STATE."
  (when (chirp-dm-conversation--send-owner-current-p view owner)
    (appkit-with-live-view view
      (appkit-compose-operation-finish owner)
      (setq buffer-read-only nil)
      (setf (plist-get state :send-error) nil)
      (appkit-chatbuf-input-history-push text)
      (appkit-chatbuf-input-set-text ""))
    (appkit-request-sync view :part 'frame :position t)
    (chirp-dm-conversation--request view 'refresh)
    (message "Direct message sent")))

;;;; Commands

(defun chirp-dm-conversation--reject-compose-object (_value _text)
  "Reject one structured compose object unsupported by XChat plain text."
  '(reject . xchat-plain-text-only))

(defun chirp-dm-submit ()
  "Encrypt and send the current plain-text XChat composer input once."
  (interactive)
  (if-let* ((view (chirp-dm-conversation--current-view)))
      (let* ((state (chirp-dm-conversation--state view))
             callback-ran-p request capture owner text)
        (unless (appkit-chatbuf-point-in-input-p)
          (user-error "Point is not in the direct-message composer"))
        (when (appkit-compose-operation-active-p)
          (user-error "A direct message is already being sent"))
        (when (appkit-chatbuf-composer-idle-p)
          (user-error "Direct message is empty"))
        (setq capture
              (condition-case nil
                  (appkit-markup-compose-capture)
                (appkit-markup-object-rejected
                 (user-error
                  "XChat sending currently supports plain text only")))
              text
              (appkit-markup-compose-output-source
               (appkit-markup-compose-output capture 'plain))
              owner
              (appkit-compose-operation-begin
               'dm-send
               :generation
               (appkit-markup-compose-capture-generation capture)
               :label "Sending direct message"))
        (setf (plist-get state :send-error) nil)
        (setq buffer-read-only t)
        (appkit-request-sync view :part 'frame :position t)
        (condition-case err
            (setq request
                  (chirp-backend-dm-send-text
                   (chirp-dm-conversation--id state) text
                   (lambda (_event _envelope)
                     (setq callback-ran-p t)
                     (chirp-dm-conversation--settle-send-success
                      view state owner text))
                   :errback
                   (lambda (message)
                     (setq callback-ran-p t)
                     (chirp-dm-conversation--settle-send-error
                      view state owner message))
                   :owner view))
          ((error quit)
           (chirp-dm-conversation--settle-send-error
            view state owner (error-message-string err))
           (signal (car err) (cdr err))))
        (when (and (not callback-ran-p)
                   (chirp-dm-conversation--send-owner-current-p view owner)
                   (not (buffer-live-p request)))
          (chirp-dm-conversation--settle-send-error
           view state owner "XChat message request did not start"))
        (when (chirp-dm-conversation--send-owner-current-p view owner)
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
  "g" #'chirp-dm-refresh-conversation
  "N" #'chirp-dm-load-older-messages
  "n" #'chirp-dm-next-message
  "p" #'chirp-dm-previous-message
  "q" #'chirp-quit-current-buffer)

(define-minor-mode chirp-dm-conversation--timeline-mode
  "Enable direct-message navigation keys outside the XChat composer."
  :init-value nil
  :lighter nil
  :keymap chirp-dm-conversation--timeline-mode-map)

(defvar-keymap chirp-dm-conversation--mode-map
  :doc "Keymap for `chirp-dm-conversation--mode'."
  "RET" #'chirp-dm-return-dwim
  "C-c C-c" #'chirp-dm-submit
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
       "q" #'chirp-quit-current-buffer
       "i" #'appkit-evil-chatbuf-enter-input
       "g j" #'chirp-dm-next-message
       "g k" #'chirp-dm-previous-message
       "g n" #'chirp-dm-load-older-messages))
    (appkit-evil-normalize-buffers '(chirp-dm-conversation--mode))))

(with-eval-after-load 'evil
  (chirp-dm-conversation--setup-evil))

(define-derived-mode chirp-dm-conversation--mode appkit-chatbuf-mode "Chirp-DM"
  "Major mode for one XChat conversation with a plain-text composer."
  (setq-local line-spacing 0)
  (appkit-compose-setup
   :snapshot-function #'appkit-chatbuf-input-string
   :source-bounds-function #'appkit-chatbuf-input-region-bounds)
  (appkit-markup-compose-setup
   :codecs '(plain)
   :object-classifier #'chirp-dm-conversation--reject-compose-object)
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
