;;; chirp-xchat-native.el --- Optional native XChat bridge -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Lazy discovery and Appkit-session ownership for Chirp's optional XChat
;; cryptography module.  Requiring Chirp does not load the dynamic module.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'appkit-core)
(require 'chirp-core)

(declare-function chirp-xchat-native-version
                  "chirp-xchat-native-module" ())
(declare-function chirp-xchat-native-session-create
                  "chirp-xchat-native-module" ())
(declare-function chirp-xchat-native-session-live-p
                  "chirp-xchat-native-module" (session))
(declare-function chirp-xchat-native-session-unlocked-p
                  "chirp-xchat-native-module" (session))
(declare-function chirp-xchat-native-decrypt
                  "chirp-xchat-native-module" (session input-json))
(declare-function chirp-xchat-native-decrypt-media
                  "chirp-xchat-native-module"
                  (session conversation-id key-version encrypted-base64))
(declare-function chirp-xchat-native-encrypt-text
                  "chirp-xchat-native-module" (session input-json))
(declare-function chirp-xchat-native-encrypt-reply
                  "chirp-xchat-native-module" (session input-json))
(declare-function chirp-xchat-native-encrypt-reaction
                  "chirp-xchat-native-module" (session input-json))
(declare-function chirp-xchat-native-prepare-media
                  "chirp-xchat-native-module" (session input-json))
(declare-function chirp-xchat-native-release-media
                  "chirp-xchat-native-module" (session stage-id))
(declare-function chirp-xchat-native-recovery-start
                  "chirp-xchat-native-module"
                  (session epoch pin input-json))
(declare-function chirp-xchat-native-recovery-poll
                  "chirp-xchat-native-module" (session job-id epoch))
(declare-function chirp-xchat-native-recovery-cancel
                  "chirp-xchat-native-module" (session job-id epoch))

;;; Options

(defcustom chirp-xchat-native-module-file nil
  "Absolute file name of Chirp's optional XChat dynamic module.

Chirp never searches for this security-sensitive module.  Build it locally,
set this option explicitly, and then unlock encrypted XChat support on demand."
  :type '(choice (const :tag "Disabled" nil) file)
  :group 'chirp)

;;; Constants

(defconst chirp-xchat-native--expected-version "0.2.5/chat-xdk-0.4.3"
  "Native adapter and official XChat SDK version required by Chirp.")

;;; Variables

(defvar chirp-xchat-native--next-epoch 0
  "Monotonic identity source for native sessions in this Emacs process.")

;;; Loading

(defun chirp-xchat-native--validate-loaded ()
  "Validate the loaded native module and return non-nil."
  (unless (featurep 'chirp-xchat-native-module)
    (error "XChat native module did not provide its feature"))
  (unless (and (fboundp 'chirp-xchat-native-version)
               (equal (chirp-xchat-native-version)
                      chirp-xchat-native--expected-version))
    (error "XChat native module version is incompatible"))
  (dolist (function '(chirp-xchat-native-session-create
                      chirp-xchat-native-session-live-p
                      chirp-xchat-native-session-unlocked-p
                      chirp-xchat-native-decrypt
                      chirp-xchat-native-decrypt-media
                      chirp-xchat-native-encrypt-text
                      chirp-xchat-native-encrypt-reply
                      chirp-xchat-native-encrypt-reaction
                      chirp-xchat-native-prepare-media
                      chirp-xchat-native-release-media
                      chirp-xchat-native-recovery-start
                      chirp-xchat-native-recovery-poll
                      chirp-xchat-native-recovery-cancel))
    (unless (fboundp function)
      (error "XChat native module lacks %s" function)))
  t)

(defun chirp-xchat-native-load ()
  "Load and validate the explicitly configured XChat native module."
  (condition-case err
      (progn
        (unless (featurep 'chirp-xchat-native-module)
          (cond
           ((null chirp-xchat-native-module-file)
            (error "`chirp-xchat-native-module-file' is not configured"))
           ((not (and (stringp chirp-xchat-native-module-file)
                      (file-name-absolute-p chirp-xchat-native-module-file)))
            (error "`chirp-xchat-native-module-file' must be absolute"))
           ((not (file-readable-p chirp-xchat-native-module-file))
            (error "Configured XChat module is not readable: %s"
                   chirp-xchat-native-module-file))
           (t
            (module-load chirp-xchat-native-module-file))))
        (chirp-xchat-native--validate-loaded))
    (error
     (user-error "XChat decryption module is unavailable: %s"
                 (error-message-string err)))))

;;;; Session

(defun chirp-xchat-native--session ()
  "Return the native session owned by Chirp's current Appkit session."
  (chirp-xchat-native-load)
  (let*
      ((app (chirp-app)) (state (appkit-app-model app))
       (session (chirp--session-xchat-native-session state)))
    (if (and session (chirp-xchat-native-session-live-p session))
        session
      (setq session (chirp-xchat-native-session-create))
      (setf (chirp--session-xchat-native-session state) session
            (chirp--session-xchat-native-epoch state)
            (cl-incf chirp-xchat-native--next-epoch))
      session)))

;;; Recovery

(defun chirp-xchat-native-discard-recovery-input (input)
  "Erase realm tokens carried by normalized recovery INPUT."
  (dolist (cell (cdr (assoc-string "tokens" input t)))
    (when (stringp (cdr cell))
      (clear-string (cdr cell)))))

(defun chirp-xchat-native--cancel-resource (recovery)
  "Cancel timers and native work owned by RECOVERY."
  (when-let* ((timer (plist-get recovery :timer)))
    (cancel-timer timer)
    (plist-put recovery :timer nil))
  (let ((session (plist-get recovery :session)))
    (when (and session
               (chirp-xchat-native-session-live-p session))
      (condition-case nil
          (chirp-xchat-native-recovery-cancel
           session
           (plist-get recovery :job-id)
           (plist-get recovery :epoch))
        (chirp-xchat-native-error nil)))))

(defun chirp-xchat-native--settle-recovery (recovery result error-message)
  "Settle RECOVERY with RESULT or ERROR-MESSAGE."
  (let* ((state (plist-get recovery :state))
         (current-p (eq recovery
                        (chirp--session-xchat-recovery state)))
         (handle (plist-get recovery :handle))
         (callback (plist-get recovery :callback))
         (errback (plist-get recovery :errback)))
    (when current-p
      (setf (chirp--session-xchat-recovery state) nil))
    (when (appkit-handle-p handle)
      (appkit-retire-handle handle))
    (plist-put recovery :callback nil)
    (plist-put recovery :errback nil)
    (when current-p
      (if error-message
          (funcall errback error-message)
        (funcall callback result)))))

(defun chirp-xchat-native--poll-recovery (recovery)
  "Poll one app-owned native RECOVERY."
  (plist-put recovery :timer nil)
  (let* ((app (plist-get recovery :app))
         (state (plist-get recovery :state)))
    (when (and (appkit-app-live-p app)
               (eq recovery (chirp--session-xchat-recovery state)))
      (condition-case err
          (let ((result
                 (chirp-xchat-native-recovery-poll
                  (plist-get recovery :session)
                  (plist-get recovery :job-id)
                  (plist-get recovery :epoch))))
            (if (eq (plist-get result :status) 'pending)
                (plist-put
                 recovery :timer
                 (run-at-time 0.05 nil
                              #'chirp-xchat-native--poll-recovery recovery))
              (chirp-xchat-native--settle-recovery recovery result nil)))
        (chirp-xchat-native-error
         (chirp-xchat-native--settle-recovery
          recovery nil (error-message-string err)))))))

;;; Native JSON Decoding

(defconst chirp-xchat-native--max-message-bytes 16384
  "Maximum UTF-8 bytes accepted for one native plaintext field.")

(defconst chirp-xchat-native--max-attachments 100
  "Maximum verified attachments accepted for one native message.")

(defconst chirp-xchat-native--max-media-bytes (* 50 1024 1024)
  "Maximum plaintext bytes accepted for one XChat attachment.")

(defconst chirp-xchat-native--max-media-ciphertext-bytes
  (+ chirp-xchat-native--max-media-bytes
     (* 17 (/ chirp-xchat-native--max-media-bytes 1024))
     24)
  "Maximum ciphertext bytes for one bounded XChat attachment.")

(defun chirp-xchat-native--optional-string (object key limit label)
  "Decode optional string KEY from OBJECT up to LIMIT bytes for LABEL."
  (let ((value (alist-get key object)))
    (cond
     ((null value) nil)
     ((and (stringp value) (<= (string-bytes value) limit)) value)
     (t (error "XChat native module returned invalid %s" label)))))

(defun chirp-xchat-native--required-string (object key limit label)
  "Decode required string KEY from OBJECT up to LIMIT bytes for LABEL."
  (or (chirp-xchat-native--optional-string object key limit label)
      (error "XChat native module omitted %s" label)))

(defun chirp-xchat-native--optional-integer (object key limit label)
  "Decode optional nonnegative integer KEY from OBJECT up to LIMIT for LABEL."
  (let ((value (alist-get key object)))
    (cond
     ((null value) nil)
     ((and (integerp value) (<= 0 value limit)) value)
     (t (error "XChat native module returned invalid %s" label)))))

(defun chirp-xchat-native--decode-attachment (raw)
  "Decode one verified native attachment RAW into a domain plist."
  (unless (listp raw)
    (error "XChat native module returned an invalid attachment"))
  (let* ((kind-name
          (chirp-xchat-native--required-string
           raw 'kind 32 "attachment kind"))
         (kind
          (and (member kind-name
                       '("image" "gif" "video" "audio" "file" "svg"
                         "media" "url" "post" "unified-card" "money"))
               (intern kind-name))))
    (unless kind
      (error "XChat native module returned an unknown attachment kind"))
    (list :kind kind
          :media-hash
          (chirp-xchat-native--optional-string
           raw 'media_hash_key 2048 "attachment media hash")
          :url
          (chirp-xchat-native--optional-string
           raw 'url 8192 "attachment URL")
          :preview-url
          (chirp-xchat-native--optional-string
           raw 'preview_url 8192 "attachment preview URL")
          :name
          (chirp-xchat-native--optional-string
           raw 'name 1024 "attachment name")
          :attachment-id
          (chirp-xchat-native--optional-string
           raw 'attachment_id 1024 "attachment ID")
          :filesize-bytes
          (chirp-xchat-native--optional-integer
           raw 'filesize_bytes chirp-xchat-native--max-media-bytes
           "attachment size")
          :width
          (chirp-xchat-native--optional-integer
           raw 'width 100000 "attachment width")
          :height
          (chirp-xchat-native--optional-integer
           raw 'height 100000 "attachment height"))))

(defun chirp-xchat-native--decode-message (raw)
  "Decode one verified native message RAW into a bounded domain plist."
  (unless (and (listp raw) (eq (alist-get 'verified raw) t))
    (error "XChat native module returned an unverified message"))
  (let* ((sequence-id
          (chirp-xchat-native--optional-string
           raw 'sequence_id 1024 "message sequence ID"))
         (message-id
          (chirp-xchat-native--optional-string
           raw 'id 1024 "message ID"))
         (conversation-id
          (chirp-xchat-native--required-string
           raw 'conversation_id 1024 "conversation ID"))
         (content-name
          (chirp-xchat-native--required-string
           raw 'content_kind 32 "message content kind"))
         (content-kind
          (and (member content-name
                       '("text" "reaction" "reaction-removed" "edit"
                         "mark-read" "mark-unread" "unknown"))
               (intern content-name)))
         (target-message-id
          (chirp-xchat-native--optional-string
           raw 'target_message_id 1024 "target message ID"))
         (attachments (alist-get 'attachments raw))
         (reply (alist-get 'reply raw))
         (reply-count (alist-get 'reply_attachment_count raw)))
    (unless (or sequence-id message-id)
      (error "XChat native module omitted message identity"))
    (unless content-kind
      (error "XChat native module returned an unknown content kind"))
    (when (and (memq content-kind '(reaction reaction-removed edit))
               (not (and (stringp target-message-id)
                         (not (string-empty-p target-message-id)))))
      (error "XChat native module omitted target message identity"))
    (unless (and (listp attachments)
                 (<= (length attachments)
                     chirp-xchat-native--max-attachments))
      (error "XChat native module returned invalid attachments"))
    (unless (memq reply '(t :json-false))
      (error "XChat native module returned an invalid reply flag"))
    (unless (and (integerp reply-count)
                 (<= 0 reply-count chirp-xchat-native--max-attachments))
      (error "XChat native module returned an invalid reply attachment count"))
    (list :sequence-id sequence-id
          :message-id message-id
          :sender-id
          (chirp-xchat-native--optional-string
           raw 'sender_id 1024 "sender ID")
          :conversation-id conversation-id
          :created-at-msec
          (let ((value (alist-get 'created_at_msec raw)))
            (when (and value (not (integerp value)))
              (error "XChat native module returned an invalid timestamp"))
            value)
          :content-kind content-kind
          :text
          (chirp-xchat-native--optional-string
           raw 'text chirp-xchat-native--max-message-bytes "message text")
          :target-message-id target-message-id
          :attachments
          (mapcar #'chirp-xchat-native--decode-attachment attachments)
          :reply-p (eq reply t)
          :reply-text
          (chirp-xchat-native--optional-string
           raw 'reply_text chirp-xchat-native--max-message-bytes
           "reply text")
          :reply-attachment-count reply-count
          :key-version
          (chirp-xchat-native--optional-string
           raw 'key_version 1024 "conversation key version"))))

(defun chirp-xchat-native--decode-decrypt-output (raw)
  "Decode bounded native decryption output RAW into verified messages."
  (let ((messages (alist-get 'messages raw))
        (errors (alist-get 'errors raw)))
    (unless (and (listp messages) (<= (length messages) 200))
      (error "XChat native module returned invalid decrypted messages"))
    (unless (and (listp errors) (<= (length errors) 200)
                 (cl-every (lambda (entry) (stringp (cdr entry))) errors))
      (error "XChat native module returned invalid decryption errors"))
    (mapcar #'chirp-xchat-native--decode-message messages)))

;;; Cryptographic Operations

(defun chirp-xchat-native-unlocked-p ()
  "Return non-nil when the current Chirp session has recovered XChat keys."
  (and (appkit-app-live-p chirp--app)
       (let*
           ((state (appkit-app-model chirp--app))
            (session (chirp--session-xchat-native-session state)))
         (and session (chirp-xchat-native-session-live-p session)
              (chirp-xchat-native-session-unlocked-p session)))))

(defun chirp-xchat-native-decrypt-events
    (conversation-id events signing-keys)
  "Decode CONVERSATION-ID's verified messages from EVENTS using SIGNING-KEYS."
  (let (input-json output-json)
    (unwind-protect
        (progn
          (setq input-json
                (json-encode
                 `(("conversation_id" . ,conversation-id)
                   ("events" . ,(vconcat events))
                   ("signing_keys" . ,signing-keys)))
                output-json
                (chirp-xchat-native-decrypt
                 (chirp-xchat-native--session) input-json))
          (chirp-xchat-native--decode-decrypt-output
           (json-parse-string
            output-json :object-type 'alist :array-type 'list
            :null-object nil :false-object :json-false)))
      (when (stringp input-json)
        (clear-string input-json))
      (when (stringp output-json)
        (clear-string output-json)))))

(defun chirp-xchat-native-decrypt-media-bytes
    (conversation-id key-version encrypted)
  "Decrypt ENCRYPTED XChat media for CONVERSATION-ID and KEY-VERSION."
  (unless (and (stringp encrypted)
               (not (multibyte-string-p encrypted))
               (<= (string-bytes encrypted)
                   chirp-xchat-native--max-media-ciphertext-bytes))
    (error "XChat media ciphertext is invalid"))
  (let (input-base64 output-base64 plaintext)
    (unwind-protect
        (progn
          (setq input-base64 (base64-encode-string encrypted t)
                output-base64
                (chirp-xchat-native-decrypt-media
                 (chirp-xchat-native--session)
                 conversation-id key-version input-base64)
                plaintext (base64-decode-string output-base64))
          (unless (and (not (multibyte-string-p plaintext))
                       (<= (string-bytes plaintext)
                           chirp-xchat-native--max-media-bytes))
            (clear-string plaintext)
            (error "XChat native module returned invalid media plaintext"))
          plaintext)
      (when (stringp input-base64)
        (clear-string input-base64))
      (when (stringp output-base64)
        (clear-string output-base64)))))

(defun chirp-xchat-native-prepare-media-file (conversation-id file)
  "Encrypt FILE for CONVERSATION-ID and return native staging metadata."
  (let (input-json output-json)
    (unwind-protect
        (let* ((parsed
                (progn
                  (setq input-json
                        (json-encode
                         `(("conversation_id" . ,conversation-id)
                           ("file_path" . ,(expand-file-name file))))
                        output-json
                        (chirp-xchat-native-prepare-media
                         (chirp-xchat-native--session) input-json))
                  (json-parse-string
                   output-json :object-type 'alist :array-type 'list
                   :null-object nil :false-object :json-false)))
               (stage-id (alist-get 'stage_id parsed))
               (encrypted-file (alist-get 'encrypted_file parsed))
               (encrypted-bytes (alist-get 'encrypted_bytes parsed))
               (plaintext-bytes (alist-get 'plaintext_bytes parsed))
               (key-version (alist-get 'key_version parsed))
               (filename (alist-get 'filename parsed))
               (mime-type (alist-get 'mime_type parsed))
               (media-type (alist-get 'media_type parsed))
               (width (alist-get 'width parsed))
               (height (alist-get 'height parsed)))
          (unless
              (and (integerp stage-id) (> stage-id 0)
                   (stringp encrypted-file)
                   (file-regular-p encrypted-file)
                   (file-readable-p encrypted-file)
                   (integerp encrypted-bytes) (> encrypted-bytes 0)
                   (= encrypted-bytes
                      (file-attribute-size (file-attributes encrypted-file)))
                   (integerp plaintext-bytes)
                   (<= 1 plaintext-bytes chirp-xchat-native--max-media-bytes)
                   (stringp key-version)
                   (not (string-empty-p key-version))
                   (stringp filename) (not (string-empty-p filename))
                   (stringp mime-type) (not (string-empty-p mime-type))
                   (integerp media-type) (<= 1 media-type 6)
                   (integerp width) (>= width 0)
                   (integerp height) (>= height 0))
            (when (and (integerp stage-id) (> stage-id 0))
              (ignore-errors
                (chirp-xchat-native-release-media
                 (chirp-xchat-native--session) stage-id)))
            (error "XChat native module returned invalid media staging metadata"))
          (list :stage-id stage-id
                :encrypted-file encrypted-file
                :encrypted-bytes encrypted-bytes
                :plaintext-bytes plaintext-bytes
                :key-version key-version
                :filename filename
                :mime-type mime-type
                :media-type media-type
                :width width
                :height height))
      (when (stringp input-json)
        (clear-string input-json))
      (when (stringp output-json)
        (clear-string output-json)))))

(defun chirp-xchat-native-release-media-stage (stage-id)
  "Release native encrypted-media STAGE-ID."
  (chirp-xchat-native-release-media
   (chirp-xchat-native--session) stage-id))

(defun chirp-xchat-native--encode-media-attachments (attachments)
  "Return ATTACHMENTS as bounded native JSON objects."
  (vconcat
   (mapcar
    (lambda (attachment)
      `(("media_hash_key" . ,(plist-get attachment :media-hash-key))
        ("width" . ,(or (plist-get attachment :width) 0))
        ("height" . ,(or (plist-get attachment :height) 0))
        ("filesize_bytes" . ,(plist-get attachment :plaintext-bytes))
        ("filename" . ,(plist-get attachment :filename))
        ("media_type" . ,(plist-get attachment :media-type))
        ,@(when-let* ((duration (plist-get attachment :duration-millis)))
            `(("duration_millis" . ,duration)))))
    attachments)))

(defun chirp-xchat-native--attachment-key-version (attachments)
  "Return the one verified key version shared by ATTACHMENTS."
  (let ((versions
         (delete-dups
          (mapcar (lambda (attachment)
                    (plist-get attachment :key-version))
                  attachments))))
    (unless (and (= (length versions) 1)
                 (stringp (car versions))
                 (not (string-empty-p (car versions))))
      (error "XChat uploaded attachments do not share one key version"))
    (car versions)))

(defun chirp-xchat-native--prepare-send (input native-function)
  "Prepare public XChat send fields from INPUT using NATIVE-FUNCTION."
  (let (input-json output-json)
    (unwind-protect
        (let* ((parsed
                (progn
                  (setq input-json (json-encode input)
                        output-json
                        (funcall native-function
                                 (chirp-xchat-native--session) input-json))
                  (json-parse-string
                   output-json :object-type 'alist :array-type 'list
                   :null-object nil :false-object :json-false)))
               (message-id (alist-get 'message_id parsed))
               (event (alist-get 'encoded_message_create_event parsed))
               (signature
                (alist-get 'encoded_message_event_signature parsed)))
          (unless (cl-every (lambda (value)
                              (and (stringp value)
                                   (not (string-empty-p value))))
                            (list message-id event signature))
            (error "XChat native module returned an invalid send payload"))
          (list :message-id message-id
                :encoded-message-create-event event
                :encoded-message-event-signature signature))
      (when (stringp input-json)
        (clear-string input-json))
      (when (stringp output-json)
        (clear-string output-json)))))

(defun chirp-xchat-native-prepare-text
    (conversation-id text &optional attachments)
  "Prepare encrypted XChat TEXT and optional ATTACHMENTS for CONVERSATION-ID.

The native session supplies the sender identity bound during key recovery."
  (chirp-xchat-native--prepare-send
   `(("conversation_id" . ,conversation-id)
     ("text" . ,text)
     ,@(when attachments
         `(("conversation_key_version" .
            ,(chirp-xchat-native--attachment-key-version attachments))))
     ("attachments" .
      ,(chirp-xchat-native--encode-media-attachments attachments)))
   #'chirp-xchat-native-encrypt-text))

(defun chirp-xchat-native-prepare-reply
    (conversation-id text target-event key-events &optional attachments)
  "Prepare encrypted XChat TEXT replying to TARGET-EVENT in CONVERSATION-ID.

KEY-EVENTS supplies bounded raw conversation-key events needed to validate an
older reply target.  ATTACHMENTS are optional uploaded-media descriptors."
  (chirp-xchat-native--prepare-send
   `(("conversation_id" . ,conversation-id)
     ("text" . ,text)
     ,@(when attachments
         `(("conversation_key_version" .
            ,(chirp-xchat-native--attachment-key-version attachments))))
     ("target_event" . ,target-event)
     ("key_events" . ,(vconcat key-events))
     ("attachments" .
      ,(chirp-xchat-native--encode-media-attachments attachments)))
   #'chirp-xchat-native-encrypt-reply))

(defun chirp-xchat-native-prepare-reaction
    (conversation-id target-event emoji remove-p)
  "Prepare an encrypted reaction operation for CONVERSATION-ID.

TARGET-EVENT is the exact raw message event.  EMOJI identifies the reaction;
when REMOVE-P is non-nil, prepare a removal instead of an addition."
  (chirp-xchat-native--prepare-send
   `(("conversation_id" . ,conversation-id)
     ("target_event" . ,target-event)
     ("emoji" . ,emoji)
     ("remove" . ,(and remove-p t)))
   #'chirp-xchat-native-encrypt-reaction))

;;; Recovery Commands

(defun chirp-xchat-native-recovery-active-p ()
  "Return non-nil when the current Chirp session is recovering XChat keys."
  (and (appkit-app-live-p chirp--app)
       (chirp--session-xchat-recovery (appkit-app-model chirp--app))))

(cl-defun chirp-xchat-native-recover (pin input callback &key errback)
  "Consume PIN and normalized INPUT to start one XChat key recovery.\n\nCALLBACK receives one non-sensitive terminal status plist.  ERRBACK receives\nsetup or worker failures.  PIN and INPUT's Juicebox realm tokens are erased\nbefore this function returns."
  (unless (functionp callback)
    (error "XChat recovery callback is not callable"))
  (let
      ((error-fn
        (or errback (lambda (message) (message "%s" message))))
       input-json job-id)
    (unless (functionp error-fn)
      (error "XChat recovery error callback is not callable"))
    (condition-case err
        (let*
            ((app (chirp-app)) (state (appkit-app-model app))
             (session (chirp-xchat-native--session))
             (epoch (chirp--session-xchat-native-epoch state)))
          (when (chirp--session-xchat-recovery state)
            (error "XChat key recovery is already active"))
          (unwind-protect
              (progn
                (setq input-json (json-encode input) job-id
                      (chirp-xchat-native-recovery-start session epoch
                                                         pin
                                                         input-json)))
            (when (stringp pin) (clear-string pin))
            (when (stringp input-json) (clear-string input-json))
            (chirp-xchat-native-discard-recovery-input input))
          (let
              ((recovery
                (list :app app :state state :session session :epoch
                      epoch :job-id job-id :timer nil :handle nil
                      :callback callback :errback error-fn)))
            (plist-put recovery :handle
                       (appkit-register-handle app 'function recovery
                                               #'chirp-xchat-native--cancel-resource))
            (setf (chirp--session-xchat-recovery state) recovery)
            (plist-put recovery :timer
                       (run-at-time 0.05 nil
                                    #'chirp-xchat-native--poll-recovery
                                    recovery))
            job-id))
      (error (when (stringp pin) (clear-string pin))
             (when (stringp input-json) (clear-string input-json))
             (chirp-xchat-native-discard-recovery-input input)
             (funcall error-fn (error-message-string err)) nil))))

(defun chirp-xchat-native-cancel-recovery ()
  "Request cancellation of the current Chirp session's XChat recovery."
  (when-let*
      (((appkit-app-live-p chirp--app))
       (state (appkit-app-model chirp--app))
       (recovery (chirp--session-xchat-recovery state)))
    (chirp-xchat-native-recovery-cancel (plist-get recovery :session)
                                        (plist-get recovery :job-id)
                                        (plist-get recovery :epoch))))

(provide 'chirp-xchat-native)

;;; chirp-xchat-native.el ends here
