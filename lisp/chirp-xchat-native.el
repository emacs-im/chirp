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
(declare-function chirp-xchat-native-encrypt-text
                  "chirp-xchat-native-module" (session input-json))
(declare-function chirp-xchat-native-recovery-start
                  "chirp-xchat-native-module"
                  (session epoch pin input-json))
(declare-function chirp-xchat-native-recovery-poll
                  "chirp-xchat-native-module" (session job-id epoch))
(declare-function chirp-xchat-native-recovery-cancel
                  "chirp-xchat-native-module" (session job-id epoch))

(defcustom chirp-xchat-native-module-file nil
  "Absolute file name of Chirp's optional XChat dynamic module.

Chirp never searches for this security-sensitive module.  Build it locally,
set this option explicitly, and then unlock encrypted XChat support on demand."
  :type '(choice (const :tag "Disabled" nil) file)
  :group 'chirp)

(defconst chirp-xchat-native--expected-version "0.2.2/chat-xdk-0.4.3"
  "Native adapter and official XChat SDK version required by Chirp.")

(defvar chirp-xchat-native--next-epoch 0
  "Monotonic identity source for native sessions in this Emacs process.")

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
                      chirp-xchat-native-encrypt-text
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

(defun chirp-xchat-native--session ()
  "Return the native session owned by Chirp's current Appkit session."
  (chirp-xchat-native-load)
  (let* ((app (chirp-app))
         (state (appkit-app-state app))
         (session (chirp--session-xchat-native-session state)))
    (if (and session (chirp-xchat-native-session-live-p session))
        session
      (setq session (chirp-xchat-native-session-create))
      (setf (chirp--session-xchat-native-session state) session
            (chirp--session-xchat-native-epoch state)
            (cl-incf chirp-xchat-native--next-epoch))
      session)))

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

(defun chirp-xchat-native-unlocked-p ()
  "Return non-nil when the current Chirp session has recovered XChat keys."
  (and (appkit-app-live-p chirp--app)
       (let* ((state (appkit-app-state chirp--app))
              (session (chirp--session-xchat-native-session state)))
         (and session
              (chirp-xchat-native-session-live-p session)
              (chirp-xchat-native-session-unlocked-p session)))))

(defun chirp-xchat-native-decrypt-events (events signing-keys)
  "Return verified plaintext for encoded XChat EVENTS using SIGNING-KEYS."
  (let (input-json output-json)
    (unwind-protect
        (progn
          (setq input-json
                (json-encode
                 `(("events" . ,(vconcat events))
                   ("signing_keys" . ,signing-keys)))
                output-json
                (chirp-xchat-native-decrypt
                 (chirp-xchat-native--session) input-json))
          (json-parse-string
           output-json :object-type 'alist :array-type 'list
           :null-object nil :false-object :json-false))
      (when (stringp input-json)
        (clear-string input-json))
      (when (stringp output-json)
        (clear-string output-json)))))

(defun chirp-xchat-native-prepare-text (conversation-id text)
  "Prepare encrypted XChat TEXT for CONVERSATION-ID.

The native session supplies the sender identity bound during key recovery."
  (let (input-json output-json)
    (unwind-protect
        (let* ((input
                `(("conversation_id" . ,conversation-id)
                  ("text" . ,text)))
               (parsed
                (progn
                  (setq input-json (json-encode input)
                        output-json
                        (chirp-xchat-native-encrypt-text
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

(defun chirp-xchat-native-recovery-active-p ()
  "Return non-nil when the current Chirp session is recovering XChat keys."
  (and (appkit-app-live-p chirp--app)
       (chirp--session-xchat-recovery (appkit-app-state chirp--app))))

(cl-defun chirp-xchat-native-recover (pin input callback &key errback)
  "Consume PIN and normalized INPUT to start one XChat key recovery.

CALLBACK receives one non-sensitive terminal status plist.  ERRBACK receives
setup or worker failures.  PIN and INPUT's Juicebox realm tokens are erased
before this function returns."
  (unless (functionp callback)
    (error "XChat recovery callback is not callable"))
  (let ((error-fn (or errback (lambda (message) (message "%s" message))))
        input-json job-id)
    (unless (functionp error-fn)
      (error "XChat recovery error callback is not callable"))
    (condition-case err
        (let* ((app (chirp-app))
               (state (appkit-app-state app))
               (session (chirp-xchat-native--session))
               (epoch (chirp--session-xchat-native-epoch state)))
          (when (chirp--session-xchat-recovery state)
            (error "XChat key recovery is already active"))
          (unwind-protect
              (progn
                (setq input-json (json-encode input)
                      job-id
                      (chirp-xchat-native-recovery-start
                       session epoch pin input-json)))
            (when (stringp pin)
              (clear-string pin))
            (when (stringp input-json)
              (clear-string input-json))
            (chirp-xchat-native-discard-recovery-input input))
          (let ((recovery
                 (list :app app :state state :session session
                       :epoch epoch :job-id job-id :timer nil
                       :handle nil :callback callback :errback error-fn)))
            (plist-put
             recovery :handle
             (appkit-register-handle
              app 'function recovery #'chirp-xchat-native--cancel-resource))
            (setf (chirp--session-xchat-recovery state) recovery)
            (plist-put
             recovery :timer
             (run-at-time 0.05 nil
                          #'chirp-xchat-native--poll-recovery recovery))
            job-id))
      (error
       (when (stringp pin)
         (clear-string pin))
       (when (stringp input-json)
         (clear-string input-json))
       (chirp-xchat-native-discard-recovery-input input)
       (funcall error-fn (error-message-string err))
       nil))))

(defun chirp-xchat-native-cancel-recovery ()
  "Request cancellation of the current Chirp session's XChat recovery."
  (when-let* (((appkit-app-live-p chirp--app))
              (state (appkit-app-state chirp--app))
              (recovery (chirp--session-xchat-recovery state)))
    (chirp-xchat-native-recovery-cancel
     (plist-get recovery :session)
     (plist-get recovery :job-id)
     (plist-get recovery :epoch))))

(provide 'chirp-xchat-native)

;;; chirp-xchat-native.el ends here
