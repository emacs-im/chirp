;;; chirp-xchat-native-test.el --- Native XChat module tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Chirp contributors
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT smoke tests for the optional native module.  Set
;; CHIRP_XCHAT_MODULE_FILE to a test-vector build of the module to run them.

;;; Code:

(require 'ert)
(require 'json)
(require 'chirp-xchat-native)

(declare-function chirp-xchat-native-version
                  "chirp-xchat-native-module" ())
(declare-function chirp-xchat-native-session-create
                  "chirp-xchat-native-module" ())
(declare-function chirp-xchat-native-session-live-p
                  "chirp-xchat-native-module" (session))
(declare-function chirp-xchat-native-session-unlocked-p
                  "chirp-xchat-native-module" (session))
(declare-function chirp-xchat-native-session-destroy
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
(declare-function chirp-xchat-native-test-decrypt-official-vector
                  "chirp-xchat-native-module" (session))
(declare-function chirp-xchat-native-test-recovery-start
                  "chirp-xchat-native-module"
                  (session epoch pin delay-msec))
(declare-function chirp-xchat-native-test-recovery-poll
                  "chirp-xchat-native-module" (session job-id epoch))
(declare-function chirp-xchat-native-test-recovery-cancel
                  "chirp-xchat-native-module" (session job-id epoch))

(defconst chirp-xchat-native-test--fixture-file
  (expand-file-name
   "../native/chirp-xchat-module/tests/fixtures/sdk_vectors.json"
   (file-name-directory (or load-file-name buffer-file-name)))
  "Official synthetic XChat vector used by native tests.")

(defun chirp-xchat-native-test--load ()
  "Load the test module named by `CHIRP_XCHAT_MODULE_FILE'."
  (let ((file (getenv "CHIRP_XCHAT_MODULE_FILE")))
    (cond
     ((null file) nil)
     ((not (file-readable-p file))
      (error "CHIRP_XCHAT_MODULE_FILE is not readable: %s" file))
     ((featurep 'chirp-xchat-native-module) t)
     (t
      (let ((chirp-xchat-native-module-file file))
        (chirp-xchat-native-load))))))

(defun chirp-xchat-native-test--abandon-session (table &optional busy)
  "Put a new native session in weak TABLE without retaining it.

When BUSY is non-nil, abandon it with one pending synthetic recovery."
  (let ((session (chirp-xchat-native-session-create)))
    (when busy
      (chirp-xchat-native-test-recovery-start session 1 "2580" 1000))
    (puthash session t table))
  nil)

(defun chirp-xchat-native-test--fixture ()
  "Return the parsed official synthetic XChat vector."
  (with-temp-buffer
    (insert-file-contents chirp-xchat-native-test--fixture-file)
    (json-parse-buffer :object-type 'alist :array-type 'list)))

(defun chirp-xchat-native-test--await (session job-id epoch)
  "Poll SESSION until synthetic JOB-ID for EPOCH settles."
  (let ((deadline (+ (float-time) 2.0))
        result)
    (while (and (< (float-time) deadline)
                (eq (plist-get
                     (setq result
                           (chirp-xchat-native-test-recovery-poll
                            session job-id epoch))
                    :status)
                    'pending))
      (sleep-for 0.005))
    (when (eq (plist-get result :status) 'pending)
      (ert-fail "Synthetic native recovery did not settle"))
    result))

(ert-deftest chirp-xchat-native-decodes-verified-message-domain ()
  "Native JSON should decode into bounded message and attachment values."
  (let* ((messages
          (chirp-xchat-native--decode-decrypt-output
           '((messages
              . (((sequence_id . "20")
                  (id . "message-20")
                  (sender_id . "42")
                  (conversation_id . "conversation-1")
                  (created_at_msec . 1700000000000)
                  (content_kind . "text")
                  (text . "hello")
                  (attachments
                   . (((kind . "image")
                       (media_hash_key . "media-hash")
                       (url . "https://pbs.twimg.com/media/example.jpg")
                       (preview_url)
                       (name . "example.jpg")
                       (attachment_id . "attachment-1")
                       (filesize_bytes . 4096)
                       (width . 640)
                       (height . 480))))
                  (reply . t)
                  (reply_text . "earlier")
                  (reply_attachment_count . 0)
                  (key_version . "7")
                  (verified . t))))
             (errors))))
         (message (car messages))
         (attachment (car (plist-get message :attachments))))
    (should (equal (plist-get message :sequence-id) "20"))
    (should (eq (plist-get message :content-kind) 'text))
    (should (plist-get message :reply-p))
    (should (equal (plist-get attachment :kind) 'image))
    (should (equal (plist-get attachment :media-hash) "media-hash"))
    (should (= (plist-get attachment :filesize-bytes) 4096))
    (should (equal (plist-get attachment :name) "example.jpg"))))

(ert-deftest chirp-xchat-native-rejects-invalid-domain-output ()
  "Native JSON decoding should reject unverified or structurally invalid data."
  (should-error
   (chirp-xchat-native--decode-decrypt-output
    '((messages
       . (((sequence_id . "20")
           (conversation_id . "conversation-1")
           (content_kind . "text")
           (attachments)
           (reply . :json-false)
           (reply_attachment_count . 0)
           (verified . :json-false))))
      (errors))))
  (should-error
   (chirp-xchat-native--decode-decrypt-output
    '((messages
       . (((sequence_id . "20")
           (conversation_id . "conversation-1")
           (content_kind . "html")
           (attachments)
           (reply . :json-false)
           (reply_attachment_count . 0)
           (verified . t))))
      (errors)))))

(ert-deftest chirp-xchat-native-decrypt-wrapper-decodes-domain-output ()
  "The public native bridge should not expose raw JSON message objects."
  (cl-letf (((symbol-function 'chirp-xchat-native--session)
             (lambda () :session))
            ((symbol-function 'chirp-xchat-native-decrypt)
             (lambda (session input-json)
               (should (eq session :session))
               (should
                (equal
                 (alist-get
                  'conversation_id
                  (json-parse-string input-json :object-type 'alist))
                 "conversation-1"))
               (json-encode
                '(("messages"
                   . [(("sequence_id" . "20")
                       ("conversation_id" . "conversation-1")
                       ("content_kind" . "text")
                       ("text" . "hello")
                       ("attachments" . [])
                       ("reply" . :json-false)
                       ("reply_attachment_count" . 0)
                       ("verified" . t))])
                  ("errors" . ()))))))
    (let ((messages
           (chirp-xchat-native-decrypt-events
            "conversation-1" '("event") [])))
      (should (= (length messages) 1))
      (should (eq (plist-get (car messages) :content-kind) 'text))
      (should (equal (plist-get (car messages) :text) "hello")))))

(ert-deftest chirp-xchat-native-media-wrapper-preserves-binary-bytes ()
  "The Lisp bridge should Base64-frame arbitrary native media bytes exactly."
  (let ((ciphertext (unibyte-string 0 255 1 2))
        (plaintext (unibyte-string 137 80 78 71 0 255)))
    (cl-letf (((symbol-function 'chirp-xchat-native--session)
               (lambda () :session))
              ((symbol-function 'chirp-xchat-native-decrypt-media)
               (lambda (session conversation-id key-version encoded)
                 (should (eq session :session))
                 (should (equal conversation-id "conversation-1"))
                 (should (equal key-version "7"))
                 (should (equal (base64-decode-string encoded) ciphertext))
                 (base64-encode-string plaintext t))))
      (should
       (equal
        (chirp-xchat-native-decrypt-media-bytes
         "conversation-1" "7" ciphertext)
        plaintext)))))

(ert-deftest chirp-xchat-native-loader-requires-explicit-readable-file ()
  (skip-when (featurep 'chirp-xchat-native-module))
  (dolist (file
           (list nil "relative-module.so"
                 (expand-file-name
                  "missing-chirp-xchat-module" temporary-file-directory)))
    (let ((chirp-xchat-native-module-file file))
      (should-error (chirp-xchat-native-load) :type 'user-error))))

(ert-deftest chirp-xchat-native-loads-and-reports-version ()
  (skip-unless (chirp-xchat-native-test--load))
  (should (module-function-p (symbol-function 'chirp-xchat-native-version)))
  (should (equal (chirp-xchat-native-version)
                 "0.2.3/chat-xdk-0.4.3"))
  (dolist (function '(chirp-xchat-native-encrypt-text
                      chirp-xchat-native-decrypt-media
                      chirp-xchat-native-recovery-start
                      chirp-xchat-native-recovery-poll
                      chirp-xchat-native-recovery-cancel))
    (should (module-function-p (symbol-function function)))))

(ert-deftest chirp-xchat-native-session-lifecycle-is-explicit ()
  (skip-unless (chirp-xchat-native-test--load))
  (let ((session (chirp-xchat-native-session-create)))
    (should (eq (type-of session) 'user-ptr))
    (should (chirp-xchat-native-session-live-p session))
    (should-not (chirp-xchat-native-session-unlocked-p session))
    (should (chirp-xchat-native-session-destroy session))
    (should-not (chirp-xchat-native-session-destroy session))
    (should-not (chirp-xchat-native-session-live-p session))
    (should-error (chirp-xchat-native-session-unlocked-p session)
                  :type 'chirp-xchat-native-error)
    (should-error (chirp-xchat-native-test-decrypt-official-vector session)
                  :type 'chirp-xchat-native-error)))

(ert-deftest chirp-xchat-native-real-recovery-rejects-input-before-network ()
  "Malformed production input should not claim a recovery job."
  (skip-unless (chirp-xchat-native-test--load))
  (let ((session (chirp-xchat-native-session-create)))
    (unwind-protect
        (progn
          (should-error
           (chirp-xchat-native-recovery-start session 1 "2580" "{}")
           :type 'chirp-xchat-native-error)
          (let ((job-id
                 (chirp-xchat-native-test-recovery-start
                  session 1 "1111" 0)))
            (should (eq (plist-get
                         (chirp-xchat-native-test--await session job-id 1)
                         :status)
                        'incorrect-pin))))
      (chirp-xchat-native-session-destroy session))))

(ert-deftest chirp-xchat-native-finalizer-is-a-gc-safe-fallback ()
  "GC should safely finalize abandoned idle and busy native sessions."
  (skip-unless (chirp-xchat-native-test--load))
  (let ((weak-sessions (make-hash-table :test #'eq :weakness 'key)))
    (chirp-xchat-native-test--abandon-session weak-sessions)
    (chirp-xchat-native-test--abandon-session weak-sessions t)
    (garbage-collect)
    (garbage-collect)
    (should (zerop (hash-table-count weak-sessions)))))

(ert-deftest chirp-xchat-native-rejects-foreign-session-values ()
  (skip-unless (chirp-xchat-native-test--load))
  (should-error (chirp-xchat-native-session-live-p 42)
                :type 'wrong-type-argument))

(ert-deftest chirp-xchat-native-decrypts-and-prepares-official-vector ()
  (skip-unless (chirp-xchat-native-test--load))
  (let ((session (chirp-xchat-native-session-create))
        input-json output-json)
    (unwind-protect
        (let* ((json (chirp-xchat-native-test-decrypt-official-vector session))
               (result (json-parse-string json :object-type 'alist
                                          :array-type 'list))
               (messages (alist-get 'messages result))
               (message (car messages))
               (errors (alist-get 'errors result))
               (conversation-id (alist-get 'conversation_id message)))
          (should (= (length messages) 1))
          (should (equal (alist-get 'text message) "fixture event message"))
          (should (equal (alist-get 'content_kind message) "text"))
          (should-not (alist-get 'attachments message))
          (should (eq (alist-get 'verified message) t))
          (should (null errors))
          (dolist (secret-field
                   '("private_key" "conversation_key" "original_b64"))
            (should-not (string-match-p secret-field json)))
          (setq input-json
                (json-encode
                 `(("conversation_id" . ,conversation-id)
                   ("text" . "outbound fixture message")))
                output-json
                (chirp-xchat-native-encrypt-text session input-json))
          (let ((prepared
                 (json-parse-string output-json :object-type 'alist)))
            (should (stringp (alist-get 'message_id prepared)))
            (should (stringp
                     (alist-get 'encoded_message_create_event prepared)))
            (should (stringp
                     (alist-get 'encoded_message_event_signature prepared))))
          (should-not (string-match-p "outbound fixture message" output-json))
          (should-not (string-match-p "conversation_key" output-json))
          (let ((spoof-json
                 (json-encode
                  `(("conversation_id" . ,conversation-id)
                    ("sender_id" . "9999")
                    ("text" . "forged sender")))))
            (unwind-protect
                (should-error
                 (chirp-xchat-native-encrypt-text session spoof-json)
                 :type 'chirp-xchat-native-error)
              (clear-string spoof-json))))
      (when (stringp input-json)
        (clear-string input-json))
      (when (stringp output-json)
        (clear-string output-json))
      (chirp-xchat-native-session-destroy session))))

(ert-deftest chirp-xchat-native-async-recovery-imports-without-key-output ()
  (skip-unless (chirp-xchat-native-test--load))
  (let* ((session (chirp-xchat-native-session-create))
         (epoch 17)
         (job-id
          (chirp-xchat-native-test-recovery-start
           session epoch "2580" 50)))
    (unwind-protect
        (progn
          (should-error
           (chirp-xchat-native-test-recovery-start
            session epoch "2580" 0)
           :type 'chirp-xchat-native-error)
          (let* ((result
                  (chirp-xchat-native-test--await session job-id epoch))
                 (printed (prin1-to-string result)))
            (should (eq (plist-get result :status) 'unlocked))
            (should (equal (plist-get result :public-key-version) "1"))
            (should (chirp-xchat-native-session-unlocked-p session))
            (should-not (string-match-p "2580" printed))
            (should-not (string-match-p "private\\|conversation.*key" printed))))
      (chirp-xchat-native-session-destroy session))))

(ert-deftest chirp-xchat-native-production-decrypt-returns-verified-plaintext ()
  "The production decrypt ABI should return only verified message fields."
  (skip-unless (chirp-xchat-native-test--load))
  (let* ((fixture (chirp-xchat-native-test--fixture))
         (session (chirp-xchat-native-session-create))
         (epoch 20)
         input-json output-json)
    (unwind-protect
        (progn
          (let ((job-id
                 (chirp-xchat-native-test-recovery-start
                  session epoch "2580" 0)))
            (should (eq (plist-get
                         (chirp-xchat-native-test--await session job-id epoch)
                         :status)
                        'unlocked)))
          (setq input-json
                (json-encode
                 (list
                  (cons "conversation_id"
                        (alist-get 'event_conversation_id fixture))
                  (cons
                   "events"
                   (vector (alist-get 'event_key_change_b64 fixture)
                           (alist-get 'event_message_b64 fixture)))
                  (cons
                   "signing_keys"
                   (vector
                    (list
                     (cons "user_id" (alist-get 'event_sender_id fixture))
                     (cons "public_key_version"
                           (alist-get 'event_signing_key_version fixture))
                     (cons "public_key"
                           (alist-get 'signing_public_b64 fixture))
                     (cons "identity_public_key"
                           (alist-get 'identity_public_b64 fixture))
                     (cons "identity_public_key_signature"
                           (alist-get
                            'identity_public_key_signature_b64 fixture)))))))
                output-json
                (chirp-xchat-native-decrypt session input-json))
          (let* ((output
                  (json-parse-string
                   output-json :object-type 'alist :array-type 'list))
                 (messages (alist-get 'messages output)))
            (should (= (length messages) 1))
            (should (equal (alist-get 'text (car messages))
                           (alist-get 'event_message_text fixture)))
            (should (eq (alist-get 'verified (car messages)) t))
            (should-not (string-match-p "private_key\\|conversation_key"
                                        output-json))))
      (when (stringp input-json)
        (clear-string input-json))
      (when (stringp output-json)
        (clear-string output-json))
      (chirp-xchat-native-session-destroy session))))

(ert-deftest chirp-xchat-native-recovery-preserves-guesses-and-stale-barriers ()
  (skip-unless (chirp-xchat-native-test--load))
  (let* ((session (chirp-xchat-native-session-create))
         (epoch 18)
         (job-id
          (chirp-xchat-native-test-recovery-start
           session epoch "1111" 50)))
    (unwind-protect
        (progn
          (should-error
           (chirp-xchat-native-test-recovery-poll
            session (1+ job-id) epoch)
           :type 'chirp-xchat-native-error)
          (should-error
           (chirp-xchat-native-test-recovery-cancel
            session job-id (1+ epoch))
           :type 'chirp-xchat-native-error)
          (let ((result
                 (chirp-xchat-native-test--await session job-id epoch)))
            (should (eq (plist-get result :status) 'incorrect-pin))
            (should (= (plist-get result :guesses-remaining) 19))
            (should-not (chirp-xchat-native-session-unlocked-p session)))
          (let ((next-job-id
                 (chirp-xchat-native-test-recovery-start
                  session epoch "2580" 0)))
            (should (> next-job-id job-id))
            (should (eq (plist-get
                         (chirp-xchat-native-test--await
                          session next-job-id epoch)
                         :status)
                        'unlocked))))
      (chirp-xchat-native-session-destroy session))))

(ert-deftest chirp-xchat-native-recovery-cancellation-is-terminal ()
  (skip-unless (chirp-xchat-native-test--load))
  (let* ((session (chirp-xchat-native-session-create))
         (epoch 19)
         (job-id
          (chirp-xchat-native-test-recovery-start
           session epoch "2580" 1000)))
    (unwind-protect
        (progn
          (should (chirp-xchat-native-test-recovery-cancel
                   session job-id epoch))
          (should-not (chirp-xchat-native-test-recovery-cancel
                       session job-id epoch))
          (should (eq (plist-get
                       (chirp-xchat-native-test--await
                        session job-id epoch)
                       :status)
                      'cancelled))
          (should-not (chirp-xchat-native-session-unlocked-p session)))
      (chirp-xchat-native-session-destroy session))))

(ert-deftest chirp-xchat-native-wrapper-shapes-prepared-text ()
  "The Lisp bridge should expose only the prepared public wire payload."
  (let (captured)
    (cl-letf (((symbol-function 'chirp-xchat-native--session)
               (lambda () 'session))
              ((symbol-function 'chirp-xchat-native-encrypt-text)
               (lambda (session input-json)
                 (should (eq session 'session))
                 (setq captured
                       (json-parse-string input-json :object-type 'alist))
                 (json-encode
                  '(("message_id" . "01234567-89ab-cdef-0123-456789abcdef")
                    ("encoded_message_create_event" . "ZXZlbnQ=")
                    ("encoded_message_event_signature" . "c2ln"))))))
      (should
       (equal
        (chirp-xchat-native-prepare-text "1-2" "private text")
        '(:message-id "01234567-89ab-cdef-0123-456789abcdef"
          :encoded-message-create-event "ZXZlbnQ="
          :encoded-message-event-signature "c2ln"))))
    (should (equal (alist-get 'conversation_id captured) "1-2"))
    (should-not (alist-get 'sender_id captured))
    (should (equal (alist-get 'text captured) "private text"))))

(ert-deftest chirp-xchat-native-wrapper-erases-input-and-settles-app-state ()
  "The Lisp bridge should erase secrets and retire its Appkit recovery."
  (skip-unless (chirp-xchat-native-test--load))
  (let* ((chirp--app nil)
         (pin (copy-sequence "2580"))
         (token (copy-sequence "short-lived-token"))
         (input
          `(("user_id" . "2222")
            ("sdk_config" . "{}")
            ("tokens" . (("realm" . ,token)))
            ("max_guess_count" . 20)
            ("registered_keys" . [])))
         result)
    (unwind-protect
        (cl-letf (((symbol-function 'chirp-xchat-native-recovery-start)
                   (lambda (session epoch received-pin _input-json)
                     (chirp-xchat-native-test-recovery-start
                      session epoch received-pin 20)))
                  ((symbol-function 'chirp-xchat-native-recovery-poll)
                   #'chirp-xchat-native-test-recovery-poll)
                  ((symbol-function 'chirp-xchat-native-recovery-cancel)
                   #'chirp-xchat-native-test-recovery-cancel))
          (should
           (chirp-xchat-native-recover
            pin input (lambda (status) (setq result status))))
          (should (cl-every #'zerop (string-to-list pin)))
          (should (cl-every #'zerop (string-to-list token)))
          (should (chirp-xchat-native-recovery-active-p))
          (let ((deadline (+ (float-time) 2.0)))
            (while (and (null result) (< (float-time) deadline))
              (sleep-for 0.01)))
          (should (eq (plist-get result :status) 'unlocked))
          (should-not (chirp-xchat-native-recovery-active-p)))
      (chirp-stop))))

(ert-deftest chirp-stop-destroys-an-app-owned-pending-native-session ()
  (skip-unless (chirp-xchat-native-test--load))
  (let ((chirp--app nil))
    (unwind-protect
        (let* ((session (chirp-xchat-native--session))
               (state (appkit-app-state chirp--app))
               (epoch (chirp--session-xchat-native-epoch state)))
          (should (eq session (chirp-xchat-native--session)))
          (chirp-xchat-native-test-recovery-start
           session epoch "2580" 1000)
          (chirp-stop)
          (should-not chirp--app)
          (should-not (chirp-xchat-native-session-live-p session)))
      (chirp-stop))))

(provide 'chirp-xchat-native-test)
;;; chirp-xchat-native-test.el ends here
