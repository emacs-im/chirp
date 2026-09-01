;;; chirp-xchat-test.el --- Tests for XChat protocol normalization -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Exercise bounded XChat wire decoding and backend adaptation.

;;; Code:

(add-to-list 'load-path
             (file-name-directory (or load-file-name buffer-file-name)))

(require 'ert)
(require 'cl-lib)
(require 'appkit-markup)
(require 'chirp)
(require 'chirp-xchat-native)
(require 'chirp-dm-test-helper)
(require 'chirp-xchat)


(ert-deftest chirp-xchat-recovery-input-selects-latest-bounded-key-config ()
  "Recovery normalization should select the latest key's exact token map."
  (let* ((input
          (chirp-xchat-recovery-input
           (chirp-dm-test--public-key-payload) "42"))
         (tokens (cdr (assoc "tokens" input)))
         (registered (cdr (assoc "registered_keys" input)))
         (key (aref registered 0)))
    (should (equal (cdr (assoc "user_id" input)) "42"))
    (should (= (cdr (assoc "max_guess_count" input)) 20))
    (should (equal (mapcar #'cdr tokens) '("new-token")))
    (should (= (length registered) 1))
    (should (equal (cdr (assoc "version" key)) "2"))
    (should
     (equal (cdr (assoc "identity_public_key" key))
            (base64-encode-string (make-string 91 ?C) t)))))

(ert-deftest chirp-xchat-recovery-input-rejects-another-users-keys ()
  "Recovery normalization should bind registered keys to the requested user."
  (should-error
   (chirp-xchat-recovery-input
    (chirp-dm-test--public-key-payload) "99")
   :type 'error))

(ert-deftest chirp-xchat-recovery-input-rejects-non-https-realms ()
  "Recovery normalization should reject an untrusted realm before PIN entry."
  (should-error
   (chirp-xchat-recovery-input
    (chirp-dm-test--public-key-payload "http://realm.example/") "42")
   :type 'error))

(ert-deftest chirp-xchat-signing-keys-normalizes-all-registered-versions ()
  "Signing-key normalization should preserve every bounded key version."
  (let ((keys
         (chirp-xchat-signing-keys
          (chirp-dm-test--public-key-payload) '("42"))))
    (should (= (length keys) 2))
    (should (equal (mapcar (lambda (key)
                             (cdr (assoc "public_key_version" key)))
                           (append keys nil))
                   '("1" "2")))
    (should (cl-every
             (lambda (key)
               (equal (cdr (assoc "user_id" key)) "42"))
             (append keys nil)))))

(ert-deftest chirp-backend-xchat-signing-keys-omit-juicebox-tokens ()
  "Signing-key lookup should not request private Juicebox realm tokens."
  (let (variables result)
    (cl-letf (((symbol-function 'chirp-x-graphql-request)
               (lambda (_operation requested-variables callback
                                   &rest _options)
                 (setq variables requested-variables)
                 (funcall callback (chirp-dm-test--public-key-payload)))))
      (chirp-backend-dm-signing-keys
       '("42") (lambda (keys _envelope) (setq result keys))))
    (should (equal (append (cdr (assoc "ids" variables)) nil) '("42")))
    (should (eq (cdr (assoc "include_juicebox_tokens" variables))
                :json-false))
    (should (= (length result) 2))))

(ert-deftest chirp-backend-xchat-media-uses-cookie-authenticated-cdn-request ()
  "XChat media should use its bounded cookie-authenticated CDN operation."
  (let (conversation media-hash owner ciphertext)
    (cl-letf (((symbol-function 'chirp-x-chat-media-request)
               (lambda (request-conversation request-hash callback
                                             &rest options)
                 (setq conversation request-conversation
                       media-hash request-hash
                       owner (plist-get options :owner))
                 (funcall callback (unibyte-string 0 255)))))
      (chirp-backend-dm-media
       "1:2" "media_hash"
       (lambda (value) (setq ciphertext value))
       :owner :view))
    (should (equal conversation "1:2"))
    (should (equal media-hash "media_hash"))
    (should (eq owner :view))
    (should (equal ciphertext (unibyte-string 0 255)))))

(ert-deftest chirp-backend-xchat-recovery-input-shapes-current-operation ()
  "Recovery lookup should use the current Web operation and exact variables."
  (let (operation variables result)
    (cl-letf (((symbol-function 'chirp-x-graphql-request)
               (lambda (requested-operation requested-variables callback
                                            &rest _options)
                 (setq operation requested-operation
                       variables requested-variables)
                 (funcall callback (chirp-dm-test--public-key-payload)))))
      (chirp-backend-dm-recovery-input
       "42" (lambda (input _envelope) (setq result input))))
    (should (equal (plist-get operation :query-id)
                   "nyLCqvDlxI4YoEBf-2ARmQ"))
    (should (equal (plist-get operation :name) "GetPublicKeysQuery"))
    (should (equal (append (cdr (assoc "ids" variables)) nil) '("42")))
    (should (eq (cdr (assoc "include_juicebox_tokens" variables)) t))
    (should (= (cdr (assoc "max_guess_count" result)) 20))))

(ert-deftest chirp-backend-xchat-inbox-decodes-plaintext-thrift ()
  "Inbox adaptation should decode plaintext events and preserve its cursor."
  (let* ((encoded
          (chirp-dm-test--event
           :sequence "20" :message-id "message-20" :sender-id "42"
           :conversation-id "conversation-1" :text "hello from XChat"
           :attachment-count 1 :message-request-p t))
         (cursor
          '(("__typename" . "XChatGetInboxPageContinueCursor")
            ("cursor_id" . "cursor-next")
            ("graph_snapshot_id" . "snapshot")
            ("graph_snapshot_restarted" . t)))
         operation variables request-owner conversations envelope)
    (cl-letf (((symbol-function 'chirp-x-graphql-request)
               (lambda (request-operation request-variables callback
                                          &rest options)
                 (setq operation request-operation
                       variables request-variables
                       request-owner (plist-get options :owner))
                 (funcall
                  callback
                  (chirp-dm-test--inbox-payload
                   (list (chirp-dm-test--conversation-item
                          :events (list encoded) :has-more t))
                   cursor)))))
      (chirp-backend-dm-inbox
       (lambda (items response-envelope)
         (setq conversations items
               envelope response-envelope))
       :max-results 20 :owner 'test-owner))
    (should (equal (plist-get operation :name) "GetInitialXChatPageQuery"))
    (should (equal (plist-get operation :query-id)
                   "8ryvCvaARbYYM1zXie8Q9g"))
    (should (eq request-owner 'test-owner))
    (let ((settings (alist-get "query_settings" variables nil nil #'string=)))
      (should (eq (alist-get "enable_legacy_overlay" settings nil nil #'string=)
                  t))
      (should (= (alist-get "inbox_conversation_limit"
                            settings nil nil #'string=)
                 20)))
    (let* ((conversation (car conversations))
           (event (car (plist-get conversation :events)))
           (next (chirp-backend-envelope-next-cursor envelope)))
      (should (equal (plist-get conversation :title) "Alice"))
      (should (equal (plist-get conversation :preview) "hello from XChat"))
      (should (equal (plist-get conversation :latest-event) event))
      (should (plist-get conversation :has-more))
      (should (equal (plist-get event :text) "hello from XChat"))
      (should
       (equal
        (appkit-markup-plain-text (plist-get event :document))
        "hello from XChat"))
      (should (= (plist-get event :attachment-count) 1))
      (should (plist-get event :message-request-p))
      (should (equal (plist-get next :cursor-id) "cursor-next"))
      (should (plist-get next :graph-snapshot-restarted-p)))))

(ert-deftest chirp-backend-xchat-conversation-data-allows-missing-deletion-flag ()
  "Focused conversation data may omit the inbox-only deletion flag."
  (let* ((item
          (cl-remove-if
           (lambda (cell) (equal (car cell) "is_deleted_by_viewer"))
           (chirp-dm-test--conversation-item)))
         (payload
          `(("data" .
             (("get_inbox_page_conversation_data" .
               (("items" . (,item))))))))
         conversation failure)
    (cl-letf (((symbol-function 'chirp-x-graphql-request)
               (lambda (_operation _variables callback &rest _options)
                 (funcall callback payload))))
      (chirp-backend-dm-conversation-data
       "conversation-1"
       (lambda (result _envelope) (setq conversation result))
       :errback (lambda (message) (setq failure message))))
    (should-not failure)
    (should (equal (plist-get conversation :id) "conversation-1"))))

(ert-deftest chirp-backend-xchat-inbox-requires-deletion-flag ()
  "Inbox data must retain its authoritative deletion flag."
  (let* ((item
          (cl-remove-if
           (lambda (cell) (equal (car cell) "is_deleted_by_viewer"))
           (chirp-dm-test--conversation-item)))
         (payload (chirp-dm-test--inbox-payload (list item)))
         success failure)
    (cl-letf (((symbol-function 'chirp-x-graphql-request)
               (lambda (_operation _variables callback &rest _options)
                 (funcall callback payload))))
      (chirp-backend-dm-inbox
       (lambda (&rest _ignored) (setq success t))
       :errback (lambda (message) (setq failure message))))
    (should-not success)
    (should (string-match-p "conversation deletion flag" failure))))

(ert-deftest chirp-backend-xchat-inbox-allows-missing-restart-flag ()
  "A continuation cursor may omit its false snapshot restart flag."
  (let ((cursor
         '(("__typename" . "XChatGetInboxPageContinueCursor")
           ("cursor_id" . "cursor-next")
           ("graph_snapshot_id" . "snapshot")))
        envelope failure)
    (cl-letf (((symbol-function 'chirp-x-graphql-request)
               (lambda (_operation _variables callback &rest _options)
                 (funcall callback (chirp-dm-test--inbox-payload nil cursor)))))
      (chirp-backend-dm-inbox
       (lambda (_items result) (setq envelope result))
       :errback (lambda (message) (setq failure message))))
    (should-not failure)
    (let ((next (chirp-backend-envelope-next-cursor envelope)))
      (should (equal (plist-get next :cursor-id) "cursor-next"))
      (should-not (plist-get next :graph-snapshot-restarted-p)))))

(ert-deftest chirp-backend-xchat-inbox-continuation-shapes-exact-cursor ()
  "Inbox continuation should use XChat's three cursor input field names."
  (let (operation variables completed envelope failure)
    (cl-letf (((symbol-function 'chirp-x-graphql-request)
               (lambda (request-operation request-variables callback
                                          &rest _options)
                 (setq operation request-operation
                       variables request-variables)
                 (funcall callback (chirp-dm-test--inbox-payload nil)))))
      (chirp-backend-dm-inbox
       (lambda (_items response-envelope)
         (setq completed t envelope response-envelope))
       :cursor '(:cursor-id "cursor"
                 :graph-snapshot-id "snapshot"
                 :graph-snapshot-restarted-p nil)
       :errback (lambda (message) (setq failure message))))
    (should (equal (plist-get operation :name) "GetInboxPageRequestQuery"))
    (should completed)
    (should-not failure)
    (should (eq (chirp-get-in envelope '("pagination" "complete")) t))
    (should-not (chirp-backend-envelope-next-cursor envelope))
    (let ((cursor (alist-get "continue_cursor" variables nil nil #'string=)))
      (should (equal (alist-get "cursor_id" cursor nil nil #'string=)
                     "cursor"))
      (should (equal (alist-get "graph_snapshot_id" cursor nil nil #'string=)
                     "snapshot"))
      (should (eq (alist-get "graph_snapshot_restarted"
                             cursor nil nil #'string=)
                  :json-false)))))

(ert-deftest chirp-backend-xchat-encrypted-event-is-explicitly-unavailable ()
  "Keyed XChat content should become a visible unavailable placeholder."
  (let* ((encoded
          (chirp-dm-test--event
           :sequence "20" :message-id "message-20" :sender-id "42"
           :conversation-id "conversation-1" :text "must not decode"
           :key-version "7"))
         (event (chirp-xchat-decode-event encoded)))
    (should (plist-get event :encrypted-p))
    (should (equal (plist-get event :conversation-key-version) "7"))
    (should (equal (plist-get event :encoded-event) encoded))
    (should (equal (plist-get event :text)
                   "[Encrypted message unavailable]"))
    (should
     (equal
      (appkit-markup-plain-text (plist-get event :document))
      "[Encrypted message unavailable]"))))

(ert-deftest chirp-backend-xchat-rejects-malformed-thrift-booleans ()
  "XChat decoding should accept only the two Thrift BOOL encodings."
  (let* ((encoded
          (chirp-dm-test--event
           :sequence "20" :message-id "message-20" :sender-id "42"
           :conversation-id "conversation-1" :text "hello"))
         (bytes (base64-decode-string encoded)))
    (aset bytes (- (length bytes) 2) 2)
    (should
     (string-match-p
      "invalid Thrift BOOL value"
      (error-message-string
       (should-error
        (chirp-xchat-decode-event (base64-encode-string bytes t))))))))

(ert-deftest chirp-backend-xchat-system-events-keep-sequence-boundaries ()
  "Non-message XChat events should retain stable sequence keys."
  (let ((event
         (chirp-xchat-decode-event
          (chirp-dm-test--event
           :sequence "15" :message-id "target-message" :sender-id "42"
           :conversation-id "conversation-1" :detail-field 7))))
    (should (equal (plist-get event :id) "15"))
    (should (equal (plist-get event :message-id) "target-message"))
    (should (eq (plist-get event :kind) 'message-delete))))

(ert-deftest chirp-backend-xchat-malformed-event-fails-the-page ()
  "Malformed Thrift should reach errback without partial inbox success."
  (let (success failure)
    (cl-letf (((symbol-function 'chirp-x-graphql-request)
               (lambda (_operation _variables callback &rest _options)
                 (funcall
                  callback
                  (chirp-dm-test--inbox-payload
                   (list (chirp-dm-test--conversation-item
                          :events '("not-base64!"))))))))
      (chirp-backend-dm-inbox
       (lambda (&rest _ignored) (setq success t))
       :errback (lambda (message) (setq failure message))))
    (should-not success)
    (should (string-match-p "invalid encoded message event" failure))))

(ert-deftest chirp-backend-xchat-does-not-catch-consumer-callback-errors ()
  "Backend adaptation should not misreport consumer bugs as X failures."
  (let ((payload
         (chirp-dm-test--inbox-payload
          (list (chirp-dm-test--conversation-item))))
        failure)
    (cl-letf (((symbol-function 'chirp-x-graphql-request)
               (lambda (_operation _variables callback &rest _options)
                 (funcall callback payload))))
      (should-error
       (chirp-backend-dm-inbox
        (lambda (&rest _ignored) (error "consumer callback failed"))
        :errback (lambda (message) (setq failure message)))
       :type 'error))
    (should-not failure)))

(ert-deftest chirp-backend-xchat-inbox-rejects-unbounded-event-pages ()
  "Inbox adaptation should reject excessive events before Thrift decoding."
  (let ((payload
         (chirp-dm-test--inbox-payload
          (list (chirp-dm-test--conversation-item
                 :events (make-list 1001 "AA==")))))
        success failure)
    (cl-letf (((symbol-function 'chirp-x-graphql-request)
               (lambda (_operation _variables callback &rest _options)
                 (funcall callback payload))))
      (chirp-backend-dm-inbox
       (lambda (&rest _ignored) (setq success t))
       :errback (lambda (message) (setq failure message))))
    (should-not success)
    (should (string-match-p "too many encoded message events" failure))))

(ert-deftest chirp-backend-xchat-inbox-rejects-unknown-cursor-unions ()
  "Inbox adaptation should not mistake a drifted cursor for exhaustion."
  (let ((payload
         (chirp-dm-test--inbox-payload
          nil '(("__typename" . "XChatFutureCursor"))))
        success failure)
    (cl-letf (((symbol-function 'chirp-x-graphql-request)
               (lambda (_operation _variables callback &rest _options)
                 (funcall callback payload))))
      (chirp-backend-dm-inbox
       (lambda (&rest _ignored) (setq success t))
       :errback (lambda (message) (setq failure message))))
    (should-not success)
    (should (string-match-p "unknown inbox cursor type" failure))))

(ert-deftest chirp-backend-xchat-inbox-rejects-unknown-conversation-unions ()
  "Inbox adaptation should reject unknown conversation detail variants."
  (let* ((item (chirp-dm-test--conversation-item))
         (detail (cdr (assoc "conversation_detail" item)))
         (payload (chirp-dm-test--inbox-payload (list item)))
         success failure)
    (setcdr (assoc "__typename" detail) "XChatFutureConversationDetail")
    (cl-letf (((symbol-function 'chirp-x-graphql-request)
               (lambda (_operation _variables callback &rest _options)
                 (funcall callback payload))))
      (chirp-backend-dm-inbox
       (lambda (&rest _ignored) (setq success t))
       :errback (lambda (message) (setq failure message))))
    (should-not success)
    (should (string-match-p "unknown conversation type" failure))))

(ert-deftest chirp-backend-xchat-inbox-rejects-foreign-events ()
  "Inbox adaptation should reject events from another conversation."
  (let* ((event
          (chirp-dm-test--event
           :sequence "20" :message-id "message-20" :sender-id "42"
           :conversation-id "conversation-2" :key-version "7"))
         (payload
          (chirp-dm-test--inbox-payload
           (list (chirp-dm-test--conversation-item
                  :id "conversation-1" :events (list event)))))
         success failure)
    (cl-letf (((symbol-function 'chirp-x-graphql-request)
               (lambda (_operation _variables callback &rest _options)
                 (funcall callback payload))))
      (chirp-backend-dm-inbox
       (lambda (&rest _ignored) (setq success t))
       :errback (lambda (message) (setq failure message))))
    (should-not success)
    (should (string-match-p "event for another conversation" failure))))

(ert-deftest chirp-backend-xchat-history-preserves-pagination-facts ()
  "History adaptation should send the exact cursor and return older events."
  (let* ((older
          (chirp-dm-test--event
           :sequence "10" :message-id "message-10" :sender-id "42"
           :conversation-id "conversation-1" :text "older"))
         (payload
          `(("data" .
             (("get_conversation_page" .
               (("encoded_message_events" . (,older))
                ("has_more" . nil)
                ("message_request_state" . "NONE")))))))
         operation variables owner events envelope)
    (cl-letf (((symbol-function 'chirp-x-graphql-request)
               (lambda (request-operation request-variables callback
                                          &rest options)
                 (setq operation request-operation
                       variables request-variables
                       owner (plist-get options :owner))
                 (funcall callback payload))))
      (chirp-backend-dm-history
       "conversation-1" '(:sequence-id "20" :key-version "0")
       (lambda (items response-envelope)
         (setq events items envelope response-envelope))
       :max-results 50 :owner 'history-owner))
    (should (equal (plist-get operation :name) "GetConversationPageQuery"))
    (should (eq owner 'history-owner))
    (should (equal (alist-get "min_local_sequence_id"
                              variables nil nil #'string=)
                   "20"))
    (should (equal (alist-get "min_conversation_key_version"
                              variables nil nil #'string=)
                   "0"))
    (should (= (alist-get
                "conversation_event_limit"
                (alist-get "query_settings" variables nil nil #'string=)
                nil nil #'string=)
               50))
    (should (equal (mapcar (lambda (event) (plist-get event :id)) events)
                   '("10")))
    (should (eq (chirp-get-in envelope '("pagination" "complete")) t))
    (should-not (chirp-backend-envelope-next-cursor envelope))))

(ert-deftest chirp-backend-xchat-history-preserves-recovery-key-events ()
  "History adaptation should retain bounded key changes outside display events."
  (let* ((message
          (chirp-dm-test--event
           :sequence "10" :message-id "message-10" :sender-id "42"
           :conversation-id "conversation-1" :key-version "7"))
         (key-event
          (chirp-dm-test--event
           :sequence "9" :message-id "key-9" :sender-id "42"
           :conversation-id "conversation-1" :detail-field 3))
         (payload
          `(("data" .
             (("get_conversation_page" .
               (("encoded_message_events" . (,message))
                ("missing_conversation_key_change_events" . (,key-event))
                ("has_more" . nil)))))))
         events envelope failure)
    (cl-letf (((symbol-function 'chirp-x-graphql-request)
               (lambda (_operation _variables callback &rest _options)
                 (funcall callback payload))))
      (chirp-backend-dm-history
       "conversation-1" '(:sequence-id "20" :key-version "7")
       (lambda (items response-envelope)
         (setq events items envelope response-envelope))
       :errback (lambda (message-text) (setq failure message-text))))
    (should-not failure)
    (should (equal (mapcar (lambda (event) (plist-get event :id)) events)
                   '("10")))
    (should (equal (plist-get (car events) :encoded-event) message))
    (should (equal (chirp-get envelope "encodedKeyEvents")
                   (list key-event)))
    (should (equal (plist-get (car (chirp-get envelope "keyEvents"))
                              :sender-id)
                   "42"))))

(ert-deftest chirp-backend-xchat-history-rejects-non-key-recovery-events ()
  "History adaptation should reject message events in the recovery-key field."
  (let* ((message
          (chirp-dm-test--event
           :sequence "10" :message-id "message-10" :sender-id "42"
           :conversation-id "conversation-1" :text "not a key"))
         (payload
          `(("data" .
             (("get_conversation_page" .
               (("encoded_message_events" . nil)
                ("missing_conversation_key_change_events" . (,message))
                ("has_more" . nil)))))))
         success failure)
    (cl-letf (((symbol-function 'chirp-x-graphql-request)
               (lambda (_operation _variables callback &rest _options)
                 (funcall callback payload))))
      (chirp-backend-dm-history
       "conversation-1" '(:sequence-id "20" :key-version "0")
       (lambda (&rest _ignored) (setq success t))
       :errback (lambda (message-text) (setq failure message-text))))
    (should-not success)
    (should (string-match-p "non-key recovery event" failure))))

(ert-deftest chirp-backend-xchat-history-requires-a-progress-cursor ()
  "A history page claiming more data should provide an older event cursor."
  (let ((payload
         '(("data" .
            (("get_conversation_page" .
              (("encoded_message_events" . nil)
               ("has_more" . t)))))))
        success failure)
    (cl-letf (((symbol-function 'chirp-x-graphql-request)
               (lambda (_operation _variables callback &rest _options)
                 (funcall callback payload))))
      (chirp-backend-dm-history
       "conversation-1" '(:sequence-id "20" :key-version "0")
       (lambda (&rest _ignored) (setq success t))
       :errback (lambda (message) (setq failure message))))
    (should-not success)
    (should (string-match-p "has more events but no continuation cursor"
                            failure))))

(ert-deftest chirp-backend-xchat-send-encrypts-once-and-requires-an-ack ()
  "Text send should use the fixed write and reject malformed acknowledgements."
  (let* ((chirp--app nil)
         (message-id "01234567-89ab-cdef-0123-456789abcdef")
         (encoded
          (chirp-dm-test--event
           :sequence "30" :message-id message-id :sender-id "42"
           :conversation-id "42:99" :text "sent" :key-version "1"))
         calls sent failure)
    (unwind-protect
        (progn
          (setf (chirp--session-xchat-user-id (chirp--session)) "42")
          (cl-letf (((symbol-function 'chirp-xchat-native-prepare-text)
                     (lambda (conversation-id text)
                       (push 'encrypt calls)
                       (should (equal conversation-id "42-99"))
                       (should (equal text "hello"))
                       (list
                        :message-id message-id
                        :encoded-message-create-event "ZXZlbnQ="
                        :encoded-message-event-signature "c2ln")))
                    ((symbol-function 'chirp-x-graphql-request)
                     (lambda (operation variables callback &rest options)
                       (push 'write calls)
                       (should (equal (plist-get operation :name)
                                      "SendMessageCreateMutation"))
                       (should (equal (plist-get operation :query-id)
                                      "TWRPP7gnKwV_R8-tE-Dd3Q"))
                       (should (eq (plist-get operation :method) 'post))
                       (should
                        (equal
                         variables
                         `(("conversation_id" . "42-99")
                           ("message_id" . ,message-id)
                           ("encoded_message_create_event" . "ZXZlbnQ=")
                           ("encoded_message_event_signature" . "c2ln"))))
                       (should (eq (plist-get options :owner) 'owner))
                       (funcall
                        callback
                        `(("data" .
                           (("xchat_send_create_message_event" .
                             (("__typename" .
                               "XChatSendMessageCreateEventResponse")
                              ("encoded_message_event" . ,encoded)))))))
                       nil)))
            (chirp-backend-dm-send-text
             "42-99" "hello"
             (lambda (event _envelope) (setq sent event))
             :errback (lambda (message) (setq failure message))
             :owner 'owner))
          (should (equal (nreverse calls) '(encrypt write)))
          (should-not failure)
          (should (equal (plist-get sent :message-id) message-id))
          (cl-letf (((symbol-function 'chirp-xchat-native-prepare-text)
                     (lambda (&rest _args)
                       (list
                        :message-id message-id
                        :encoded-message-create-event "ZXZlbnQ="
                        :encoded-message-event-signature "c2ln")))
                    ((symbol-function 'chirp-x-graphql-request)
                     (lambda (_operation _variables callback &rest _options)
                       (funcall callback '(("data" . nil)))
                       nil)))
            (setq failure nil)
            (chirp-backend-dm-send-text
             "42-99" "hello" #'ignore
             :errback (lambda (message) (setq failure message)))
            (should (string-prefix-p "X write outcome is unknown" failure))))
      (chirp-stop))))

(ert-deftest chirp-xchat-send-acknowledgement-binds-message-and-participants ()
  "A send acknowledgement should match its message, sender, and conversation."
  (let* ((message-id "01234567-89ab-cdef-0123-456789abcdef")
         (encoded
          (chirp-dm-test--event
           :sequence "30" :message-id message-id :sender-id "42"
           :conversation-id "42:99" :text "sent" :key-version "1"))
         (payload
          `(("data" .
             (("xchat_send_create_message_event" .
               (("__typename" . "XChatSendMessageCreateEventResponse")
                ("encoded_message_event" . ,encoded))))))))
    (should-error
     (chirp-xchat-send-result payload "42-99" "43" message-id))
    (should-error
     (chirp-xchat-send-result payload "42-100" "42" message-id))
    (should-error
     (chirp-xchat-send-result
      payload "42-99" "42" "11111111-1111-1111-1111-111111111111"))))

(ert-deftest chirp-xchat-live-token-is-bounded-and-never-adapted-loosely ()
  "Live token normalization should require one bounded JWT-shaped value."
  (let ((payload
         '(("data" .
            (("user_get_x_chat_auth_token" .
              (("token" . "header.payload.signature"))))))))
    (should (equal (chirp-xchat-live-token payload)
                   "header.payload.signature"))
    (should-error
     (chirp-xchat-live-token
      '(("data" .
         (("user_get_x_chat_auth_token" . (("token" . "not-a-jwt"))))))))))

(ert-deftest chirp-xchat-live-frame-decodes-event-and-instructions-boundedly ()
  "Binary live frames should expose events and classify control instructions."
  (let* ((encoded
          (chirp-dm-test--event
           :sequence "31" :message-id "message-31" :sender-id "42"
           :conversation-id "conversation-1" :text "live"))
         (event-frame
          (concat (unibyte-string 12 0 1)
                  (base64-decode-string encoded)
                  (unibyte-string 0)))
         (event-result (chirp-xchat-decode-live-frame event-frame)))
    (should (eq (plist-get event-result :kind) 'event))
    (should (equal (plist-get (plist-get event-result :event) :id) "31"))
    (should
     (eq (plist-get
          (chirp-xchat-decode-live-frame
           (chirp-xchat-live-keepalive-frame))
          :kind)
         'keepalive))
    (should
     (eq (plist-get
          (chirp-xchat-decode-live-frame
           (unibyte-string 12 0 2 12 0 8 2 0 1 1 0 0 0))
          :kind)
         'instruction))
    (should-error (chirp-xchat-decode-live-frame "multibyte-λ"))))

(ert-deftest chirp-backend-xchat-live-token-uses-fixed-write-operation ()
  "Live token acquisition should use the fixed mutation and strict adapter."
  (let (operation variables owner token)
    (cl-letf
        (((symbol-function 'chirp-x-graphql-request)
          (lambda (requested-operation requested-variables callback
                                       &rest options)
            (setq operation requested-operation
                  variables requested-variables
                  owner (plist-get options :owner))
            (funcall
             callback
             '(("data" .
                (("user_get_x_chat_auth_token" .
                  (("token" . "header.payload.signature")))))))
            'request)))
      (chirp-backend-dm-live-token
       (lambda (value _envelope) (setq token value))
       :owner 'app))
    (should (equal (plist-get operation :query-id)
                   "Qh3fZRjPPtPoHYR_2sCZsA"))
    (should (equal (plist-get operation :name)
                   "GenerateXChatTokenMutation"))
    (should (eq (plist-get operation :method) 'post))
    (should-not variables)
    (should (eq owner 'app))
    (should (equal token "header.payload.signature"))))

(provide 'chirp-xchat-test)

;;; chirp-xchat-test.el ends here
