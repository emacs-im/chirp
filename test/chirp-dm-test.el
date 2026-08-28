;;; chirp-dm-test.el --- Tests for XChat views and composer -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Commentary:

;; Exercise synthetic XChat wire adaptation and Appkit-owned direct messages.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'chirp)
(require 'chirp-xchat)
(require 'chirp-xchat-native)

(declare-function evil-mode "evil" (&optional arg))
(declare-function evil-insert "evil-commands"
                  (count &optional vcount skip-empty-lines))
(declare-function evil-insert-state "evil-states" ())
(declare-function evil-normal-state "evil-states" ())
(defvar evil-mode)
(defvar evil-state)

(defun chirp-dm-test--u8 (value)
  "Encode VALUE as one unsigned byte."
  (unibyte-string (logand value #xff)))

(defun chirp-dm-test--unsigned (value count)
  "Encode nonnegative VALUE as COUNT big-endian bytes."
  (apply #'unibyte-string
         (cl-loop for shift downfrom (* 8 (1- count)) to 0 by 8
                  collect (logand (ash value (- shift)) #xff))))

(defun chirp-dm-test--i16 (value)
  "Encode VALUE as one Thrift i16."
  (chirp-dm-test--unsigned value 2))

(defun chirp-dm-test--i32 (value)
  "Encode VALUE as one Thrift i32."
  (chirp-dm-test--unsigned value 4))

(defun chirp-dm-test--i64 (value)
  "Encode VALUE as one Thrift i64."
  (chirp-dm-test--unsigned value 8))

(defun chirp-dm-test--binary (bytes)
  "Encode unibyte BYTES as one Thrift binary value."
  (concat (chirp-dm-test--i32 (length bytes)) bytes))

(defun chirp-dm-test--text (text)
  "Encode UTF-8 TEXT as one Thrift string value."
  (chirp-dm-test--binary (encode-coding-string text 'utf-8)))

(defun chirp-dm-test--field (type id value)
  "Encode a Thrift field with TYPE, ID, and encoded VALUE."
  (concat (chirp-dm-test--u8 type)
          (chirp-dm-test--i16 id)
          value))

(defun chirp-dm-test--struct (&rest fields)
  "Encode Thrift FIELDS as one binary struct."
  (concat (apply #'concat fields) (chirp-dm-test--u8 0)))

(defun chirp-dm-test--struct-list (structs)
  "Encode STRUCTS as one Thrift list of structs."
  (concat (chirp-dm-test--u8 12)
          (chirp-dm-test--i32 (length structs))
          (apply #'concat structs)))

(cl-defun chirp-dm-test--event
    (&key sequence message-id sender-id conversation-id text
          key-version attachment-count message-request-p
          (detail-field 1) (trusted-p t))
  "Return one synthetic Base64 XChat event from keyword fields."
  (let* ((message
          (chirp-dm-test--struct
           (chirp-dm-test--field 11 1 (chirp-dm-test--text (or text "")))
           (if (> (or attachment-count 0) 0)
               (chirp-dm-test--field
                15 3
                (chirp-dm-test--struct-list
                 (make-list attachment-count
                            (chirp-dm-test--struct))))
             "")))
         (entry-union
          (chirp-dm-test--struct
           (chirp-dm-test--field 12 1 message)))
         (holder
          (chirp-dm-test--struct
           (chirp-dm-test--field 12 1 entry-union)))
         (create
          (chirp-dm-test--struct
           (chirp-dm-test--field
            11 100
            (chirp-dm-test--binary
             (if key-version (encode-coding-string "ciphertext" 'utf-8)
               holder)))
           (if key-version
               (chirp-dm-test--field 11 101
                                     (chirp-dm-test--text key-version))
             "")
           (chirp-dm-test--field 2 102 (chirp-dm-test--u8 1))
           (chirp-dm-test--field 10 104
                                 (chirp-dm-test--i64 1700000000000))
           (chirp-dm-test--field
            2 109 (chirp-dm-test--u8 (if message-request-p 1 0)))))
         (detail
          (chirp-dm-test--struct
           (chirp-dm-test--field
            12 detail-field
            (if (= detail-field 1) create
              (chirp-dm-test--struct)))))
         (event
          (chirp-dm-test--struct
           (chirp-dm-test--field 11 1
                                 (chirp-dm-test--text sequence))
           (chirp-dm-test--field 11 2
                                 (chirp-dm-test--text message-id))
           (chirp-dm-test--field 11 3
                                 (chirp-dm-test--text sender-id))
           (chirp-dm-test--field 11 4
                                 (chirp-dm-test--text conversation-id))
           (chirp-dm-test--field 11 6
                                 (chirp-dm-test--text "1700000000000"))
           (chirp-dm-test--field 12 7 detail)
           (chirp-dm-test--field 8 8 (chirp-dm-test--i32 1))
           (chirp-dm-test--field
            2 11 (chirp-dm-test--u8 (if trusted-p 1 0))))))
    (base64-encode-string event t)))

(defun chirp-dm-test--user (id name handle)
  "Return one synthetic XChat user wrapper for ID, NAME, and HANDLE."
  `(("rest_id" . ,id)
    ("result" .
     (("rest_id" . ,id)
      ("core" . (("name" . ,name) ("screen_name" . ,handle)))
      ("avatar" . (("image_url" . "https://example.invalid/avatar.jpg")))))))

(cl-defun chirp-dm-test--conversation-item
    (&key (id "conversation-1") events (name "Alice")
          (handle "alice") has-more message-request-p group-p)
  "Return a synthetic XChat inbox item carrying EVENTS."
  (let ((detail
         (append
          `(("__typename" . ,(if group-p
                                  "XChatGroupConversationDetail"
                                "XChatDirectConversationDetail"))
            ("conversation_id" . ,id)
            ("participants_results" .
             (,(chirp-dm-test--user "42" name handle))))
          (when group-p
            '(("group_metadata" . (("group_name" . "Test Group"))))))))
    `(("conversation_detail" . ,detail)
      ("latest_message_events" . ,events)
      ("has_more" . ,(and has-more t))
      ("is_deleted_by_viewer" . nil)
      ,@(when message-request-p
          `(("latest_notifiable_message_create_event" . ,(car events)))))))

(defun chirp-dm-test--token-map (&optional address token)
  "Return one synthetic bounded Juicebox token map using ADDRESS and TOKEN."
  (let* ((realm-id "01010101010101010101010101010101")
         (realm-address (or address "https://realm.example/"))
         (realm-key (make-string 64 ?1))
         (sdk-config
          (json-encode
           (list
            (cons "realms"
                  (vector
                   (list (cons "id" realm-id)
                         (cons "address" realm-address)
                         (cons "public_key" realm-key))))
            (cons "register_threshold" 1)
            (cons "recover_threshold" 1)
            (cons "pin_hashing_mode" "Standard2019")))))
    (list
     (cons "__typename" "KeyStoreTokenMap")
     (cons "max_guess_count" 20)
     (cons "recover_threshold" 1)
     (cons "register_threshold" 1)
     (cons "token_map"
           (list
            (list
             (cons "key" realm-id)
             (cons "value"
                   (list (cons "token" (or token "realm-token"))
                         (cons "address" realm-address)
                         (cons "public_key" realm-key))))))
     (cons "key_store_token_map_json" sdk-config))))

(defun chirp-dm-test--public-key-item (version token-map fill)
  "Return a synthetic XChat key VERSION with TOKEN-MAP and byte FILL."
  (let ((identity (base64-encode-string (make-string 91 fill) t))
        (signing (base64-encode-string (make-string 91 ?S) t))
        (binding (base64-encode-string (make-string 64 ?B) t)))
    (list
     (cons "public_key_with_metadata"
           (list
            (cons "version" version)
            (cons "public_key"
                  (list (cons "public_key" identity)
                        (cons "signing_public_key" signing)
                        (cons "identity_public_key_signature" binding)
                        (cons "registration_method" "CustomPin")))))
     (cons "token_map" token-map))))

(defun chirp-dm-test--public-key-payload (&optional latest-address)
  "Return a synthetic GetPublicKeys response using LATEST-ADDRESS."
  (let ((keys
         (list
          (chirp-dm-test--public-key-item
           "1" (chirp-dm-test--token-map nil "old-token") ?A)
          (chirp-dm-test--public-key-item
           "2" (chirp-dm-test--token-map latest-address "new-token") ?C))))
    (list
     (cons
      "data"
      (list
       (cons
        "user_results_by_rest_ids"
        (list
         (list
          (cons "rest_id" "42")
          (cons
           "result"
           (list
            (cons "__typename" "User")
            (cons
             "get_public_keys"
             (list
              (cons "__typename" "GetPublicKeysResult")
              (cons "public_keys_with_token_map" keys)))))))))))))

(defun chirp-dm-test--inbox-payload (items &optional cursor)
  "Return a synthetic initial inbox payload with ITEMS and CURSOR."
  `(("data" .
     (("get_initial_chat_page" .
      (("__typename" . "XChatGetInboxPageResponse")
       ("items" . ,items)
       ("inboxCursor" .
        ,(or cursor
             '(("__typename" . "XChatGetInboxPageEndCursor")
               ("graph_snapshot_id" . "snapshot"))))))))))

(defun chirp-dm-test--normalized-event
    (id sequence text &optional sender-id conversation-id)
  "Return a normalized event with ID, SEQUENCE, and TEXT."
  (list :id id
        :sequence-id sequence
        :sender-id (or sender-id "42")
        :conversation-id (or conversation-id "conversation-1")
        :created-at-msec "1700000000000"
        :kind 'message
        :text text))

(defun chirp-dm-test--normalized-conversation (&rest events)
  "Return one normalized direct conversation carrying EVENTS."
  (list :id "conversation-1"
        :type 'direct
        :title "Alice"
        :participants '((:id "42" :name "Alice" :handle "alice"))
        :events events
        :preview (and events (plist-get (car (last events)) :text))
        :updated-at-msec "1700000000000"
        :has-more t
        :older-cursor
        (and events
             (list :sequence-id (plist-get (car events) :sequence-id)
                   :key-version "0"))))

(ert-deftest chirp-dm-inbox-time-is-localized-and-right-aligned ()
  "Inbox rows should put compact localized time at the view's right edge."
  (let* ((chirp-language "zh-CN")
         (now (encode-time 0 0 12 13 8 2026))
         (milliseconds
          (number-to-string
           (* 1000
              (time-convert
               (time-subtract now (seconds-to-time (* 6 3600)))
               'integer))))
         (entry
          (appkit-directory-entry-create
           :key "conversation-1"
           :payload
           (list :id "conversation-1"
                 :title "Alice"
                 :preview "Hello"
                 :updated-at-msec milliseconds))))
    (with-temp-buffer
      (setq-local fill-column 40)
      (cl-letf (((symbol-function 'current-time) (lambda () now)))
        (chirp-dm--insert-inbox-item nil entry))
      (goto-char (point-min))
      (search-forward "6小时")
      (should
       (equal
        (get-text-property (1- (match-beginning 0)) 'display)
        `(space :align-to
                (- right (,(string-width "6小时") . width))))))))

(ert-deftest chirp-dm-message-time-is-localized-and-right-aligned ()
  "Message headings should put compact localized time at the right edge."
  (let* ((chirp-language "zh-CN")
         (now (encode-time 0 0 12 13 8 2026))
         (event
          (chirp-dm-test--normalized-event "20" "20" "Hello"))
         (row
          (appkit-chat-timeline-row-create
           :key "20" :payload event :context '(:sender-label "Alice"))))
    (setf (plist-get event :created-at-msec)
          (number-to-string
           (* 1000
              (time-convert
               (time-subtract now (seconds-to-time (* 6 3600)))
               'integer))))
    (with-temp-buffer
      (setq-local fill-column 40)
      (cl-letf (((symbol-function 'current-time) (lambda () now)))
        (chirp-dm--print-event-row row))
      (goto-char (point-min))
      (search-forward "6小时")
      (should
       (equal
        (get-text-property (1- (match-beginning 0)) 'display)
        `(space :align-to
                (- right (,(string-width "6小时") . width))))))))

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

(ert-deftest chirp-direct-messages-unlocks-before-opening-and-erases-secrets ()
  "The DM entry path should unlock first without retaining its secrets."
  (let* ((chirp--app nil)
         (pin (copy-sequence "2580"))
         (token (copy-sequence "realm-token"))
         (input `(("tokens" . (("realm" . ,token)))))
         call-order received-pin owner buffer)
    (unwind-protect
        (cl-letf (((symbol-function 'chirp-xchat-native-load)
                   (lambda () t))
                  ((symbol-function 'chirp-xchat-native-unlocked-p)
                   (lambda () nil))
                  ((symbol-function 'chirp-xchat-native-recovery-active-p)
                   (lambda () nil))
                  ((symbol-function 'chirp-backend-whoami)
                   (lambda (callback &optional _errback)
                     (push 'viewer call-order)
                     (funcall callback '(:id "42") nil)))
                  ((symbol-function 'chirp-backend-dm-recovery-input)
                   (lambda (_user-id callback &rest options)
                     (setq owner (plist-get options :owner))
                     (push 'configuration call-order)
                     (funcall callback input nil)))
                  ((symbol-function 'read-passwd)
                   (lambda (&rest _arguments)
                     (push 'pin call-order)
                     pin))
                  ((symbol-function 'chirp-xchat-native-recover)
                   (lambda (received _input callback &rest _options)
                     (setq received-pin (copy-sequence received))
                     (push 'recovery call-order)
                     (funcall callback '(:status unlocked))
                     1))
                  ((symbol-function 'chirp-backend-dm-inbox)
                   (lambda (callback &rest options)
                     (push 'inbox call-order)
                     (setq buffer
                           (appkit-view-buffer (plist-get options :owner)))
                     (funcall callback nil nil)
                     nil)))
          (should-not (chirp-direct-messages))
          (should (equal (nreverse call-order)
                         '(viewer configuration pin recovery inbox)))
          (should (equal received-pin "2580"))
          (should (eq owner chirp--app))
          (should (cl-every #'zerop (string-to-list pin)))
          (should (cl-every #'zerop (string-to-list token)))
          (should (buffer-live-p buffer)))
      (chirp-stop)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

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
      (should (plist-get conversation :has-more))
      (should (equal (plist-get event :text) "hello from XChat"))
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
         (event (chirp-xchat-normalize-event encoded)))
    (should (plist-get event :encrypted-p))
    (should (equal (plist-get event :conversation-key-version) "7"))
    (should (equal (plist-get event :encoded-event) encoded))
    (should (equal (plist-get event :text)
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
        (chirp-xchat-normalize-event (base64-encode-string bytes t))))))))

(ert-deftest chirp-backend-xchat-system-events-keep-sequence-boundaries ()
  "Non-message XChat events should retain stable sequence keys."
  (let ((event
         (chirp-xchat-normalize-event
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

(ert-deftest chirp-dm-inbox-pagination-merges-by-conversation-id ()
  "Older inbox pages should use the cursor and append unique conversations."
  (let ((chirp--app nil)
        buffer callbacks options)
    (unwind-protect
        (save-window-excursion
          (cl-letf (((symbol-function 'chirp-xchat-native-load)
                     (lambda () t))
                    ((symbol-function 'chirp-xchat-native-unlocked-p)
                     (lambda () t))
                    ((symbol-function 'chirp-backend-dm-inbox)
                     (lambda (callback &rest request-options)
                       (setq callbacks (append callbacks (list callback))
                             options (append options (list request-options)))
                       (list 'request (length callbacks)))))
            (setf (chirp--session-xchat-user-id (chirp--session)) "42")
            (setq buffer (chirp-direct-messages))
            (let* ((view (with-current-buffer buffer (appkit-current-view)))
                   (first
                    (chirp-dm-test--normalized-conversation
                     (chirp-dm-test--normalized-event "20" "20" "first")))
                   (second (copy-tree first))
                   (cursor '(:cursor-id "next" :graph-snapshot-id "snapshot")))
              (setf (plist-get second :id) "conversation-2"
                    (plist-get second :title) "Bob")
              (funcall (nth 0 callbacks)
                       (list first)
                       `(("pagination" . (("nextCursor" . ,cursor)))))
              (with-current-buffer buffer
                (chirp-dm-load-more-inbox))
              (should (equal (plist-get (nth 1 options) :cursor) cursor))
              (funcall (nth 1 callbacks) (list first second) nil)
              (should (equal
                       (mapcar (lambda (item) (plist-get item :id))
                               (plist-get (appkit-view-state view) :items))
                       '("conversation-1" "conversation-2")))
              (should (plist-get
                       (plist-get (appkit-view-state view) :page)
                       :exhausted-p)))))
      (chirp-stop)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

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

(ert-deftest chirp-direct-messages-projects-only-through-appkit-sync ()
  "Inbox completion should update state before its directory projection."
  (let ((chirp--app nil)
        buffer callback owner)
    (unwind-protect
        (save-window-excursion
          (cl-letf (((symbol-function 'chirp-xchat-native-load)
                     (lambda () t))
                    ((symbol-function 'chirp-xchat-native-unlocked-p)
                     (lambda () t))
                    ((symbol-function 'chirp-backend-dm-inbox)
                     (lambda (success &rest options)
                       (setq callback success
                             owner (plist-get options :owner))
                       'inbox-request)))
            (setf (chirp--session-xchat-user-id (chirp--session)) "42")
            (setq buffer (chirp-direct-messages))
            (let* ((view (with-current-buffer buffer (appkit-current-view)))
                   (conversation
                    (chirp-dm-test--normalized-conversation
                     (chirp-dm-test--normalized-event
                      "20" "20" "projected later"))))
              (should (eq owner view))
              (should (eq (plist-get (appkit-view-state view) :type)
                          'dm-inbox))
              (funcall callback (list conversation) nil)
              (should (equal (plist-get
                              (car (plist-get (appkit-view-state view) :items))
                              :id)
                             "conversation-1"))
              (with-current-buffer buffer
                (should-not (string-match-p "projected later" (buffer-string))))
              (appkit-sync-invalidations view)
              (with-current-buffer buffer
                (should (string-match-p "projected later" (buffer-string)))
                (goto-char (point-min))
                (forward-line 1)
                (should (equal (appkit-directory-key-at-point)
                               '(dm-conversation "conversation-1")))))))
      (chirp-stop)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest chirp-dm-decryption-updates-canonical-events-before-sync ()
  "Verified native plaintext should update state and then project through sync."
  (let ((chirp--app nil)
        buffer owner)
    (unwind-protect
        (save-window-excursion
          (let* ((event
                  (chirp-dm-test--normalized-event
                   "20" "20" "[Encrypted message unavailable]"))
                 (_encrypted
                  (setf (plist-get event :encoded-event) "encoded-event"
                        (plist-get event :encrypted-p) t))
                 (conversation (chirp-dm-test--normalized-conversation event)))
            (setf (plist-get conversation :has-more) nil
                  (plist-get conversation :older-cursor) nil)
            (cl-letf (((symbol-function 'chirp-backend-dm-signing-keys)
                       (lambda (_user-ids callback &rest options)
                         (setq owner (plist-get options :owner))
                         (funcall callback [] nil)
                         nil))
                      ((symbol-function 'chirp-xchat-native-decrypt-events)
                       (lambda (_events _signing-keys)
                         '((messages .
                            (((sequence_id . "20")
                              (id . "message-20")
                              (conversation_id . "conversation-1")
                              (text . "verified plaintext")
                              (verified . t))))))))
              (setq buffer (chirp-dm--open-conversation conversation))
              (let* ((view (with-current-buffer buffer (appkit-current-view)))
                     (state (appkit-view-state view)))
                (should (eq owner view))
                (should (equal (plist-get (car (plist-get state :events)) :text)
                               "verified plaintext"))
                (should-not
                 (plist-get (car (plist-get state :events)) :encrypted-p))
                (appkit-sync-invalidations view)
                (with-current-buffer buffer
                  (should (string-match-p "verified plaintext" (buffer-string))))))))
      (chirp-stop)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest chirp-dm-decryption-loads-conversation-key-history ()
  "Decryption should load one key-bearing history page before native work."
  (let ((chirp--app nil)
        buffer calls owners)
    (unwind-protect
        (save-window-excursion
          (let* ((event
                  (chirp-dm-test--normalized-event
                   "20" "20" "[Encrypted message unavailable]"))
                 (_encrypted
                  (setf (plist-get event :encoded-event) "encoded-message"
                        (plist-get event :encrypted-p) t
                        (plist-get event :conversation-key-version) "1700000000000"))
                 (key-event
                  (chirp-dm-test--normalized-event "10" "10" nil))
                 (_key
                  (setf (plist-get key-event :kind) 'conversation-key-change
                        (plist-get key-event :encoded-event) "encoded-key"))
                 (conversation (chirp-dm-test--normalized-conversation event)))
            (cl-letf (((symbol-function 'chirp-backend-dm-history)
                       (lambda (_conversation-id _cursor callback &rest options)
                         (push 'history calls)
                         (push (plist-get options :owner) owners)
                         (funcall callback
                                  (list key-event)
                                  '(("pagination" . (("complete" . t)))))
                         nil))
                      ((symbol-function 'chirp-backend-dm-signing-keys)
                       (lambda (_user-ids callback &rest options)
                         (push 'signing calls)
                         (push (plist-get options :owner) owners)
                         (funcall callback [] nil)
                         nil))
                      ((symbol-function 'chirp-xchat-native-decrypt-events)
                       (lambda (events _signing-keys)
                         (push 'decrypt calls)
                         (should (equal events
                                        '("encoded-key" "encoded-message")))
                         '((messages .
                            (((sequence_id . "20")
                              (id . "message-20")
                              (conversation_id . "conversation-1")
                              (text . "verified plaintext")
                              (verified . t))))))))
              (setq buffer (chirp-dm--open-conversation conversation))
              (let* ((view (with-current-buffer buffer (appkit-current-view)))
                     (state (appkit-view-state view)))
                (should (equal (nreverse calls) '(history signing decrypt)))
                (should (cl-every (lambda (owner) (eq owner view)) owners))
                (should
                 (equal (plist-get (car (last (plist-get state :events))) :text)
                        "verified plaintext"))))))
      (chirp-stop)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest chirp-dm-verified-attachments-and-replies-are-not-encrypted ()
  "Verified non-text facts should replace encrypted placeholders."
  (let* ((image
          (chirp-dm-test--normalized-event
           "20" "20" "[Encrypted message unavailable]"))
         (reply
          (chirp-dm-test--normalized-event
           "21" "21" "[Encrypted message unavailable]"))
         (_encrypted
          (dolist (event (list image reply))
            (setf (plist-get event :encrypted-p) t)))
         (state
          (list :conversation-id "conversation-1"
                :events (list image reply)))
         (messages
          '(((sequence_id . "20")
             (conversation_id . "conversation-1")
             (content_kind . "text")
             (text . "")
             (attachments . (((kind . "image")
                              (url . "https://pbs.twimg.com/media/example.jpg")
                              (name . "example.jpg"))))
             (reply . :json-false)
             (reply_attachment_count . 0)
             (verified . t))
            ((sequence_id . "21")
             (conversation_id . "conversation-1")
             (content_kind . "text")
             (text . "")
             (attachments)
             (reply . t)
             (reply_text . "earlier message")
             (reply_attachment_count . 0)
             (verified . t)))))
    (should (= (chirp-dm--apply-plaintext state messages) 2))
    (pcase-let ((`(,image-event ,reply-event) (plist-get state :events)))
      (should-not (plist-get image-event :encrypted-p))
      (should-not (plist-get reply-event :encrypted-p))
      (let ((attachment (car (plist-get image-event :attachments))))
        (should (eq (plist-get attachment :kind) 'image))
        (should (equal (plist-get attachment :url)
                       "https://pbs.twimg.com/media/example.jpg"))
        (should (equal (plist-get attachment :resource-key)
                       '(xchat-media "conversation-1" "20" 0))))
      (let* ((rows
              (chirp-dm--project-conversation-events
               '(:participants nil) (list image-event reply-event)))
             (reply-model
              (plist-get
               (appkit-chat-timeline-row-context (cadr rows)) :reply)))
        (should (equal (appkit-chat-timeline-row-dependencies (car rows))
                       '((xchat-media "conversation-1" "20" 0))))
        (should (equal reply-model
                       '(:text "earlier message" :attachment-count 0)))
        (with-temp-buffer
          (chirp-dm--insert-reply-preview reply-model)
          (should (equal (buffer-string) "↪ earlier message"))))
      (should-not
       (chirp-dm--trusted-media-url-p "https://example.com/private.jpg"))
      (should-not
       (chirp-dm--trusted-media-url-p
        "https://pbs.twimg.com:444/media/private.jpg")))))

(ert-deftest chirp-dm-image-resources-use-appkit-and-row-dependencies ()
  "Verified images should use Appkit acquisition and resource invalidation."
  (let ((chirp--app nil)
        buffer resource success printed)
    (unwind-protect
        (save-window-excursion
          (let* ((event (chirp-dm-test--normalized-event "20" "20" "photo"))
                 (other (chirp-dm-test--normalized-event "21" "21" "text"))
                 (resource-key '(xchat-media "conversation-1" "20" 0))
                 (_attachment
                  (setf (plist-get event :attachments)
                        (list (list :kind 'image
                                    :url "https://pbs.twimg.com/media/example.jpg"
                                    :resource-key resource-key))
                        (plist-get event :attachment-count) 1))
                 (conversation
                  (chirp-dm-test--normalized-conversation event other))
                 (printer (symbol-function 'chirp-dm--print-event-row)))
            (cl-letf (((symbol-function 'chirp-media--prefetch-enabled-p)
                       (lambda () t))
                      ((symbol-function 'appkit-media-image-cache-existing-file)
                       (lambda (_cache-base) nil))
                      ((symbol-function 'chirp-media--valid-cache-file-p)
                       (lambda (path)
                         (equal path "/tmp/chirp-dm-image.jpg")))
                      ((symbol-function 'appkit-media-cache-image-resource-async)
                       (lambda (requested-resource _cache-base callback
                                _errback &rest _options)
                         (setq resource requested-resource
                               success callback)
                         nil))
                      ((symbol-function 'chirp-dm--print-event-row)
                       (lambda (row)
                         (push (plist-get
                                (appkit-chat-timeline-row-payload row) :id)
                               printed)
                         (funcall printer row))))
              (setq buffer (chirp-dm--open-conversation conversation))
              (let* ((view (with-current-buffer buffer (appkit-current-view)))
                     (store
                      (appkit-app-resource-store (appkit-view-app view))))
                (should (equal (alist-get 'url resource)
                               "https://pbs.twimg.com/media/example.jpg"))
                (should (eq (plist-get (gethash resource-key store) :status)
                            'pending))
                (setq printed nil)
                (funcall success "/tmp/chirp-dm-image.jpg")
                (appkit-sync-invalidations view)
                (should (eq (plist-get (gethash resource-key store) :status)
                            'ready))
                (should (equal printed '("20")))))))
      (chirp-stop)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest chirp-dm-conversations-are-fresh-chat-views-with-composers ()
  "Opening one conversation twice should create distinct editable-tail views."
  (let ((chirp--app nil)
        buffers)
    (unwind-protect
        (save-window-excursion
          (let* ((conversation
                  (chirp-dm-test--normalized-conversation
                   (chirp-dm-test--normalized-event
                    "20" "20" "hello")))
                 (first (chirp-dm--open-conversation conversation))
                 (second (chirp-dm--open-conversation conversation))
                 (first-view (with-current-buffer first (appkit-current-view)))
                 (second-view (with-current-buffer second (appkit-current-view))))
            (setq buffers (list first second))
            (should-not (eq first second))
            (should-not (equal (appkit-view-id first-view)
                               (appkit-view-id second-view)))
            (with-current-buffer first
              (should (eq major-mode 'chirp-dm--conversation-mode))
              (should-not (derived-mode-p 'special-mode))
              (should-not buffer-read-only)
              (should appkit-chatbuf-owns-wrap-prefix-p)
              (should visual-line-mode)
              (should word-wrap)
              (should-not truncate-lines)
              (should (appkit-chatbuf-prompt-start-position))
              (should (appkit-chatbuf-input-start-position))
              (should-not
               (text-property-not-all
                (point-min) (appkit-chatbuf-prompt-start-position)
                'read-only t))
              (goto-char (point-min))
              (appkit-chatbuf-update-context-mode)
              (should chirp-dm--timeline-mode)
              (should (eq (key-binding (kbd "q"))
                          #'chirp-quit-current-buffer))
              (search-forward "hello")
              (should (get-text-property (match-beginning 0) 'read-only))
              (goto-char (point-max))
              (appkit-chatbuf-update-context-mode)
              (should-not chirp-dm--timeline-mode)
              (should (eq (key-binding (kbd "q")) #'self-insert-command))
              (insert "draft")
              (should (equal (appkit-chatbuf-input-string) "draft"))
              (should (appkit-chat-history-window-known-p))
              (should-not (appkit-chat-history-window-partial-p))
              (should (equal (appkit-chat-history-window-first-key)
                             "20"))
              (should (string-match-p "hello" (buffer-string))))))
      (chirp-stop)
      (dolist (buffer buffers)
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest chirp-dm-inbox-activation-bridges-focused-latest-data ()
  "Opening from inbox should bridge latest data into the same timeline."
  (let ((chirp--app nil)
        (request (generate-new-buffer " *chirp-dm-open-refresh*"))
        buffer refresh bridge owner)
    (unwind-protect
        (save-window-excursion
          (cl-letf (((symbol-function 'chirp-backend-dm-conversation-data)
                     (lambda (_conversation-id callback &rest options)
                       (setq refresh callback
                             owner (plist-get options :owner))
                       request))
                    ((symbol-function 'chirp-backend-dm-history)
                     (lambda (_conversation-id _cursor callback &rest options)
                       (setq bridge callback
                             owner (plist-get options :owner))
                       'bridge-request)))
            (let* ((old-event
                    (chirp-dm-test--normalized-event "20" "20" "old"))
                   (new-event
                    (chirp-dm-test--normalized-event "30" "30" "new"))
                   (conversation
                    (chirp-dm-test--normalized-conversation old-event))
                   (entry
                    (appkit-directory-entry-create
                     :key '(dm-conversation "conversation-1")
                     :role 'item
                     :payload conversation)))
              (setq buffer (chirp-dm--activate-inbox-item nil entry))
              (let* ((view (with-current-buffer buffer (appkit-current-view)))
                     (state (appkit-view-state view)))
                (should (eq owner view))
                (should refresh)
                (funcall refresh
                         (chirp-dm-test--normalized-conversation new-event)
                         nil)
                (should bridge)
                (should (equal
                         (mapcar (lambda (event) (plist-get event :id))
                                 (plist-get state :events))
                         '("20")))
                (funcall bridge (list old-event new-event)
                         '(("pagination" . (("complete" . t)))))
                (with-current-buffer buffer
                  (appkit-sync-invalidations view)
                  (should (appkit-chat-timeline-node "20"))
                  (should (appkit-chat-timeline-node "30")))
                (should (equal
                         (mapcar (lambda (event) (plist-get event :id))
                                 (plist-get state :events))
                         '("20" "30")))))))
      (chirp-stop)
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (when (buffer-live-p request)
        (kill-buffer request)))))

(ert-deftest chirp-dm-evil-enters-insert-state-in-the-composer ()
  "Installed Evil integration should leave the Appkit composer editable."
  (skip-unless (require 'evil nil t))
  (let ((chirp--app nil)
        (evil-was-enabled (bound-and-true-p evil-mode))
        buffer)
    (unwind-protect
        (save-window-excursion
          (unless evil-was-enabled
            (evil-mode 1))
          (let ((conversation
                 (chirp-dm-test--normalized-conversation
                  (chirp-dm-test--normalized-event "20" "20" "hello"))))
            (setq buffer (chirp-dm--open-conversation conversation))
            (with-current-buffer buffer
              (goto-char (point-max))
              (appkit-chatbuf-update-context-mode)
              (should (eq evil-state 'normal))
              (should (eq (key-binding (kbd "i")) #'evil-insert))
              (evil-insert-state)
              (should (eq evil-state 'insert))
              (should (eq (key-binding (kbd "q")) #'self-insert-command))
              (let ((last-command-event ?q))
                (call-interactively #'self-insert-command))
              (should (equal (appkit-chatbuf-input-string) "q"))
              (evil-normal-state)
              (goto-char (point-min))
              (appkit-chatbuf-update-context-mode)
              (should (eq (key-binding (kbd "q"))
                          #'chirp-quit-current-buffer)))))
      (chirp-stop)
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (unless evil-was-enabled
        (evil-mode -1)))))

(ert-deftest chirp-dm-send-clears-only-after-ack-and-canonical-refresh ()
  "Acknowledged sends should merge focused deltas without optimistic rows."
  (let ((chirp--app nil)
        buffers send-success refresh-success bridge-success send-owner sent-text)
    (unwind-protect
        (save-window-excursion
          (let ((send-request (generate-new-buffer " *chirp-dm-send*"))
                (refresh-request (generate-new-buffer " *chirp-dm-refresh*")))
            (setq buffers (list send-request refresh-request))
            (cl-letf (((symbol-function 'chirp-backend-dm-send-text)
                       (lambda (_conversation-id text callback &rest options)
                         (setq sent-text text
                               send-success callback
                               send-owner (plist-get options :owner))
                         send-request))
                      ((symbol-function 'chirp-backend-dm-conversation-data)
                       (lambda (_conversation-id callback &rest _options)
                         (setq refresh-success callback)
                         refresh-request))
                      ((symbol-function 'chirp-backend-dm-history)
                       (lambda (_conversation-id _cursor callback &rest _options)
                         (setq bridge-success callback)
                         'bridge-request)))
              (let* ((first-event
                      (chirp-dm-test--normalized-event "20" "20" "old"))
                     (conversation
                      (chirp-dm-test--normalized-conversation first-event))
                     (buffer (chirp-dm--open-conversation conversation))
                     (view (with-current-buffer buffer (appkit-current-view)))
                     (state (appkit-view-state view)))
                (push buffer buffers)
                (with-current-buffer buffer
                  (goto-char (point-max))
                  (insert "hello")
                  (chirp-dm-submit)
                  (should-error (chirp-dm-submit) :type 'user-error))
                (should (eq send-owner view))
                (should (equal sent-text "hello"))
                (should (= (length (plist-get state :events)) 1))
                (funcall send-success '(:message-id "message-21") nil)
                (should refresh-success)
                (with-current-buffer buffer
                  (appkit-sync-invalidations view)
                  (should (equal (appkit-chatbuf-input-string) ""))
                  (should (equal (appkit-chatbuf-input-history-elements)
                                 '("hello"))))
                (let* ((sent-event
                        (chirp-dm-test--normalized-event "21" "21" "hello"))
                       (refreshed
                        (chirp-dm-test--normalized-conversation sent-event)))
                  (funcall refresh-success refreshed nil)
                  (should bridge-success)
                  (should (= (length (plist-get state :events)) 1))
                  (funcall bridge-success (list first-event sent-event)
                           '(("pagination" . (("complete" . t)))))
                  (with-current-buffer buffer
                    (appkit-sync-invalidations view)
                    (should (appkit-chat-timeline-node "20"))
                    (should (appkit-chat-timeline-node "21")))
                  (should (= (length (plist-get state :events)) 2))
                  (should (= (cl-count "21" (plist-get state :events)
                                       :key (lambda (event)
                                              (plist-get event :id))
                                       :test #'equal)
                             1)))))))
      (chirp-stop)
      (dolist (buffer buffers)
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest chirp-dm-send-error-preserves-the-draft ()
  "A synchronous ambiguous send error should preserve and re-enable input."
  (let ((chirp--app nil)
        buffer)
    (unwind-protect
        (save-window-excursion
          (cl-letf (((symbol-function 'chirp-backend-dm-send-text)
                     (lambda (_conversation-id _text _callback &rest options)
                       (funcall
                        (plist-get options :errback)
                        (concat
                         "X write outcome is unknown; the request may have "
                         "succeeded. Check X before trying again."))
                       nil)))
            (let* ((conversation
                    (chirp-dm-test--normalized-conversation
                     (chirp-dm-test--normalized-event "20" "20" "old")))
                   (_buffer (setq buffer
                                  (chirp-dm--open-conversation conversation)))
                   (view (with-current-buffer buffer (appkit-current-view)))
                   (state (appkit-view-state view)))
              (with-current-buffer buffer
                (goto-char (point-max))
                (insert "keep this draft")
                (chirp-dm-submit)
                (appkit-sync-invalidations view)
                (should-not buffer-read-only)
                (should (equal (appkit-chatbuf-input-string)
                               "keep this draft"))
                (should (string-match-p "Unable to send message"
                                        (buffer-string))))
              (should-not (plist-get state :send-generation))
              (should (= (length (plist-get state :events)) 1)))))
      (chirp-stop)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest chirp-dm-send-synchronous-error-restores-the-composer ()
  "A native preparation error should release send ownership and retain input."
  (let ((chirp--app nil)
        buffer)
    (unwind-protect
        (save-window-excursion
          (cl-letf (((symbol-function 'chirp-backend-dm-send-text)
                     (lambda (&rest _args)
                       (error "No verified conversation key"))))
            (let* ((conversation
                    (chirp-dm-test--normalized-conversation
                     (chirp-dm-test--normalized-event "20" "20" "old")))
                   (_buffer (setq buffer
                                  (chirp-dm--open-conversation conversation)))
                   (view (with-current-buffer buffer (appkit-current-view)))
                   (state (appkit-view-state view)))
              (with-current-buffer buffer
                (goto-char (point-max))
                (insert "retain me")
                (should-error (chirp-dm-submit) :type 'error)
                (appkit-sync-invalidations view)
                (should-not buffer-read-only)
                (should (equal (appkit-chatbuf-input-string) "retain me")))
              (should-not (plist-get state :send-generation)))))
      (chirp-stop)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest chirp-dm-send-callback-is-inert-after-view-kill ()
  "A late send acknowledgement should not revive a killed conversation view."
  (let ((chirp--app nil)
        (request (generate-new-buffer " *chirp-dm-stale-send*"))
        success refreshed-p)
    (unwind-protect
        (save-window-excursion
          (cl-letf (((symbol-function 'chirp-backend-dm-send-text)
                     (lambda (_conversation-id _text callback &rest _options)
                       (setq success callback)
                       request))
                    ((symbol-function 'chirp-backend-dm-conversation-data)
                     (lambda (&rest _args)
                       (setq refreshed-p t))))
            (let* ((conversation
                    (chirp-dm-test--normalized-conversation
                     (chirp-dm-test--normalized-event "20" "20" "old")))
                   (buffer (chirp-dm--open-conversation conversation))
                   (view (with-current-buffer buffer (appkit-current-view))))
              (with-current-buffer buffer
                (goto-char (point-max))
                (insert "late")
                (chirp-dm-submit))
              (appkit-kill-view view t)
              (funcall success '(:message-id "late") nil)
              (should-not refreshed-p))))
      (chirp-stop)
      (when (buffer-live-p request)
        (kill-buffer request)))))

(ert-deftest chirp-dm-unknown-senders-are-not-misattributed-to-the-viewer ()
  "Missing participant metadata should render an unknown sender, not `You'."
  (let ((chirp--app nil)
        buffer)
    (unwind-protect
        (save-window-excursion
          (let ((conversation
                 (chirp-dm-test--normalized-conversation
                  (chirp-dm-test--normalized-event
                   "20" "20" "hello" "99"))))
            (setf (plist-get conversation :participants) nil)
            (setq buffer (chirp-dm--open-conversation conversation))
            (with-current-buffer buffer
              (should (string-match-p "Unknown sender" (buffer-string)))
              (should-not (string-match-p "\\`You" (buffer-string))))))
      (chirp-stop)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest chirp-dm-older-history-prepends-with-node-and-point-identity ()
  "Older pages should prepend events while preserving retained message nodes."
  (let ((chirp--app nil)
        buffer callback request-owner)
    (unwind-protect
        (save-window-excursion
          (cl-letf (((symbol-function 'chirp-backend-dm-history)
                     (lambda (_conversation-id _cursor success &rest options)
                       (setq callback success
                             request-owner (plist-get options :owner))
                       'history-request)))
            (let* ((current
                    (chirp-dm-test--normalized-event
                     "20" "20" "current"))
                   (conversation
                    (chirp-dm-test--normalized-conversation current)))
              (setq buffer (chirp-dm--open-conversation conversation))
              (let ((view (with-current-buffer buffer (appkit-current-view))))
                (with-current-buffer buffer
                  (let ((node (appkit-chat-timeline-node "20")))
                    (goto-char
                     (appkit-chat-timeline-key-position "20"))
                    (chirp-dm-load-older-messages)
                    (should (eq request-owner view))
                    (funcall
                     callback
                     (list
                      (chirp-dm-test--normalized-event
                       "10" "10" "older")
                      current)
                     '(("pagination" . (("complete" . t)))
                       ("encodedKeyEvents" . ("recovery-key-event"))))
                    (should (equal
                             (mapcar
                              (lambda (event) (plist-get event :id))
                              (plist-get (appkit-view-state view) :events))
                             '("10" "20")))
                    (should (equal
                             (plist-get (appkit-view-state view)
                                        :recovery-key-events)
                             '((:id "recovery-key-event"
                                :encoded-event "recovery-key-event"))))
                    (should-not (string-match-p "older" (buffer-string)))
                    (appkit-sync-invalidations view)
                    (should (eq node
                                (appkit-chat-timeline-node "20")))
                    (should (equal (appkit-chat-timeline-key-at-point)
                                   "20"))
                    (should (string-match-p "older" (buffer-string)))
                    (should (appkit-chat-history-older-loaded-p))))))))
      (chirp-stop)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest chirp-dm-disjoint-refresh-bridges-the-canonical-timeline ()
  "A focused fragment must prove continuity before joining loaded messages."
  (let ((chirp--app nil)
        buffer refresh-callback bridge-callback bridge-cursors)
    (unwind-protect
        (save-window-excursion
          (cl-letf (((symbol-function 'chirp-backend-dm-conversation-data)
                     (lambda (_id callback &rest _options)
                       (setq refresh-callback callback)
                       'refresh-request))
                    ((symbol-function 'chirp-backend-dm-history)
                     (lambda (_id cursor callback &rest _options)
                       (setq bridge-callback callback
                             bridge-cursors
                             (append bridge-cursors (list cursor)))
                       'bridge-request)))
            (let* ((oldest
                    (chirp-dm-test--normalized-event
                     "10" "10" "oldest"))
                   (current
                    (chirp-dm-test--normalized-event
                     "20" "20" "current"))
                   (intermediate
                    (chirp-dm-test--normalized-event
                     "25" "25" "intermediate"))
                   (fresh
                    (chirp-dm-test--normalized-event
                     "30" "30" "fresh"))
                   (conversation
                    (chirp-dm-test--normalized-conversation current)))
              (setq buffer (chirp-dm--open-conversation conversation))
              (with-current-buffer buffer
                (chirp-dm-refresh-conversation))
              (funcall
               refresh-callback
               (list :id "conversation-1" :type 'direct :title "Alice"
                     :participants '((:id "42" :name "Alice"))
                     :events (list fresh)
                     :has-more t
                     :older-cursor '(:sequence-id "30" :key-version "0"))
               nil)
              (let* ((view (with-current-buffer buffer (appkit-current-view)))
                     (state (appkit-view-state view)))
                (should (equal bridge-cursors
                               '((:sequence-id "30" :key-version "0"))))
                (should (equal
                         (mapcar (lambda (event) (plist-get event :id))
                                 (plist-get state :events))
                         '("20")))
                (with-current-buffer buffer
                  (should (eq (appkit-chat-history-loading) 'refresh))
                  (should (equal (appkit-chat-history-window-first-key) "20")))
                (funcall
                 bridge-callback (list intermediate fresh)
                 '(("pagination"
                    . (("nextCursor"
                        . (:sequence-id "25" :key-version "0"))))))
                (should (equal bridge-cursors
                               '((:sequence-id "30" :key-version "0")
                                 (:sequence-id "25" :key-version "0"))))
                (should (equal
                         (mapcar (lambda (event) (plist-get event :id))
                                 (plist-get state :events))
                         '("20")))
                (funcall bridge-callback (list oldest current intermediate)
                         '(("pagination" . (("complete" . t)))))
                (should (equal
                         (mapcar (lambda (event) (plist-get event :id))
                                 (plist-get state :events))
                         '("10" "20" "25" "30")))
                (with-current-buffer buffer
                  (should-not (appkit-chat-history-loading-p))
                  (should (equal (appkit-chat-history-window-first-key) "10"))
                  (should (appkit-chat-history-older-loaded-p))
                  (appkit-sync-invalidations view)
                  (should (appkit-chat-timeline-node "10"))
                  (should (appkit-chat-timeline-node "20"))
                  (should (appkit-chat-timeline-node "25"))
                  (should (appkit-chat-timeline-node "30")))))))
      (chirp-stop)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest chirp-dm-refresh-bridge-page-limit-preserves-exact-window ()
  "A cyclic bridge must stop without joining a disjoint focused fragment."
  (let ((chirp--app nil)
        (chirp-dm--refresh-bridge-page-limit 2)
        buffer refresh-callback bridge-callback bridge-cursors)
    (unwind-protect
        (save-window-excursion
          (cl-letf (((symbol-function 'chirp-backend-dm-conversation-data)
                     (lambda (_id callback &rest _options)
                       (setq refresh-callback callback)
                       'refresh-request))
                    ((symbol-function 'chirp-backend-dm-history)
                     (lambda (_id cursor callback &rest _options)
                       (setq bridge-callback callback
                             bridge-cursors
                             (append bridge-cursors (list cursor)))
                       'bridge-request)))
            (let* ((current
                    (chirp-dm-test--normalized-event
                     "20" "20" "current"))
                   (fresh
                    (chirp-dm-test--normalized-event
                     "30" "30" "fresh"))
                   (conversation
                    (chirp-dm-test--normalized-conversation current)))
              (setq buffer (chirp-dm--open-conversation conversation))
              (with-current-buffer buffer
                (chirp-dm-refresh-conversation))
              (funcall refresh-callback
                       (chirp-dm-test--normalized-conversation fresh)
                       nil)
              (funcall
               bridge-callback
               (list (chirp-dm-test--normalized-event "29" "29" "gap"))
               '(("pagination"
                  . (("nextCursor"
                      . (:sequence-id "25" :key-version "0"))))))
              (funcall
               bridge-callback
               (list (chirp-dm-test--normalized-event "28" "28" "gap"))
               '(("pagination"
                  . (("nextCursor"
                      . (:sequence-id "30" :key-version "0"))))))
              (let* ((view (with-current-buffer buffer (appkit-current-view)))
                     (state (appkit-view-state view)))
                (should (equal bridge-cursors
                               '((:sequence-id "30" :key-version "0")
                                 (:sequence-id "25" :key-version "0"))))
                (should (equal
                         (mapcar (lambda (event) (plist-get event :id))
                                 (plist-get state :events))
                         '("20")))
                (should (eq (plist-get (plist-get state :status) :phase)
                            'error))
                (with-current-buffer buffer
                  (should-not (appkit-chat-history-loading-p))
                  (should (equal (appkit-chat-history-window-first-key)
                                 "20")))))))
      (chirp-stop)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest chirp-dm-superseded-history-callback-is-inert ()
  "A superseded older callback should not overwrite refreshed conversation state."
  (let ((chirp--app nil)
        buffer older-callback refresh-callback canceled)
    (unwind-protect
        (save-window-excursion
          (cl-letf (((symbol-function 'chirp-backend-dm-history)
                     (lambda (_id _cursor callback &rest _options)
                       (setq older-callback callback)
                       'older-request))
                    ((symbol-function 'chirp-backend-dm-conversation-data)
                     (lambda (_id callback &rest _options)
                       (setq refresh-callback callback)
                       'refresh-request))
                    ((symbol-function 'chirp-x-cancel-request)
                     (lambda (request)
                       (push request canceled))))
            (let* ((current
                    (chirp-dm-test--normalized-event
                     "20" "20" "current"))
                   (conversation
                    (chirp-dm-test--normalized-conversation current)))
              (setq buffer (chirp-dm--open-conversation conversation))
              (with-current-buffer buffer
                (chirp-dm-load-older-messages)
                (chirp-dm-refresh-conversation))
              (should (equal canceled '(older-request)))
              (funcall older-callback
                       (list (chirp-dm-test--normalized-event
                              "10" "10" "stale"))
                       nil)
              (let ((view (with-current-buffer buffer (appkit-current-view))))
                (should (equal (mapcar
                                (lambda (event) (plist-get event :id))
                                (plist-get (appkit-view-state view) :events))
                               '("20")))
                (funcall
                 refresh-callback
                 (list :id "conversation-1" :type 'direct
                       :title "Alice Renamed"
                       :participants '((:id "42" :name "Alice New"))
                       :events
                       (list current
                             (chirp-dm-test--normalized-event
                              "30" "30" "fresh"))
                       :has-more t
                       :older-cursor '(:sequence-id "20" :key-version "0"))
                 nil)
                (should (equal (mapcar
                                (lambda (event) (plist-get event :id))
                                (plist-get (appkit-view-state view) :events))
                               '("20" "30")))
                (appkit-sync-invalidations view)
                (with-current-buffer buffer
                  (should (equal chirp--view-title "DM: Alice Renamed"))
                  (goto-char (appkit-chat-timeline-key-position "20"))
                  (should (looking-at "Alice New")))))))
      (chirp-stop)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(provide 'chirp-dm-test)

;;; chirp-dm-test.el ends here
