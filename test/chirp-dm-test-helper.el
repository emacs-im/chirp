;;; chirp-dm-test-helper.el --- Synthetic XChat fixtures -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Shared bounded wire and normalized-event fixtures for direct-message tests.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'appkit-markup-codec)
(require 'appkit-markup-codecs)

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
           (if sequence
               (chirp-dm-test--field 11 1
                                     (chirp-dm-test--text sequence))
             "")
           (if message-id
               (chirp-dm-test--field 11 2
                                     (chirp-dm-test--text message-id))
             "")
           (if sender-id
               (chirp-dm-test--field 11 3
                                     (chirp-dm-test--text sender-id))
             "")
           (if conversation-id
               (chirp-dm-test--field 11 4
                                     (chirp-dm-test--text conversation-id))
             "")
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
        :text text
        :document
        (and (stringp text)
             (appkit-markup-parse-result-document
              (appkit-markup-parse 'plain text)))))

(defun chirp-dm-test--normalized-conversation (&rest events)
  "Return one normalized direct conversation carrying EVENTS."
  (list :id "conversation-1"
        :type 'direct
        :title "Alice"
        :participants '((:id "42" :name "Alice" :handle "alice"))
        :events events
        :latest-event (car (last events))
        :preview (and events (plist-get (car (last events)) :text))
        :updated-at-msec "1700000000000"
        :has-more t
        :older-cursor
        (and events
             (list :sequence-id (plist-get (car events) :sequence-id)
                   :key-version "0"))))

(provide 'chirp-dm-test-helper)

;;; chirp-dm-test-helper.el ends here
