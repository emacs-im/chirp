;;; chirp-xchat.el --- Modern XChat wire protocol for Chirp -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Strict, bounded shaping and normalization for modern XChat GraphQL data.
;; This module is transport-free and sends no requests or read acknowledgments.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'url-parse)
(require 'chirp-core)

;;; Constants

(defconst chirp-xchat-max-inbox-items 100
  "Maximum number of conversations accepted in one XChat inbox page.")

(defconst chirp-xchat-max-history-events 200
  "Maximum number of events accepted in one XChat history page.")

(defconst chirp-xchat--max-document-bytes (* 1024 1024)
  "Maximum decoded size accepted for one XChat Thrift document.")

(defconst chirp-xchat--max-container-items 10000
  "Maximum number of values accepted in one XChat Thrift container.")

(defconst chirp-xchat--max-inbox-events 1000
  "Maximum number of preview events accepted in one XChat inbox page.")

(defconst chirp-xchat--max-page-encoded-bytes (* 16 1024 1024)
  "Maximum aggregate Base64 size accepted in one XChat response page.")

(defconst chirp-xchat--max-public-keys 32
  "Maximum registered public keys accepted for one XChat user.")

(defconst chirp-xchat--max-juicebox-realms 16
  "Maximum Juicebox realms accepted in one XChat configuration.")

(defconst chirp-xchat--max-recovery-config-bytes (* 1024 1024)
  "Maximum serialized native recovery input size.")

(defconst chirp-xchat--max-send-event-bytes (* 2 1024 1024)
  "Maximum Base64 size accepted for one prepared outbound XChat event.")

(defconst chirp-xchat--detail-kinds
  '((1 . message-create)
    (3 . conversation-key-change)
    (4 . group-change)
    (5 . message-failure)
    (6 . typing)
    (7 . message-delete)
    (8 . conversation-delete)
    (9 . conversation-metadata-change)
    (10 . grok-search-response)
    (12 . mark-read)
    (13 . mark-unread)
    (14 . member-account-delete)
    (15 . grok-message)
    (16 . grok-response)
    (17 . pin-message)
    (18 . unpin-message))
  "Map XChat MessageEventDetail field IDs to normalized kinds.")

(defconst chirp-xchat--entry-kinds
  '((1 . message)
    (2 . reaction-add)
    (3 . reaction-remove)
    (4 . message-edit)
    (5 . mark-read)
    (6 . mark-unread)
    (7 . pin-conversation)
    (8 . unpin-conversation)
    (9 . screen-capture)
    (10 . call-ended)
    (11 . call-missed)
    (12 . draft-message)
    (13 . accept-message-request)
    (14 . nickname-message)
    (15 . set-verified-status)
    (16 . call-started))
  "Map serialized XChat MessageEntryContents field IDs to kinds.")

;;; Thrift Decoding

(cl-defstruct (chirp-xchat--thrift-state
               (:constructor chirp-xchat--thrift-state-create))
  "Cursor over one bounded Thrift binary document."
  bytes
  (position 0))

(defun chirp-xchat--thrift-take (state count)
  "Consume COUNT bytes from Thrift STATE."
  (let* ((bytes (chirp-xchat--thrift-state-bytes state))
         (start (chirp-xchat--thrift-state-position state))
         (end (+ start count)))
    (unless (and (integerp count) (>= count 0) (<= end (length bytes)))
      (error "XChat returned a truncated Thrift document"))
    (setf (chirp-xchat--thrift-state-position state) end)
    (substring bytes start end)))

(defun chirp-xchat--thrift-unsigned (state count)
  "Consume an unsigned integer of COUNT bytes from Thrift STATE."
  (let ((value 0))
    (dotimes (_ count value)
      (setq value
            (+ (ash value 8)
               (aref (chirp-xchat--thrift-take state 1) 0))))))

(defun chirp-xchat--thrift-signed (state count)
  "Consume a signed integer of COUNT bytes from Thrift STATE."
  (let* ((unsigned (chirp-xchat--thrift-unsigned state count))
         (bits (* count 8))
         (sign-bit (ash 1 (1- bits))))
    (if (zerop (logand unsigned sign-bit))
        unsigned
      (- unsigned (ash 1 bits)))))

(defun chirp-xchat--thrift-length (state label)
  "Consume a bounded Thrift length from STATE described by LABEL."
  (let ((length (chirp-xchat--thrift-signed state 4)))
    (unless (and (>= length 0)
                 (<= length chirp-xchat--max-document-bytes))
      (error "XChat returned an invalid Thrift %s length" label))
    length))

(defun chirp-xchat--thrift-document (bytes)
  "Decode one bounded Thrift binary document from BYTES.

Each decoded struct is a list of (FIELD-ID TYPE VALUE) entries."
  (unless (and (stringp bytes)
               (<= (length bytes) chirp-xchat--max-document-bytes))
    (error "XChat Thrift document is too large"))
  (let ((state (chirp-xchat--thrift-state-create :bytes bytes)))
    (cl-labels
        ((read-value
          (type depth)
          (when (> depth 32)
            (error "XChat Thrift nesting is too deep"))
          (pcase type
            (2
             (pcase (chirp-xchat--thrift-unsigned state 1)
               (0 nil)
               (1 t)
               (value
                (error "XChat returned invalid Thrift BOOL value %s"
                       value))))
            (3 (chirp-xchat--thrift-signed state 1))
            (4 (chirp-xchat--thrift-take state 8))
            (6 (chirp-xchat--thrift-signed state 2))
            (8 (chirp-xchat--thrift-signed state 4))
            (10 (chirp-xchat--thrift-signed state 8))
            ((or 11 16 17)
             (chirp-xchat--thrift-take
              state (chirp-xchat--thrift-length state "binary")))
            (12 (read-struct (1+ depth)))
            ((or 14 15)
             (let ((element-type
                    (chirp-xchat--thrift-unsigned state 1))
                   (count (chirp-xchat--thrift-length state "container")))
               (unless (<= count chirp-xchat--max-container-items)
                 (error "XChat Thrift container has too many items"))
               (cl-loop repeat count
                        collect (read-value element-type (1+ depth)))))
            (13
             (let ((key-type (chirp-xchat--thrift-unsigned state 1))
                   (value-type (chirp-xchat--thrift-unsigned state 1))
                   (count (chirp-xchat--thrift-length state "map")))
               (unless (<= count chirp-xchat--max-container-items)
                 (error "XChat Thrift map has too many items"))
               (cl-loop repeat count
                        collect (cons (read-value key-type (1+ depth))
                                      (read-value value-type (1+ depth))))))
            (_ (error "XChat returned unsupported Thrift type %s" type))))
         (read-struct
          (depth)
          (let (fields type)
            (while (not (zerop
                         (setq type
                               (chirp-xchat--thrift-unsigned state 1))))
              (when (>= (length fields) 256)
                (error "XChat Thrift struct has too many fields"))
              (let ((field-id (chirp-xchat--thrift-signed state 2)))
                (push (list field-id type (read-value type depth)) fields)))
            (nreverse fields))))
      (let ((document (read-struct 0)))
        (unless (= (chirp-xchat--thrift-state-position state)
                   (length bytes))
          (error "XChat Thrift document has trailing bytes"))
        document))))

(defun chirp-xchat--thrift-field-entry (struct field-id)
  "Return FIELD-ID's entry from decoded Thrift STRUCT."
  (assq field-id struct))

(defun chirp-xchat--thrift-field (struct field-id &optional expected-type)
  "Return FIELD-ID's value from decoded Thrift STRUCT.

When EXPECTED-TYPE is non-nil, reject a field carrying another Thrift type."
  (when-let* ((entry (chirp-xchat--thrift-field-entry struct field-id)))
    (when (and expected-type (/= (nth 1 entry) expected-type))
      (error "XChat Thrift field %s has type %s instead of %s"
             field-id (nth 1 entry) expected-type))
    (nth 2 entry)))

(defun chirp-xchat--text (bytes label)
  "Decode UTF-8 BYTES from XChat, described by LABEL."
  (unless (stringp bytes)
    (error "XChat %s is not binary text" label))
  (let ((text (decode-coding-string bytes 'utf-8)))
    (unless (equal bytes (encode-coding-string text 'utf-8))
      (error "XChat %s is not valid UTF-8" label))
    text))

(defun chirp-xchat--field-text (struct field-id label)
  "Return text FIELD-ID from decoded XChat STRUCT, described by LABEL."
  (when-let* ((bytes (chirp-xchat--thrift-field struct field-id 11)))
    (chirp-xchat--text bytes label)))

;;; Message Events

(defun chirp-xchat--decode-event (encoded)
  "Decode one Base64 ENCODED XChat MessageEvent."
  (unless (and (stringp encoded)
               (<= (length encoded)
                   (* 2 chirp-xchat--max-document-bytes))
               (string-match-p "\\`[A-Za-z0-9+/]*=\\{0,2\\}\\'" encoded))
    (error "XChat returned an invalid encoded message event"))
  (let ((bytes (condition-case nil
                   (base64-decode-string encoded)
                 (error nil))))
    (unless bytes
      (error "XChat returned invalid Base64 message data"))
    (chirp-xchat--thrift-document bytes)))

(defun chirp-xchat--entry-content (contents)
  "Decode MessageCreateEvent CONTENTS into a normalized entry plist."
  (let* ((holder (chirp-xchat--thrift-document contents))
         (entry-union (chirp-xchat--thrift-field holder 1 12)))
    (unless (= (length entry-union) 1)
      (error "XChat message entry must contain exactly one content kind"))
    (let* ((entry (car entry-union))
           (entry-id (car entry))
           (entry-kind (or (cdr (assq entry-id chirp-xchat--entry-kinds))
                           'unknown)))
      (unless (= (nth 1 entry) 12)
        (error "XChat message entry content is not a struct"))
      (if (eq entry-kind 'message)
          (let* ((message (nth 2 entry))
                 (text-bytes (chirp-xchat--thrift-field message 1 11))
                 (attachments (chirp-xchat--thrift-field message 3 15)))
            (list :kind 'message
                  :text (and text-bytes
                             (chirp-xchat--text text-bytes "message text"))
                  :attachment-count (length attachments)))
        (list :kind entry-kind)))))

(defun chirp-xchat--event-detail (detail)
  "Return normalized metadata for decoded XChat event DETAIL."
  (unless (= (length detail) 1)
    (error "XChat message event must contain exactly one detail kind"))
  (let* ((entry (car detail))
         (field-id (car entry))
         (kind (or (cdr (assq field-id chirp-xchat--detail-kinds))
                   'unknown)))
    (unless (= (nth 1 entry) 12)
      (error "XChat message event detail is not a struct"))
    (if (not (eq kind 'message-create))
        (list :kind kind)
      (let* ((create (nth 2 entry))
             (contents (chirp-xchat--thrift-field create 100 11))
             (key-version
              (chirp-xchat--field-text create 101 "conversation key version"))
             (encrypted-p (and key-version
                               (not (string-empty-p key-version))))
             (content
              (cond
               (encrypted-p
                '(:kind message :encrypted-p t
                  :text "[Encrypted message unavailable]"))
               (contents (chirp-xchat--entry-content contents))
               (t '(:kind message
                    :text "[Message content unavailable]")))))
        (append content
                (list :conversation-key-version key-version
                      :message-request-p
                      (and (chirp-xchat--thrift-field create 109 2) t)))))))

(defun chirp-xchat-normalize-event (encoded)
  "Normalize one Base64 ENCODED XChat event into a plist."
  (let* ((event (chirp-xchat--decode-event encoded))
         (sequence-id (chirp-xchat--field-text event 1 "sequence ID"))
         (message-id (chirp-xchat--field-text event 2 "message ID"))
         (sender-id (chirp-xchat--field-text event 3 "sender ID"))
         (conversation-id
          (chirp-xchat--field-text event 4 "conversation ID"))
         (created-at (chirp-xchat--field-text event 6 "event timestamp"))
         (detail (chirp-xchat--thrift-field event 7 12))
         (metadata (chirp-xchat--event-detail detail)))
    (unless (and (stringp sequence-id)
                 (string-match-p "\\`[0-9]+\\'" sequence-id)
                 (stringp message-id)
                 (not (string-empty-p message-id))
                 (stringp sender-id)
                 (not (string-empty-p sender-id))
                 (stringp conversation-id)
                 (not (string-empty-p conversation-id))
                 (stringp created-at)
                 (string-match-p "\\`[0-9]+\\'" created-at))
      (error "XChat message event has incomplete identity"))
    (append (list :id sequence-id
                  :sequence-id sequence-id
                  :message-id message-id
                  :sender-id sender-id
                  :conversation-id conversation-id
                  :created-at-msec created-at
                  :trusted-p
                  (and (chirp-xchat--thrift-field event 11 2) t))
            metadata
            (when (or (plist-get metadata :encrypted-p)
                      (eq (plist-get metadata :kind)
                          'conversation-key-change))
              (list :encoded-event encoded)))))

;;;; Sending

(defun chirp-xchat--send-base64-p (value limit)
  "Return non-nil when VALUE is bounded Base64 no longer than LIMIT."
  (and (stringp value)
       (not (string-empty-p value))
       (<= (length value) limit)
       (zerop (% (length value) 4))
       (string-match-p "\\`[A-Za-z0-9+/]*=\\{0,2\\}\\'" value)))

(defun chirp-xchat--canonical-conversation-id (conversation-id sender-id)
  "Return canonical CONVERSATION-ID for SENDER-ID when it is one-to-one."
  (cond
   ((and (string-match
          "\\`\\([0-9]+\\)[:-]\\([0-9]+\\)\\'" conversation-id)
         (not (equal (match-string 1 conversation-id)
                     (match-string 2 conversation-id))))
    (mapconcat #'identity
               (sort (list (match-string 1 conversation-id)
                           (match-string 2 conversation-id))
                     #'chirp-xchat--numeric-string-less-p)
               ":"))
   ((and (string-match-p "\\`[0-9]+\\'" conversation-id)
         (string-match-p "\\`[0-9]+\\'" sender-id)
         (not (equal conversation-id sender-id)))
    (mapconcat #'identity
               (sort (list conversation-id sender-id)
                     #'chirp-xchat--numeric-string-less-p)
               ":"))
   (t conversation-id)))

(defun chirp-xchat-send-variables (conversation-id prepared)
  "Return mutation variables for CONVERSATION-ID and native PREPARED payload."
  (let ((message-id (plist-get prepared :message-id))
        (event (plist-get prepared :encoded-message-create-event))
        (signature (plist-get prepared :encoded-message-event-signature)))
    (unless (and (stringp conversation-id)
                 (not (string-empty-p conversation-id))
                 (<= (length conversation-id) 256)
                 (not (string-match-p "[[:cntrl:],]" conversation-id)))
      (error "XChat send conversation ID is invalid"))
    (unless (and (stringp message-id)
                 (string-match-p
                  (concat "\\`[0-9A-Fa-f]\\{8\\}-[0-9A-Fa-f]\\{4\\}-"
                          "[0-9A-Fa-f]\\{4\\}-[0-9A-Fa-f]\\{4\\}-"
                          "[0-9A-Fa-f]\\{12\\}\\'")
                  message-id))
      (error "XChat prepared message ID is invalid"))
    (unless (chirp-xchat--send-base64-p
             event chirp-xchat--max-send-event-bytes)
      (error "XChat prepared message event is invalid"))
    (unless (chirp-xchat--send-base64-p signature 16384)
      (error "XChat prepared message signature is invalid"))
    `(("conversation_id" . ,conversation-id)
      ("message_id" . ,message-id)
      ("encoded_message_create_event" . ,event)
      ("encoded_message_event_signature" . ,signature))))

(defun chirp-xchat-send-result
    (payload conversation-id sender-id message-id)
  "Validate send PAYLOAD for CONVERSATION-ID, SENDER-ID, and MESSAGE-ID."
  (let* ((root
          (chirp-get-in payload
                        '("data" "xchat_send_create_message_event")))
         (typename (chirp-get root "__typename"))
         (encoded (chirp-get root "encoded_message_event")))
    (unless (and (equal typename "XChatSendMessageCreateEventResponse")
                 (chirp-xchat--send-base64-p
                  encoded chirp-xchat--max-send-event-bytes))
      (error "XChat send response has no valid acknowledgement"))
    (let ((event (chirp-xchat-normalize-event encoded)))
      (unless (and (eq (plist-get event :kind) 'message)
                   (equal (plist-get event :message-id) message-id)
                   (equal (plist-get event :sender-id) sender-id)
                   (equal
                    (chirp-xchat--canonical-conversation-id
                     (plist-get event :conversation-id) sender-id)
                    (chirp-xchat--canonical-conversation-id
                     conversation-id sender-id)))
        (error "XChat send acknowledgement does not match the message"))
      event)))

;;; Timeline and Inbox

(defun chirp-xchat--numeric-string-less-p (left right)
  "Return non-nil when numeric string LEFT is less than RIGHT."
  (cond
   ((null left) (not (null right)))
   ((null right) nil)
   ((and (string-match-p "\\`[0-9]+\\'" left)
         (string-match-p "\\`[0-9]+\\'" right))
    (or (< (length left) (length right))
        (and (= (length left) (length right))
             (string< left right))))
   (t (string< left right))))

(defun chirp-xchat--sort-events (events)
  "Return XChat EVENTS ordered from oldest to newest."
  (sort (copy-sequence events)
        (lambda (left right)
          (chirp-xchat--numeric-string-less-p
           (plist-get left :sequence-id)
           (plist-get right :sequence-id)))))

(defun chirp-xchat--validate-encoded-events (encoded-events max-events)
  "Validate ENCODED-EVENTS against MAX-EVENTS and the page byte budget."
  (unless (listp encoded-events)
    (error "XChat returned invalid encoded message events"))
  (let ((event-count 0)
        (encoded-bytes 0))
    (dolist (encoded encoded-events)
      (unless (stringp encoded)
        (error "XChat returned a non-string encoded message event"))
      (cl-incf event-count)
      (when (> event-count max-events)
        (error "XChat returned too many encoded message events"))
      (cl-incf encoded-bytes (length encoded))
      (when (> encoded-bytes chirp-xchat--max-page-encoded-bytes)
        (error "XChat encoded message page is too large"))))
  encoded-events)

(defun chirp-xchat--normalize-events (encoded-events &optional max-events)
  "Normalize and order ENCODED-EVENTS, accepting at most MAX-EVENTS."
  (chirp-xchat--validate-encoded-events
   encoded-events (or max-events chirp-xchat-max-history-events))
  (chirp-xchat--sort-events
   (mapcar #'chirp-xchat-normalize-event encoded-events)))

(defun chirp-xchat--normalize-user (wrapper)
  "Normalize one XChat user result WRAPPER."
  (let* ((result (and (chirp-object-p wrapper)
                      (chirp-get wrapper "result")))
         (core (and (chirp-object-p result) (chirp-get result "core")))
         (id (chirp-first-nonblank
              (and (chirp-object-p result) (chirp-get result "rest_id"))
              (and (chirp-object-p wrapper) (chirp-get wrapper "rest_id"))))
         (name (and (chirp-object-p core)
                    (chirp-first-nonblank (chirp-get core "name"))))
         (handle (and (chirp-object-p core)
                      (chirp-first-nonblank
                       (chirp-get core "screen_name")))))
    (unless id
      (error "XChat participant has no identity"))
    (list :id id
          :name name
          :handle handle
          :avatar-url
          (and (chirp-object-p result)
               (chirp-first-nonblank
                (chirp-get-in result '("avatar" "image_url")))))))

(defun chirp-xchat--event-label (event)
  "Return a one-line preview label for normalized XChat EVENT."
  (let ((text (plist-get event :text))
        (kind (plist-get event :kind)))
    (cond
     ((and (stringp text) (not (string-empty-p text)))
      (replace-regexp-in-string "[[:space:]\n\r]+" " " text))
     ((and (eq kind 'message)
           (> (or (plist-get event :attachment-count) 0) 0))
      "[Attachment message]")
     ((eq kind 'message) "[Message content unavailable]")
     (t (format "[XChat event: %s]" kind)))))

(defun chirp-xchat--min-key-version (events &optional fallback)
  "Return the lowest key version in XChat EVENTS, or FALLBACK."
  (let ((versions
         (delq nil
               (mapcar (lambda (event)
                         (plist-get event :conversation-key-version))
                       events))))
    (if versions
        (car (sort versions #'chirp-xchat--numeric-string-less-p))
      (or fallback "0"))))

(defun chirp-xchat--older-cursor (events &optional key-version)
  "Return an older-history cursor from XChat EVENTS and KEY-VERSION."
  (when-let* ((oldest (car events))
              (sequence-id (plist-get oldest :sequence-id)))
    (list :sequence-id sequence-id
          :key-version (chirp-xchat--min-key-version events key-version))))

(defun chirp-xchat--item-encoded-events (item)
  "Return ITEM's required encoded XChat preview events."
  (let ((cell (and (chirp-object-p item)
                   (assoc-string "latest_message_events" item t))))
    (unless (and cell (listp (cdr cell)))
      (error "XChat inbox item has invalid preview events"))
    (cdr cell)))

(defun chirp-xchat--required-boolean (object key label)
  "Return OBJECT's required Boolean KEY described by LABEL."
  (let ((cell (and (chirp-object-p object) (assoc-string key object t))))
    (unless (and cell (memq (cdr cell) '(nil t)))
      (error "XChat %s is missing or not Boolean" label))
    (cdr cell)))

(defun chirp-xchat--validate-inbox-items (items)
  "Validate XChat inbox ITEMS and their aggregate preview event budget."
  (unless (listp items)
    (error "XChat inbox page has invalid items"))
  (let ((item-count 0)
        (event-count 0)
        (encoded-bytes 0))
    (dolist (item items)
      (cl-incf item-count)
      (when (> item-count chirp-xchat-max-inbox-items)
        (error "XChat returned too many inbox conversations"))
      (dolist (encoded (chirp-xchat--item-encoded-events item))
        (unless (stringp encoded)
          (error "XChat returned a non-string encoded message event"))
        (cl-incf event-count)
        (when (> event-count chirp-xchat--max-inbox-events)
          (error "XChat returned too many encoded message events"))
        (cl-incf encoded-bytes (length encoded))
        (when (> encoded-bytes chirp-xchat--max-page-encoded-bytes)
          (error "XChat encoded message page is too large")))))
  items)

(defun chirp-xchat--conversation-type (detail)
  "Return normalized conversation type from XChat DETAIL."
  (pcase (chirp-get detail "__typename")
    ("XChatDirectConversationDetail" 'direct)
    ("XChatGroupConversationDetail" 'group)
    (typename
     (error "XChat returned an unknown conversation type: %S" typename))))

(cl-defun chirp-xchat--normalize-conversation
    (item &key require-deletion-flag-p)
  "Normalize one XChat conversation ITEM, or return nil when deleted.

REQUIRE-DELETION-FLAG-P rejects responses that omit the deletion flag."
  (let ((detail (chirp-get item "conversation_detail")))
    (unless (and detail (chirp-object-p detail))
      (error "XChat conversation is missing its detail"))
    (let* ((type (chirp-xchat--conversation-type detail))
           (has-more
            (chirp-xchat--required-boolean item "has_more"
                                           "conversation history flag"))
           (deletion-cell
            (assoc-string "is_deleted_by_viewer" item t))
           (deleted
            (and (or deletion-cell require-deletion-flag-p)
                 (chirp-xchat--required-boolean
                  item "is_deleted_by_viewer"
                  "conversation deletion flag"))))
      (unless deleted
        (let* ((id (chirp-first-nonblank
                    (chirp-get detail "conversation_id")))
               (raw-participants (chirp-get detail "participants_results"))
               (_ (unless (listp raw-participants)
                    (error "XChat conversation has invalid participants")))
               (participants
                (mapcar #'chirp-xchat--normalize-user raw-participants))
               (events
                (chirp-xchat--normalize-events
                 (chirp-xchat--item-encoded-events item)))
               (foreign-event
                (and id
                     (cl-find-if
                      (lambda (event)
                        (not (equal (plist-get event :conversation-id) id)))
                      events)))
               (latest (car (last events)))
               (older-cursor (chirp-xchat--older-cursor events))
               (group-metadata (chirp-get detail "group_metadata"))
               (participant-title
                (string-join
                 (delq nil
                       (mapcar
                        (lambda (user)
                          (or (plist-get user :name)
                              (and-let* ((handle (plist-get user :handle)))
                                (concat "@" handle))))
                        participants))
                 ", "))
               (title
                (or (chirp-first-nonblank
                     (chirp-get group-metadata "group_name")
                     participant-title)
                    (if (eq type 'group)
                        "Group conversation"
                      "Direct message"))))
          (unless id
            (error "XChat conversation has no identity"))
          (when foreign-event
            (error "XChat inbox returned an event for another conversation"))
          (when (and has-more (null older-cursor))
            (error "XChat conversation has more events but no history cursor"))
          (list :id id
                :type type
                :title title
                :participants participants
                :events events
                :latest-event latest
                :preview (and latest (chirp-xchat--event-label latest))
                :updated-at-msec
                (and latest (plist-get latest :created-at-msec))
                :muted-p (and (chirp-get detail "is_muted") t)
                :message-request-p
                (and latest (plist-get latest :message-request-p))
                :has-more has-more
                :older-cursor older-cursor))))))

(defun chirp-xchat--inbox-cursor (raw-cursor)
  "Normalize documented RAW-CURSOR from an XChat inbox page."
  (unless (chirp-object-p raw-cursor)
    (error "XChat inbox page has no cursor"))
  (let ((typename (chirp-get raw-cursor "__typename"))
        (snapshot-id (chirp-first-nonblank
                      (chirp-get raw-cursor "graph_snapshot_id"))))
    (pcase typename
      ("XChatGetInboxPageContinueCursor"
       (let* ((cursor-id (chirp-first-nonblank
                          (chirp-get raw-cursor "cursor_id")))
              (restarted-cell
               (assoc-string "graph_snapshot_restarted" raw-cursor t))
              (restarted
               (and restarted-cell
                    (chirp-xchat--required-boolean
                     raw-cursor "graph_snapshot_restarted"
                     "inbox snapshot restart flag"))))
         (unless (and cursor-id snapshot-id)
           (error "XChat inbox continuation cursor is incomplete"))
         (list :cursor-id cursor-id
               :graph-snapshot-id snapshot-id
               :graph-snapshot-restarted-p restarted)))
      ("XChatGetInboxPageEndCursor"
       (let ((exhausted-cell
              (or (assoc-string "inbox_exhausted" raw-cursor t)
                  (assoc-string "inboxExhausted" raw-cursor t))))
         (unless snapshot-id
           (error "XChat inbox end cursor is incomplete"))
         (when (and exhausted-cell
                    (not (memq (cdr exhausted-cell) '(nil t))))
           (error "XChat inbox exhaustion flag is not Boolean"))
         nil))
      (_ (error "XChat returned an unknown inbox cursor type: %S" typename)))))

(defun chirp-xchat-inbox-page (payload)
  "Normalize one modern XChat inbox page from GraphQL PAYLOAD."
  (let ((page (or (chirp-get-in payload '("data" "get_initial_chat_page"))
                  (chirp-get-in payload '("data" "get_inbox_page")))))
    (unless (and page (chirp-object-p page))
      (error "X did not return an XChat inbox page"))
    (let* ((items-cell (assoc-string "items" page t))
           (cursor-cell (or (assoc-string "inboxCursor" page t)
                            (assoc-string "cursor" page t))))
      (unless (and items-cell cursor-cell)
        (error "XChat inbox page is missing required fields"))
      (let* ((items (chirp-xchat--validate-inbox-items (cdr items-cell)))
             (cursor (chirp-xchat--inbox-cursor (cdr cursor-cell))))
        (cons (delq nil
                    (mapcar
                     (lambda (item)
                       (chirp-xchat--normalize-conversation
                        item :require-deletion-flag-p t))
                     items))
              `(("pagination" .
                 (,@(when cursor `(("nextCursor" . ,cursor)))
                  ("complete" . ,(not cursor))))))))))

;;; Recovery

(defun chirp-xchat--required (object key label)
  "Return OBJECT's required KEY described by LABEL."
  (let ((cell (and (chirp-object-p object)
                   (assoc-string key object t))))
    (unless cell
      (error "XChat %s is missing" label))
    (cdr cell)))

(defun chirp-xchat--ascii-secret-p (value max-length)
  "Return non-nil when VALUE is bounded graphic ASCII up to MAX-LENGTH."
  (and (stringp value)
       (<= 1 (length value) max-length)
       (cl-every (lambda (character)
                   (<= 33 character 126))
                 value)))

(defun chirp-xchat--hex-p (value length)
  "Return non-nil when VALUE contains exactly LENGTH hexadecimal digits."
  (and (stringp value)
       (= (length value) length)
       (string-match-p "\\`[[:xdigit:]]+\\'" value)))

(defun chirp-xchat--base64-length-p (value lengths)
  "Return non-nil when Base64 VALUE decodes to one of LENGTHS."
  (and (stringp value)
       (<= (length value) 2048)
       (string-match-p "\\`[A-Za-z0-9+/]*=\\{0,2\\}\\'" value)
       (condition-case nil
           (memq (length (base64-decode-string value)) lengths)
         (error nil))))

(defun chirp-xchat--https-realm-p (address)
  "Return non-nil when ADDRESS is a bounded credential-free HTTPS URL."
  (and (chirp-xchat--ascii-secret-p address 2048)
       (not (string-match-p "[?#]" address))
       (condition-case nil
           (let ((parsed (url-generic-parse-url address)))
             (and (equal (url-type parsed) "https")
                  (not (string-empty-p (or (url-host parsed) "")))
                  (null (url-user parsed))
                  (null (url-password parsed))))
         (error nil))))

(defun chirp-xchat--known-fields-p (object fields)
  "Return non-nil when every field in OBJECT belongs to FIELDS."
  (and (chirp-object-p object)
       (cl-every (lambda (cell)
                   (member (format "%s" (car cell)) fields))
                 object)))

(defun chirp-xchat--sdk-config (encoded)
  "Validate Juicebox SDK JSON ENCODED by XChat and return its facts."
  (unless (and (stringp encoded)
               (<= (string-bytes encoded)
                   (/ chirp-xchat--max-recovery-config-bytes 2)))
    (error "XChat Juicebox SDK configuration is too large"))
  (let ((config
         (condition-case nil
             (json-parse-string
              encoded :object-type 'alist :array-type 'list
              :null-object nil :false-object :json-false)
           (error nil))))
    (unless (chirp-xchat--known-fields-p
             config '("realms" "register_threshold" "recover_threshold"
                      "pin_hashing_mode"))
      (error "XChat Juicebox SDK configuration is invalid"))
    (let* ((realms (chirp-xchat--required config "realms" "realm list"))
           (register (chirp-xchat--required
                      config "register_threshold" "register threshold"))
           (recover (chirp-xchat--required
                     config "recover_threshold" "recover threshold"))
           (hashing (chirp-xchat--required
                     config "pin_hashing_mode" "PIN hashing mode"))
           (realm-count (length realms))
           (realm-table (make-hash-table :test #'equal)))
      (unless (and (listp realms)
                   (<= 1 realm-count chirp-xchat--max-juicebox-realms)
                   (integerp recover) (integerp register)
                   (> recover (/ realm-count 2))
                   (<= recover register realm-count)
                   (equal hashing "Standard2019"))
        (error "XChat Juicebox SDK configuration has invalid policy"))
      (dolist (realm realms)
        (unless (chirp-xchat--known-fields-p
                 realm '("id" "address" "public_key"))
          (error "XChat Juicebox realm is invalid"))
        (let ((id (chirp-xchat--required realm "id" "realm identity"))
              (address (chirp-xchat--required
                        realm "address" "realm address"))
              (public-key (chirp-get realm "public_key")))
          (unless (and (chirp-xchat--hex-p id 32)
                       (chirp-xchat--https-realm-p address)
                       (or (null public-key)
                           (chirp-xchat--hex-p public-key 64))
                       (not (gethash id realm-table)))
            (error "XChat Juicebox realm is invalid"))
          (puthash id (cons address public-key) realm-table)))
      (list :register-threshold register
            :recover-threshold recover
            :realm-table realm-table))))

(defun chirp-xchat--token-map (raw)
  "Normalize and validate one current XChat Juicebox token map RAW."
  (unless (chirp-object-p raw)
    (error "XChat Juicebox token map is missing"))
  (let* ((max-guesses
          (chirp-xchat--required raw "max_guess_count" "guess limit"))
         (recover
          (chirp-xchat--required raw "recover_threshold" "recover threshold"))
         (register
          (chirp-xchat--required raw "register_threshold" "register threshold"))
         (entries (chirp-xchat--required raw "token_map" "realm tokens"))
         (encoded
          (chirp-xchat--required
           raw "key_store_token_map_json" "SDK configuration"))
         (sdk (chirp-xchat--sdk-config encoded))
         (realm-table (plist-get sdk :realm-table))
         (seen (make-hash-table :test #'equal))
         tokens)
    (unless (and (integerp max-guesses) (<= 1 max-guesses 20)
                 (equal recover (plist-get sdk :recover-threshold))
                 (equal register (plist-get sdk :register-threshold))
                 (listp entries)
                 (= (length entries) (hash-table-count realm-table)))
      (error "XChat Juicebox token-map policy is invalid"))
    (dolist (entry entries)
      (let* ((id (chirp-xchat--required entry "key" "token realm identity"))
             (value (chirp-xchat--required entry "value" "realm token"))
             (token (chirp-xchat--required value "token" "realm token"))
             (address (chirp-xchat--required value "address" "realm address"))
             (public-key (chirp-get value "public_key"))
             (realm (and (stringp id) (gethash id realm-table))))
        (unless (and realm
                     (not (gethash id seen))
                     (chirp-xchat--ascii-secret-p token (* 16 1024))
                     (equal address (car realm))
                     (equal public-key (cdr realm)))
          (error "XChat Juicebox realm token is invalid"))
        (puthash id t seen)
        (push (cons id token) tokens)))
    `(("sdk_config" . ,encoded)
      ("tokens" . ,(nreverse tokens))
      ("max_guess_count" . ,max-guesses))))

(defun chirp-xchat--public-key-record (raw)
  "Normalize one registered XChat public-key record RAW."
  (let* ((metadata
          (chirp-xchat--required
           raw "public_key_with_metadata" "public-key metadata"))
         (version (chirp-xchat--required metadata "version" "key version"))
         (key (chirp-xchat--required metadata "public_key" "public key"))
         (identity (chirp-xchat--required key "public_key" "identity key"))
         (signing
          (chirp-xchat--required key "signing_public_key" "signing key"))
         (binding
          (chirp-xchat--required
           key "identity_public_key_signature" "key binding")))
    (unless (and (stringp version)
                 (<= (length version) 128)
                 (string-match-p "\\`\\(?:0\\|[1-9][0-9]*\\)\\'" version)
                 (chirp-xchat--base64-length-p identity '(33 65 91))
                 (chirp-xchat--base64-length-p signing '(33 65 91))
                 (chirp-xchat--base64-length-p binding '(64)))
      (error "XChat registered public key is invalid"))
    (list :version version
          :identity-public-key identity
          :signing-public-key signing
          :identity-public-key-signature binding
          :token-map (chirp-get raw "token_map"))))

(defun chirp-xchat--newer-public-key (left right)
  "Return the newer registered key from LEFT and RIGHT."
  (let ((left-version (plist-get left :version))
        (right-version (plist-get right :version)))
    (if (or (> (length left-version) (length right-version))
            (and (= (length left-version) (length right-version))
                 (string> left-version right-version)))
        left
      right)))

(defun chirp-xchat--user-public-keys (wrapper user-id)
  "Return USER-ID's strict public-key records from GraphQL WRAPPER."
  (let* ((returned-id (chirp-first-nonblank
                       (chirp-get wrapper "rest_id" "id")))
         (user (chirp-get wrapper "result"))
         (get-public-keys (and (chirp-object-p user)
                               (chirp-get user "get_public_keys"))))
    (unless (and (equal returned-id user-id)
                 (equal (chirp-get user "__typename") "User")
                 (chirp-object-p get-public-keys))
      (error "XChat did not return the requested user's public keys"))
    (unless (equal (chirp-get get-public-keys "__typename")
                   "GetPublicKeysResult")
      (error "XChat public-key lookup failed"))
    (let ((raw-keys
           (chirp-xchat--required
            get-public-keys "public_keys_with_token_map"
            "registered public keys")))
      (unless (and (listp raw-keys)
                   (<= 1 (length raw-keys) chirp-xchat--max-public-keys))
        (error "XChat registered public-key count is invalid"))
      (let* ((keys (mapcar #'chirp-xchat--public-key-record raw-keys))
             (versions (mapcar (lambda (key) (plist-get key :version)) keys)))
        (unless (= (length versions)
                   (length (delete-dups (copy-sequence versions))))
          (error "XChat returned duplicate public-key versions"))
        keys))))

(defun chirp-xchat-recovery-input (payload user-id)
  "Return strict native recovery input for USER-ID from GraphQL PAYLOAD."
  (let ((results
         (chirp-get-in payload '("data" "user_results_by_rest_ids"))))
    (unless (and (listp results) (= (length results) 1))
      (error "XChat public-key response has invalid user results"))
    (let* ((keys (chirp-xchat--user-public-keys (car results) user-id))
           (latest (cl-reduce #'chirp-xchat--newer-public-key keys))
           (token-map (chirp-xchat--token-map
                       (plist-get latest :token-map))))
      (append
       (list (cons "user_id" user-id))
       token-map
       (list
        (cons
         "registered_keys"
         (vector
          (list
           (cons "version" (plist-get latest :version))
           (cons "identity_public_key"
                 (plist-get latest :identity-public-key))))))))))

(defun chirp-xchat-signing-keys (payload user-ids)
  "Return strict native signing keys for USER-IDS from GraphQL PAYLOAD."
  (unless (and (listp user-ids)
               (<= 1 (length user-ids) 100)
               (= (length user-ids)
                  (length (delete-dups (copy-sequence user-ids))))
               (cl-every (lambda (id)
                           (and (stringp id)
                                (string-match-p "\\`[0-9]+\\'" id)))
                         user-ids))
    (error "XChat signing-key user IDs are invalid"))
  (let ((results
         (chirp-get-in payload '("data" "user_results_by_rest_ids")))
        (table (make-hash-table :test #'equal)))
    (unless (and (listp results) (= (length results) (length user-ids)))
      (error "XChat signing-key response has invalid user results"))
    (dolist (wrapper results)
      (let ((user-id (chirp-first-nonblank
                      (chirp-get wrapper "rest_id" "id"))))
        (unless (and (member user-id user-ids)
                     (not (gethash user-id table)))
          (error "XChat signing-key response has an unexpected user"))
        (puthash user-id
                 (chirp-xchat--user-public-keys wrapper user-id)
                 table)))
    (vconcat
     (cl-loop for user-id in user-ids
              append
              (mapcar
               (lambda (key)
                 `(("user_id" . ,user-id)
                   ("public_key_version" . ,(plist-get key :version))
                   ("public_key" . ,(plist-get key :signing-public-key))
                   ("identity_public_key" .
                    ,(plist-get key :identity-public-key))
                   ("identity_public_key_signature" .
                    ,(plist-get key :identity-public-key-signature))))
               (or (gethash user-id table)
                   (error "XChat signing keys are missing")))))))

;;; Query Adapters

(defun chirp-xchat-query-settings (inbox-limit event-limit)
  "Return XChat query settings for INBOX-LIMIT and EVENT-LIMIT."
  (unless (and (integerp inbox-limit)
               (<= 1 inbox-limit chirp-xchat-max-inbox-items)
               (integerp event-limit)
               (<= 1 event-limit chirp-xchat-max-history-events))
    (error "XChat query settings limits are invalid"))
  `(("conversation_event_limit" . ,event-limit)
    ("enable_legacy_overlay" . t)
    ("inbox_conversation_event_limit" . 5)
    ("inbox_conversation_limit" . ,inbox-limit)
    ("user_event_limit" . 200)))

(defun chirp-xchat-inbox-cursor-variables (cursor)
  "Return GraphQL variables for normalized inbox CURSOR."
  (let ((cursor-id (plist-get cursor :cursor-id))
        (snapshot-id (plist-get cursor :graph-snapshot-id))
        (restarted (plist-get cursor :graph-snapshot-restarted-p)))
    (unless (and (stringp cursor-id) (not (string-empty-p cursor-id))
                 (stringp snapshot-id) (not (string-empty-p snapshot-id))
                 (memq restarted '(nil t)))
      (error "XChat inbox cursor is invalid"))
    `(("cursor_id" . ,cursor-id)
      ("graph_snapshot_id" . ,snapshot-id)
      ("graph_snapshot_restarted" .
       ,(if restarted t :json-false)))))

(defun chirp-xchat-conversation-data (payload conversation-id)
  "Return CONVERSATION-ID's normalized conversation from GraphQL PAYLOAD."
  (let* ((result
          (chirp-get-in payload
                        '("data" "get_inbox_page_conversation_data")))
         (items-cell (and (chirp-object-p result)
                          (assoc-string "items" result t))))
    (unless items-cell
      (error "XChat conversation data is missing items"))
    (let* ((items (chirp-xchat--validate-inbox-items (cdr items-cell)))
           (conversations
            (delq nil (mapcar #'chirp-xchat--normalize-conversation items)))
           (conversation
            (cl-find conversation-id conversations
                     :key (lambda (item) (plist-get item :id))
                     :test #'equal)))
      (or conversation
          (error "X did not return the requested XChat conversation")))))

(defun chirp-xchat-history-page
    (payload conversation-id previous-key-version)
  "Normalize XChat history PAYLOAD for CONVERSATION-ID.

PREVIOUS-KEY-VERSION is retained when the page contains no older key version."
  (let ((page (chirp-get-in payload '("data" "get_conversation_page"))))
    (unless (and page (chirp-object-p page))
      (error "X did not return an XChat conversation page"))
    (let ((events-cell (assoc-string "encoded_message_events" page t))
          (key-events-cell
           (assoc-string "missing_conversation_key_change_events" page t))
          (has-more-cell (assoc-string "has_more" page t)))
      (unless (and events-cell has-more-cell
                   (memq (cdr has-more-cell) '(nil t)))
        (error "XChat conversation page is missing required fields"))
      (let* ((encoded-events (cdr events-cell))
             (encoded-key-events (and key-events-cell (cdr key-events-cell)))
             (_valid-shape
              (unless (and (listp encoded-events)
                           (listp encoded-key-events))
                (error "XChat history has invalid encoded events")))
             (_valid-budget
              (chirp-xchat--validate-encoded-events
               (append encoded-events encoded-key-events)
               chirp-xchat-max-history-events))
             (events
              (chirp-xchat--normalize-events
               encoded-events chirp-xchat-max-history-events))
             (key-events
              (chirp-xchat--normalize-events
               encoded-key-events chirp-xchat-max-history-events))
             (all-events (append events key-events))
             (foreign
              (cl-find-if
               (lambda (event)
                 (not (equal (plist-get event :conversation-id)
                             conversation-id)))
               all-events))
             (non-key-event
              (cl-find-if
               (lambda (event)
                 (not (eq (plist-get event :kind)
                          'conversation-key-change)))
               key-events))
             (has-more (cdr has-more-cell))
             (cursor
              (and has-more
                   (chirp-xchat--older-cursor
                    events previous-key-version))))
        (when foreign
          (error "XChat history returned an event for another conversation"))
        (when non-key-event
          (error "XChat history returned a non-key recovery event"))
        (when (and has-more (null cursor))
          (error "XChat history has more events but no continuation cursor"))
        (cons events
              `(("pagination" .
                 (,@(when cursor `(("nextCursor" . ,cursor)))
                  ("complete" . ,(not has-more))
                  ("hasMore" . ,has-more)))
                ("keyEvents" . ,key-events)
                ("encodedKeyEvents" .
                 ,(mapcar (lambda (event)
                            (plist-get event :encoded-event))
                          key-events))
                ("messageRequestState" .
                 ,(chirp-get page "message_request_state"))))))))

(provide 'chirp-xchat)

;;; chirp-xchat.el ends here
