;;; chirp-dm-render-test.el --- Tests for XChat presentation -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Exercise Appkit timeline projection, resource dependencies, and rendering.

;;; Code:
(add-to-list 'load-path
             (file-name-directory (or load-file-name buffer-file-name)))

(require 'ert)
(require 'cl-lib)
(require 'chirp)
(require 'chirp-dm-test-helper)

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
        (chirp-dm-render-print-event-row row))
      (goto-char (point-min))
      (search-forward "6小时")
      (should
       (equal
        (get-text-property (1- (match-beginning 0)) 'display)
        `(space :align-to
          (- right (,(string-width "6小时") . width))))))))

(ert-deftest chirp-dm-system-events-use-appkit-divider-rows ()
  "Non-message events should use Appkit's shared full-width divider."
  (let* ((event
          (chirp-dm-test--normalized-event "20" "20" ""))
         (row
          (appkit-chat-timeline-row-create
           :key "20" :payload event :context nil)))
    (setf (plist-get event :kind) 'conversation-key-change
          (plist-get event :created-at-msec) nil)
    (with-temp-buffer
      (setq-local fill-column 40)
      (chirp-dm-render-print-event-row row)
      (should (string-prefix-p "─" (buffer-string)))
      (should
       (string-match-p "Conversation encryption keys changed"
                       (buffer-string)))
      (should (get-text-property (point-min) 'read-only)))))

(ert-deftest chirp-dm-reactions-project-beneath-their-target-message ()
  "Reaction operations should become chips on their target message row."
  (let* ((message (chirp-dm-test--normalized-event "20" "message-20" "wowo"))
         (added (chirp-dm-test--normalized-event "21" "reaction-21" "🔥" "42"))
         (also-added
          (chirp-dm-test--normalized-event "22" "reaction-22" "🔥" "99"))
         (removed
          (chirp-dm-test--normalized-event "23" "reaction-23" "🔥" "99"))
         (conversation
          (chirp-dm-test--normalized-conversation
           message added also-added removed)))
    (setq added
          (plist-put
           (plist-put added :content-kind 'reaction)
           :target-message-id "20")
          also-added
          (plist-put
           (plist-put also-added :content-kind 'reaction)
           :target-message-id "20")
          removed
          (plist-put
           (plist-put removed :content-kind 'reaction-removed)
           :target-message-id "20"))
    (chirp-dm-state-set-events
     conversation (list message added also-added removed))
    (let* ((message (car (plist-get conversation :events)))
           (reaction (car (plist-get message :reactions)))
           (rows
            (chirp-dm-render-project-events
             nil '(:participants ((:id "42" :name "Alice")))
             (plist-get conversation :events))))
      (should (= (length rows) 1))
      (should
       (equal (plist-get (plist-get conversation :latest-event) :id) "20"))
      (should (equal (plist-get conversation :preview) "wowo"))
      (should (equal (plist-get reaction :emoji) "🔥"))
      (should (= (plist-get reaction :count) 1))
      (should (equal (plist-get reaction :senders) '("42")))
      (with-temp-buffer
        (setq-local fill-column 40)
        (chirp-dm-render-print-event-row (car rows))
        (should (string-match-p (regexp-quote "wowo\n 🔥 ")
                                (buffer-string)))
        (should-not (string-match-p "Reaction:" (buffer-string)))))))

(ert-deftest chirp-dm-reactions-distinguish-current-user-selection ()
  "Current-user reactions should use the selected chip presentation."
  (let* ((message (chirp-dm-test--normalized-event "20" "20" "wowo"))
         (mine
          '(:id "21" :sequence-id "21" :kind message
            :content-kind reaction :text "🔥" :sender-id "42"
            :target-message-id "20" :created-at-msec "1700000000001"))
         (theirs
          '(:id "22" :sequence-id "22" :kind message
            :content-kind reaction :text "🧠" :sender-id "99"
            :target-message-id "20" :created-at-msec "1700000000002"))
         (conversation (chirp-dm-test--normalized-conversation))
         rows)
    (chirp-dm-state-set-events conversation (list message mine theirs))
    (cl-letf (((symbol-function 'chirp-dm-render--view-user-id)
               (lambda (_view) "42")))
      (setq rows
            (chirp-dm-render-project-events
             nil '(:participants ((:id "42" :name "Alice")))
             (plist-get conversation :events))))
    (let ((reactions
           (plist-get
            (appkit-chat-timeline-row-context (car rows)) :reactions)))
      (should (equal (mapcar (lambda (item)
                               (plist-get item :selected-p))
                             reactions)
                     '(t nil))))
    (with-temp-buffer
      (chirp-dm-render-print-event-row (car rows))
      (search-backward "🔥")
      (should (eq (get-text-property (point) 'face)
                  'chirp-dm-reaction-selected))
      (search-forward "🧠")
      (should (eq (get-text-property (1- (point)) 'face)
                  'chirp-dm-reaction)))))

(ert-deftest chirp-dm-render-projects-verified-attachments-and-replies ()
  "Verified message facts should reach timeline dependencies and rendering."
  (let ((image
         (chirp-dm-test--normalized-event
          "20" "20" ""))
        (reply
         (chirp-dm-test--normalized-event
          "21" "21" "")))
    (setf (plist-get image :attachments)
          '((:kind image
             :url "https://pbs.twimg.com/media/example.jpg"
             :resource-key (xchat-media "conversation-1" "20" 0)))
          (plist-get image :attachment-count) 1
          (plist-get reply :reply-p) t
          (plist-get reply :reply-text) "earlier message"
          (plist-get reply :reply-document)
          (plist-get
           (chirp-dm-test--normalized-event
            "reply" "reply" "earlier message")
           :document)
          (plist-get reply :reply-attachment-count) 0)
    (let* ((rows
            (chirp-dm-render-project-events nil '(:participants nil) (list image reply)))
           (reply-model
            (plist-get
             (appkit-chat-timeline-row-context (cadr rows)) :reply)))
      (should (equal (appkit-chat-timeline-row-dependencies (car rows))
                     '((xchat-media "conversation-1" "20" 0))))
      (should
       (appkit-markup-document-p (plist-get reply-model :document)))
      (should (zerop (plist-get reply-model :attachment-count)))
      (with-temp-buffer
        (chirp-dm-render--insert-reply-preview reply-model)
        (should (equal (buffer-string) "↪ earlier message"))))))

(ert-deftest chirp-dm-attachments-use-appkit-media-cards-and-actions ()
  "A ready local card opens locally while a post link remains a browser action."
  (let ((chirp--app nil) buffer opened browsed)
    (unwind-protect
        (save-window-excursion
          (let ((event (chirp-dm-test--normalized-event "20" "20" "photo")))
            (setf (plist-get event :attachments)
                  '((:kind gif :name "giphy.gif" :media-hash "gif-hash"
                     :resource-key (xchat-media "gif"))
                    (:kind file :name "d.cpp" :media-hash "file-hash"
                     :resource-key (xchat-media "file"))
                    (:kind post :url "https://x.com/i/status/123"
                     :resource-key (xchat-media "post")))
                  (plist-get event :attachment-count) 3)
            (cl-letf (((symbol-function 'chirp-media--prefetch-enabled-p) (lambda () nil))
                      ((symbol-function 'chirp-backend-tweet) (lambda (&rest _) nil))
                      ((symbol-function 'chirp-media-xchat-resource)
                       (lambda (_view key attachment)
                         (appkit-media-resource-create
                          :file (and (equal key '(xchat-media "file")) "/tmp/d.cpp")
                          :name (plist-get attachment :name))))
                      ((symbol-function 'chirp-media-xchat-resource-status)
                       (lambda (_view key)
                         (if (equal key '(xchat-media "gif")) 'pending 'ready)))
                      ((symbol-function 'chirp-media-open-local)
                       (lambda (surface file kind) (setq opened (list surface file kind))))
                      ((symbol-function 'browse-url)
                       (lambda (url &rest _) (setq browsed url))))
              (setq buffer (chirp-dm-conversation-open
                            (chirp-dm-test--normalized-conversation event)))
              (with-current-buffer buffer
                (let ((surface (appkit-current-surface)))
                  (chirp-dm-test--drain surface)
                  (goto-char (point-min))
                  (search-forward "giphy.gif")
                  (goto-char (match-beginning 0))
                  (should-error (appkit-media-card-open) :type 'user-error)
                  (goto-char (point-min))
                  (search-forward "d.cpp")
                  (goto-char (match-beginning 0))
                  (appkit-media-card-open)
                  (should (equal opened (list surface "/tmp/d.cpp" 'file)))
                  (goto-char (point-min))
                  (search-forward "Attached post")
                  (goto-char (match-beginning 0))
                  (appkit-ui-activate)
                  (should (equal browsed "https://x.com/i/status/123")))))))
      (chirp-stop)
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest chirp-dm-post-resource-redraws-as-an-embedded-tweet ()
  "Resource completion should redraw an attached post as a tweet card."
  (let ((chirp--app nil) buffer callback requested)
    (unwind-protect
        (save-window-excursion
          (let*
              ((event (chirp-dm-test--normalized-event "20" "20" ""))
               (conversation
                (chirp-dm-test--normalized-conversation event)))
            (setf (plist-get event :attachments)
                  '((:kind post :url
                     "https://x.com/jschopplich/status/123"
                     :resource-key
                     (xchat-media "conversation-1" "20" 0)))
                  (plist-get event :attachment-count) 1)
            (setq conversation
                  (chirp-dm-test--normalized-conversation event))
            (cl-letf
                (((symbol-function 'chirp-backend-tweet)
                  (lambda (tweet-id success _error)
                    (setq requested tweet-id callback success)
                    nil))
                 ((symbol-function 'chirp-render-insert-tweet-card)
                  (lambda (tweet &rest _options)
                    (insert
                     (format "Embedded: %s" (plist-get tweet :text))))))
              (setq buffer (chirp-dm-conversation-open conversation))
              (with-current-buffer buffer
                (chirp-dm-test--drain (appkit-current-surface)))
              (should (equal requested "123"))
              (with-current-buffer buffer
                (should-not
                 (string-match-p "Embedded: Post body" (buffer-string))))
              (funcall callback
                       '(:kind tweet :id "123" :text "Post body" :url
                         "https://x.com/i/status/123")
                       nil)
              (let
                  ((view
                    (with-current-buffer buffer
                      (appkit-current-surface))))
                (chirp-dm-test--drain view))
              (with-current-buffer buffer
                (should
                 (string-match-p "Embedded: Post body" (buffer-string)))))))
      (chirp-stop) (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest chirp-dm-message-avatars-use-appkit-prefix-geometry ()
  "Message rows should project participant avatars into two-line prefixes."
  (let*
      ((event
        (chirp-dm-test--normalized-event "20" "20"
                                         "first line\nsecond line"))
       (state
        '(:participants
          ((:id "42" :name "Alice" :avatar-url
                "https://example.invalid/alice.jpg"))))
       (row
        (car (chirp-dm-render-project-events nil state (list event))))
       seen)
    (should
     (equal (appkit-chat-timeline-row-dependencies row)
            '((xchat-avatar "42"))))
    (with-temp-buffer
      (cl-letf
          (((symbol-function 'appkit-current-surface)
            (lambda () nil))
           ((symbol-function 'appkit-chat-avatar-two-line-pixel-size)
            (lambda () 42))
           ((symbol-function 'chirp-media-avatar-resource-image)
            (lambda (view resource-key pixel-size)
              (setq seen (list view resource-key pixel-size))
              :avatar-image))
           ((symbol-function 'appkit-chat-avatar-prefixes)
            (lambda (image fallback &rest options)
              (should (eq image :avatar-image))
              (should (equal fallback "@"))
              (should (equal options '(:pixel-size 42 :resize t)))
              '(:header "TOP " :first-body "BOTTOM " :rest-body
                "REST "))))
        (chirp-dm-render-print-event-row row))
      (goto-char (point-min))
      (should (equal (get-text-property (point) 'line-prefix) "TOP "))
      (should
       (equal
        (get-text-property 0 'chirp-dm-avatar-sender-id
                           (get-text-property (point) 'line-prefix))
        "42"))
      (forward-line 1)
      (should
       (equal (get-text-property (point) 'line-prefix) "BOTTOM "))
      (forward-line 1)
      (should (equal (get-text-property (point) 'line-prefix) "REST ")))))

(ert-deftest
    chirp-dm-participant-avatar-resources-redraw-dependent-rows ()
  "Avatar completion should redraw only messages from that participant."
  (let ((chirp--app nil) buffer resource success printed)
    (unwind-protect
        (save-window-excursion
          (let*
              ((alice
                (chirp-dm-test--normalized-event "20" "20" "hello"
                                                 "42"))
               (bob
                (chirp-dm-test--normalized-event "21" "21" "hi" "99"))
               (conversation
                (chirp-dm-test--normalized-conversation alice bob))
               (printer
                (symbol-function 'chirp-dm-render-print-event-row)))
            (setf (plist-get conversation :participants)
                  '((:id "42" :name "Alice" :avatar-url
                     "https://example.invalid/alice.jpg")
                    (:id "99" :name "Bob")))
            (cl-letf
                (((symbol-function 'chirp-media--prefetch-enabled-p)
                  (lambda () t))
                 ((symbol-function
                   'appkit-media-image-cache-existing-file)
                  (lambda (_cache-base) nil))
                 ((symbol-function 'chirp-media--valid-cache-file-p)
                  (lambda (path)
                    (equal path "/tmp/chirp-dm-avatar.jpg")))
                 ((symbol-function
                   'chirp-media--load-image-resource)
                  (lambda
                    (_context input callback _errback)
                    (setq resource (appkit-media-image-acquisition-resource input) success callback)
                    (appkit-cancellation-create :kind 'transport :cancel #'ignore)))
                 ((symbol-function 'chirp-dm-render-print-event-row)
                  (lambda (row)
                    (push
                     (plist-get (appkit-chat-timeline-row-payload row)
                                :id)
                     printed)
                    (funcall printer row))))
              (setq buffer (chirp-dm-conversation-open conversation))
              (with-current-buffer buffer
                (chirp-dm-test--drain (appkit-current-surface)))
              (let*
                  ((view
                    (with-current-buffer buffer
                      (appkit-current-surface)))
                   (_drained (chirp-dm-test--drain view))
                   (resource-key '(xchat-avatar "42")))
                (should
                 (equal (alist-get 'url resource)
                        "https://example.invalid/alice.jpg"))
                (should
                 (eq (appkit-resource-state-status (appkit-resource-state view resource-key))
                     'pending))
                (setq printed nil)
                (funcall success "/tmp/chirp-dm-avatar.jpg")
                (chirp-dm-test--drain view)
                (should
                 (eq (appkit-resource-state-status (appkit-resource-state view resource-key))
                     'ready))
                (should (equal printed '("20")))))))
      (chirp-stop) (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest chirp-dm-image-resources-use-appkit-and-row-dependencies
    ()
  "Verified images should use Appkit acquisition and resource invalidation."
  (let ((chirp--app nil) buffer resource success printed)
    (unwind-protect
        (save-window-excursion
          (let*
              ((event
                (chirp-dm-test--normalized-event "20" "20" "photo"))
               (other
                (chirp-dm-test--normalized-event "21" "21" "text"))
               (resource-key '(xchat-media "conversation-1" "20" 0))
               (_attachment
                (setf (plist-get event :attachments)
                      (list
                       (list :kind 'image :url
                             "https://pbs.twimg.com/media/example.jpg"
                             :resource-key resource-key))
                      (plist-get event :attachment-count) 1))
               (conversation
                (chirp-dm-test--normalized-conversation event other))
               (printer
                (symbol-function 'chirp-dm-render-print-event-row)))
            (cl-letf
                (((symbol-function 'chirp-media--prefetch-enabled-p)
                  (lambda () t))
                 ((symbol-function
                   'appkit-media-image-cache-existing-file)
                  (lambda (_cache-base) nil))
                 ((symbol-function 'chirp-media--valid-cache-file-p)
                  (lambda (path)
                    (equal path "/tmp/chirp-dm-image.jpg")))
                 ((symbol-function
                   'chirp-media--load-image-resource)
                  (lambda
                    (_context input callback _errback)
                    (setq resource (appkit-media-image-acquisition-resource input) success callback)
                    (appkit-cancellation-create :kind 'transport :cancel #'ignore)))
                 ((symbol-function 'chirp-dm-render-print-event-row)
                  (lambda (row)
                    (push
                     (plist-get (appkit-chat-timeline-row-payload row)
                                :id)
                     printed)
                    (funcall printer row))))
              (setq buffer (chirp-dm-conversation-open conversation))
              (with-current-buffer buffer
                (chirp-dm-test--drain (appkit-current-surface)))
              (let*
                  ((view
                    (with-current-buffer buffer
                      (appkit-current-surface)))
                   (_drained (chirp-dm-test--drain view)))
                (should
                 (equal (alist-get 'url resource)
                        "https://pbs.twimg.com/media/example.jpg"))
                (should
                 (eq (appkit-resource-state-status (appkit-resource-state view resource-key))
                     'pending))
                (setq printed nil)
                (funcall success "/tmp/chirp-dm-image.jpg")
                (chirp-dm-test--drain view)
                (should
                 (eq (appkit-resource-state-status (appkit-resource-state view resource-key))
                     'ready))
                (should (equal printed '("20")))))))
      (chirp-stop) (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest
    chirp-dm-encrypted-media-is-downloaded-decrypted-and-cached ()
  "A media hash should cross authenticated transport and native decryption."
  (let ((chirp--app nil) buffer downloaded decrypted written owner)
    (unwind-protect
        (save-window-excursion
          (let*
              ((event
                (chirp-dm-test--normalized-event "20" "20" "gif" "42"
                                                 "conversation-1"))
               (resource-key
                '(xchat-media "conversation-1" "message-20" 0))
               (conversation
                (chirp-dm-test--normalized-conversation event)))
            (setf (plist-get event :key-version) "7"
                  (plist-get event :attachments)
                  `
                  ((:kind gif :media-hash "media_hash" :name
                          "giphy.gif" :resource-key ,resource-key))
                  (plist-get event :attachment-count) 1)
            (setq conversation
                  (chirp-dm-test--normalized-conversation event))
            (cl-letf
                (((symbol-function 'chirp-media--prefetch-enabled-p)
                  (lambda () t))
                 ((symbol-function
                   'appkit-media-image-cache-existing-file)
                  (lambda (_cache-base) nil))
                 ((symbol-function 'chirp-media--valid-cache-file-p)
                  (lambda (path) (and written (equal path written))))
                 ((symbol-function 'chirp-backend-dm-media)
                  (lambda
                    (conversation-id media-hash callback &rest options)
                    (setq downloaded (list conversation-id media-hash)
                          owner (plist-get options :owner))
                    (funcall callback (unibyte-string 0 1 2))))
                 ((symbol-function
                   'chirp-xchat-native-decrypt-media-bytes)
                  (lambda (conversation-id key-version ciphertext)
                    (setq decrypted
                          (list conversation-id key-version
                                (copy-sequence ciphertext)))
                    (encode-coding-string "GIF89a" 'binary)))
                 ((symbol-function
                   'chirp-media--write-xchat-attachment)
                  (lambda (_bytes path) (setq written path) path)))
              (setq buffer (chirp-dm-conversation-open conversation))
              (with-current-buffer buffer
                (chirp-dm-test--drain (appkit-current-surface)))
              (let*
                  ((view
                    (with-current-buffer buffer
                      (appkit-current-surface)))
                   (_drained (chirp-dm-test--drain view))
                   (entry
                    (appkit-resource-state view resource-key)))
                (should
                 (equal downloaded '("conversation-1" "media_hash")))
                (should (eq owner (appkit-surface-app view)))
                (should
                 (equal decrypted
                        (list "conversation-1" "7"
                              (unibyte-string 0 1 2))))
                (should (string-suffix-p ".gif" written))
                (should (eq (appkit-resource-state-status entry) 'ready))
                (should (equal (appkit-resource-state-value entry) written))))))
      (chirp-stop) (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest
    chirp-dm-encrypted-media-failure-settles-without-retry-loop ()
  "A failed attachment request should leave one terminal failed resource."
  (let ((chirp--app nil) buffer errback requests)
    (unwind-protect
        (save-window-excursion
          (let*
              ((event
                (chirp-dm-test--normalized-event "20" "20" "file" "42"
                                                 "conversation-1"))
               (resource-key
                '(xchat-media "conversation-1" "message-20" 0)))
            (setf (plist-get event :key-version) "7"
                  (plist-get event :attachments)
                  `
                  ((:kind file :media-hash "media_hash" :name
                          "file.txt" :resource-key ,resource-key))
                  (plist-get event :attachment-count) 1)
            (cl-letf
                (((symbol-function 'chirp-media--prefetch-enabled-p)
                  (lambda () t))
                 ((symbol-function
                   'appkit-media-image-cache-existing-file)
                  (lambda (_cache-base) nil))
                 ((symbol-function 'chirp-backend-dm-media)
                  (lambda
                    (_conversation-id _media-hash _callback &rest
                                      options)
                    (setq requests (1+ (or requests 0)) errback
                          (plist-get options :errback))
                    :request)))
              (setq buffer
                    (chirp-dm-conversation-open
                     (chirp-dm-test--normalized-conversation event)))
              (let*
                  ((view
                    (with-current-buffer buffer
                      (appkit-current-surface)))
                   (_drained (chirp-dm-test--drain view)))
                (should
                 (eq (appkit-resource-state-status (appkit-resource-state view resource-key))
                     'pending))
                (funcall errback "HTTP 403")
                (chirp-dm-test--drain view)
                (should
                 (eq (appkit-resource-state-status (appkit-resource-state view resource-key))
                     'failed))
                (appkit-surface-post view (appkit-projection-change-create :full-p t))
                (chirp-dm-test--drain view)
                (should (= requests 1))))))
      (chirp-stop) (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest chirp-dm-closing-media-interest-revokes-decryption ()
  "Closing the last media reader cancels transport and erases late ciphertext."
  (let ((chirp--app nil) buffer request callback decrypted written)
    (unwind-protect
        (save-window-excursion
          (let ((event (chirp-dm-test--normalized-event "20" "20" "file")))
            (setf (plist-get event :key-version) "7"
                  (plist-get event :attachment-count) 1
                  (plist-get event :attachments)
                  '((:kind file :media-hash "media_hash" :name "file.txt"
                     :resource-key (xchat-media "conversation-1" "20" 0))))
            (cl-letf (((symbol-function 'chirp-media--prefetch-enabled-p) (lambda () t))
                      ((symbol-function 'chirp-media--valid-cache-file-p) (lambda (_) nil))
                      ((symbol-function 'appkit-media-image-cache-existing-file) (lambda (_) nil))
                      ((symbol-function 'chirp-backend-dm-media)
                       (lambda (_conversation _hash success &rest _options)
                         (setq callback success
                               request (generate-new-buffer " *chirp-encrypted-transfer*"))))
                      ((symbol-function 'chirp-xchat-native-decrypt-media-bytes)
                       (lambda (&rest _) (setq decrypted t) (unibyte-string 1)))
                      ((symbol-function 'chirp-media--write-xchat-attachment)
                       (lambda (&rest _) (setq written t))))
              (setq buffer (chirp-dm-conversation-open
                            (chirp-dm-test--normalized-conversation event)))
              (with-current-buffer buffer
                (chirp-dm-test--drain (appkit-current-surface)))
              (should (buffer-live-p request))
              (kill-buffer buffer)
              (should-not (buffer-live-p request))
              (let ((ciphertext (unibyte-string 9 8 7)))
                (funcall callback ciphertext)
                (should (equal ciphertext (unibyte-string 0 0 0))))
              (should-not decrypted)
              (should-not written))))
      (chirp-stop)
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (when (buffer-live-p request) (kill-buffer request)))))

(ert-deftest chirp-dm-render-requests-only-allowlisted-xchat-media ()
  "Verified attachments should not make arbitrary URLs fetchable."
  (let (sources)
    (cl-letf (((symbol-function 'chirp-media-image-demand)
               (lambda (_view _resource-key source &rest _options)
                 (push source sources))))
      (chirp-dm-render--event-media-demands
       nil
       '(:attachments
         ((:kind image :resource-key (xchat-media "external")
           :url "https://example.com/private.jpg")
          (:kind image :resource-key (xchat-media "port")
           :url "https://pbs.twimg.com:444/media/private.jpg")
          (:kind image :resource-key (xchat-media "trusted")
           :url "https://pbs.twimg.com/media/example.jpg")))))
    (should
     (equal sources '("https://pbs.twimg.com/media/example.jpg")))))

(provide 'chirp-dm-render-test)

;;; chirp-dm-render-test.el ends here
