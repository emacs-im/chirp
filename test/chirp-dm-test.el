;;; chirp-dm-test.el --- Tests for XChat views and composer -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Commentary:

;; Exercise synthetic XChat wire adaptation and Appkit-owned direct messages.

;;; Code:

(add-to-list 'load-path
             (file-name-directory (or load-file-name buffer-file-name)))

(require 'ert)
(require 'cl-lib)
(require 'chirp)
(require 'chirp-xchat)
(require 'chirp-xchat-native)
(require 'chirp-dm-test-helper)





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
          (should
           (equal (chirp--session-xchat-user (chirp--session))
                  '(:id "42")))
          (should (cl-every #'zerop (string-to-list pin)))
          (should (cl-every #'zerop (string-to-list token)))
          (should (buffer-live-p buffer)))
      (chirp-stop)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))




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
            (setf (chirp--session-xchat-user (chirp--session)) '(:id "42")
                  (chirp--session-xchat-user-id (chirp--session)) "42")
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
                (should (string-match-p
                         "1 conversation · 0 requests · 0 muted"
                         (buffer-string)))
                (appkit-directory-next-item)
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
                       (lambda (conversation-id _events _signing-keys)
                         (should (equal conversation-id "conversation-1"))
                         '((:sequence-id "20"
                            :message-id "message-20"
                            :conversation-id "conversation-1"
                            :content-kind text
                            :text "verified plaintext"
                            :attachments nil
                            :reply-p nil
                            :reply-text nil
                            :reply-attachment-count 0)))))
              (setq buffer (chirp-dm-conversation-open conversation))
              (let* ((view (with-current-buffer buffer (appkit-current-view)))
                     (state (appkit-view-state view))
                     (decrypted (car (chirp-dm-conversation--events state))))
                (should (eq owner view))
                (should (equal (plist-get decrypted :text)
                               "verified plaintext"))
                (should
                 (equal
                  (appkit-markup-plain-text (plist-get decrypted :document))
                  "verified plaintext"))
                (should-not (plist-get decrypted :encrypted-p))
                (appkit-sync-invalidations view)
                (with-current-buffer buffer
                  (should (string-match-p "verified plaintext" (buffer-string))))))))
      (chirp-stop)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest chirp-dm-decryption-retains-reaction-target-identity ()
  "Verified reaction targets should reach their canonical message aggregate."
  (let* ((target (chirp-dm-test--normalized-event "20" "20" "wowo" "42"))
         (reaction
          (chirp-dm-test--normalized-event
           "21" "message-21" "[Encrypted message unavailable]" "42"))
         (conversation
          (chirp-dm-test--normalized-conversation target reaction))
         (state (list :conversation conversation)))
    (setf (plist-get reaction :encrypted-p) t)
    (chirp-dm-conversation--apply-verified-messages
     state
     '((:sequence-id "message-21"
        :message-id "21"
        :sender-id "42"
        :conversation-id "conversation-1"
        :content-kind reaction
        :text "🔥"
        :target-message-id "20"
        :attachments nil
        :reply-p nil
        :reply-text nil
        :reply-attachment-count 0)))
    (let* ((events (plist-get conversation :events))
           (target (car events))
           (operation (cadr events))
           (aggregate (car (plist-get target :reactions))))
      (should (equal (plist-get operation :target-message-id) "20"))
      (should (equal (plist-get aggregate :emoji) "🔥"))
      (should (equal (plist-get aggregate :senders) '("42"))))))

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
                       (lambda (conversation-id events _signing-keys)
                         (should (equal conversation-id "conversation-1"))
                         (push 'decrypt calls)
                         (should (equal events
                                        '("encoded-key" "encoded-message")))
                         '((:sequence-id "20"
                            :message-id "message-20"
                            :conversation-id "conversation-1"
                            :content-kind text
                            :text "verified plaintext"
                            :attachments nil
                            :reply-p nil
                            :reply-text nil
                            :reply-attachment-count 0)))))
              (setq buffer (chirp-dm-conversation-open conversation))
              (let* ((view (with-current-buffer buffer (appkit-current-view)))
                     (state (appkit-view-state view)))
                (should (equal (nreverse calls) '(history signing decrypt)))
                (should (cl-every (lambda (owner) (eq owner view)) owners))
                (should
                 (equal (plist-get (car (last (chirp-dm-conversation--events state))) :text)
                        "verified plaintext"))))))
      (chirp-stop)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest chirp-dm-new-group-ingests-key-before-send-preflight ()
  "Opening a new group should ingest its key before local message encryption."
  (let ((chirp--app nil)
        buffer ingested send-variables send-error)
    (unwind-protect
        (save-window-excursion
          (let* ((key-event
                  (chirp-dm-test--normalized-event "10" "10" nil))
                 (_key
                  (setf (plist-get key-event :kind) 'conversation-key-change
                        (plist-get key-event :encoded-event) "encoded-group-key"))
                 (conversation
                  (chirp-dm-test--normalized-conversation key-event)))
            (setf (plist-get conversation :id) "group-1"
                  (plist-get conversation :type) 'group
                  (plist-get conversation :has-more) nil
                  (plist-get conversation :older-cursor) nil
                  (chirp--session-xchat-native-epoch (chirp--session)) 7
                  (chirp--session-xchat-user-id (chirp--session)) "42")
            (cl-letf
                (((symbol-function 'chirp-backend-dm-signing-keys)
                  (lambda (_user-ids callback &rest _options)
                    (funcall callback [] nil)
                    nil))
                 ((symbol-function 'chirp-xchat-native-decrypt-events)
                  (lambda (conversation-id events _signing-keys)
                    (should (equal conversation-id "group-1"))
                    (should (equal events '("encoded-group-key")))
                    (setq ingested t)
                    nil))
                 ((symbol-function 'chirp-xchat-native-prepare-text)
                  (lambda (conversation-id text)
                    (should ingested)
                    (should (equal conversation-id "group-1"))
                    (should (equal text "hello group"))
                    '(:message-id "01234567-89ab-cdef-0123-456789abcdef"
                      :encoded-message-create-event "ZXZlbnQ="
                      :encoded-message-event-signature "c2ln")))
                 ((symbol-function 'chirp-x-graphql-request)
                  (lambda (_operation variables _callback &rest _options)
                    (setq send-variables variables)
                    'send-request)))
              (setq buffer (chirp-dm-conversation-open conversation))
              (should ingested)
              (let* ((view (with-current-buffer buffer (appkit-current-view)))
                     (state (appkit-view-state view))
                     (canonical-key
                      (car (chirp-dm-conversation--events state))))
                (should (= (plist-get canonical-key :native-key-epoch) 7)))
              (should
               (eq
                (chirp-backend-dm-send-text
                 "group-1" "hello group" #'ignore
                 :errback (lambda (message) (setq send-error message)))
                'send-request))
              (should send-variables)
              (should-not send-error))))
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
                 (first (chirp-dm-conversation-open conversation))
                 (second (chirp-dm-conversation-open conversation))
                 (first-view (with-current-buffer first (appkit-current-view)))
                 (second-view (with-current-buffer second (appkit-current-view))))
            (setq buffers (list first second))
            (should-not (eq first second))
            (should-not (equal (appkit-view-id first-view)
                               (appkit-view-id second-view)))
            (with-current-buffer first
              (should (eq major-mode 'chirp-dm-conversation--mode))
              (should-not (derived-mode-p 'special-mode))
              (should-not buffer-read-only)
              (should appkit-chatbuf-owns-wrap-prefix-p)
              (should (eq appkit-markup-compose-active-codec 'plain))
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
              (should chirp-dm-conversation--timeline-mode)
              (should (eq (key-binding (kbd "q"))
                          #'chirp-quit-current-buffer))
              (search-forward "hello")
              (should (get-text-property (match-beginning 0) 'read-only))
              (goto-char (point-max))
              (appkit-chatbuf-update-context-mode)
              (should-not chirp-dm-conversation--timeline-mode)
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

(ert-deftest chirp-dm-fresh-views-share-canonical-conversation-facts ()
  "Refreshing one fresh view should update every view without sharing drafts."
  (let ((chirp--app nil)
        buffers callback request)
    (unwind-protect
        (save-window-excursion
          (setq request (generate-new-buffer " *chirp-dm-shared-refresh*"))
          (push request buffers)
          (let* ((old
                  (chirp-dm-test--normalized-event "20" "20" "old"))
                 (fresh
                  (chirp-dm-test--normalized-event "30" "30" "fresh"))
                 (conversation
                  (chirp-dm-test--normalized-conversation old))
                 first second first-view second-view first-state second-state)
            (cl-letf
                (((symbol-function 'chirp-backend-dm-conversation-data)
                  (lambda (_conversation-id success &rest _options)
                    (setq callback success)
                    request)))
              (setq first (chirp-dm-conversation-open conversation)
                    second (chirp-dm-conversation-open conversation)
                    buffers (append (list first second) buffers)
                    first-view
                    (with-current-buffer first (appkit-current-view))
                    second-view
                    (with-current-buffer second (appkit-current-view))
                    first-state (appkit-view-state first-view)
                    second-state (appkit-view-state second-view))
              (should
               (eq (plist-get first-state :conversation)
                   (plist-get second-state :conversation)))
              (with-current-buffer first
                (goto-char (point-max))
                (insert "first draft"))
              (with-current-buffer second
                (goto-char (point-max))
                (insert "second draft"))
              (with-current-buffer first
                (chirp-dm-refresh-conversation))
              (funcall callback
                       (chirp-dm-test--normalized-conversation old fresh)
                       nil)
              (appkit-sync-invalidations first-view)
              (appkit-sync-invalidations second-view)
              (should
               (equal
                (mapcar
                 (lambda (event) (plist-get event :id))
                 (chirp-dm-conversation--events second-state))
                '("20" "30")))
              (with-current-buffer first
                (should (equal (appkit-chatbuf-input-string) "first draft"))
                (should (appkit-chat-timeline-node "30")))
              (with-current-buffer second
                (should (equal (appkit-chatbuf-input-string) "second draft"))
                (should (appkit-chat-timeline-node "30"))))))
      (chirp-stop)
      (dolist (buffer buffers)
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))



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
                     (buffer (chirp-dm-conversation-open conversation))
                     (view (with-current-buffer buffer (appkit-current-view)))
                     (state (appkit-view-state view)))
                (push buffer buffers)
                (with-current-buffer buffer
                  (goto-char (point-max))
                  (insert "hello")
                  (chirp-dm-submit)
                  (should-error (chirp-dm-submit) :type 'user-error)
                  (should (appkit-compose-operation-active-p))
                  (should (eq (appkit-compose-operation-kind) 'dm-send))
                  (should (equal (appkit-compose-label)
                                 "Sending direct message")))
                (should (eq send-owner view))
                (should (equal sent-text "hello"))
                (should (= (length (chirp-dm-conversation--events state)) 1))
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
                  (should (= (length (chirp-dm-conversation--events state)) 1))
                  (funcall bridge-success (list first-event sent-event)
                           '(("pagination" . (("complete" . t)))))
                  (with-current-buffer buffer
                    (appkit-sync-invalidations view)
                    (should (appkit-chat-timeline-node "20"))
                    (should (appkit-chat-timeline-node "21")))
                  (should (= (length (chirp-dm-conversation--events state)) 2))
                  (should (= (cl-count "21" (chirp-dm-conversation--events state)
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
                                  (chirp-dm-conversation-open conversation)))
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
              (with-current-buffer buffer
                (should-not (appkit-compose-operation-active-p)))
              (should (= (length (chirp-dm-conversation--events state)) 1)))))
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
                                  (chirp-dm-conversation-open conversation)))
                   (view (with-current-buffer buffer (appkit-current-view))))
              (with-current-buffer buffer
                (goto-char (point-max))
                (insert "retain me")
                (should-error (chirp-dm-submit) :type 'error)
                (appkit-sync-invalidations view)
                (should-not buffer-read-only)
                (should (equal (appkit-chatbuf-input-string) "retain me")))
              (with-current-buffer buffer
                (should-not (appkit-compose-operation-active-p))))))
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
                   (buffer (chirp-dm-conversation-open conversation))
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
            (setq buffer (chirp-dm-conversation-open conversation))
            (with-current-buffer buffer
              (should (string-match-p "Unknown sender" (buffer-string)))
              (should-not (string-match-p "\\`You" (buffer-string))))))
      (chirp-stop)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest chirp-dm-self-sender-uses-authenticated-profile ()
  "An omitted self participant should retain its profile label and avatar."
  (let ((chirp--app nil)
        buffer requested-avatar)
    (unwind-protect
        (save-window-excursion
          (let ((conversation
                 (chirp-dm-test--normalized-conversation
                  (chirp-dm-test--normalized-event
                   "20" "20" "outgoing" "99")))
                (user
                 '(:id "99" :name "Me" :handle "me"
                   :avatar-url "https://example.invalid/me.jpg")))
            (setf (chirp--session-xchat-user (chirp--session)) user
                  (chirp--session-xchat-user-id (chirp--session)) "99"
                  (plist-get conversation :participants) nil)
            (cl-letf
                (((symbol-function
                   'chirp-media-request-xchat-avatar-resource)
                  (lambda (_view identity url)
                    (setq requested-avatar (list identity url))
                    (list 'xchat-avatar identity))))
              (setq buffer (chirp-dm-conversation-open conversation)))
            (should
             (equal requested-avatar
                    '("99" "https://example.invalid/me.jpg")))
            (with-current-buffer buffer
              (should (string-match-p "^Me" (buffer-string)))
              (should-not
               (string-match-p "Unknown sender" (buffer-string))))))
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
              (setq buffer (chirp-dm-conversation-open conversation))
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
                              (chirp-dm-conversation--events
                               (appkit-view-state view)))
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
              (setq buffer (chirp-dm-conversation-open conversation))
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
                                 (chirp-dm-conversation--events state))
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
                                 (chirp-dm-conversation--events state))
                         '("20")))
                (funcall bridge-callback (list oldest current intermediate)
                         '(("pagination" . (("complete" . t)))))
                (should (equal
                         (mapcar (lambda (event) (plist-get event :id))
                                 (chirp-dm-conversation--events state))
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
        (chirp-dm-conversation--refresh-bridge-page-limit 2)
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
              (setq buffer (chirp-dm-conversation-open conversation))
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
                                 (chirp-dm-conversation--events state))
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
              (setq buffer (chirp-dm-conversation-open conversation))
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
                                (chirp-dm-conversation--events
                                 (appkit-view-state view)))
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
                                (chirp-dm-conversation--events
                                 (appkit-view-state view)))
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
