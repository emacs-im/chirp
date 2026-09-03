;;; chirp-dm-inbox-test.el --- Tests for XChat inbox views -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Exercise Appkit directory projection and inbox request ownership.

;;; Code:
(add-to-list 'load-path
             (file-name-directory (or load-file-name buffer-file-name)))


(require 'ert)
(require 'cl-lib)
(require 'chirp)
(require 'chirp-dm-test-helper)

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
           :label "Alice"
           :payload
           (list :id "conversation-1"
                 :title "Alice"
                 :preview "Hello"
                 :updated-at-msec milliseconds))))
    (with-temp-buffer
      (setq-local fill-column 40)
      (cl-letf (((symbol-function 'current-time) (lambda () now)))
        (chirp-dm-inbox--insert-item nil entry))
      (goto-char (point-min))
      (search-forward "6小时")
      (should
       (eq (get-text-property (match-beginning 0) 'face) 'shadow))
      (goto-char (match-end 0))
      (should (= (current-column) (chirp--view-width))))))

(ert-deftest chirp-dm-inbox-row-inserts-ready-participant-avatar ()
  "A direct-conversation row should display its ready participant avatar."
  (let ((entry
         (appkit-directory-entry-create
          :key "conversation-1"
          :label "Alice"
          :payload
          '(:id "conversation-1" :type direct :title "Alice"
            :participants
            ((:id "42" :name "Alice"
              :avatar-url "https://example.invalid/alice.jpg")))))
        seen)
    (with-temp-buffer
      (cl-letf (((symbol-function 'appkit-current-view)
                 (lambda () :view))
                ((symbol-function 'chirp-dm-inbox--avatar-key)
                 (lambda (view conversation)
                   (should (eq view :view))
                   (should (equal (plist-get conversation :id)
                                  "conversation-1"))
                   '(xchat-avatar "42")))
                ((symbol-function 'chirp-media-avatar-resource-image)
                 (lambda (view resource-key &optional _pixel-size)
                   (setq seen (list view resource-key))
                   :avatar-image))
                ((symbol-function 'insert-image)
                 (lambda (image &optional string _area _slice)
                   (should (eq image :avatar-image))
                   (insert (or string " ")))))
        (chirp-dm-inbox--insert-item nil entry))
      (should (equal seen '(:view (xchat-avatar "42"))))
      (should (string-match-p "\\[Alice" (buffer-string)))
      (should (equal
               (get-text-property (point-min) 'chirp-dm-conversation-id)
               "conversation-1")))))

(ert-deftest chirp-dm-inbox-projection-retains-recent-activity-metadata ()
  "Inbox projection should keep recent order and honest activity metadata."
  (let* ((regular
          '(:id "regular" :type direct :title "Alice" :preview "hello"
            :updated-at-msec "20" :muted-p t :participants nil :events nil))
         (request
           '(:id "request" :type direct :title "Visitor" :preview "request"
             :updated-at-msec "21" :message-request-p t
             :participants nil :events nil))
         entries)
    (cl-letf (((symbol-function 'chirp-dm-inbox--view-user-id)
               (lambda (_view) "99")))
      (setq
       entries
       (chirp-dm-inbox--project
        :view
        (list :type 'dm-inbox :instance 1
              :items (list request regular)
              :status '(:phase idle :message nil)))))
    (should
     (equal (mapcar #'appkit-directory-entry-key entries)
            '((dm-inbox summary)
              (dm-inbox recent)
              (dm-conversation "request")
              (dm-conversation "regular"))))
    (should
     (equal (appkit-directory-entry-label (car entries))
            "2 conversations · 1 request · 1 muted"))
    (let ((request-entry (nth 2 entries)))
      (should-not (appkit-directory-entry-unread-p request-entry))
      (should
       (equal (appkit-directory-entry-section-key request-entry)
              '(dm-inbox recent)))
      (should (appkit-directory-entry-stamp request-entry)))))

(ert-deftest chirp-dm-inbox-activity-model-labels-group-sender ()
  "A group activity preview should identify its latest sender."
  (let* ((alice '(:id "42" :name "Alice" :handle "alice"))
         (self '(:id "99" :name "Me" :handle "me"))
         (event
          (chirp-dm-test--normalized-event "20" "20" "hello" "42"))
         (group
          (list :type 'group :title "Team" :preview "hello"
                :participants (list self alice) :latest-event event))
         preview)
    (cl-letf (((symbol-function 'chirp-dm-inbox--view-user-id)
               (lambda (_view) "99")))
      (setq preview (chirp-dm-inbox--preview-model :view group)))
    (should (equal (appkit-ui-one-line-preview-label preview) "Alice"))
    (should (equal (appkit-ui-one-line-preview-separator preview) ":"))
    (should (equal (appkit-ui-one-line-preview-text preview) "hello"))))

(ert-deftest chirp-dm-inbox-projects-only-the-direct-peer-avatar ()
  "A direct inbox row should use the peer avatar and retain its normalized title."
  (let* ((self
          '(:id "99" :name "Me"
            :avatar-url "https://example.invalid/me.jpg"))
         (alice
          '(:id "42" :name "Alice"
            :avatar-url "https://example.invalid/alice.jpg"))
         (direct
          (list :id "direct" :type 'direct :title "Me, Alice"
                :participants (list self alice)))
         (group
          (list :id "group" :type 'group :title "Team"
                :participants (list self alice)))
         avatar-key
         opened
         requested direct-entry)
    (cl-letf (((symbol-function 'chirp-dm-inbox--view-user-id)
               (lambda (_view) "99"))
              ((symbol-function 'chirp-media-request-xchat-avatar-resource)
               (lambda (_view identity url)
                 (push (list identity url) requested)
                 (list 'xchat-avatar identity))))
      (setq direct-entry
            (chirp-dm-inbox--conversation-entry :view direct))
      (setq avatar-key (chirp-dm-inbox--avatar-key :view direct))
      (chirp-dm-inbox--conversation-entry :view group))
    (cl-letf (((symbol-function 'chirp-dm-conversation-open)
               (lambda (conversation &rest _options)
                 (setq opened conversation))))
      (chirp-dm-inbox--activate-item nil direct-entry))
    (should
     (equal requested
            '(("42" "https://example.invalid/alice.jpg"))))
    (should (equal (appkit-directory-entry-label direct-entry) "Me, Alice"))
    (should (equal (plist-get opened :title) "Me, Alice"))
    (should (equal avatar-key '(xchat-avatar "42")))))

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
            (setf (chirp--session-xchat-user (chirp--session)) '(:id "42")
                  (chirp--session-xchat-user-id (chirp--session)) "42")
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
              (setq buffer (chirp-dm-inbox--activate-item nil entry))
              (let* ((view (with-current-buffer buffer (appkit-current-view)))
                     (state (appkit-view-state view)))
                (should (appkit-view-operation-p owner))
                (should (eq (appkit-view-operation-view owner) view))
                (should refresh)
                (funcall refresh
                         (chirp-dm-test--normalized-conversation new-event)
                         nil)
                (should bridge)
                (should (equal
                         (mapcar (lambda (event) (plist-get event :id))
                                 (chirp-dm-conversation--events state))
                         '("20")))
                (funcall bridge (list old-event new-event)
                         '(("pagination" . (("complete" . t)))))
                (with-current-buffer buffer
                  (appkit-sync-invalidations view)
                  (should (appkit-chat-timeline-node "20"))
                  (should (appkit-chat-timeline-node "30")))
                (should (equal
                         (mapcar (lambda (event) (plist-get event :id))
                                 (chirp-dm-conversation--events state))
                         '("20" "30")))))))
      (chirp-stop)
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (when (buffer-live-p request)
        (kill-buffer request)))))

(ert-deftest chirp-dm-conversation-refresh-updates-open-inbox ()
  "Canonical conversation changes should update an existing inbox row."
  (let ((chirp--app nil)
        buffers inbox-callback refresh-callback)
    (unwind-protect
        (save-window-excursion
          (let ((inbox-request
                 (generate-new-buffer " *chirp-dm-canonical-inbox*"))
                (refresh-request
                 (generate-new-buffer " *chirp-dm-canonical-refresh*")))
            (setq buffers (list inbox-request refresh-request))
            (cl-letf
                (((symbol-function 'chirp-backend-dm-inbox)
                  (lambda (callback &rest _options)
                    (setq inbox-callback callback)
                    inbox-request))
                 ((symbol-function 'chirp-backend-dm-conversation-data)
                  (lambda (_conversation-id callback &rest _options)
                    (setq refresh-callback callback)
                    refresh-request)))
              (let* ((old
                      (chirp-dm-test--normalized-event
                       "20" "20" "old preview"))
                     (fresh
                      (chirp-dm-test--normalized-event
                       "30" "30" "new preview"))
                     (inbox-buffer (chirp-dm-inbox-open))
                     (inbox-view
                      (with-current-buffer inbox-buffer
                        (appkit-current-view))))
                (push inbox-buffer buffers)
                (funcall inbox-callback
                         (list
                          (chirp-dm-test--normalized-conversation old))
                         nil)
                (let* ((conversation
                        (car
                         (plist-get
                          (appkit-view-state inbox-view) :items)))
                       (conversation-buffer
                        (chirp-dm-conversation-open
                         conversation :refresh-p t)))
                  (push conversation-buffer buffers)
                  (funcall refresh-callback
                           (chirp-dm-test--normalized-conversation
                            old fresh)
                           nil)
                  (appkit-sync-invalidations inbox-view)
                  (should (equal (plist-get conversation :preview)
                                 "new preview"))
                  (with-current-buffer inbox-buffer
                    (goto-char (point-min))
                    (should (search-forward "new preview" nil t))))))))
      (chirp-stop)
      (dolist (buffer buffers)
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(provide 'chirp-dm-inbox-test)

;;; chirp-dm-inbox-test.el ends here
