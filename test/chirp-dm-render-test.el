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

(ert-deftest chirp-dm-message-avatars-use-appkit-prefix-geometry ()
  "Message rows should project participant avatars into two-line prefixes."
  (let* ((event
          (chirp-dm-test--normalized-event
           "20" "20" "first line\nsecond line"))
         (state
          '(:participants
            ((:id "42" :name "Alice"
              :avatar-url "https://example.invalid/alice.jpg"))))
         (row
          (car (chirp-dm-render-project-events nil state (list event))))
         seen)
    (should
     (equal (appkit-chat-timeline-row-dependencies row)
            '((xchat-avatar "42"))))
    (with-temp-buffer
      (cl-letf (((symbol-function 'appkit-current-view)
                 (lambda () :view))
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
                   (should (equal options
                                  '(:pixel-size 42 :resize t)))
                   '(:header "TOP " :first-body "BOTTOM "
                     :rest-body "REST "))))
        (chirp-dm-render-print-event-row row))
      (should (equal seen '(:view (xchat-avatar "42") 42)))
      (goto-char (point-min))
      (should (equal (get-text-property (point) 'line-prefix) "TOP "))
      (should
       (equal
        (get-text-property
         0 'chirp-dm-avatar-sender-id
         (get-text-property (point) 'line-prefix))
        "42"))
      (forward-line 1)
      (should (equal (get-text-property (point) 'line-prefix) "BOTTOM "))
      (forward-line 1)
      (should (equal (get-text-property (point) 'line-prefix) "REST ")))))

(ert-deftest chirp-dm-conversation-projection-ensures-visible-row-resources ()
  "Timeline projection should ensure resources only for projected events."
  (let* ((visible
          (chirp-dm-test--normalized-event "20" "20" "visible" "42"))
         (state
          '(:participants
            ((:id "42" :name "Alice"
              :avatar-url "https://example.invalid/alice.jpg")
             (:id "99" :name "Bob"
              :avatar-url "https://example.invalid/bob.jpg"))))
         participants media rows)
    (setf (plist-get visible :attachments)
          '((:kind image :resource-key (xchat-media "20"))
            (:kind file :resource-key (xchat-media "ignored"))))
    (cl-letf (((symbol-function 'chirp-media-request-xchat-avatar-resource)
               (lambda (view identity _url)
                 (should (eq view :view))
                 (push identity participants)
                 (list 'xchat-avatar identity)))
              ((symbol-function 'chirp-dm-render--request-event-media)
               (lambda (view event)
                 (should (eq view :view))
                 (push (plist-get event :id) media)
                 '((xchat-media "20")))))
      (setq rows
            (chirp-dm-render-project-events :view state (list visible))))
    (should (equal participants '("42")))
    (should (equal media '("20")))
    (should
     (equal (appkit-chat-timeline-row-dependencies (car rows))
            '((xchat-avatar "42") (xchat-media "20"))))))

(ert-deftest chirp-dm-participant-avatar-resources-redraw-dependent-rows ()
  "Avatar completion should redraw only messages from that participant."
  (let ((chirp--app nil)
        buffer resource success printed)
    (unwind-protect
        (save-window-excursion
          (let* ((alice
                  (chirp-dm-test--normalized-event
                   "20" "20" "hello" "42"))
                 (bob
                  (chirp-dm-test--normalized-event
                   "21" "21" "hi" "99"))
                 (conversation
                  (chirp-dm-test--normalized-conversation alice bob))
                 (printer (symbol-function 'chirp-dm-render-print-event-row)))
            (setf
             (plist-get conversation :participants)
             '((:id "42" :name "Alice"
                :avatar-url "https://example.invalid/alice.jpg")
               (:id "99" :name "Bob")))
            (cl-letf (((symbol-function 'chirp-media--prefetch-enabled-p)
                       (lambda () t))
                      ((symbol-function 'appkit-media-image-cache-existing-file)
                       (lambda (_cache-base) nil))
                      ((symbol-function 'chirp-media--valid-cache-file-p)
                       (lambda (path)
                         (equal path "/tmp/chirp-dm-avatar.jpg")))
                      ((symbol-function 'appkit-media-cache-image-resource-async)
                       (lambda (requested-resource _cache-base callback
                                                   _errback &rest _options)
                         (setq resource requested-resource
                               success callback)
                         nil))
                      ((symbol-function 'chirp-dm-render-print-event-row)
                       (lambda (row)
                         (push (plist-get
                                (appkit-chat-timeline-row-payload row) :id)
                               printed)
                         (funcall printer row))))
              (setq buffer (chirp-dm-conversation-open conversation))
              (let* ((view (with-current-buffer buffer (appkit-current-view)))
                     (store
                      (appkit-app-resource-store (appkit-view-app view)))
                     (resource-key '(xchat-avatar "42")))
                (should (equal (alist-get 'url resource)
                               "https://example.invalid/alice.jpg"))
                (should (eq (plist-get (gethash resource-key store) :status)
                            'pending))
                (setq printed nil)
                (funcall success "/tmp/chirp-dm-avatar.jpg")
                (appkit-sync-invalidations view)
                (should (eq (plist-get (gethash resource-key store) :status)
                            'ready))
                (should (equal printed '("20")))))))
      (chirp-stop)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

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
                 (printer (symbol-function 'chirp-dm-render-print-event-row)))
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
                      ((symbol-function 'chirp-dm-render-print-event-row)
                       (lambda (row)
                         (push (plist-get
                                (appkit-chat-timeline-row-payload row) :id)
                               printed)
                         (funcall printer row))))
              (setq buffer (chirp-dm-conversation-open conversation))
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

(ert-deftest chirp-dm-render-requests-only-allowlisted-xchat-media ()
  "Verified attachments should not make arbitrary URLs fetchable."
  (let (sources)
    (cl-letf (((symbol-function 'chirp-media-request-image-resource)
               (lambda (_view _resource-key source &rest _options)
                 (push source sources))))
      (chirp-dm-render--request-event-media
       :view
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
