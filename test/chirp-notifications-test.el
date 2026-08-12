;;; chirp-notifications-test.el --- Tests for Chirp notifications -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'chirp-notifications)

(defun chirp-notifications-test--timeline-payload (entries)
  "Return a notification timeline payload containing ENTRIES."
  (let ((value `(("instructions" . ((("entries" . ,entries)))))))
    (dolist (key (reverse '("data" "viewer_v2" "user_results" "result"
                            "notification_timeline" "timeline"))
                  value)
      (setq value (list (cons key value))))))

(ert-deftest chirp-backend-notifications-use-direct-timeline ()
  "Notification requests should adapt X GraphQL activities and pagination."
  (let* ((notification
          '(("entryId" . "notification-entry")
            ("content" .
             (("itemContent" .
               (("id" . "notification-1")
                ("notification_icon" . "heart_icon")
                ("rich_message" . (("text" . "Alice liked your post")))
                ("timestamp_ms" . "1700000000000")
                ("template" .
                 (("target_objects" .
                   ((("tweet_results" .
                      (("result" .
                        (("rest_id" . "123")
                         ("legacy" . (("full_text" . "Post"))))))))))))))))))
         (cursor
          '(("entryId" . "cursor-bottom")
            ("content" . (("cursorType" . "Bottom")
                           ("value" . "next")))))
         (payload (chirp-notifications-test--timeline-payload
                   (list notification cursor)))
         operation variables result envelope failure)
    (cl-letf
        (((symbol-function 'chirp-x-graphql-request)
          (lambda (request-operation request-variables callback &rest _options)
            (setq operation request-operation
                  variables request-variables)
            (funcall callback payload))))
      (chirp-backend-notifications
       (lambda (notifications response-envelope)
         (setq result notifications
               envelope response-envelope))
       (lambda (message)
         (setq failure message))
       12))
    (should-not failure)
    (should (equal (plist-get operation :name) "NotificationsTimeline"))
    (should (equal variables
                   '(("timeline_type" . "All") ("count" . 12))))
    (should
     (equal result
            '((("id" . "notification-1")
               ("type" . "like")
               ("message" . "Alice liked your post")
               ("timestampMs" . "1700000000000")
               ("tweetId" . "123")))))
    (should (equal (chirp-backend-envelope-next-cursor envelope) "next"))))

(ert-deftest chirp-notifications-first-check-is-baseline-and-new-items-notify-once ()
  "The first response seeds ids; later unseen activities notify once."
  (let ((chirp-notifications-mode t)
        (chirp-notifications--initialized nil)
        (chirp-notifications--seen-ids nil)
        (chirp-notifications--checking t)
        notified)
    (cl-letf (((symbol-function 'chirp-notifications--notify)
               (lambda (notification)
                 (push (chirp-get notification "id") notified))))
      (chirp-notifications--handle-success
       '((("id" . "n2")) (("id" . "n1"))) nil)
      (should-not notified)
      (should chirp-notifications--initialized)
      (chirp-notifications--handle-success
       '((("id" . "n3")) (("id" . "n2"))) nil)
      (should (equal notified '("n3")))
      (chirp-notifications--handle-success
       '((("id" . "n3")) (("id" . "n2"))) nil)
      (should (equal notified '("n3"))))))

(ert-deftest chirp-notifications-platform-boundary-strips-text-properties ()
  "Native backends should receive plain strings."
  (let ((system-type 'darwin)
        captured)
    (cl-letf (((symbol-function 'chirp-notifications--notify-macos)
               (lambda (title body)
                 (setq captured (list title body)))))
      (chirp-notifications--notify
       `(("type" . "reply")
         ("message" . ,(propertize "hello" 'face 'bold)))))
    (should (equal captured '("Chirp · Reply" "hello")))
    (should-not (text-properties-at 0 (cadr captured)))))

(ert-deftest chirp-notifications-macos-passes-content-as-arguments ()
  "AppleScript source should not embed notification content."
  (let (command)
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest args)
                 (setq command (plist-get args :command)))))
      (chirp-notifications--notify-macos "A \"title\"" "line 1\nline 2"))
    (should (equal (last command 2) '("A \"title\"" "line 1\nline 2")))
    (should-not (string-match-p "A \"title\"" (nth 2 command)))
    (should-not (string-match-p "line 1" (nth 4 command)))))

(ert-deftest chirp-notifications-stop-app-cancels-polling-timer ()
  "Stopping Chirp should disable app-owned notification polling."
  (let ((chirp--app nil)
        (chirp-notifications-mode nil)
        (chirp-notifications--timer nil)
        (chirp-notifications--timer-handle nil))
    (unwind-protect
        (cl-letf (((symbol-function 'chirp-backend-notifications)
                   (lambda (&rest _args) nil)))
          (chirp-notifications-mode 1)
          (should (appkit-handle-p chirp-notifications--timer-handle))
          (chirp-stop)
          (should-not chirp-notifications-mode)
          (should-not chirp-notifications--timer)
          (should-not chirp-notifications--timer-handle))
      (when (appkit-app-live-p chirp--app)
        (chirp-stop))
      (when chirp-notifications-mode
        (chirp-notifications-mode -1))
      (setq chirp-notifications--timer nil
            chirp-notifications--timer-handle nil))))

(provide 'chirp-notifications-test)

;;; chirp-notifications-test.el ends here
