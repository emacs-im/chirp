;;; chirp-dm-live-test.el --- Tests for XChat realtime delivery -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Exercise exact websocket ownership and canonical live-event publication.

;;; Code:

(add-to-list 'load-path
             (file-name-directory (or load-file-name buffer-file-name)))

(require 'ert)
(require 'cl-lib)
(require 'chirp)
(require 'chirp-dm-live)
(require 'chirp-dm-test-helper)

(ert-deftest chirp-dm-live-service-owns-one-exact-websocket ()
  "The Chirp app should own one fenced websocket and close it on shutdown."
  (let ((chirp--app nil)
        opened-options closed reconnect)
    (cl-letf
        (((symbol-function 'chirp-backend-dm-live-token)
          (lambda (callback &rest _options)
            (funcall callback "header.payload.signature" nil)
            'token-request))
         ((symbol-function 'chirp-x-chat-live-open)
          (lambda (_token on-open on-message on-close on-error)
            (setq opened-options
                  (list on-open on-message on-close on-error))
            'live-socket))
         ((symbol-function 'chirp-x-chat-live-close)
          (lambda (socket) (push socket closed)))
         ((symbol-function 'run-with-timer)
          (lambda (&rest _arguments) 'keepalive-timer))
         ((symbol-function 'run-at-time)
          (lambda (&rest arguments)
            (setq reconnect arguments)
            'reconnect-timer))
         ((symbol-function 'timerp)
          (lambda (value)
            (memq value '(keepalive-timer reconnect-timer)))))
      (unwind-protect
          (let ((service (chirp-dm-live-ensure)))
            (should (eq (chirp-dm-live--service-socket service)
                        'live-socket))
            (should (eq service
                        (chirp--session-dm-live
                         (appkit-app-state (chirp-app)))))
            (funcall (nth 2 opened-options) 'stale-socket)
            (should-not reconnect))
        (chirp-stop)))
    (should (memq 'live-socket closed))))

(ert-deftest chirp-dm-live-event-updates-open-canonical-conversation ()
  "A websocket event should appear through the open canonical conversation."
  (let ((chirp--app nil)
        buffer)
    (unwind-protect
        (let* ((old
                (chirp-dm-test--normalized-event "20" "20" "old"))
               (conversation
                (chirp-dm-test--normalized-conversation old))
               (encoded
                (chirp-dm-test--event
                 :sequence "31" :message-id "message-31" :sender-id "42"
                 :conversation-id "conversation-1" :text "live message"))
               (event (chirp-xchat-decode-event encoded)))
          (setq buffer (chirp-dm-conversation-open conversation))
          (let ((service
                 (chirp-dm-live--service-create
                  :app (chirp-app)
                  :pending-conversations (make-hash-table :test #'equal))))
            (chirp-dm-live--accept-event service event))
          (with-current-buffer buffer
            (let ((view (appkit-current-view)))
              (appkit-sync-invalidations view)
              (let* ((state (appkit-view-state view))
                     (events
                      (plist-get (plist-get state :conversation) :events)))
                (should
                 (equal (mapcar (lambda (item) (plist-get item :id)) events)
                        '("20" "31")))
                (should
                 (string-match-p "live message" (buffer-string)))))))
      (chirp-stop)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest chirp-dm-live-event-promotes-its-canonical-inbox-row ()
  "A live event should move its existing canonical conversation to recent."
  (let ((chirp--app nil)
        buffer)
    (unwind-protect
        (let* ((first-event
                (chirp-dm-test--normalized-event
                 "20" "20" "first" "42" "conversation-1"))
               (second-event
                (chirp-dm-test--normalized-event
                 "21" "21" "second" "42" "conversation-2"))
               (first
                (chirp-dm-test--normalized-conversation first-event))
               (second
                (chirp-dm-test--normalized-conversation second-event))
               (live
                (chirp-dm-test--normalized-event
                 "31" "31" "new second" "42" "conversation-2")))
          (setf (plist-get second :id) "conversation-2")
          (cl-letf
              (((symbol-function 'chirp-backend-dm-inbox)
                (lambda (callback &rest _options)
                  (funcall callback (list first second) nil)
                  'request)))
            (setq buffer (chirp-dm-inbox-open)))
          (with-current-buffer buffer
            (let* ((view (appkit-current-view))
                   (state (appkit-view-state view)))
              (should
               (equal (mapcar (lambda (item) (plist-get item :id))
                              (plist-get state :items))
                      '("conversation-1" "conversation-2")))
              (should (chirp-dm-state-accept-live-event live))
              (appkit-sync-invalidations view)
              (should
               (equal (mapcar (lambda (item) (plist-get item :id))
                              (plist-get state :items))
                      '("conversation-2" "conversation-1")))
              (should (string-match-p "new second" (buffer-string))))))
      (chirp-stop)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(provide 'chirp-dm-live-test)

;;; chirp-dm-live-test.el ends here
