;;; chirp-dm-live.el --- XChat realtime delivery -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; One Appkit application-owned XChat websocket, binary event dispatch, exact
;; connection fencing, keepalive, fallback refresh, and bounded reconnect.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'appkit-core)
(require 'appkit-view)
(require 'chirp-backend)
(require 'chirp-core)
(require 'chirp-dm-conversation)
(require 'chirp-dm-inbox)
(require 'chirp-dm-state)
(require 'chirp-x)
(require 'chirp-xchat)

(defcustom chirp-dm-live-reconnect-delay 5
  "Seconds before reconnecting a closed XChat live websocket."
  :type '(number :tag "Seconds")
  :group 'chirp)

(defconst chirp-dm-live--keepalive-interval 30
  "Seconds between official XChat websocket keepalive frames.")

(cl-defstruct (chirp-dm-live--service
               (:constructor chirp-dm-live--service-create))
  "Application-owned XChat live transport state."
  app
  (generation 0)
  owner
  socket
  keepalive-timer
  reconnect-timer
  refresh-timer
  pending-inbox-p
  pending-conversations
  stopped-p)

(defun chirp-dm-live--current-p (service owner &optional socket)
  "Return non-nil when SERVICE still owns OWNER and optional SOCKET."
  (and (chirp-dm-live--service-p service)
       (not (chirp-dm-live--service-stopped-p service))
       (appkit-app-live-p (chirp-dm-live--service-app service))
       (eq owner (chirp-dm-live--service-owner service))
       (= (or (plist-get owner :generation) -1)
          (chirp-dm-live--service-generation service))
       (let ((owned (plist-get owner :socket)))
         (or (null socket) (null owned) (eq socket owned)))))

(defun chirp-dm-live--cancel-timer (timer)
  "Cancel TIMER when it remains live."
  (when (timerp timer)
    (cancel-timer timer)))

(defun chirp-dm-live--cancel-keepalive (service)
  "Cancel SERVICE's current keepalive timer."
  (chirp-dm-live--cancel-timer
   (chirp-dm-live--service-keepalive-timer service))
  (setf (chirp-dm-live--service-keepalive-timer service) nil))

(defun chirp-dm-live--retire-owner (service owner)
  "Retire exact connection OWNER from SERVICE."
  (when (chirp-dm-live--current-p service owner)
    (cl-incf (chirp-dm-live--service-generation service))
    (setf (chirp-dm-live--service-owner service) nil
          (chirp-dm-live--service-socket service) nil)
    (chirp-dm-live--cancel-keepalive service)
    t))

(defun chirp-dm-live--matching-conversation-view (service conversation)
  "Return one live view in SERVICE for canonical CONVERSATION."
  (let (matched)
    (maphash
     (lambda (_id view)
       (when (and (null matched) (appkit-view-live-p view))
         (let ((state (appkit-view-state view)))
           (when (and (eq (plist-get state :type) 'dm-conversation)
                      (eq (plist-get state :conversation) conversation))
             (setq matched view)))))
     (appkit-app-view-registry (chirp-dm-live--service-app service)))
    matched))

(defun chirp-dm-live--accept-event (service event)
  "Merge normalized live EVENT through canonical SERVICE state."
  (if-let* ((conversation (chirp-dm-state-accept-live-event event)))
      (when-let* ((view
                   (chirp-dm-live--matching-conversation-view
                    service conversation)))
        (chirp-dm-conversation-accept-live-event view conversation))
    (setf (chirp-dm-live--service-pending-inbox-p service) t)
    (chirp-dm-live--dispatch-fallback service)))

(defun chirp-dm-live--collect-fallback (service)
  "Collect current DM surfaces into SERVICE's fallback refresh set."
  (maphash
   (lambda (_id view)
     (when (appkit-view-live-p view)
       (let ((state (appkit-view-state view)))
         (pcase (plist-get state :type)
           ('dm-inbox
            (setf (chirp-dm-live--service-pending-inbox-p service) t))
           ('dm-conversation
            (puthash
             (plist-get (plist-get state :conversation) :id) t
             (chirp-dm-live--service-pending-conversations service)))))))
   (appkit-app-view-registry (chirp-dm-live--service-app service))))

(defun chirp-dm-live--views-of-type (service type)
  "Return SERVICE's live views whose state has TYPE."
  (let (views)
    (maphash
     (lambda (_id view)
       (when (and (appkit-view-live-p view)
                  (eq (plist-get (appkit-view-state view) :type) type))
         (push view views)))
     (appkit-app-view-registry (chirp-dm-live--service-app service)))
    views))

(defun chirp-dm-live--dispatch-inbox-fallback (service)
  "Try SERVICE's pending inbox fallback refresh."
  (when (chirp-dm-live--service-pending-inbox-p service)
    (let ((views (chirp-dm-live--views-of-type service 'dm-inbox))
          started)
      (if (null views)
          (setf (chirp-dm-live--service-pending-inbox-p service) nil)
        (while (and views (not started))
          (setq started
                (chirp-dm-inbox-refresh-live-view (pop views))))
        (when started
          (setf (chirp-dm-live--service-pending-inbox-p service) nil))))))

(defun chirp-dm-live--dispatch-conversation-fallbacks (service)
  "Try SERVICE's pending per-conversation fallback refreshes."
  (let ((pending (chirp-dm-live--service-pending-conversations service))
        completed)
    (maphash
     (lambda (conversation-id _value)
       (let ((views (chirp-dm-live--views-of-type service 'dm-conversation))
             candidates started)
         (dolist (view views)
           (when (equal
                  conversation-id
                  (plist-get
                   (plist-get (appkit-view-state view) :conversation) :id))
             (push view candidates)))
         (cond
          ((null candidates) (push conversation-id completed))
          (t
           (while (and candidates (not started))
             (setq started
                   (chirp-dm-conversation-refresh-live-view
                    (pop candidates))))
           (when started (push conversation-id completed))))))
     pending)
    (dolist (conversation-id completed)
      (remhash conversation-id pending))))

(defun chirp-dm-live--fallback-pending-p (service)
  "Return non-nil when SERVICE still has a fallback refresh pending."
  (or (chirp-dm-live--service-pending-inbox-p service)
      (> (hash-table-count
          (chirp-dm-live--service-pending-conversations service))
         0)))

(defun chirp-dm-live--refresh-timer-fire (service)
  "Retry pending fallback refreshes for SERVICE."
  (setf (chirp-dm-live--service-refresh-timer service) nil)
  (when (and (not (chirp-dm-live--service-stopped-p service))
             (appkit-app-live-p (chirp-dm-live--service-app service)))
    (chirp-dm-live--dispatch-fallback service)))

(defun chirp-dm-live--dispatch-fallback (service)
  "Dispatch or reschedule SERVICE's coalesced fallback refreshes."
  (chirp-dm-live--dispatch-inbox-fallback service)
  (chirp-dm-live--dispatch-conversation-fallbacks service)
  (when (and (chirp-dm-live--fallback-pending-p service)
             (not (timerp (chirp-dm-live--service-refresh-timer service))))
    (setf (chirp-dm-live--service-refresh-timer service)
          (run-at-time 1 nil #'chirp-dm-live--refresh-timer-fire service))))

(defun chirp-dm-live--handle-frame (service owner socket opcode payload)
  "Handle one live OPCODE and PAYLOAD for exact SERVICE OWNER and SOCKET."
  (when (chirp-dm-live--current-p service owner socket)
    (condition-case err
        (let ((decoded (chirp-backend-dm-live-frame opcode payload)))
          (pcase (plist-get decoded :kind)
            ('event
             (chirp-dm-live--accept-event service (plist-get decoded :event)))
            ((or 'pull 'batch)
             (chirp-dm-live--collect-fallback service)
             (chirp-dm-live--dispatch-fallback service))
            ((or 'keepalive 'instruction) nil)
            (kind (error "Unknown XChat live event kind: %S" kind))))
      (error
       (message "Chirp XChat live frame failed: %s"
                (error-message-string err))))))

(defun chirp-dm-live--keepalive-fire (service owner socket)
  "Send a keepalive for exact SERVICE OWNER and SOCKET."
  (when (chirp-dm-live--current-p service owner socket)
    (condition-case err
        (chirp-x-chat-live-send-bytes
         socket (chirp-xchat-live-keepalive-frame))
      (error
       (chirp-dm-live--handle-error service owner socket 'keepalive err)))))

(defun chirp-dm-live--start-keepalive (service owner socket)
  "Start SERVICE keepalives for exact OWNER and SOCKET."
  (chirp-dm-live--cancel-keepalive service)
  (when (chirp-dm-live--current-p service owner socket)
    (setf (chirp-dm-live--service-keepalive-timer service)
          (run-with-timer
           chirp-dm-live--keepalive-interval
           chirp-dm-live--keepalive-interval
           #'chirp-dm-live--keepalive-fire service owner socket))))

(defun chirp-dm-live--schedule-reconnect (service)
  "Schedule one bounded reconnect for SERVICE."
  (unless (or (chirp-dm-live--service-stopped-p service)
              (timerp (chirp-dm-live--service-reconnect-timer service)))
    (setf (chirp-dm-live--service-reconnect-timer service)
          (run-at-time
           (max 0.1 chirp-dm-live-reconnect-delay) nil
           #'chirp-dm-live--reconnect-fire service))))

(defun chirp-dm-live--handle-close (service owner socket)
  "Handle closure of exact SERVICE OWNER and SOCKET."
  (when (and (chirp-dm-live--current-p service owner socket)
             (chirp-dm-live--retire-owner service owner))
    (chirp-dm-live--schedule-reconnect service)))

(defun chirp-dm-live--handle-error (service owner socket type error-data)
  "Handle TYPE failure for exact SERVICE OWNER and SOCKET.

ERROR-DATA is intentionally not formatted because websocket errors can retain
the short-lived token-bearing constructor URL."
  (when (and (chirp-dm-live--current-p service owner socket)
             (chirp-dm-live--retire-owner service owner))
    (message "Chirp XChat live websocket failed during %s (%S)"
             type (car-safe error-data))
    (chirp-x-chat-live-close socket)
    (chirp-dm-live--schedule-reconnect service)))

(defun chirp-dm-live--publish-socket (service owner token)
  "Connect SERVICE OWNER using short-lived TOKEN."
  (when (chirp-dm-live--current-p service owner)
    (let (socket published-p)
      (unwind-protect
          (progn
            (setq socket
                  (chirp-x-chat-live-open
                   token
                   #'ignore
                   (lambda (message-socket opcode payload)
                     (chirp-dm-live--handle-frame
                      service owner message-socket opcode payload))
                   (lambda (closed-socket)
                     (chirp-dm-live--handle-close
                      service owner closed-socket))
                   (lambda (error-socket type error-data)
                     (chirp-dm-live--handle-error
                      service owner error-socket type error-data))))
            (when (chirp-dm-live--current-p service owner socket)
              (setf (plist-get owner :socket) socket
                    (chirp-dm-live--service-socket service) socket
                    published-p t)
              (chirp-dm-live--start-keepalive service owner socket))
            published-p)
        (unless published-p
          (when (chirp-dm-live--current-p service owner)
            (chirp-dm-live--retire-owner service owner))
          (chirp-x-chat-live-close socket))))))

(defun chirp-dm-live--connect-error (service owner message)
  "Settle SERVICE OWNER token failure described by MESSAGE."
  (when (and (chirp-dm-live--current-p service owner)
             (chirp-dm-live--retire-owner service owner))
    (message "Chirp XChat live connection failed: %s"
             (replace-regexp-in-string "[\r\n]+" "  " message))
    (chirp-dm-live--schedule-reconnect service)))

(defun chirp-dm-live--connect (service)
  "Start one token and websocket attempt for SERVICE."
  (unless (or (chirp-dm-live--service-stopped-p service)
              (chirp-dm-live--service-owner service)
              (chirp-dm-live--service-socket service))
    (let* ((generation
            (cl-incf (chirp-dm-live--service-generation service)))
           (owner (list :generation generation :socket nil)))
      (setf (chirp-dm-live--service-owner service) owner)
      (chirp-backend-dm-live-token
       (lambda (token _envelope)
         (chirp-dm-live--publish-socket service owner token))
       :errback
       (lambda (message)
         (chirp-dm-live--connect-error service owner message))
       :owner (chirp-dm-live--service-app service)))))

(defun chirp-dm-live--reconnect-fire (service)
  "Run SERVICE's scheduled reconnect attempt."
  (setf (chirp-dm-live--service-reconnect-timer service) nil)
  (when (and (not (chirp-dm-live--service-stopped-p service))
             (appkit-app-live-p (chirp-dm-live--service-app service)))
    (chirp-dm-live--connect service)))

(defun chirp-dm-live-stop (service)
  "Stop application-owned XChat live SERVICE exactly once."
  (when (and (chirp-dm-live--service-p service)
             (not (chirp-dm-live--service-stopped-p service)))
    (setf (chirp-dm-live--service-stopped-p service) t)
    (cl-incf (chirp-dm-live--service-generation service))
    (let ((socket (chirp-dm-live--service-socket service)))
      (setf (chirp-dm-live--service-owner service) nil
            (chirp-dm-live--service-socket service) nil)
      (chirp-dm-live--cancel-keepalive service)
      (chirp-dm-live--cancel-timer
       (chirp-dm-live--service-reconnect-timer service))
      (chirp-dm-live--cancel-timer
       (chirp-dm-live--service-refresh-timer service))
      (setf (chirp-dm-live--service-reconnect-timer service) nil
            (chirp-dm-live--service-refresh-timer service) nil)
      (chirp-x-chat-live-close socket))))

(defun chirp-dm-live-ensure ()
  "Ensure the current Chirp app owns one XChat realtime service."
  (let* ((app (chirp-app))
         (state (appkit-app-state app))
         (service (chirp--session-dm-live state)))
    (unless (and (chirp-dm-live--service-p service)
                 (not (chirp-dm-live--service-stopped-p service)))
      (setq service
            (chirp-dm-live--service-create
             :app app
             :pending-conversations (make-hash-table :test #'equal)))
      (setf (chirp--session-dm-live state) service)
      (appkit-register-handle app 'dm-live service #'chirp-dm-live-stop)
      (chirp-dm-live--connect service))
    service))

(provide 'chirp-dm-live)

;;; chirp-dm-live.el ends here
