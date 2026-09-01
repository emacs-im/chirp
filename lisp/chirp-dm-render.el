;;; chirp-dm-render.el --- XChat conversation presentation -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Appkit timeline projection and rendering for normalized XChat events.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'appkit-chat-avatar)
(require 'appkit-chat-history)
(require 'appkit-chat-ins)
(require 'appkit-chat-timeline)
(require 'appkit-name-color)
(require 'appkit-markup)
(require 'appkit-markup-ui)
(require 'appkit-ui)
(require 'appkit-view)
(require 'chirp-core)
(require 'chirp-media)
(require 'chirp-time)

(defun chirp-dm-render--one-line (text)
  "Return TEXT collapsed into one trimmed display line."
  (string-trim
   (replace-regexp-in-string "[[:space:]\n\r]+" " " (or text ""))))

(defun chirp-dm-render--format-time (milliseconds)
  "Return a compact local timestamp for MILLISECONDS, or an empty string."
  (if (and (stringp milliseconds)
           (string-match-p "\\`[0-9]+\\'" milliseconds))
      (chirp-time-format-compact
       (seconds-to-time (/ (string-to-number milliseconds) 1000)))
    ""))

(defun chirp-dm-render--status (state)
  "Return canonical status plist from conversation STATE."
  (or (plist-get state :status)
      (error "Direct-message conversation state has no status")))

(defun chirp-dm-render--participant (state sender-id)
  "Return SENDER-ID's participant from conversation STATE."
  (cl-find sender-id (plist-get state :participants)
           :key (lambda (participant) (plist-get participant :id))
           :test #'equal))

(defun chirp-dm-render--participant-label (participant)
  "Return PARTICIPANT's display label, or nil."
  (or (plist-get participant :name)
      (and-let* ((handle (plist-get participant :handle)))
        (concat "@" handle))))

(defun chirp-dm-render--event-sender-label (state event)
  "Return display sender label for EVENT in conversation STATE."
  (or (chirp-dm-render--participant-label
       (chirp-dm-render--participant state (plist-get event :sender-id)))
      "Unknown sender"))

(defun chirp-dm-render--event-system-label (event)
  "Return a passive label for non-message XChat EVENT."
  (pcase (plist-get event :kind)
    ('message-delete "A message was deleted")
    ('conversation-key-change "Conversation encryption keys changed")
    ('group-change "Group details changed")
    ('conversation-metadata-change "Conversation details changed")
    ('conversation-delete "Conversation was deleted")
    ('mark-read "Conversation was marked read")
    ('mark-unread "Conversation was marked unread")
    ('pin-message "A message was pinned")
    ('unpin-message "A message was unpinned")
    (kind (format "XChat event: %s" kind))))

(defun chirp-dm-render--insert-reply-preview (reply)
  "Insert verified REPLY preview chrome."
  (let* ((document (plist-get reply :document))
         (text
          (chirp-dm-render--one-line
           (if document (appkit-markup-plain-text document) ""))))
    (insert
     (propertize
      (cond
       ((not (string-empty-p text))
        (format "↪ %s" (truncate-string-to-width text 80 nil nil "…")))
       ((> (or (plist-get reply :attachment-count) 0) 0)
        "↪ [Attachment reply]")
       (t "↪ [Reply]"))
      'face 'shadow))))

(defun chirp-dm-render--attachment-label (attachment)
  "Return a passive fallback label for verified ATTACHMENT."
  (pcase (plist-get attachment :kind)
    ('image "[Image unavailable]")
    ('gif "[GIF unavailable]")
    ('video "[Video attachment]")
    ('audio "[Audio attachment]")
    ('file "[File attachment]")
    ('svg "[SVG unavailable]")
    ('url "[Link attachment]")
    ('post "[Post attachment]")
    ('unified-card "[Card attachment]")
    ('money "[Payment attachment]")
    (_ "[Attachment]")))

(defun chirp-dm-render--insert-attachment (view attachment)
  "Insert verified ATTACHMENT using VIEW's shared media workflow."
  (let ((status
         (when (memq (plist-get attachment :kind) '(image gif svg))
           (chirp-media-insert-image-resource
            view (plist-get attachment :resource-key)
            :alternate-text "[image]" :help-echo "Open image in Emacs"))))
    (pcase status
      ('rendered nil)
      ('pending
       (insert (propertize "[Image loading…]" 'face 'shadow)))
      (_
       (insert (propertize
                (chirp-dm-render--attachment-label attachment)
                'face 'shadow))))))

(defun chirp-dm-render--insert-message-content (view event context)
  "Insert EVENT content and projected CONTEXT using Appkit VIEW resources."
  (let* ((document (plist-get event :document))
         (content-label
          (pcase (plist-get event :content-kind)
            ('reaction "Reaction: ")
            ('reaction-removed "Reaction removed: ")
            ('edit "Edited message: ")
            (_ nil)))
         (reply (plist-get context :reply))
         (attachments (plist-get event :attachments))
         inserted-p)
    (when reply
      (chirp-dm-render--insert-reply-preview reply)
      (setq inserted-p t))
    (when document
      (when inserted-p (insert "\n"))
      (when content-label
        (insert content-label))
      (pcase-let ((`(,start . ,end)
                   (appkit-markup-ui-insert-document
                    document :final-newline-p nil)))
        (setq inserted-p
              (or inserted-p content-label (< start end)))))
    (dolist (attachment attachments)
      (when inserted-p (insert "\n"))
      (chirp-dm-render--insert-attachment view attachment)
      (setq inserted-p t))
    (dotimes (_index (max 0 (- (or (plist-get event :attachment-count) 0)
                               (length attachments))))
      (when inserted-p (insert "\n"))
      (insert (propertize "[Attachment]" 'face 'shadow))
      (setq inserted-p t))
    (unless inserted-p
      (insert (if (plist-get event :decrypted-p)
                  "[Verified non-text message]"
                "[Message content unavailable]")))))

(defun chirp-dm-render--message-avatar-prefixes (view context)
  "Return shared two-line avatar prefixes for CONTEXT rendered in VIEW."
  (let* ((pixel-size (appkit-chat-avatar-two-line-pixel-size))
         (resource-key (plist-get context :avatar-key))
         (image
          (and resource-key
               (chirp-media-avatar-resource-image
                view resource-key pixel-size)))
         (prefixes
          (appkit-chat-avatar-prefixes
           image "@" :pixel-size pixel-size :resize t))
         (properties
          (list 'help-echo "Participant avatar"
                'chirp-dm-avatar-sender-id
                (plist-get context :sender-id))))
    (dolist (key '(:header :first-body))
      (let ((prefix (copy-sequence (plist-get prefixes key))))
        (when (> (length prefix) 0)
          (add-text-properties 0 (length prefix) properties prefix))
        (setq prefixes (plist-put prefixes key prefix))))
    prefixes))

(defun chirp-dm-render--insert-message-row (event context timestamp)
  "Insert one message EVENT with projected CONTEXT and TIMESTAMP."
  (let* ((view (appkit-current-view))
         (prefixes (chirp-dm-render--message-avatar-prefixes view context))
         (header-prefix (plist-get prefixes :header))
         (body-rest-prefix (plist-get prefixes :rest-body))
         (body-prefix
          (appkit-ui-make-prefix-state
           (plist-get prefixes :first-body) body-rest-prefix))
         (sender-face
          (appkit-name-color-face (plist-get context :sender-id)))
         (header-start (point))
         body-start)
    (insert
     (propertize (or (plist-get context :sender-label) "Unknown sender")
                 'face (if sender-face (list sender-face 'bold) 'bold)))
    (unless (string-empty-p timestamp)
      (appkit-chat-ins-insert-right-aligned-text
       timestamp (chirp--view-width)
       :face 'shadow
       :left-prefix-width (string-width header-prefix)
       :right-edge-margin 0))
    (insert "\n")
    (appkit-ui-apply-line-prefix
     header-start (point)
     (appkit-ui-make-prefix-state header-prefix body-rest-prefix))
    (setq body-start (point))
    (chirp-dm-render--insert-message-content view event context)
    (insert "\n")
    (appkit-ui-apply-line-prefix body-start (point) body-prefix)
    (insert "\n")))

(defun chirp-dm-render-header (state)
  "Return generated header text for conversation STATE."
  (let ((title (chirp-dm-render--one-line (plist-get state :title))))
    (concat
     (propertize
      (format "Direct message: %s\n"
              (if (string-empty-p title) "Conversation" title))
      'face 'bold)
     "\n")))

(defun chirp-dm-render-footer (state)
  "Return generated footer text for conversation STATE."
  (let* ((status (chirp-dm-render--status state))
         (phase (plist-get status :phase))
         (message (plist-get status :message))
         (loading-text
          (pcase (appkit-chat-history-loading)
            ('older "loading older messages…")
            ('refresh "refreshing…")
            (_ nil))))
    (concat
     "\n"
     (appkit-chat-history-delimiter-string
      (max 20 (min 80 (window-width)))
      :loading-text loading-text)
     "\n"
     (when (plist-get state :older-stalled-p)
       (propertize "Older history made no progress.\n" 'face 'warning))
     (when (eq phase 'error)
       (propertize (format "Unable to load messages: %s\n" message)
                   'face 'error))
     (when-let* ((send-error (plist-get state :send-error)))
       (propertize (format "Unable to send message: %s\n" send-error)
                   'face 'error)))))

(defun chirp-dm-render-print-event-row (row)
  "Insert one projected XChat event ROW."
  (let* ((event (appkit-chat-timeline-row-payload row))
         (context (appkit-chat-timeline-row-context row))
         (start (point))
         (timestamp (chirp-dm-render--format-time
                     (plist-get event :created-at-msec)))
         (properties
          (list 'read-only t
                'front-sticky '(read-only)
                'rear-nonsticky '(read-only)
                'chirp-dm-message-id (plist-get event :id)
                'chirp-dm-event event)))
    (if (eq (plist-get event :kind) 'message)
        (chirp-dm-render--insert-message-row event context timestamp)
      (insert (propertize
               (format "— %s —" (chirp-dm-render--event-system-label event))
               'face 'shadow))
      (unless (string-empty-p timestamp)
        (appkit-chat-ins-insert-right-aligned-text
         timestamp (chirp--view-width)
         :face 'shadow
         :right-edge-margin 0))
      (insert "\n\n"))
    (add-text-properties start (point) properties)))

(defun chirp-dm-render--request-event-media (view event)
  "Request verified image resources carried by EVENT on behalf of VIEW."
  (cl-loop
   for attachment in (plist-get event :attachments)
   when (memq (plist-get attachment :kind) '(image gif svg))
   when (chirp-media-request-xchat-image-resource
         view
         (plist-get attachment :resource-key)
         (or (plist-get attachment :preview-url)
             (plist-get attachment :url))
         :name (plist-get attachment :name))
   collect (plist-get attachment :resource-key)))

(defun chirp-dm-render--event-resource-keys (view state event)
  "Ensure and return resources affecting EVENT from STATE in VIEW."
  (when (eq (plist-get event :kind) 'message)
    (let* ((participant
            (chirp-dm-render--participant
             state (plist-get event :sender-id)))
           (avatar-key
            (chirp-media-request-xchat-avatar-resource
             view
             (plist-get participant :id)
             (plist-get participant :avatar-url)))
           (media-keys
            (chirp-dm-render--request-event-media view event)))
      (delete-dups (delq nil (cons avatar-key media-keys))))))

(defun chirp-dm-render-project-events (view state events)
  "Project XChat EVENTS from STATE and ensure their resources for VIEW."
  (appkit-chat-timeline-project
   events
   (lambda (event) (plist-get event :id))
   :context-function
   (lambda (_previous event)
     (when (eq (plist-get event :kind) 'message)
       (let ((participant
              (chirp-dm-render--participant
               state (plist-get event :sender-id))))
         (list :sender-label
               (chirp-dm-render--event-sender-label state event)
               :sender-id (plist-get event :sender-id)
               :avatar-key
               (chirp-media-xchat-avatar-resource-key
                (plist-get participant :id)
                (plist-get participant :avatar-url))
               :reply
               (when (plist-get event :reply-p)
                 (list :document (plist-get event :reply-document)
                       :attachment-count
                       (plist-get event :reply-attachment-count)))))))
   :dependencies-function
   (lambda (event)
     (chirp-dm-render--event-resource-keys view state event))))

(provide 'chirp-dm-render)

;;; chirp-dm-render.el ends here
