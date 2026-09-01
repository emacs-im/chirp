;;; chirp-dm-render.el --- XChat conversation presentation -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Appkit timeline projection and rendering for normalized XChat events.

;;; Code:

(require 'cl-lib)
(require 'browse-url)
(require 'url-parse)
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
(require 'chirp-backend)
(require 'chirp-media)
(require 'chirp-time)
(require 'chirp-render)

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
  "Return a visible semantic label for verified ATTACHMENT."
  (let ((name
         (when-let* ((value (plist-get attachment :name)))
           (truncate-string-to-width
            (chirp-dm-render--one-line value) 80 nil nil "…"))))
    (pcase (plist-get attachment :kind)
      ('image (if name (format "Image: %s" name) "Image"))
      ('gif (if name (format "GIF: %s" name) "GIF"))
      ('video (if name (format "Video: %s" name) "Video"))
      ('audio (if name (format "Audio: %s" name) "Audio"))
      ('file (if name (format "File: %s" name) "File"))
      ('svg (if name (format "SVG: %s" name) "SVG"))
      ('url (or name "Link"))
      ('post "Open attached post")
      ('unified-card (or name "Open attached card"))
      ('money (or name "Payment attachment"))
      (_ (or name "Attachment")))))

(defun chirp-dm-render--safe-attachment-url-p (value)
  "Return non-nil when VALUE is a safe user-activated attachment URL."
  (and (stringp value)
       (<= (length value) 8192)
       (not (string-match-p "[[:cntrl:]]" value))
       (condition-case nil
           (let ((parsed (url-generic-parse-url value)))
             (and (equal (url-type parsed) "https")
                  (null (url-user parsed))
                  (null (url-password parsed))
                  (memq (url-port parsed) '(nil 443))))
         (error nil))))

(defun chirp-dm-render--post-resource-key (attachment)
  "Return the tweet resource key represented by ATTACHMENT, or nil."
  (when-let* ((url (plist-get attachment :url))
              ((chirp-dm-render--safe-attachment-url-p url))
              ((string-match
                "\\`https://x\\.com/i/status/\\([0-9]+\\)\\'" url)))
    (list 'xchat-post (match-string 1 url))))

(defun chirp-dm-render--finish-post-resource
    (app resource-key entry status &optional tweet)
  "Finish APP post ENTRY for RESOURCE-KEY with STATUS and optional TWEET."
  (when (and (appkit-app-live-p app)
             (eq entry
                 (gethash resource-key (appkit-app-resource-store app))))
    (setf (plist-get entry :status) status
          (plist-get entry :tweet) tweet)
    (dolist (view (plist-get entry :views))
      (when (appkit-view-live-p view)
        (appkit-request-sync view :resource resource-key :position t)))))

(defun chirp-dm-render--request-post-resource (view attachment)
  "Ensure ATTACHMENT's embedded post resource for Appkit VIEW."
  (when-let* (((appkit-view-live-p view))
              (resource-key
               (chirp-dm-render--post-resource-key attachment)))
    (let* ((app (appkit-view-app view))
           (store (appkit-app-resource-store app))
           (current (gethash resource-key store)))
      (cond
       ((eq (plist-get current :status) 'ready))
       ((eq (plist-get current :status) 'pending)
        (cl-pushnew view (plist-get current :views) :test #'eq))
       ((eq (plist-get current :status) 'failed))
       (t
        (let ((entry
               (list :status 'pending :tweet nil
                     :views (cl-adjoin view (plist-get current :views)
                                       :test #'eq))))
          (puthash resource-key entry store)
          (chirp-backend-tweet
           (cadr resource-key)
           (lambda (tweet _envelope)
             (chirp-dm-render--finish-post-resource
              app resource-key entry 'ready tweet))
           (lambda (_message)
             (chirp-dm-render--finish-post-resource
              app resource-key entry 'failed))))))
      resource-key)))

(defun chirp-dm-render--attachment-block (attachment)
  "Return ATTACHMENT as one Appkit semantic provider-object block."
  (appkit-markup-object-block
   attachment
   (list
    (appkit-markup-paragraph
     (list
      (appkit-markup-text
       (chirp-dm-render--attachment-label attachment)))))))

(defun chirp-dm-render--message-document (event)
  "Return EVENT's text and attachments as one semantic document."
  (let* ((document (plist-get event :document))
         (attachments (plist-get event :attachments))
         (blocks
          (append
           (and document (appkit-markup-document-blocks document))
           (mapcar #'chirp-dm-render--attachment-block attachments))))
    (dotimes (_index (max 0 (- (or (plist-get event :attachment-count) 0)
                               (length attachments))))
      (setq blocks
            (append
             blocks
             (list
              (chirp-dm-render--attachment-block
               '(:kind unknown))))))
    (and blocks (appkit-markup-document blocks))))

(defun chirp-dm-render--insert-attachment-action
    (label action help-echo)
  "Insert attachment LABEL with optional ACTION and HELP-ECHO."
  (let ((start (point)))
    (insert label)
    (if action
        (appkit-ui-add-action
         start (point) action :help-echo help-echo
         :face 'appkit-markup-link-face)
      (add-face-text-property start (point) 'shadow 'append))))

(defun chirp-dm-render--insert-attachment-object (view node)
  "Insert XChat attachment object NODE using resources in VIEW."
  (let* ((attachment
          (and (appkit-markup-object-block-p node)
               (appkit-markup-object-block-value node)))
         (kind (and (listp attachment) (plist-get attachment :kind)))
         (resource-key
          (and attachment (plist-get attachment :resource-key)))
         (media-resource-p
          (and resource-key (plist-get attachment :media-hash)))
         (label (chirp-dm-render--attachment-label attachment)))
    (cond
     ((eq kind 'post)
      (let* ((url (plist-get attachment :url))
             (post-key (chirp-dm-render--post-resource-key attachment))
             (entry
              (and post-key
                   (appkit-view-live-p view)
                   (gethash post-key
                            (appkit-app-resource-store
                             (appkit-view-app view))))))
        (if-let* ((tweet (and (eq (plist-get entry :status) 'ready)
                              (plist-get entry :tweet))))
            (chirp-render-insert-tweet-card tweet)
          (chirp-dm-render--insert-attachment-action
           (if (eq (plist-get entry :status) 'pending)
               "Attached post loading…"
             label)
           (and (chirp-dm-render--safe-attachment-url-p url)
                (lambda () (browse-url url)))
           (and (chirp-dm-render--safe-attachment-url-p url) url)))))
     ((and media-resource-p (memq kind '(image gif svg)))
      (pcase (chirp-media-insert-image-resource
              view resource-key
              :alternate-text (format "[%s]" label)
              :help-echo "Open decrypted attachment in Emacs")
        ('rendered nil)
        ('pending
         (insert (propertize (format "%s loading…" label) 'face 'shadow)))
        (_
         (insert
          (propertize (format "%s unavailable" label) 'face 'shadow)))))
     (media-resource-p
      (let ((file (chirp-media-xchat-resource-file view resource-key))
            (status (chirp-media-xchat-resource-status view resource-key)))
        (chirp-dm-render--insert-attachment-action
         (if (eq status 'pending) (format "%s loading…" label) label)
         (and file
              (lambda ()
                (chirp-media-open-xchat-resource view resource-key)))
         (and file "Open decrypted attachment in Emacs"))))
     ((chirp-dm-render--safe-attachment-url-p
       (plist-get attachment :url))
      (let ((url (plist-get attachment :url)))
        (chirp-dm-render--insert-attachment-action
         label (lambda () (browse-url url)) url)))
     (t
      (chirp-dm-render--insert-attachment-action label nil nil)))))

(defun chirp-dm-render--insert-message-content (view event context)
  "Insert EVENT content and projected CONTEXT using Appkit VIEW resources."
  (let* ((document (chirp-dm-render--message-document event))
         (content-label
          (pcase (plist-get event :content-kind)
            ('reaction "Reaction: ")
            ('reaction-removed "Reaction removed: ")
            ('edit "Edited message: ")
            (_ nil)))
         (reply (plist-get context :reply))
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
                    document
                    :final-newline-p nil
                    :interactive-p t
                    :object-inserter
                    (lambda (node)
                      (chirp-dm-render--insert-attachment-object
                       view node)))))
        (setq inserted-p
              (or inserted-p content-label (< start end)))))
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
  "Request resources carried by verified EVENT for VIEW."
  (cl-loop
   for attachment in (plist-get event :attachments)
   for resource-key = (plist-get attachment :resource-key)
   for source = (cl-find-if
                 (lambda (value)
                   (and (stringp value) (not (string-empty-p value))))
                 (list (plist-get attachment :preview-url)
                       (plist-get attachment :url)))
   for post-key =
   (and (eq (plist-get attachment :kind) 'post)
        (chirp-dm-render--request-post-resource view attachment))
   when
   (or
    post-key
    (and (plist-get attachment :media-hash)
         (chirp-media-request-xchat-attachment-resource
          view resource-key attachment
          :conversation-id (plist-get event :conversation-id)
          :key-version
          (or (plist-get event :key-version)
              (plist-get event :conversation-key-version))))
    (and (memq (plist-get attachment :kind) '(image gif svg media))
         (chirp-media-request-xchat-image-resource
          view resource-key source :name (plist-get attachment :name))))
   collect (or post-key resource-key)))

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
