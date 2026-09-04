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
(require 'appkit-media-card)
(require 'appkit-media-resource)
(require 'appkit-chat-timeline)
(require 'appkit-name-color)
(require 'appkit-markup)
(require 'appkit-markup-ui)
(require 'appkit-ui)
(require 'appkit-presentation)
(require 'chirp-core)
(require 'chirp-backend)
(require 'chirp-dm-state)
(require 'chirp-media)
(require 'chirp-time)
(require 'chirp-render)
(require 'chirp-url)

(defface chirp-dm-reaction
  '((((class color) (background dark))
     (:foreground "white" :background "#35353c"))
    (((class color) (background light))
     (:foreground "#014d98" :background "#c6cbd1"))
    (t :inherit mode-line-inactive))
  "Face used for XChat reaction chips not selected by the current user."
  :group 'chirp)

(defface chirp-dm-reaction-selected
  '((((class color) (min-colors 88))
     (:inherit chirp-dm-reaction
      :foreground "white" :background "RoyalBlue3"))
    (t :inherit chirp-dm-reaction :inverse-video t))
  "Face used for XChat reaction chips selected by the current user."
  :group 'chirp)


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

(defun chirp-dm-render--view-user-id (view)
  "Return VIEW's current XChat user ID, or nil."
  (when (appkit-view-live-p view)
    (chirp--session-xchat-user-id
     (appkit-app-state (appkit-view-app view)))))

(defun chirp-dm-render--view-user (view)
  "Return VIEW's authenticated XChat user profile, or nil."
  (when (appkit-view-live-p view)
    (chirp--session-xchat-user
     (appkit-app-state (appkit-view-app view)))))

(defun chirp-dm-render--event-participant (view state event)
  "Resolve EVENT's participant from conversation STATE or VIEW identity."
  (let ((sender-id (plist-get event :sender-id)))
    (or (chirp-dm-render--participant state sender-id)
        (when-let* ((user (chirp-dm-render--view-user view))
                    ((equal (plist-get user :id) sender-id)))
          user))))

(defun chirp-dm-render--participant-label (participant)
  "Return PARTICIPANT's display label, or nil."
  (or (plist-get participant :name)
      (and-let* ((handle (plist-get participant :handle)))
        (concat "@" handle))))

(defun chirp-dm-render--event-sender-label (participant event self-id)
  "Return EVENT's sender label from PARTICIPANT and SELF-ID."
  (or (chirp-dm-render--participant-label participant)
      (and (equal (plist-get event :sender-id) self-id) "You")
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

(defun chirp-dm-render--attachment-title (attachment)
  "Return ATTACHMENT's bounded provider title."
  (or (when-let* ((value (plist-get attachment :name)))
        (truncate-string-to-width
         (chirp-dm-render--one-line value) 80 nil nil "…"))
      (pcase (plist-get attachment :kind)
        ('post "Attached post")
        ('url "Link")
        ('unified-card "Attached card")
        ('money "Payment attachment")
        (_ "Attachment"))))

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
              (tweet-id (chirp-url-tweet-id url)))
    (list 'xchat-post tweet-id)))

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
  "Return ATTACHMENT as one semantic provider-object block."
  (let* ((title (chirp-dm-render--attachment-title attachment))
         (url (plist-get attachment :url))
         (fallback
          (if (chirp-dm-render--safe-attachment-url-p url)
              (appkit-markup-link url (list (appkit-markup-text title)))
            (appkit-markup-text title))))
    (appkit-markup-object-block
     attachment
     (list (appkit-markup-paragraph (list fallback))))))

(defun chirp-dm-render--attachment-document (event)
  "Return EVENT's attachments as one semantic document, or nil."
  (let* ((attachments (plist-get event :attachments))
         (blocks
          (mapcar #'chirp-dm-render--attachment-block attachments)))
    (dotimes (_index (max 0 (- (or (plist-get event :attachment-count) 0)
                               (length attachments))))
      (setq blocks
            (append
             blocks
             (list
              (chirp-dm-render--attachment-block
               '(:kind unknown))))))
    (and blocks (appkit-markup-document blocks))))

(defun chirp-dm-render--link-action (url)
  "Return a safe browser action for semantic URL, or nil."
  (when (chirp-dm-render--safe-attachment-url-p url)
    (lambda () (browse-url url))))

(defun chirp-dm-render--attachment-card-kind (attachment)
  "Return Appkit's presentation kind for ATTACHMENT."
  (pcase (plist-get attachment :kind)
    ((or 'image 'gif 'svg) 'image)
    ('video 'video)
    ('audio 'audio)
    (_ 'file)))

(defun chirp-dm-render--attachment-open-kind (attachment)
  "Return Appkit's resource-opening kind for ATTACHMENT."
  (pcase (plist-get attachment :kind)
    ((or 'image 'gif 'svg) 'image)
    ('video 'video)
    (_ 'file)))

(defun chirp-dm-render--attachment-details (attachment)
  "Return Appkit media-card details for ATTACHMENT."
  (let ((width (plist-get attachment :width))
        (height (plist-get attachment :height))
        (size (plist-get attachment :filesize-bytes)))
    (delq
     nil
     (list
      (and (integerp width) (> width 0)
           (integerp height) (> height 0)
           (format "%d×%d" width height))
      (and (integerp size) (>= size 0)
           (file-size-human-readable size 'iec " " "B"))))))

(defun chirp-dm-render--insert-image-preview
    (view resource-key title prefix-state)
  "Insert VIEW's ready image RESOURCE-KEY under PREFIX-STATE using TITLE."
  (let ((start (point)))
    (when (eq
           (chirp-media-insert-image-resource
            view resource-key
            :alternate-text (format "[%s]" title)
            :help-echo "Open decrypted attachment in Emacs")
           'rendered)
      (unless (bolp)
        (insert "\n"))
      (appkit-ui-apply-line-prefix start (point) prefix-state))))

(defun chirp-dm-render--insert-media-card (view attachment)
  "Insert ATTACHMENT through Appkit's shared media-card UI in VIEW."
  (let* ((resource-key (plist-get attachment :resource-key))
         (resource
          (if resource-key
              (chirp-media-xchat-resource view resource-key attachment)
            (appkit-media-resource-create
             :name (plist-get attachment :name))))
         (file (alist-get 'file resource))
         (status
          (and resource-key
               (chirp-media-xchat-resource-status view resource-key)))
         (open-kind (chirp-dm-render--attachment-open-kind attachment))
         (open-action
          (and file
               (lambda ()
                 (appkit-media-open-resource
                  resource :kind open-kind :owner view
                  :client-label "Chirp XChat"))))
         (context
          (appkit-media-card-context-create
           :payload attachment
           :kind (chirp-dm-render--attachment-card-kind attachment)
           :title (chirp-dm-render--attachment-title attachment)
           :open-action open-action))
         (status-text
          (pcase status
            ('pending
             (appkit-chat-ins-media-transfer-status-text
              '(:status downloading)))
            ('failed
             (appkit-chat-ins-media-transfer-status-text
              '(:status error))))))
    (appkit-chat-ins-insert-media-card
     :kind (plist-get context :kind)
     :title (plist-get context :title)
     :details (chirp-dm-render--attachment-details attachment)
     :status status-text
     :title-face 'bold
     :meta-face 'shadow
     :context context
     :open-help-echo "Open decrypted attachment"
     :body-inserter
     (and resource-key
          (eq (plist-get context :kind) 'image)
          (eq status 'ready)
          (lambda (prefix-state)
            (chirp-dm-render--insert-image-preview
             view resource-key (plist-get context :title) prefix-state))))))

(defun chirp-dm-render--insert-attachment-object
    (view node prefix-state)
  "Insert attachment NODE in VIEW under Appkit PREFIX-STATE."
  (let* ((attachment
          (and (appkit-markup-object-block-p node)
               (appkit-markup-object-block-value node)))
         (kind (and (listp attachment) (plist-get attachment :kind)))
         (url (and attachment (plist-get attachment :url))))
    (cond
     ((eq kind 'post)
      (let* ((post-key (chirp-dm-render--post-resource-key attachment))
             (entry
              (and post-key
                   (appkit-view-live-p view)
                   (gethash post-key
                            (appkit-app-resource-store
                             (appkit-view-app view))))))
        (if-let* ((tweet (and (eq (plist-get entry :status) 'ready)
                              (plist-get entry :tweet))))
            (chirp-render-insert-tweet-card tweet :prefix prefix-state)
          (appkit-chat-ins-insert-prefixed-line
           (if (eq (plist-get entry :status) 'pending)
               "Attached post · loading…"
             (chirp-dm-render--attachment-title attachment))
           :prefix prefix-state
           :face 'shadow
           :action (chirp-dm-render--link-action url)
           :help-echo url))))
     ((and (chirp-dm-render--safe-attachment-url-p url)
           (not (memq kind '(image gif video audio file svg))))
      (appkit-chat-ins-insert-prefixed-line
       (chirp-dm-render--attachment-title attachment)
       :prefix prefix-state
       :action (chirp-dm-render--link-action url)
       :help-echo url))
     (t
      (chirp-dm-render--insert-media-card view attachment)))))

(defun chirp-dm-render--insert-message-content (event context)
  "Insert EVENT's primary content and projected CONTEXT."
  (let* ((document (plist-get event :document))
         (content-label
          (and (eq (plist-get event :content-kind) 'edit)
               "Edited message: "))
         (reply (plist-get context :reply))
         inserted-p)
    (when reply
      (chirp-dm-render--insert-reply-preview reply)
      (setq inserted-p t))
    (when document
      (when inserted-p
        (insert "\n"))
      (when content-label
        (insert content-label))
      (pcase-let ((`(,start . ,end)
                   (appkit-markup-ui-insert-document
                    document
                    :final-newline-p nil
                    :interactive-p t
                    :link-action #'chirp-dm-render--link-action)))
        (setq inserted-p
              (or inserted-p content-label (< start end)))))
    (when (and (not inserted-p)
               (zerop (or (plist-get event :attachment-count) 0)))
      (insert (if (plist-get event :decrypted-p)
                  "[Verified non-text message]"
                "[Message content unavailable]"))
      (setq inserted-p t))
    inserted-p))

(defun chirp-dm-render--insert-message-attachments
    (view event prefix-state)
  "Insert EVENT attachments in VIEW using Appkit PREFIX-STATE."
  (when-let* ((document (chirp-dm-render--attachment-document event)))
    (let ((appkit-ui-card-indent-prefix-state prefix-state)
          (appkit-ui-card-indent-prefix
           (appkit-ui-prefix-string prefix-state nil "  ")))
      (pcase-let ((`(,start . ,end)
                   (appkit-markup-ui-insert-document
                    document
                    :final-newline-p nil
                    :interactive-p t
                    :object-inserter
                    (lambda (node)
                      (chirp-dm-render--insert-attachment-object
                       view node prefix-state)))))
        (< start end)))))

(defun chirp-dm-render--reaction-label (reaction)
  "Return compact chip text for normalized REACTION."
  (let ((count (or (plist-get reaction :count) 0)))
    (format " %s%s "
            (plist-get reaction :emoji)
            (if (> count 1) (format " %d" count) ""))))

(defun chirp-dm-render--insert-reactions (context prefix-state)
  "Insert CONTEXT reaction chips beneath a message using PREFIX-STATE."
  (appkit-chat-ins-insert-reaction-line
   (plist-get context :reactions)
   :prefix prefix-state
   :selected-face 'chirp-dm-reaction-selected
   :unselected-face 'chirp-dm-reaction
   :label-function #'chirp-dm-render--reaction-label
   :selected-p-function
   (lambda (reaction) (plist-get reaction :selected-p))))


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
    (when (chirp-dm-render--insert-message-content event context)
      (insert "\n")
      (appkit-ui-apply-line-prefix body-start (point) body-prefix))
    (chirp-dm-render--insert-message-attachments view event body-prefix)
    (chirp-dm-render--insert-reactions context body-prefix)
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
        (progn
          (chirp-dm-render--insert-message-row event context timestamp)
          (add-text-properties start (point) properties))
      (appkit-chat-ins-insert-divider-row
       (string-join
        (delq nil
              (list
               (chirp-dm-render--event-system-label event)
               (unless (string-empty-p timestamp) timestamp)))
        " · ")
       'shadow (chirp--view-width) properties)
      (insert "\n"))))

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
            (chirp-dm-render--event-participant view state event))
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
   (chirp-dm-state-visible-events events)
   (lambda (event) (plist-get event :id))
   :context-function
   (lambda (_previous event)
     (when (eq (plist-get event :kind) 'message)
       (let* ((participant
               (chirp-dm-render--event-participant view state event))
              (self-id (chirp-dm-render--view-user-id view)))
         (list :sender-label
               (chirp-dm-render--event-sender-label
                participant event self-id)
               :sender-id (plist-get event :sender-id)
               :avatar-key
               (chirp-media-xchat-avatar-resource-key
                (plist-get participant :id)
                (plist-get participant :avatar-url))
               :reply
               (when (plist-get event :reply-p)
                 (list :document (plist-get event :reply-document)
                       :attachment-count
                       (plist-get event :reply-attachment-count)))
               :reactions
               (mapcar
                (lambda (reaction)
                  (let ((projected (copy-tree reaction)))
                    (setf (plist-get projected :selected-p)
                          (and self-id
                               (member self-id
                                       (plist-get reaction :senders))
                               t))
                    projected))
                (plist-get event :reactions))))))
   :dependencies-function
   (lambda (event)
     (chirp-dm-render--event-resource-keys view state event))))

(provide 'chirp-dm-render)

;;; chirp-dm-render.el ends here
