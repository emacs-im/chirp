;;; chirp-actions.el --- Transient write actions for chirp -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Provide Chirp's compose buffer and interactive read/write actions.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'transient)
(require 'appkit-core)
(require 'appkit-compose)
(require 'appkit-chat-compose)
(require 'appkit-evil)
(require 'appkit-media-image)
(require 'chirp-core)
(require 'chirp-backend)
(require 'chirp-media)

(declare-function chirp-home "chirp" ())
(declare-function chirp-following "chirp" ())
(declare-function chirp-bookmarks "chirp" ())
(declare-function chirp-likes "chirp" ())
(declare-function chirp-list "chirp" (&optional list-id))
(declare-function chirp-me "chirp" ())
(declare-function chirp-unsent-drafts "chirp-unsent" ())
(declare-function chirp-unsent-scheduled "chirp-unsent" ())
(declare-function chirp-edit-history-open-at-point "chirp-edit-history" ())

;;; Options

(defcustom chirp-compose-temporary-directory
  (expand-file-name "compose/" (locate-user-emacs-file "chirp/"))
  "Directory used for temporary clipboard image attachments.

Files created here are owned by the compose buffer and removed when the draft is
cancelled, the attachment is removed, or the send completes."
  :type 'directory
  :group 'chirp)

(defcustom chirp-translation-language "zh"
  "Language code used by `chirp-translate-at-point'."
  :type 'string
  :group 'chirp)

;;; Constants

(defconst chirp-compose--reply-audience-choices
  '((everyone . "Everyone")
    (community . "People you follow")
    (verified . "Verified accounts")
    (byinvitation . "Accounts you mention"))
  "Reply audience symbols and their compose status labels.")

;;; Variables

(defvar-local chirp-compose-kind nil
  "Compose action kind for the current Chirp compose buffer.")

(defvar-local chirp-compose-target-id nil
  "Reply or quote target id for the current Chirp compose buffer.")

(defvar-local chirp-compose-target-handle nil
  "Target handle shown in the current Chirp compose buffer.")

(defvar-local chirp-compose-target-url nil
  "Target URL shown in the current Chirp compose buffer.")

(defvar-local chirp-compose-source-buffer nil
  "Source Chirp view buffer that opened the current compose buffer.")

(defvar-local chirp-compose-temp-attachments nil
  "Temporary attachment paths owned by the current compose buffer.")

(defvar-local chirp-compose--submit-temps nil
  "Temporary attachment paths held for the current in-flight submit.")

(defvar-local chirp-compose-draft-id nil
  "X server draft ID for the current compose buffer, or nil.")

(defvar-local chirp-compose-scheduled-id nil
  "X scheduled-post ID for the current compose buffer, or nil.")

(defvar-local chirp-compose-execute-at nil
  "Unix seconds last scheduled for the current compose buffer, or nil.")

(defvar-local chirp-compose-reply-audience nil
  "Reply audience for the current post or quote draft.

`everyone' omits a conversation-control rule.  Replies leave this nil.")

(defvar-local chirp-compose-write-outcome nil
  "Last uncertain Chirp write outcome, or nil.

Only `unknown' persists because it changes whether repeating a write is safe.")

(defvar-local chirp-compose--mention-cache nil
  "User completion results keyed by query in the current draft.")

(defvar-local chirp-compose--mention-pending nil
  "Mention queries currently being fetched for the current draft.")

(defvar-local chirp-compose--mention-timer nil
  "Idle timer that starts mention completion prefetching.")

(defvar-local chirp-compose--shown-weight nil
  "Last weighted length rendered in the compose status fields.")

(defvar-local chirp-compose--chrome-timer nil
  "Idle timer that refreshes compose status after body edits.")

;;; Compose Mode

(defvar-keymap chirp-compose-mode-map
  :doc "Keymap for `chirp-compose-mode'."
  "C-c C-c" #'chirp-compose-send
  "C-c C-s" #'chirp-compose-save
  "C-c C-t" #'chirp-compose-schedule
  "C-c C-k" #'chirp-compose-cancel
  "C-c C-a" #'chirp-compose-attach-image
  "C-c C-v" #'chirp-compose-paste-image
  "C-c C-d" #'chirp-compose-remove-image
  "C-c C-e" #'chirp-compose-describe-image
  "C-c C-n" #'chirp-compose-add-post
  "C-c C-p" #'chirp-compose-remove-post
  "M-TAB" #'completion-at-point)

(define-derived-mode chirp-compose-mode appkit-chat-compose-mode "Chirp-Compose"
  "Major mode for composing Chirp posts."
  (setq-local completion-ignore-case t)
  (setq-local chirp-compose--mention-cache nil)
  (setq-local chirp-compose--mention-pending nil)
  (setq-local chirp-compose--mention-timer nil)
  (setq-local chirp-compose--shown-weight nil)
  (setq-local chirp-compose--chrome-timer nil)
  (appkit-compose-setup
   :snapshot-function #'chirp-compose--draft
   :source-bounds-function #'appkit-chatbuf-input-region-bounds)
  (add-hook 'completion-at-point-functions
            #'chirp-compose-mention-completion-at-point nil t)
  (add-hook 'post-command-hook #'chirp-compose--after-command nil t)
  (add-hook 'kill-buffer-hook #'chirp-compose--cancel-mention-prefetch nil t)
  (add-hook 'kill-buffer-hook #'chirp-compose--cancel-chrome-refresh nil t)
  (appkit-evil-normalize-keymaps)
  (visual-line-mode 1))

(defun chirp-compose--setup-evil ()
  "Install optional Evil bindings for compose buffers."
  (when appkit-evil-enable-integration
    (appkit-evil-set-initial-states '(chirp-compose-mode) 'insert)
    (appkit-evil-normalize-buffers '(chirp-compose-mode))))

(chirp-compose--setup-evil)

(with-eval-after-load 'evil
  (chirp-compose--setup-evil))

;;;; Mention Completion

(defun chirp-compose--mention-bounds ()
  "Return handle bounds at point when composing an @mention."
  (let ((body-start (appkit-chat-compose-body-start-position))
        (body-end (appkit-chat-compose-body-end-position)))
    (when (and body-start body-end
               (<= body-start (point) body-end))
      (save-restriction
        (narrow-to-region body-start body-end)
        (save-match-data
          (when (looking-back
                 "\\(?:\\`\\|[^[:word:]_@]\\)@\\([A-Za-z0-9_]+\\)"
                 (point-min))
            (cons (match-beginning 1) (point))))))))

(defun chirp-compose--mention-query ()
  "Return the mention query at point, or nil."
  (when-let* ((bounds (chirp-compose--mention-bounds))
              (query (buffer-substring-no-properties
                      (car bounds) (cdr bounds)))
              ((not (string-empty-p query))))
    query))

(defun chirp-compose--cancel-mention-prefetch ()
  "Cancel the current draft's pending mention prefetch timer."
  (when (timerp chirp-compose--mention-timer)
    (cancel-timer chirp-compose--mention-timer))
  (setq chirp-compose--mention-timer nil))

(defun chirp-compose--store-mention-candidates
    (buffer query users _envelope)
  "Store QUERY results from USERS in compose BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (derived-mode-p 'chirp-compose-mode)
        (setq chirp-compose--mention-pending
              (delete query chirp-compose--mention-pending))
        (let ((candidates
               (delete-dups
                (delq nil
                      (mapcar (lambda (user)
                                (plist-get user :handle))
                              users)))))
          (setq chirp-compose--mention-cache
                (cons (cons query candidates)
                      (assoc-delete-all
                       query chirp-compose--mention-cache))))))))

(defun chirp-compose--mention-prefetch-failed (buffer query message)
  "Finish failed mention QUERY in BUFFER with MESSAGE."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (derived-mode-p 'chirp-compose-mode)
        (setq chirp-compose--mention-pending
              (delete query chirp-compose--mention-pending))
        (message "Chirp mention completion failed: %s" message)))))

(defun chirp-compose--prefetch-mention (buffer query)
  "Fetch mention QUERY for compose BUFFER when it remains relevant."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq chirp-compose--mention-timer nil)
      (when (and (derived-mode-p 'chirp-compose-mode)
                 (equal query (chirp-compose--mention-query))
                 (not (assoc-string query chirp-compose--mention-cache t))
                 (not (member query chirp-compose--mention-pending)))
        (push query chirp-compose--mention-pending)
        (chirp-backend-search-users
         query
         (apply-partially
          #'chirp-compose--store-mention-candidates buffer query)
         (apply-partially
          #'chirp-compose--mention-prefetch-failed buffer query))))))

(defun chirp-compose--schedule-mention-prefetch ()
  "Schedule nonblocking completion prefetch for the mention at point."
  (chirp-compose--cancel-mention-prefetch)
  (when-let* ((query (chirp-compose--mention-query))
              ((not (assoc-string query chirp-compose--mention-cache t)))
              ((not (member query chirp-compose--mention-pending))))
    (setq chirp-compose--mention-timer
          (run-with-idle-timer
           0.15 nil #'chirp-compose--prefetch-mention
           (current-buffer) query))))

(defun chirp-compose--cancel-chrome-refresh ()
  "Cancel the pending compose status refresh timer."
  (when (timerp chirp-compose--chrome-timer)
    (cancel-timer chirp-compose--chrome-timer))
  (setq chirp-compose--chrome-timer nil))

(defun chirp-compose--refresh-chrome (buffer)
  "Refresh BUFFER chrome when the current body's weighted length changed."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq chirp-compose--chrome-timer nil)
      (when (and (derived-mode-p 'chirp-compose-mode)
                 (not (appkit-compose-operation-active-p)))
        (let ((weight (chirp-backend-tweet-weighted-length
                       (appkit-chat-compose-body))))
          (unless (eql weight chirp-compose--shown-weight)
            (setq chirp-compose--shown-weight weight)
            (force-mode-line-update)))))))

(defun chirp-compose--schedule-chrome-refresh ()
  "Schedule a status refresh after the current body's length changes."
  (chirp-compose--cancel-chrome-refresh)
  (unless (appkit-compose-operation-active-p)
    (setq chirp-compose--chrome-timer
          (run-with-idle-timer
           0.1 nil #'chirp-compose--refresh-chrome (current-buffer)))))

(defun chirp-compose--after-command ()
  "Refresh compose chrome and prefetch mentions after a command."
  (chirp-compose--schedule-mention-prefetch)
  (chirp-compose--schedule-chrome-refresh))

(defun chirp-compose-mention-completion-at-point ()
  "Complete the user handle following @ at point from prefetched results."
  (when-let* ((bounds (chirp-compose--mention-bounds))
              (start (car bounds))
              (end (cdr bounds))
              (query (buffer-substring-no-properties start end))
              (cached (assoc-string query chirp-compose--mention-cache t)))
    (list start end (cdr cached) :exclusive 'no)))

(defun chirp-compose--temp-directory ()
  "Return the directory used for temporary compose attachments."
  (let ((dir (file-name-as-directory
              (expand-file-name chirp-compose-temporary-directory))))
    (make-directory dir t)
    dir))

;;; Actions

(defun chirp-actions--tweet-at-point ()
  "Return the tweet entry at point, or signal a user error."
  (let ((entry (chirp-entry-at-point)))
    (if (eq (plist-get entry :kind) 'tweet)
        entry
      (user-error "Current entry is not a tweet"))))

(defun chirp-actions--tweet-id-at-point ()
  "Return the current tweet id, or signal a user error."
  (or (plist-get (chirp-actions--tweet-at-point) :id)
      (user-error "Current tweet has no id")))

(defun chirp-actions--user-at-point ()
  "Return the current user entry, or signal a user error."
  (let* ((entry (chirp-entry-at-point))
         (handle (or (plist-get entry :handle)
                     (plist-get entry :author-handle)
                     (chirp-author-handle-at-point))))
    (if handle
        (list :kind 'user
              :handle (string-remove-prefix "@" handle)
              :profile-url (format "https://x.com/%s"
                                   (string-remove-prefix "@" handle)))
      (user-error "No profile available at point"))))

(defun chirp-actions--unknown-write-outcome-p (message)
  "Return non-nil when MESSAGE reports an unknown write outcome."
  (and (stringp message)
       (string-prefix-p "X write outcome is unknown" message)))

(defun chirp-actions-show-error (message)
  "Show MESSAGE as a condensed action failure."
  (let ((condensed (replace-regexp-in-string "[\r\n]+" "  " message)))
    (if (chirp-actions--unknown-write-outcome-p condensed)
        (display-warning 'chirp condensed :warning)
      (message "Chirp action failed: %s" condensed))))

(defun chirp-actions--refresh-current-view ()
  "Refresh the current Chirp view."
  (when chirp--refresh-function
    (funcall chirp--refresh-function)))

(defun chirp-actions--refresh-buffer (buffer)
  "Refresh BUFFER when it is a live Chirp view."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (chirp-actions--refresh-current-view))))

(defun chirp-actions--refresh-user-buffer-if-needed (buffer)
  "Refresh BUFFER after a user action when it is a profile or user list."
  (when (and (buffer-live-p buffer)
             (with-current-buffer buffer
               (or chirp--profile-handle
                   (eq (plist-get (chirp-entry-at-point) :kind) 'user))))
    (chirp-actions--refresh-buffer buffer)))

;;;; Dispatch

(defun chirp-actions--perform (args on-success &optional on-error)
  "Run backend action ARGS and call ON-SUCCESS with the decoded payload.

When ON-ERROR is non-nil, call it with the human-readable error message."
  (chirp-backend-request
   args
   (lambda (data envelope)
     (chirp-backend-clear-cache)
     (funcall on-success data envelope))
   (or on-error
       #'chirp-actions-show-error)))

;;;; Tweet State

(defun chirp-actions--apply-state (buffer tweet-id state-key state-value count-key)
  "Update tweet state in BUFFER for TWEET-ID.

STATE-KEY is set to STATE-VALUE and COUNT-KEY is adjusted when needed."
  (chirp-set-tweet-state-override tweet-id state-key state-value)
  (chirp-update-tweet-by-id
   buffer
   tweet-id
   (lambda (tweet)
     (let ((old-state (chirp-boolean-value (plist-get tweet state-key))))
       (plist-put tweet state-key state-value)
       (unless (eq old-state state-value)
         (plist-put tweet
                    count-key
                    (chirp-adjust-count
                     (plist-get tweet count-key)
                     (if state-value 1 -1))))))
   t))

(defun chirp-actions--bookmarks-view-p (buffer)
  "Return non-nil when BUFFER is currently showing bookmarks."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (string= chirp--view-title "Bookmarks"))))

(defun chirp-actions--set-state (state-key command count-key state-value success-message)
  "Set STATE-KEY for the current tweet using COMMAND.

COUNT-KEY is adjusted locally to reflect STATE-VALUE.  Display SUCCESS-MESSAGE
when the request succeeds."
  (let* ((tweet-id (chirp-actions--tweet-id-at-point))
         (buffer (current-buffer)))
    (chirp-actions--perform
     (list command tweet-id)
     (lambda (_data _envelope)
       (chirp-actions--apply-state buffer tweet-id state-key state-value count-key)
       (when (and (eq state-key :bookmarked-p)
                  (not state-value)
                  (chirp-actions--bookmarks-view-p buffer))
         (chirp-actions--refresh-buffer buffer))
       (message "%s" success-message)))))

(defun chirp-actions--toggle-state (state-key command-on command-off count-key
                                              success-on success-off)
  "Toggle the current tweet STATE-KEY using COMMAND-ON and COMMAND-OFF.

Adjust COUNT-KEY and display SUCCESS-ON or SUCCESS-OFF for the resulting state."
  (let* ((tweet (chirp-actions--tweet-at-point))
         (current-state (chirp-boolean-value (plist-get tweet state-key)))
         (next-state (not current-state))
         (command (if next-state command-on command-off))
         (success-message (if next-state success-on success-off)))
    (chirp-actions--set-state state-key command count-key next-state success-message)))

(defun chirp-like-at-point ()
  "Like the tweet at point."
  (interactive)
  (chirp-actions--set-state :liked-p "like" :like-count t "Liked."))

(defun chirp-unlike-at-point ()
  "Unlike the tweet at point."
  (interactive)
  (chirp-actions--set-state :liked-p "unlike" :like-count nil "Like removed."))

(defun chirp-toggle-like-at-point ()
  "Toggle like state for the tweet at point."
  (interactive)
  (chirp-actions--toggle-state
   :liked-p
   "like"
   "unlike"
   :like-count
   "Liked."
   "Like removed."))

(defun chirp-bookmark-at-point ()
  "Bookmark the tweet at point."
  (interactive)
  (chirp-actions--set-state :bookmarked-p "bookmark" :bookmark-count t "Bookmarked."))

(defun chirp-unbookmark-at-point ()
  "Remove the current tweet from bookmarks."
  (interactive)
  (chirp-actions--set-state :bookmarked-p "unbookmark" :bookmark-count nil "Bookmark removed."))

(defun chirp-toggle-bookmark-at-point ()
  "Toggle bookmark state for the tweet at point."
  (interactive)
  (chirp-actions--toggle-state
   :bookmarked-p
   "bookmark"
   "unbookmark"
   :bookmark-count
   "Bookmarked."
   "Bookmark removed."))

(defun chirp-retweet-at-point ()
  "Retweet the tweet at point."
  (interactive)
  (chirp-actions--set-state :retweeted-p "retweet" :retweet-count t "Retweeted."))

(defun chirp-unretweet-at-point ()
  "Undo the retweet at point."
  (interactive)
  (chirp-actions--set-state :retweeted-p "unretweet" :retweet-count nil "Retweet removed."))

(defun chirp-toggle-retweet-at-point ()
  "Toggle retweet state for the tweet at point."
  (interactive)
  (chirp-actions--toggle-state
   :retweeted-p
   "retweet"
   "unretweet"
   :retweet-count
   "Retweeted."
   "Retweet removed."))

(defun chirp-follow-user-at-point ()
  "Follow the user at point."
  (interactive)
  (let* ((user (chirp-actions--user-at-point))
         (handle (plist-get user :handle))
         (buffer (current-buffer)))
    (chirp-actions--perform
     (list "follow" handle)
     (lambda (_data _envelope)
       (chirp-actions--refresh-user-buffer-if-needed buffer)
       (message "Now following @%s." handle)))))

(defun chirp-unfollow-user-at-point ()
  "Unfollow the user at point."
  (interactive)
  (let* ((user (chirp-actions--user-at-point))
         (handle (plist-get user :handle))
         (buffer (current-buffer)))
    (chirp-actions--perform
     (list "unfollow" handle)
     (lambda (_data _envelope)
       (chirp-actions--refresh-user-buffer-if-needed buffer)
       (message "Unfollowed @%s." handle)))))

(defun chirp-toggle-follow-user-at-point ()
  "Toggle follow state for the user at point."
  (interactive)
  (let* ((entry (chirp-entry-at-point))
         (following (chirp-boolean-value (plist-get entry :viewer-following-p))))
    (if following
        (chirp-unfollow-user-at-point)
      (chirp-follow-user-at-point))))

(defun chirp-delete-at-point ()
  "Delete the tweet at point after confirmation."
  (interactive)
  (let ((id (chirp-actions--tweet-id-at-point))
        (buffer (current-buffer)))
    (when (y-or-n-p (format "Delete tweet %s? " id))
      (chirp-actions--perform
       (list "delete" "--yes" id)
       (lambda (_data _envelope)
         (chirp-clear-tweet-state-overrides id)
         (unless (chirp--remove-tweet-from-primary-feeds buffer id)
           (chirp-actions--refresh-buffer buffer))
         (message "Tweet deleted."))))))

;;; Compose State

(defun chirp-compose--buffer-name ()
  "Return a compose buffer name for the current draft."
  (pcase chirp-compose-kind
    ('reply
     (format "*chirp compose: Reply @%s*"
             (or chirp-compose-target-handle "?")))
    ('quote
     (format "*chirp compose: Quote @%s*"
             (or chirp-compose-target-handle "?")))
    (_ "*chirp compose: Post*")))

(defun chirp-compose--header-string ()
  "Return reply or quote context, or an empty string for a new post."
  (let ((context
         (pcase chirp-compose-kind
           ('reply
            (if chirp-compose-target-handle
                (format "Replying to @%s" chirp-compose-target-handle)
              (format "Replying to %s" chirp-compose-target-id)))
           ('quote
            (if chirp-compose-target-handle
                (format "Quoting @%s" chirp-compose-target-handle)
              (format "Quoting %s" chirp-compose-target-id)))
           (_ nil))))
    (if (null context)
        ""
      (concat
       (propertize context 'face 'shadow)
       (when chirp-compose-target-url
         (concat "\n"
                 (propertize chirp-compose-target-url 'face 'link)))))))

(defun chirp-compose--ensure-idle ()
  "Signal a user error while another compose operation is active."
  (when (appkit-compose-operation-active-p)
    (user-error "A draft operation is already in progress")))

(defun chirp-compose--unknown-outcome-p ()
  "Return non-nil when the current draft has an uncertain write outcome."
  (eq chirp-compose-write-outcome 'unknown))

(defun chirp-compose--confirm-repeat-after-unknown (prompt)
  "Confirm PROMPT and resume editing after an unknown write outcome."
  (when (chirp-compose--unknown-outcome-p)
    (unless (yes-or-no-p prompt)
      (user-error "Operation canceled"))
    (setq-local chirp-compose-write-outcome nil)))

(defun chirp-compose--owner ()
  "Return the generated Surface owning this exact compose buffer."
  (or (appkit-current-surface)
      (appkit-open-generated-surface
       (appkit-surface-type-create
        :name 'chirp-compose
        :mode (lambda ()
                (unless (derived-mode-p 'appkit-chat-compose-mode)
                  (error "Chirp compose Surface requires an initialized composer")))
        :init (lambda (_context _input)
                (appkit-next :model (list :type 'compose :media-intent nil
                                          :media-phase 'idle :media-error nil)
                             :render appkit-render-none))
        :update #'chirp--surface-update
        :renderer-factory
        (lambda (_surface)
          (appkit-generated-renderer-create
           :mount (lambda (_surface _app _model) (appkit-chat-compose-refresh))
           :merge #'appkit-projection-change-merge
           :render (lambda (_surface _app _model _change)
                     (appkit-chat-compose-refresh) nil)
           :recover (lambda (_surface _app _model _condition)
                      (appkit-chat-compose-refresh) nil)
           :unmount (lambda (_surface) (appkit-compose-cancel-operation)))))
       :app (chirp-app) :identity (list 'compose (current-buffer))
       :buffer (current-buffer))))

(defun chirp-compose--operation-current-p (buffer owner)
  "Return non-nil when OWNER still owns compose BUFFER."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (appkit-compose-operation-current-p owner))))

(defun chirp-compose--settle-editable (buffer owner temp-attachments outcome)
  "Settle OWNER in BUFFER and restore TEMP-ATTACHMENTS for OUTCOME."
  (cond
   ((not (buffer-live-p buffer))
    (chirp-compose--cleanup-files temp-attachments)
    nil)
   ((with-current-buffer buffer
      (when (appkit-compose-operation-current-p owner)
        (appkit-compose-operation-finish owner)
        (setq-local chirp-compose-write-outcome
                    (and (eq outcome 'unknown) 'unknown))
        (setq-local chirp-compose--submit-temps nil)
        (setq-local chirp-compose-temp-attachments
                    (append (copy-sequence temp-attachments)
                            chirp-compose-temp-attachments))
        (setq-local buffer-read-only nil)
        (appkit-chat-compose-refresh)
        t)))
   (t nil)))

(defun chirp-compose--abort-operation (buffer owner) "Cancel asynchronous work and resources formerly owned by OWNER in BUFFER." (when (buffer-live-p buffer) (with-current-buffer buffer (let ((temp-attachments chirp-compose--submit-temps)) (when-let* ((view (appkit-current-surface)) ((appkit-surface-live-p view))) (ignore-errors (appkit-cancel-handles view))) (cond ((appkit-compose-operation-current-p owner) (chirp-compose--settle-editable buffer owner temp-attachments 'unknown)) ((equal temp-attachments chirp-compose--submit-temps) (setq-local chirp-compose--submit-temps nil) (chirp-compose--cleanup-files temp-attachments)))))))

(defun chirp-compose--begin-operation (kind label &optional generation)
  "Begin a compose operation of KIND with LABEL at source GENERATION."
  (chirp-compose--owner)
  (let* ((buffer (current-buffer))
         (owner (appkit-compose-operation-begin kind :label label :generation generation)))
    (appkit-compose-operation-update
     owner :cancel-function (lambda () (chirp-compose--abort-operation buffer owner)))
    (setq-local buffer-read-only t)
    (appkit-chat-compose-refresh)
    owner))

(defun chirp-compose--operation-stopped-p (buffer owner)
  "Return non-nil when OWNER no longer has authority in BUFFER."
  (or (not (chirp-compose--operation-current-p buffer owner))
      (with-current-buffer buffer
        (appkit-compose-operation-cancel-requested-p))))

(defun chirp-compose--publish-label ()
  "Return the status label used after media upload finishes."
  (or (appkit-compose-label) "Sending post..."))

(defun chirp-compose--upload-progress (buffer owner event)
  "Update compose BUFFER operation OWNER from upload EVENT."
  (when (chirp-compose--operation-current-p buffer owner)
    (with-current-buffer buffer
      (let* ((media-type (plist-get event :media-type))
             (kind (pcase media-type
                     ("video/mp4" "video")
                     ("image/gif" "GIF")
                     (_ "media")))
             (label
              (pcase (plist-get event :phase)
                ('init (format "Starting %s upload..." kind))
                ('append
                 (format "Uploading %s %d/%d" kind
                         (plist-get event :index)
                         (plist-get event :count)))
                ('finalize (format "Finalizing %s upload..." kind))
                ('status (format "Processing %s..." kind))
                ('publish (chirp-compose--publish-label))
                (_ "Uploading..."))))
        (appkit-compose-operation-update
         owner :label label :progress (plist-get event :progress))
        (when-let* ((text (appkit-compose-status-text)))
          (message "%s" text))
        (appkit-chat-compose-refresh)))))

(defun chirp-compose--submit-options (buffer owner)
  "Return backend progress and Appkit owner options for BUFFER and OWNER."
  (list :progress
        (lambda (event)
          (chirp-compose--upload-progress buffer owner event))
        :owner
        (with-current-buffer buffer
          (chirp-compose--owner))))

;;;; Reply Audience

(defun chirp-compose--reply-audience-label (audience)
  "Return the status-field label for reply AUDIENCE."
  (or (alist-get (or audience 'everyone) chirp-compose--reply-audience-choices)
      (format "%s" audience)))

(defun chirp-compose--read-reply-audience ()
  "Read a reply audience from `completing-read'."
  (let* ((choices chirp-compose--reply-audience-choices)
         (current (chirp-compose--reply-audience-label
                   chirp-compose-reply-audience))
         (label (completing-read "Who can reply: "
                                 (mapcar #'cdr choices)
                                 nil t nil nil current)))
    (or (car (rassoc label choices))
        (user-error "Reply audience is invalid"))))

(defun chirp-compose-set-reply-audience (&optional audience)
  "Set the reply audience for the current post or quote draft.

AUDIENCE is `everyone', `community', `verified', or `byinvitation'.
When called interactively, prompt for AUDIENCE."
  (interactive)
  (unless (derived-mode-p 'chirp-compose-mode)
    (user-error "Not in a Chirp compose buffer"))
  (chirp-compose--ensure-idle)
  (unless (memq chirp-compose-kind '(post quote))
    (user-error "Reply audience can be set only for a new post or quote"))
  (let ((choice (or audience (chirp-compose--read-reply-audience))))
    (unless (assq choice chirp-compose--reply-audience-choices)
      (user-error "Reply audience is invalid: %S" choice))
    (setq-local chirp-compose-reply-audience choice)
    (appkit-compose-touch)
    (appkit-chat-compose-refresh)
    choice))

;;;; Items and Attachments

(defun chirp-compose--current-index ()
  "Return the compose item index that contains point."
  (or (appkit-chat-compose-current-part-index) 0))

(defun chirp-compose--current-item ()
  "Return the compose item that contains point."
  (appkit-chat-compose-current-item))

(defun chirp-compose--set-current-item (item)
  "Replace the current compose item with ITEM."
  (appkit-chat-compose-update-current-item item))

(defun chirp-compose--item-attachments (&optional item)
  "Return attachments for ITEM or the current compose item."
  (plist-get (or item (chirp-compose--current-item)) :attachments))

(defun chirp-compose--attachment-path (attachment)
  "Return the local file path stored in ATTACHMENT, or nil."
  (if (stringp attachment)
      attachment
    (plist-get attachment :path)))

(defun chirp-compose--attachment-media-id (attachment)
  "Return the stored X media ID in ATTACHMENT, or nil."
  (and (listp attachment)
       (plist-get attachment :media-id)))

(defun chirp-compose--attachment-choice (attachment)
  "Return the `completing-read' identity for ATTACHMENT."
  (or (chirp-compose--attachment-path attachment)
      (chirp-compose--attachment-media-id attachment)
      (and (stringp attachment) attachment)))

(defun chirp-compose--media-label ()
  "Return the status-field value for the current item's attachments."
  (let ((attachments (chirp-compose--item-attachments)))
    (if (cl-some #'chirp-compose--video-attachment-p attachments)
        "1 video"
      (format "%d/4" (length attachments)))))

(defun chirp-compose--attachment-label (attachment)
  "Return the attachment-row label for ATTACHMENT."
  (if-let* ((path (chirp-compose--attachment-path attachment)))
      (abbreviate-file-name path)
    (if-let* ((media-id (chirp-compose--attachment-media-id attachment)))
        (format "%s %s"
                (if (chirp-compose--video-attachment-p attachment)
                    "Video"
                  "Image")
                media-id)
      "[media]")))

(defun chirp-compose--attachment-description (attachment)
  "Return the alt text stored in ATTACHMENT, or nil."
  (and (listp attachment)
       (plist-get attachment :description)))

(defun chirp-compose--attachment-paths (&optional item)
  "Return attachment identities for ITEM or the current compose item."
  (mapcar #'chirp-compose--attachment-choice
          (chirp-compose--item-attachments item)))

(defun chirp-compose--preview-image (url)
  "Return a small image descriptor for cached preview URL, or nil."
  (when-let* ((file (and (stringp url)
                         (chirp-media-cached-file url "media" "jpg"))))
    (appkit-media-one-line-preview-image-from-file file 48)))

(defun chirp-compose--prefetch-preview (buffer url)
  "Prefetch preview URL and refresh compose BUFFER when it arrives."
  (when (and (buffer-live-p buffer)
             (stringp url)
             (not (string-empty-p url)))
    (chirp-media-prefetch-file
     url "media" "jpg"
     (lambda (success _path)
       (when (and success (buffer-live-p buffer))
         (with-current-buffer buffer
           (when (and (derived-mode-p 'chirp-compose-mode)
                      (not (appkit-compose-operation-active-p)))
             (appkit-chat-compose-refresh))))))))

(defun chirp-compose--attachment-preview (attachment)
  "Return an image descriptor for ATTACHMENT, prefetching if needed."
  (or (when-let* ((url (and (listp attachment)
                            (plist-get attachment :preview-url))))
        (or (chirp-compose--preview-image url)
            (progn
              (chirp-compose--prefetch-preview (current-buffer) url)
              nil)))
      (and (chirp-compose--video-attachment-p attachment)
           (chirp-media-video-placeholder-image 48))))

(defun chirp-compose--length-label ()
  "Return the status-field value for the current body's weighted length."
  (let ((weight (chirp-backend-tweet-weighted-length
                 (appkit-chat-compose-body))))
    (setq chirp-compose--shown-weight weight)
    (if (> weight chirp-backend-standard-tweet-weight-limit)
        (format "%d long" weight)
      (format "%d/%d" weight chirp-backend-standard-tweet-weight-limit))))

(defun chirp-compose--status-fields ()
  "Return current Chirp compose status fields."
  (let ((fields
         (list (list :label "Length"
                     :value (chirp-compose--length-label))
               (list :label "Media"
                     :value (chirp-compose--media-label)))))
    (cond
     ((appkit-compose-status-text)
      (push (list :label "State" :value (appkit-compose-status-text)) fields))
     ((eq chirp-compose-write-outcome 'unknown)
      (push (list :label "State" :value "Outcome unknown") fields)))
    (when (and (eq chirp-compose-kind 'post)
               (> (length (appkit-chat-compose-items)) 1))
      (push (list :label "Posts"
                  :value (format "%d" (length (appkit-chat-compose-items))))
            fields))
    (when (memq chirp-compose-kind '(post quote))
      (push (list :label "Audience"
                  :value (chirp-compose--reply-audience-label
                          chirp-compose-reply-audience)
                  :action #'chirp-compose-set-reply-audience
                  :help-echo "Change who can reply to this post")
            fields))
    fields))

(defun chirp-compose--attachments-section (&optional item)
  "Return the Appkit attachment section for ITEM, or nil when empty."
  (when-let* ((attachments (chirp-compose--item-attachments item)))
    (list :title "Media"
          :items
          (mapcar
           (lambda (attachment)
             (list :label (chirp-compose--attachment-label attachment)
                   :preview (chirp-compose--attachment-preview attachment)
                   :description
                   (chirp-compose--attachment-description attachment)
                   :description-label "Alt"
                   :object (chirp-compose--attachment-choice attachment)
                   :action #'chirp-compose-describe-image
                   :help-echo "Edit alt text"))
           attachments))))

(defun chirp-compose--parts ()
  "Return Appkit compose parts for the current draft."
  (let* ((items (appkit-chat-compose-items))
         (total (length items))
         (index 0))
    (mapcar (lambda (item)
              (setq index (1+ index))
              (list :title (and (> total 1)
                                (format "Post %d/%d" index total))
                    :attachments (chirp-compose--attachments-section item)))
            items)))

;;;; Attachment Input

(defun chirp-compose--video-path-p (path)
  "Return non-nil when PATH is an MP4 file."
  (and (stringp path)
       (equal (downcase (or (file-name-extension path) "")) "mp4")))

(defun chirp-compose--video-attachment-p (attachment)
  "Return non-nil when ATTACHMENT is a video."
  (or (equal (and (listp attachment) (plist-get attachment :type)) "video")
      (chirp-compose--video-path-p
       (chirp-compose--attachment-path attachment))))

(defun chirp-compose--ensure-can-attach (path)
  "Signal a user error when PATH cannot be added to the current item."
  (let* ((attachments (chirp-compose--item-attachments))
         (video-p (chirp-compose--video-path-p path))
         (has-video (cl-some #'chirp-compose--video-attachment-p attachments)))
    (cond
     ((or video-p has-video)
      (when attachments
        (user-error "A video cannot be mixed with other attachments")))
     ((>= (length attachments) 4)
      (user-error "Up to 4 attached images are supported")))))

(defun chirp-compose--mime-extension (mime-type)
  "Return a file extension for MIME-TYPE."
  (pcase (downcase (or mime-type ""))
    ("image/png" ".png")
    ((or "image/jpeg" "image/jpg") ".jpg")
    ("image/gif" ".gif")
    ("image/webp" ".webp")
    ("image/bmp" ".bmp")
    (_ ".img")))

(defun chirp-compose--process-lines (program &rest args)
  "Return PROGRAM output lines for ARGS, or nil on failure."
  (with-temp-buffer
    (let ((status (apply #'process-file program nil (current-buffer) nil args)))
      (when (and (numberp status)
                 (zerop status))
        (split-string (buffer-string) "\n" t "[ \t\r]+")))))

(defun chirp-compose--first-image-type (types)
  "Return the first image MIME type in TYPES."
  (cl-find-if (lambda (type)
                (string-prefix-p "image/" type))
              types))

(defun chirp-compose--clipboard-image-backend ()
  "Return a plist describing how to paste an image from the clipboard."
  (let ((wl-paste (executable-find "wl-paste"))
        (pngpaste (executable-find "pngpaste"))
        (xclip (executable-find "xclip")))
    (or
     (when wl-paste
       (when-let* ((mime-type (chirp-compose--first-image-type
                               (chirp-compose--process-lines wl-paste "--list-types"))))
         (list :kind 'stdout
               :program wl-paste
               :args (list "--no-newline" "--type" mime-type)
               :extension (chirp-compose--mime-extension mime-type))))
     (when pngpaste
       (list :kind 'filearg
             :program pngpaste
             :args nil
             :extension ".png"))
     (when xclip
       (when-let* ((mime-type (chirp-compose--first-image-type
                               (chirp-compose--process-lines
                                xclip "-selection" "clipboard" "-t" "TARGETS" "-o"))))
         (list :kind 'stdout
               :program xclip
               :args (list "-selection" "clipboard" "-t" mime-type "-o")
               :extension (chirp-compose--mime-extension mime-type)))))))

(defun chirp-compose--write-command-output-to-file (program args file)
  "Write PROGRAM ARGS output to FILE, returning non-nil on success."
  (let ((coding-system-for-read 'binary)
        (coding-system-for-write 'binary))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (let ((status (apply #'process-file program nil (current-buffer) nil args)))
        (when (and (numberp status)
                   (zerop status)
                   (> (buffer-size) 0))
          (write-region nil nil file nil 'silent)
          t)))))

(defun chirp-compose--paste-image-to-file (backend file)
  "Paste clipboard image using BACKEND into FILE."
  (pcase (plist-get backend :kind)
    ('stdout
     (chirp-compose--write-command-output-to-file
      (plist-get backend :program)
      (plist-get backend :args)
      file))
    ('filearg
     (let ((status (apply #'process-file
                          (plist-get backend :program)
                          nil nil nil
                          (append (plist-get backend :args)
                                  (list file)))))
       (and (numberp status)
            (zerop status)
            (file-exists-p file)
            (> (file-attribute-size (file-attributes file)) 0))))
    (_ nil)))

(defun chirp-compose--drop-temp-attachment (file)
  "Delete FILE when it is a temporary compose attachment."
  (when (member file chirp-compose-temp-attachments)
    (setq-local chirp-compose-temp-attachments
                (delete file chirp-compose-temp-attachments))
    (ignore-errors
      (when (file-exists-p file)
        (delete-file file)))))

(defun chirp-compose--cleanup-temp-attachments ()
  "Delete all temporary attachments owned by the current compose buffer."
  (dolist (file chirp-compose-temp-attachments)
    (ignore-errors
      (when (file-exists-p file)
        (delete-file file))))
  (setq chirp-compose-temp-attachments nil))

(defun chirp-compose--take-temp-attachments ()
  "Return temporary attachments and detach them from the current buffer."
  (prog1 (copy-sequence chirp-compose-temp-attachments)
    (setq-local chirp-compose-temp-attachments nil)))

(defun chirp-compose--cleanup-files (files)
  "Delete every path in FILES, ignoring missing files."
  (dolist (file files)
    (ignore-errors
      (when (file-exists-p file)
        (delete-file file)))))

(defun chirp-compose--add-attachment (path &optional temporary)
  "Attach PATH to the current draft.

When TEMPORARY is non-nil, PATH is owned by the current compose buffer."
  (let ((file (expand-file-name path)))
    (unless (file-regular-p file)
      (user-error "Attachment is not a regular file"))
    (unless (file-readable-p file)
      (user-error "Attachment is not readable"))
    (chirp-compose--ensure-can-attach file)
    (let ((item (or (chirp-compose--current-item)
                    (user-error "No compose item at point")))
          (attachments (chirp-compose--item-attachments)))
      (when (member file (chirp-compose--attachment-paths item))
        (user-error "Media already attached"))
      (chirp-compose--set-current-item
       (plist-put (copy-sequence item) :attachments
                  (append attachments
                          (list (append (list :path file)
                                        (when (chirp-compose--video-path-p file)
                                          (list :type "video")))))))
      (when temporary
        (setq-local chirp-compose-temp-attachments
                    (append chirp-compose-temp-attachments (list file))))
      file)))

(defun chirp-compose-attach-image (path)
  "Attach image or MP4 video PATH to the current draft."
  (interactive (list (read-file-name "Attach media: " nil nil t)))
  (chirp-compose--ensure-idle)
  (message "Attached %s"
           (file-name-nondirectory
            (chirp-compose--add-attachment path))))

(defun chirp-compose-paste-image ()
  "Paste one image from the clipboard into the current draft."
  (interactive)
  (chirp-compose--ensure-idle)
  (when (cl-some #'chirp-compose--video-attachment-p
                 (chirp-compose--item-attachments))
    (user-error "A video cannot be mixed with other attachments"))
  (chirp-compose--ensure-can-attach "clipboard.png")
  (let* ((backend (chirp-compose--clipboard-image-backend))
         (file nil)
         (attached nil))
    (unless backend
      (user-error "No clipboard image available or no supported paste backend"))
    (setq file (make-temp-file
                (expand-file-name "chirp-compose-"
                                  (chirp-compose--temp-directory))
                nil
                (plist-get backend :extension)))
    (unwind-protect
        (progn
          (unless (chirp-compose--paste-image-to-file backend file)
            (user-error "Clipboard does not currently contain an image"))
          (chirp-compose--add-attachment file t)
          (setq attached t)
          (message "Pasted %s" (file-name-nondirectory file)))
      (unless attached
        (ignore-errors
          (when (and file (file-exists-p file))
            (delete-file file)))))))

(defun chirp-compose-remove-image ()
  "Remove one image attachment from the current draft."
  (interactive)
  (chirp-compose--ensure-idle)
  (let ((attachments (chirp-compose--item-attachments)))
    (unless attachments
      (user-error "No attached images"))
    (let* ((choices (chirp-compose--attachment-paths))
           (choice (if (= (length choices) 1)
                       (car choices)
                     (completing-read "Remove image: "
                                      choices
                                      nil
                                      t
                                      nil
                                      nil
                                      (car choices))))
           (item (chirp-compose--current-item)))
      (chirp-compose--set-current-item
       (plist-put (copy-sequence item) :attachments
                  (cl-remove-if
                   (lambda (attachment)
                     (equal (chirp-compose--attachment-choice attachment)
                            choice))
                   attachments)))
      (chirp-compose--drop-temp-attachment choice)
      (message "Removed %s"
               (if (and (stringp choice)
                        (file-name-absolute-p choice))
                   (file-name-nondirectory choice)
                 choice)))))

(defun chirp-compose-describe-image (&optional path)
  "Set alt text for image PATH in the current compose item.

When PATH is nil, prompt for one attached image."
  (interactive)
  (chirp-compose--ensure-idle)
  (let* ((item (or (chirp-compose--current-item)
                   (user-error "No compose item at point")))
         (attachments (chirp-compose--item-attachments item))
         (choices (chirp-compose--attachment-paths item)))
    (unless choices
      (user-error "No attached images"))
    (setq path (or path
                   (if (= (length choices) 1)
                       (car choices)
                     (completing-read "Describe image: "
                                      choices nil t nil nil (car choices)))))
    (unless (member path choices)
      (user-error "Image is not attached"))
    (let* ((current (cl-find-if
                     (lambda (attachment)
                       (equal (chirp-compose--attachment-choice attachment)
                              path))
                     attachments))
           (text (string-trim
                  (read-string "Alt text: "
                               (or (chirp-compose--attachment-description
                                    current)
                                   "")))))
      (when (> (length text) chirp-x-media-alt-text-limit)
        (user-error "Alt text cannot exceed %d characters"
                    chirp-x-media-alt-text-limit))
      (chirp-compose--set-current-item
       (plist-put (copy-sequence item) :attachments
                  (mapcar
                   (lambda (attachment)
                     (if (equal (chirp-compose--attachment-choice attachment)
                                path)
                         (let ((updated
                                (copy-sequence
                                 (if (listp attachment)
                                     attachment
                                   (list :path attachment)))))
                           (plist-put
                            updated :description
                            (and (not (string-empty-p text)) text)))
                       attachment))
                   attachments)))
      (message (if (string-empty-p text)
                   "Removed alt text from %s"
                 "Updated alt text for %s")
               (if (and (stringp path)
                        (file-name-absolute-p path))
                   (file-name-nondirectory path)
                 path)))))

;;;; Submission

(defun chirp-compose--snapshot-items ()
  "Return the current ordered chat-compose items for backend submission."
  (mapcar
   (lambda (item)
     (let ((text (string-trim (or (plist-get item :text) ""))))
       (when (string-empty-p text)
         (user-error "Text cannot be empty"))
       (list :text text
             :attachments
             (copy-sequence (plist-get item :attachments)))))
   (appkit-chat-compose-items)))

(defun chirp-compose--draft ()
  "Return the current compose buffer as a structured backend draft."
  (let ((draft (list :kind chirp-compose-kind
                     :target-id chirp-compose-target-id
                     :items (chirp-compose--snapshot-items))))
    (when chirp-compose-draft-id
      (setq draft (plist-put draft :draft-id chirp-compose-draft-id)))
    (when (memq chirp-compose-kind '(post quote))
      (setq draft
            (plist-put draft :reply-audience
                       (or chirp-compose-reply-audience 'everyone))))
    draft))

(defun chirp-compose--close-after-send
    (compose-buffer source-buffer temp-attachments success-message)
  "Delete TEMP-ATTACHMENTS and close COMPOSE-BUFFER to SOURCE-BUFFER.

Refresh the source view and show SUCCESS-MESSAGE."
  (chirp-backend-clear-cache)
  (chirp-compose--cleanup-files temp-attachments)
  (when (buffer-live-p compose-buffer)
    (chirp-compose--close compose-buffer source-buffer))
  (when (buffer-live-p source-buffer)
    (chirp-actions--refresh-buffer source-buffer))
  (message "%s" success-message))

(defun chirp-compose--delete-unsent-after-send
    (kind id compose-buffer source-buffer temp-attachments success-message)
  "Delete unsent KIND ID after publishing from COMPOSE-BUFFER.

Then close to SOURCE-BUFFER, delete TEMP-ATTACHMENTS, and show
SUCCESS-MESSAGE."
  (chirp-backend-delete-unsent
   kind id
   (lambda (_payload _envelope)
     (chirp-compose--close-after-send
      compose-buffer source-buffer temp-attachments success-message))
   (lambda (message)
     (chirp-compose--close-after-send
      compose-buffer source-buffer temp-attachments success-message)
     (chirp-actions-show-error
      (format "Post sent, but the X %s was not deleted: %s"
              (if (eq kind 'scheduled) "scheduled post" "draft")
              message)))))

(defun chirp-compose--finish-send
    (compose-buffer owner source-buffer temp-attachments success-message)
  "Accept OWNER and close COMPOSE-BUFFER after a successful write.

SOURCE-BUFFER is restored, TEMP-ATTACHMENTS are deleted, and SUCCESS-MESSAGE is
shown.  A stored draft or scheduled object is deleted after acceptance."
  (when (chirp-compose--operation-current-p compose-buffer owner)
    (let ((draft-id
           (with-current-buffer compose-buffer chirp-compose-draft-id))
          (scheduled-id
           (with-current-buffer compose-buffer chirp-compose-scheduled-id)))
      (with-current-buffer compose-buffer
        (setq-local chirp-compose--submit-temps nil
                    chirp-compose-write-outcome nil)
        (appkit-compose-operation-finish owner))
      (cond
       (draft-id
        (chirp-compose--delete-unsent-after-send
         'draft draft-id compose-buffer source-buffer
         temp-attachments success-message))
       (scheduled-id
        (chirp-compose--delete-unsent-after-send
         'scheduled scheduled-id compose-buffer source-buffer
         temp-attachments success-message))
       (t
        (chirp-compose--close-after-send
         compose-buffer source-buffer temp-attachments success-message))))))

(defun chirp-compose--fail-send
    (compose-buffer owner temp-attachments message)
  "Settle OWNER in COMPOSE-BUFFER after write failure MESSAGE."
  (let ((outcome
         (if (chirp-actions--unknown-write-outcome-p message)
             'unknown
           'rejected)))
    (when (chirp-compose--settle-editable
           compose-buffer owner temp-attachments outcome)
      (chirp-actions-show-error message))))

(defun chirp-compose--send-next
    (compose-buffer owner source-buffer draft items index previous-id
                    temp-attachments success-message)
  "Send DRAFT ITEMS from INDEX while OWNER remains current.

After the first item, reply to PREVIOUS-ID.  COMPOSE-BUFFER owns the chain,
SOURCE-BUFFER is restored after success, and TEMP-ATTACHMENTS stay reserved
until the final settlement."
  (cond
   ((chirp-compose--operation-stopped-p compose-buffer owner) nil)
   ((>= index (length items))
    (chirp-compose--finish-send
     compose-buffer owner source-buffer temp-attachments success-message))
   (t
    (let* ((item (nth index items))
           (root-p (zerop index))
           (kind (if root-p (plist-get draft :kind) 'reply))
           (target-id (if root-p (plist-get draft :target-id) previous-id)))
      (apply
       #'chirp-backend-compose
       :kind kind
       :text (plist-get item :text)
       :target-id target-id
       :attachments (plist-get item :attachments)
       :reply-audience (and root-p (plist-get draft :reply-audience))
       :callback
       (lambda (created-tweet-id _envelope)
         (chirp-compose--send-next
          compose-buffer owner source-buffer draft items (1+ index)
          created-tweet-id temp-attachments success-message))
       :errback
       (lambda (message)
         (chirp-compose--fail-send
          compose-buffer owner temp-attachments message))
       (chirp-compose--submit-options compose-buffer owner))))))

(defun chirp-compose-send ()
  "Send the current draft and retain it until the write settles."
  (interactive)
  (chirp-compose--ensure-idle)
  (chirp-compose--confirm-repeat-after-unknown
   (concat "Previous send outcome is unknown; "
           "the post may already exist. Send again? "))
  (let* ((compose-buffer (current-buffer))
         (source-buffer chirp-compose-source-buffer)
         (capture (appkit-compose-capture))
         (generation (plist-get capture :generation))
         (draft (plist-get capture :value))
         (items (plist-get draft :items))
         (temp-attachments (chirp-compose--take-temp-attachments))
         (thread-p (> (length items) 1))
         (success-message
          (cond
           (thread-p "Posts sent.")
           ((eq chirp-compose-kind 'reply) "Reply sent.")
           ((eq chirp-compose-kind 'quote) "Quote tweet sent.")
           (t "Post sent.")))
         (sending-message
          (cond
           (thread-p "Sending posts...")
           ((eq chirp-compose-kind 'reply) "Sending reply...")
           ((eq chirp-compose-kind 'quote) "Sending quote tweet...")
           (t "Sending post...")))
         owner)
    (setq-local chirp-compose--submit-temps temp-attachments)
    (condition-case error-data
        (progn
          (setq owner
                (chirp-compose--begin-operation
                 'submitting sending-message generation))
          (chirp-compose--send-next
           compose-buffer owner source-buffer draft items 0 nil
           temp-attachments success-message)
          (when (chirp-compose--operation-current-p compose-buffer owner)
            (message "%s" sending-message)))
      ((error quit)
       (if owner
           (chirp-compose--settle-editable
            compose-buffer owner temp-attachments 'rejected)
         (setq-local chirp-compose-temp-attachments
                     (append temp-attachments
                             chirp-compose-temp-attachments)))
       (signal (car error-data) (cdr error-data))))))

(defun chirp-compose--read-schedule-time ()
  "Read a future local time and return it as Unix seconds."
  (let* ((now (current-time))
         (seed (or (and (integerp chirp-compose-execute-at)
                        (seconds-to-time chirp-compose-execute-at))
                   (time-add now 3600)))
         (default (format-time-string "%Y-%m-%d %H:%M" seed))
         (input (read-string "Schedule at (YYYY-MM-DD HH:MM): " default))
         (decoded (parse-time-string input)))
    (unless (and (nth 5 decoded) (nth 4 decoded) (nth 3 decoded)
                 (nth 2 decoded) (nth 1 decoded))
      (user-error "Schedule time is invalid: %s" input))
    (setf (nth 0 decoded) (or (nth 0 decoded) 0))
    (let ((unix (time-convert (encode-time decoded) 'integer)))
      (when (<= unix (time-convert now 'integer))
        (user-error "Schedule time must be in the future"))
      unix)))

(defun chirp-compose-save ()
  "Save the current draft on X and keep the compose buffer open."
  (interactive)
  (chirp-compose--ensure-idle)
  (chirp-compose--confirm-repeat-after-unknown
   (concat "Previous save outcome is unknown; "
           "the draft may already exist. Save again? "))
  (let* ((compose-buffer (current-buffer))
         (capture (appkit-compose-capture))
         (generation (plist-get capture :generation))
         (draft (plist-get capture :value))
         owner)
    (condition-case error-data
        (progn
          (setq owner
                (chirp-compose--begin-operation
                 'saving "Saving draft..." generation))
          (apply
           #'chirp-backend-save-draft
           :kind (plist-get draft :kind)
           :target-id (plist-get draft :target-id)
           :items (plist-get draft :items)
           :draft-id (plist-get draft :draft-id)
           :callback
           (lambda (draft-id _envelope)
             (when (chirp-compose--operation-current-p compose-buffer owner)
               (with-current-buffer compose-buffer
                 (setq-local chirp-compose-draft-id draft-id
                             chirp-compose-write-outcome nil
                             buffer-read-only nil)
                 (set-buffer-modified-p nil)
                 (appkit-compose-operation-finish owner)
                 (appkit-chat-compose-refresh))
               (message "Draft saved.")))
           :errback
           (lambda (message)
             (chirp-compose--fail-send compose-buffer owner nil message))
           (chirp-compose--submit-options compose-buffer owner)))
      ((error quit)
       (when owner
         (chirp-compose--settle-editable
          compose-buffer owner nil 'rejected))
       (signal (car error-data) (cdr error-data))))))

(defun chirp-compose-schedule (execute-at)
  "Schedule the current draft for publication at EXECUTE-AT.

EXECUTE-AT is a Unix timestamp in seconds.  When called interactively, prompt
for a local date and time."
  (interactive (list (chirp-compose--read-schedule-time)))
  (chirp-compose--ensure-idle)
  (unless (and (integerp execute-at) (> execute-at 0))
    (user-error "Schedule time is invalid"))
  (chirp-compose--confirm-repeat-after-unknown
   (concat "Previous schedule outcome is unknown; "
           "the post may already exist. Schedule again? "))
  (let* ((compose-buffer (current-buffer))
         (source-buffer chirp-compose-source-buffer)
         (capture (appkit-compose-capture))
         (generation (plist-get capture :generation))
         (draft (plist-get capture :value))
         (temp-attachments (chirp-compose--take-temp-attachments))
         owner)
    (setq-local chirp-compose--submit-temps temp-attachments)
    (condition-case error-data
        (progn
          (setq owner
                (chirp-compose--begin-operation
                 'scheduling "Scheduling post..." generation))
          (apply
           #'chirp-backend-schedule
           :kind (plist-get draft :kind)
           :target-id (plist-get draft :target-id)
           :items (plist-get draft :items)
           :scheduled-id chirp-compose-scheduled-id
           :execute-at execute-at
           :callback
           (lambda (_created _envelope)
             (chirp-compose--finish-send
              compose-buffer owner source-buffer temp-attachments
              "Post scheduled."))
           :errback
           (lambda (message)
             (chirp-compose--fail-send
              compose-buffer owner temp-attachments message))
           (chirp-compose--submit-options compose-buffer owner))
          (when (chirp-compose--operation-current-p compose-buffer owner)
            (message "Scheduling post...")))
      ((error quit)
       (if owner
           (chirp-compose--settle-editable
            compose-buffer owner temp-attachments 'rejected)
         (setq-local chirp-compose-temp-attachments
                     (append temp-attachments
                             chirp-compose-temp-attachments)))
       (signal (car error-data) (cdr error-data))))))

;;; Compose Buffer Lifecycle

(defun chirp-compose--other-window-showing-buffer (buffer &optional except-window)
  "Return a live window showing BUFFER other than EXCEPT-WINDOW, or nil."
  (cl-find-if
   (lambda (window)
     (and (window-live-p window)
          (not (eq window except-window))
          (eq (window-buffer window) buffer)))
   (window-list nil 'no-minibuf)))

(defun chirp-compose--close (buffer &optional restore-buffer)
  "Close compose BUFFER and restore RESTORE-BUFFER when possible."
  (when (buffer-live-p buffer)
    (let ((window (get-buffer-window buffer t))
          (restore (and (buffer-live-p restore-buffer)
                        (not (eq buffer restore-buffer))
                        restore-buffer)))
      (with-current-buffer buffer
        (set-buffer-modified-p nil)
        (chirp-compose--cleanup-temp-attachments))
      (when (window-live-p window)
        (let ((other-restore-window
               (and restore
                    (chirp-compose--other-window-showing-buffer
                     restore
                     window))))
          (cond
           ((and other-restore-window
                 (not (one-window-p t)))
            (condition-case nil
                (delete-window window)
              (error
               (set-window-buffer window restore))))
           (restore
            (set-window-buffer window restore))
           (t
            (switch-to-prev-buffer window)))))
      (kill-buffer buffer))))

(defun chirp-compose-cancel ()
  "Cancel an active draft operation, or close an idle compose buffer."
  (interactive)
  (if (appkit-compose-operation-active-p)
      (appkit-compose-cancel-operation)
    (chirp-compose--close (current-buffer) chirp-compose-source-buffer)))

;;; Compose Entry Points

(defun chirp-compose--view-buffer-p (buffer)
  "Return non-nil when BUFFER is a live Chirp view buffer."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (derived-mode-p 'chirp-view-mode))))

(defun chirp-compose--source-buffer ()
  "Return the view buffer that should own a newly opened compose buffer."
  (let* ((current (current-buffer))
         (selected (window-buffer (selected-window)))
         (minibuffer-source
          (and (active-minibuffer-window)
               (window-live-p (minibuffer-selected-window))
               (window-buffer (minibuffer-selected-window)))))
    (or (and (chirp-compose--view-buffer-p current) current)
        (and (chirp-compose--view-buffer-p minibuffer-source) minibuffer-source)
        (and (chirp-compose--view-buffer-p selected) selected)
        selected
        current)))

(cl-defun chirp-compose-open
    (kind &key target-id target-handle target-url)
  "Open a compose buffer for KIND.

TARGET-ID, TARGET-HANDLE, and TARGET-URL describe an optional reply or quote
target."
  (let* ((source (chirp-compose--source-buffer))
         (buffer (generate-new-buffer "*chirp compose*")))
    (pop-to-buffer buffer)
    (with-current-buffer buffer
      (chirp-compose-mode)
      (setq-local chirp-compose-kind kind)
      (setq-local chirp-compose-target-id target-id)
      (setq-local chirp-compose-target-handle target-handle)
      (setq-local chirp-compose-target-url target-url)
      (setq-local chirp-compose-source-buffer source)
      (setq-local chirp-compose-temp-attachments nil)
      (setq-local chirp-compose--submit-temps nil)
      (setq-local chirp-compose-draft-id nil)
      (setq-local chirp-compose-scheduled-id nil)
      (setq-local chirp-compose-execute-at nil)
      (setq-local chirp-compose-reply-audience
                  (and (memq kind '(post quote)) 'everyone))
      (rename-buffer (chirp-compose--buffer-name) t)
      (add-hook 'kill-buffer-hook #'chirp-compose--cleanup-temp-attachments nil t)
      (appkit-chat-compose-setup
       :app (chirp-app)
       :context-function #'chirp-compose--header-string
       :status-fields-function #'chirp-compose--status-fields
       :parts-function #'chirp-compose--parts)
      (appkit-chat-compose-set-items
       (list (list :text "" :attachments nil)))
      (goto-char (appkit-chat-compose-body-start-position)))))

(defun chirp-compose--apply-unsent (entry)
  "Fill the current compose buffer from unsent ENTRY."
  (let ((items
         (or (plist-get entry :items)
             (mapcar (lambda (text)
                       (list :text text :attachments nil))
                     (or (plist-get entry :texts) '(""))))))
    (pcase (plist-get entry :kind)
      ('draft
       (setq-local chirp-compose-draft-id (plist-get entry :id)))
      ('scheduled
       (setq-local chirp-compose-scheduled-id (plist-get entry :id))
       (setq-local chirp-compose-execute-at
                   (plist-get entry :execute-at))))
    (appkit-chat-compose-set-items
     (mapcar (lambda (item)
               (list :text (or (plist-get item :text) "")
                     :attachments
                     (copy-sequence (plist-get item :attachments))))
             items))
    (goto-char (appkit-chat-compose-body-start-position))))

(defun chirp-compose-open-unsent (entry)
  "Open a compose buffer restored from unsent ENTRY."
  (unless (plist-get entry :id)
    (error "Unsent entry has no ID"))
  (chirp-compose-open
   (or (plist-get entry :compose-kind) 'post)
   :target-id (plist-get entry :target-id)
   :target-url (plist-get entry :target-url))
  (chirp-compose--apply-unsent entry))

;;; Point Commands

(defun chirp-compose-add-post ()
  "Insert an empty post after the current draft item."
  (interactive)
  (chirp-compose--ensure-idle)
  (unless (eq chirp-compose-kind 'post)
    (user-error "Another post can be added only to a new post draft"))
  (appkit-chat-compose-add-item))

(defun chirp-compose-remove-post ()
  "Remove the current extra post from the draft."
  (interactive)
  (chirp-compose--ensure-idle)
  (unless (eq chirp-compose-kind 'post)
    (user-error "Only a new post draft can drop an extra post"))
  (unless (> (length (appkit-chat-compose-items)) 1)
    (user-error "The draft already has only one post"))
  (let* ((index (chirp-compose--current-index))
         (item (nth index (appkit-chat-compose-items))))
    (dolist (attachment (plist-get item :attachments))
      (chirp-compose--drop-temp-attachment
       (chirp-compose--attachment-path attachment)))
    (appkit-chat-compose-drop-item index)
    (message "Removed post %d." (1+ index))))

(defun chirp-compose-post ()
  "Open a compose buffer for a new post."
  (interactive)
  (chirp-compose-open 'post))

(defun chirp-reply-at-point ()
  "Open a compose buffer to reply to the tweet at point."
  (interactive)
  (let ((tweet (chirp-actions--tweet-at-point)))
    (when (plist-get tweet :reply-limited-p)
      (user-error "You cannot reply to this conversation"))
    (chirp-compose-open
     'reply
     :target-id (plist-get tweet :id)
     :target-handle (plist-get tweet :author-handle)
     :target-url (plist-get tweet :url))))

(defun chirp-quote-at-point ()
  "Open a compose buffer to quote the tweet at point."
  (interactive)
  (let ((tweet (chirp-actions--tweet-at-point)))
    (chirp-compose-open
     'quote
     :target-id (plist-get tweet :id)
     :target-handle (plist-get tweet :author-handle)
     :target-url (plist-get tweet :url))))

;;; Miscellaneous Actions

(defun chirp-copy-fixupx-url-at-point ()
  "Copy the current tweet URL as a fixupx.com link."
  (interactive)
  (if-let* ((url (chirp-tweet-fixupx-url (chirp-actions--tweet-at-point))))
      (progn
        (kill-new url)
        (message "Copied %s" url))
    (user-error "Current tweet has no canonical URL")))

(defun chirp-translate-at-point ()
  "Translate the tweet at point and show the result below its text."
  (interactive)
  (let* ((tweet-id (chirp-actions--tweet-id-at-point))
         (buffer (current-buffer))
         (language (string-trim chirp-translation-language)))
    (when (string-empty-p language)
      (user-error "Translation language must not be empty"))
    (message "Translating to %s..." language)
    (chirp-backend-translate
     tweet-id
     language
     (lambda (data _envelope)
       (if-let* ((translation (chirp-first-nonblank
                               (chirp-get data "translation"))))
           (let ((destination
                  (or (chirp-first-nonblank
                       (chirp-get data "destinationLanguage"))
                      language)))
             (chirp-set-tweet-state-override
              tweet-id :translation translation)
             (chirp-set-tweet-state-override
              tweet-id :translation-language destination)
             (chirp-update-tweet-by-id
              buffer
              tweet-id
              (lambda (tweet)
                (plist-put tweet :translation translation)
                (plist-put tweet :translation-language destination))
              t)
             (message "Translated to %s." destination))
         (chirp-actions-show-error
          "No translated text returned by X")))
     #'chirp-actions-show-error)))

(transient-define-prefix chirp-dispatch ()
  "Show Chirp write actions."
  [["Timeline"
    ("h" "For You" chirp-home)
    ("f" "Following" chirp-following)
    ("u" "Me" chirp-me)
    ("b" "Bookmarks" chirp-bookmarks)
    ("L" "Liked" chirp-likes)
    ("s" "List" chirp-list)]
   ["Compose"
    ("c" "Post" chirp-compose-post)
    ("r" "Reply" chirp-reply-at-point)
    ("Q" "Quote" chirp-quote-at-point)
    ("d" "Drafts" chirp-unsent-drafts)
    ("t" "Scheduled" chirp-unsent-scheduled)]
   ["Tweet"
    ("R" "Retweet" chirp-toggle-retweet-at-point)
    ("H" "Edit history" chirp-edit-history-open-at-point)]
   ["People"
    ("+" "Follow" chirp-follow-user-at-point)
    ("-" "Unfollow" chirp-unfollow-user-at-point)]
   ["Engage"
    ("l" "Like" chirp-toggle-like-at-point)
    ("B" "Bookmark" chirp-toggle-bookmark-at-point)]
   ["Other"
    ("d" "Delete" chirp-delete-at-point)
    ("T" "Translate" chirp-translate-at-point)
    ("y" "Copy fixupx" chirp-copy-fixupx-url-at-point)
    ("o" "Browser" chirp-browse-at-point)]])

(provide 'chirp-actions)

;;; chirp-actions.el ends here
