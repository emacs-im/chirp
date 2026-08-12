;;; chirp-actions.el --- Transient write actions for chirp -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Provide Chirp's compose buffer and interactive read/write actions.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'transient)
(require 'appkit-compose)
(require 'chirp-core)
(require 'chirp-backend)

(declare-function chirp-backend-clear-cache "chirp-backend" ())
(declare-function chirp-timeline-open-home "chirp-timeline" ())
(declare-function chirp-timeline-open-following "chirp-timeline" ())
(declare-function chirp-timeline-open-bookmarks "chirp-timeline" (&optional buffer))
(declare-function chirp-timeline-open-likes "chirp-timeline" (&optional handle buffer))
(declare-function chirp-timeline-open-list "chirp-timeline" (list-target &optional buffer))
(declare-function chirp-me "chirp" ())

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

(defvar-local chirp-compose-items nil
  "Ordered compose items for the current draft.

Each item is a plist with `:attachments'.  Editable text lives in the
Appkit compose parts until the draft is snapshotted for send.")

(defvar-local chirp-compose-temp-attachments nil
  "Temporary attachment paths owned by the current compose buffer.")


(defvar-local chirp-compose-sending nil
  "Non-nil while the current compose buffer is sending a draft.")

(defvar-local chirp-compose-unknown-outcome nil
  "Non-nil when the last send for this draft had an unknown remote outcome.")

(defvar-local chirp-compose-reply-audience nil
  "Reply audience for the current post or quote draft.

`everyone' omits a conversation-control rule.  Replies leave this nil.")

(defconst chirp-compose--reply-audience-choices
  '((everyone . "Everyone")
    (community . "People you follow")
    (verified . "Verified accounts")
    (byinvitation . "Accounts you mention"))
  "Reply audience symbols and their compose status labels.")

(defvar-local chirp-compose--mention-cache nil
  "User completion results keyed by query in the current draft.")

(defvar-local chirp-compose--mention-pending nil
  "Mention queries currently being fetched for the current draft.")

(defvar-local chirp-compose--mention-timer nil
  "Idle timer that starts mention completion prefetching.")

(defvar chirp-compose-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map text-mode-map)
    (define-key map (kbd "C-c C-c") #'chirp-compose-send)
    (define-key map (kbd "C-c C-k") #'chirp-compose-cancel)
    (define-key map (kbd "C-c C-a") #'chirp-compose-attach-image)
    (define-key map (kbd "C-c C-v") #'chirp-compose-paste-image)
    (define-key map (kbd "C-c C-d") #'chirp-compose-remove-image)
    (define-key map (kbd "C-c C-n") #'chirp-compose-add-post)
    (define-key map (kbd "C-c C-p") #'chirp-compose-remove-post)
    (define-key map (kbd "M-TAB") #'completion-at-point)
    map)
  "Keymap for `chirp-compose-mode'.")

(define-derived-mode chirp-compose-mode appkit-compose-mode "Chirp-Compose"
  "Major mode for composing Chirp posts."
  (setq-local completion-ignore-case t)
  (setq-local chirp-compose--mention-cache nil)
  (setq-local chirp-compose--mention-pending nil)
  (setq-local chirp-compose--mention-timer nil)
  (add-hook 'completion-at-point-functions
            #'chirp-compose-mention-completion-at-point nil t)
  (add-hook 'post-command-hook #'chirp-compose--schedule-mention-prefetch nil t)
  (add-hook 'kill-buffer-hook #'chirp-compose--cancel-mention-prefetch nil t)
  (visual-line-mode 1))

(defun chirp-compose--mention-bounds ()
  "Return handle bounds at point when composing an @mention."
  (let ((body-start (appkit-compose-body-start-position))
        (body-end (appkit-compose-body-end-position)))
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

(defun chirp-actions--show-error (message)
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

(defun chirp-actions--perform (args on-success &optional on-error)
  "Run backend action ARGS and call ON-SUCCESS with the decoded payload.

When ON-ERROR is non-nil, call it with the human-readable error message."
  (chirp-backend-request
   args
   (lambda (data envelope)
     (chirp-backend-clear-cache)
     (funcall on-success data envelope))
   (or on-error
       #'chirp-actions--show-error)))

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
  "Return the read-only header shown above the compose body."
  (let ((title (pcase chirp-compose-kind
                 ('reply "Reply")
                 ('quote "Quote")
                 (_ "Post")))
        (context
         (pcase chirp-compose-kind
           ('reply
            (if chirp-compose-target-handle
                (format "Replying to @%s" chirp-compose-target-handle)
              (format "Replying to %s" chirp-compose-target-id)))
           ('quote
            (if chirp-compose-target-handle
                (format "Quoting @%s" chirp-compose-target-handle)
              (format "Quoting %s" chirp-compose-target-id)))
           (_ "Compose a new post."))))
    (concat
     (propertize title 'face 'bold)
     "\n"
     (propertize context 'face 'shadow)
     (when chirp-compose-target-url
       (concat "\n"
               (propertize chirp-compose-target-url 'face 'link))))))

(defun chirp-compose--ensure-idle ()
  "Signal a user error when the current draft is already sending."
  (when chirp-compose-sending
    (user-error "Draft is already sending")))

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
    (appkit-compose-refresh)
    (set-buffer-modified-p t)
    choice))

(defun chirp-compose--current-index ()
  "Return the compose item index that contains point."
  (or (appkit-compose-current-part-index) 0))

(defun chirp-compose--current-item ()
  "Return the compose item that contains point."
  (nth (chirp-compose--current-index) chirp-compose-items))

(defun chirp-compose--set-current-item (item)
  "Replace the current compose item with ITEM."
  (setcar (nthcdr (chirp-compose--current-index) chirp-compose-items) item)
  item)

(defun chirp-compose--item-attachments (&optional item)
  "Return attachment paths for ITEM or the current compose item."
  (plist-get (or item (chirp-compose--current-item)) :attachments))

(defun chirp-compose--status-fields ()
  "Return current Chirp compose status fields."
  (let ((fields
         (list (list :label "Media"
                     :value (format "%d/4"
                                    (length
                                     (chirp-compose--item-attachments)))))))
    (when (eq chirp-compose-kind 'post)
      (push (list :label "Posts"
                  :value (format "%d" (length chirp-compose-items)))
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
  "Return the Appkit attachment section for ITEM or the current item."
  (list :title "Images"
        :items
        (mapcar (lambda (path)
                  (list :label (abbreviate-file-name path)
                        :object path))
                (chirp-compose--item-attachments item))
        :empty-label "  No images attached."))

(defun chirp-compose--parts ()
  "Return Appkit compose parts for the current draft."
  (let ((total (length chirp-compose-items))
        (index 0))
    (mapcar (lambda (item)
              (setq index (1+ index))
              (list :title (and (> total 1)
                                (format "Post %d/%d" index total))
                    :attachments (chirp-compose--attachments-section item)))
            chirp-compose-items)))

(defun chirp-compose--footer-string ()
  "Return the read-only footer shown after the compose body."
  (propertize
   (concat
    "C-c C-a attach   C-c C-v paste   C-c C-d remove   "
    (when (eq chirp-compose-kind 'post)
      "C-c C-n add post   C-c C-p drop post   ")
    "C-c C-c send   C-c C-k cancel")
   'face 'shadow))

(defun chirp-compose--ensure-attachment-room ()
  "Signal a user error when the current item already has four images."
  (when (>= (length (chirp-compose--item-attachments)) 4)
    (user-error "Up to 4 attached images are supported")))

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
    (let ((item (or (chirp-compose--current-item)
                    (user-error "No compose item at point")))
          (attachments (chirp-compose--item-attachments)))
      (when (member file attachments)
        (user-error "Image already attached"))
      (chirp-compose--set-current-item
       (plist-put (copy-sequence item) :attachments
                  (append attachments (list file))))
      (when temporary
        (setq-local chirp-compose-temp-attachments
                    (append chirp-compose-temp-attachments (list file))))
      (appkit-compose-refresh)
      (set-buffer-modified-p t)
      file)))

(defun chirp-compose-attach-image (path)
  "Attach image PATH to the current draft."
  (interactive (list (read-file-name "Attach image: " nil nil t)))
  (chirp-compose--ensure-idle)
  (chirp-compose--ensure-attachment-room)
  (message "Attached %s"
           (file-name-nondirectory
            (chirp-compose--add-attachment path))))

(defun chirp-compose-paste-image ()
  "Paste one image from the clipboard into the current draft."
  (interactive)
  (chirp-compose--ensure-idle)
  (chirp-compose--ensure-attachment-room)
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
    (let* ((choice (if (= (length attachments) 1)
                       (car attachments)
                     (completing-read "Remove image: "
                                      attachments
                                      nil
                                      t
                                      nil
                                      nil
                                      (car attachments))))
           (removed (expand-file-name choice))
           (item (chirp-compose--current-item)))
      (chirp-compose--set-current-item
       (plist-put (copy-sequence item) :attachments
                  (delete removed attachments)))
      (chirp-compose--drop-temp-attachment removed)
      (appkit-compose-refresh)
      (set-buffer-modified-p t)
      (message "Removed %s" (file-name-nondirectory removed)))))

(defun chirp-compose--snapshot-items ()
  "Return draft items with text copied from the Appkit compose parts."
  (let ((bodies (appkit-compose-bodies))
        (index 0)
        items)
    (dolist (item chirp-compose-items)
      (let ((text (string-trim (or (nth index bodies) ""))))
        (when (string-empty-p text)
          (user-error "Text cannot be empty"))
        (push (list :text text
                    :attachments
                    (copy-sequence (plist-get item :attachments)))
              items))
      (setq index (1+ index)))
    (nreverse items)))

(defun chirp-compose--draft ()
  "Return the current compose buffer as a structured backend draft."
  (let ((draft (list :kind chirp-compose-kind
                     :target-id chirp-compose-target-id
                     :items (chirp-compose--snapshot-items))))
    (when (memq chirp-compose-kind '(post quote))
      (setq draft
            (plist-put draft :reply-audience
                       (or chirp-compose-reply-audience 'everyone))))
    draft))

(defun chirp-compose--unlock (buffer temp-attachments &optional unknown-p)
  "Restore BUFFER after a failed send and reattach TEMP-ATTACHMENTS.

When UNKNOWN-P is non-nil, mark the draft as having an unknown outcome."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq-local chirp-compose-sending nil)
      (setq-local chirp-compose-unknown-outcome unknown-p)
      (setq-local chirp-compose-temp-attachments
                  (append (copy-sequence temp-attachments)
                          chirp-compose-temp-attachments))
      (setq-local buffer-read-only nil))))

(defun chirp-compose--finish-send
    (compose-buffer source-buffer temp-attachments success-message)
  "Close COMPOSE-BUFFER after a successful send from SOURCE-BUFFER.

TEMP-ATTACHMENTS are deleted.  SUCCESS-MESSAGE is shown after the source
view is refreshed."
  (chirp-backend-clear-cache)
  (chirp-compose--cleanup-files temp-attachments)
  (when (buffer-live-p compose-buffer)
    (chirp-compose--close compose-buffer source-buffer))
  (when (buffer-live-p source-buffer)
    (chirp-actions--refresh-buffer source-buffer))
  (message "%s" success-message))

(defun chirp-compose--fail-send (compose-buffer temp-attachments message)
  "Restore COMPOSE-BUFFER after MESSAGE, or delete TEMP-ATTACHMENTS."
  (let ((unknown-p (chirp-actions--unknown-write-outcome-p message)))
    (if (buffer-live-p compose-buffer)
        (chirp-compose--unlock compose-buffer temp-attachments unknown-p)
      (chirp-compose--cleanup-files temp-attachments))
    (chirp-actions--show-error message)))

(defun chirp-compose--send-next
    (compose-buffer source-buffer draft items index previous-id
                    temp-attachments success-message)
  "Send ITEMS of DRAFT from INDEX, replying to PREVIOUS-ID after the first."
  (if (>= index (length items))
      (chirp-compose--finish-send
       compose-buffer source-buffer temp-attachments success-message)
    (let* ((item (nth index items))
           (root-p (zerop index))
           (kind (if root-p (plist-get draft :kind) 'reply))
           (target-id (if root-p (plist-get draft :target-id) previous-id)))
      (chirp-backend-compose
       :kind kind
       :text (plist-get item :text)
       :target-id target-id
       :attachments (plist-get item :attachments)
       :reply-audience (and root-p (plist-get draft :reply-audience))
       :callback
       (lambda (created _envelope)
         (chirp-compose--send-next
          compose-buffer source-buffer draft items (1+ index)
          (plist-get created :id) temp-attachments success-message))
       :errback
       (lambda (message)
         (chirp-compose--fail-send
          compose-buffer temp-attachments message))))))

(defun chirp-compose-send ()
  "Send the current draft and keep it until the request settles."
  (interactive)
  (chirp-compose--ensure-idle)
  (when (and chirp-compose-unknown-outcome
             (not (yes-or-no-p
                   (concat
                    "Previous send outcome is unknown; "
                    "the post may already exist. Send again? "))))
    (user-error "Send canceled"))
  (let* ((compose-buffer (current-buffer))
         (source-buffer chirp-compose-source-buffer)
         (draft (chirp-compose--draft))
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
           (t "Sending post..."))))
    (setq-local chirp-compose-sending t)
    (setq-local chirp-compose-unknown-outcome nil)
    (setq-local buffer-read-only t)
    (condition-case err
        (progn
          (chirp-compose--send-next
           compose-buffer source-buffer draft items 0 nil
           temp-attachments success-message)
          (when (and (buffer-live-p compose-buffer)
                     (with-current-buffer compose-buffer
                       chirp-compose-sending))
            (message "%s" sending-message)))
      (error
       (chirp-compose--unlock compose-buffer temp-attachments)
       (signal (car err) (cdr err))))))

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
  "Cancel the current draft."
  (interactive)
  (chirp-compose--ensure-idle)
  (chirp-compose--close (current-buffer) chirp-compose-source-buffer))

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

(defun chirp-compose-open (kind &optional tweet)
  "Open a compose buffer for KIND.

When TWEET is non-nil, use it as the reply or quote target."
  (let* ((source (chirp-compose--source-buffer))
         (buffer (generate-new-buffer "*chirp compose*")))
    (pop-to-buffer buffer)
    (with-current-buffer buffer
      (chirp-compose-mode)
      (setq-local chirp-compose-kind kind)
      (setq-local chirp-compose-target-id (plist-get tweet :id))
      (setq-local chirp-compose-target-handle (plist-get tweet :author-handle))
      (setq-local chirp-compose-target-url (plist-get tweet :url))
      (setq-local chirp-compose-source-buffer source)
      (setq-local chirp-compose-items (list (list :attachments nil)))
      (setq-local chirp-compose-temp-attachments nil)
      (setq-local chirp-compose-sending nil)
      (setq-local chirp-compose-unknown-outcome nil)
      (setq-local chirp-compose-reply-audience
                  (and (memq kind '(post quote)) 'everyone))
      (rename-buffer (chirp-compose--buffer-name) t)
      (add-hook 'kill-buffer-hook #'chirp-compose--cleanup-temp-attachments nil t)
      (appkit-compose-setup
       :context-function #'chirp-compose--header-string
       :status-fields-function #'chirp-compose--status-fields
       :parts-function #'chirp-compose--parts
       :footer-function #'chirp-compose--footer-string)
      (set-buffer-modified-p nil)
      (goto-char (appkit-compose-body-start-position)))))

(defun chirp-compose-add-post ()
  "Insert an empty post after the current draft item."
  (interactive)
  (chirp-compose--ensure-idle)
  (unless (eq chirp-compose-kind 'post)
    (user-error "Another post can be added only to a new post draft"))
  (let* ((index (1+ (chirp-compose--current-index)))
         (bodies (appkit-compose-bodies)))
    (setq-local chirp-compose-items
                (append (cl-subseq chirp-compose-items 0 index)
                        (list (list :attachments nil))
                        (cl-subseq chirp-compose-items index)))
    (appkit-compose-set-bodies
     (append (cl-subseq bodies 0 index)
             (list "")
             (cl-subseq bodies index)))
    (appkit-compose-refresh)
    (appkit-compose-goto-part index)
    (set-buffer-modified-p t)))

(defun chirp-compose-remove-post ()
  "Remove the current extra post from the draft."
  (interactive)
  (chirp-compose--ensure-idle)
  (unless (eq chirp-compose-kind 'post)
    (user-error "Only a new post draft can drop an extra post"))
  (unless (> (length chirp-compose-items) 1)
    (user-error "The draft already has only one post"))
  (let* ((index (chirp-compose--current-index))
         (item (nth index chirp-compose-items))
         (bodies (appkit-compose-bodies)))
    (dolist (file (plist-get item :attachments))
      (chirp-compose--drop-temp-attachment file))
    (setq-local chirp-compose-items
                (append (cl-subseq chirp-compose-items 0 index)
                        (cl-subseq chirp-compose-items (1+ index))))
    (appkit-compose-set-bodies
     (append (cl-subseq bodies 0 index)
             (cl-subseq bodies (1+ index))))
    (appkit-compose-refresh)
    (appkit-compose-goto-part (min index (1- (length chirp-compose-items))))
    (set-buffer-modified-p t)
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
    (chirp-compose-open 'reply tweet)))

(defun chirp--dispatch-mouse-action (event)
  "Dispatch the tweet action at mouse EVENT."
  (interactive "e")
  (let* ((position (event-start event))
         (window (posn-window position)))
    (unless (and (window-live-p window)
                 (integer-or-marker-p (posn-point position)))
      (user-error "Mouse click is not inside a Chirp buffer"))
    (with-current-buffer (window-buffer window)
      (unless (derived-mode-p 'chirp-view-mode)
        (user-error "Mouse click is not inside a Chirp view"))
      (posn-set-point position)
      (pcase (chirp--text-property-at-point 'chirp-tweet-action)
        ('reply (chirp-reply-at-point))
        ('retweet (chirp-toggle-retweet-at-point))
        ('like (chirp-toggle-like-at-point))
        ('bookmark (chirp-toggle-bookmark-at-point))
        (_ (user-error "No tweet action at mouse position"))))))

(defun chirp-quote-at-point ()
  "Open a compose buffer to quote the tweet at point."
  (interactive)
  (chirp-compose-open 'quote (chirp-actions--tweet-at-point)))

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
         (chirp-actions--show-error
          "No translated text returned by X")))
     #'chirp-actions--show-error)))

(transient-define-prefix chirp-dispatch ()
  "Show Chirp write actions."
  [["Timeline"
    ("h" "For You" chirp-timeline-open-home)
    ("f" "Following" chirp-timeline-open-following)
    ("u" "Me" chirp-me)
    ("b" "Bookmarks" chirp-timeline-open-bookmarks)
    ("L" "Liked" chirp-timeline-open-likes)
    ("s" "List" chirp-timeline-open-list)]
   ["Compose"
    ("c" "Post" chirp-compose-post)
    ("r" "Reply" chirp-reply-at-point)
    ("Q" "Quote" chirp-quote-at-point)]
   ["Tweet"
    ("R" "Retweet" chirp-toggle-retweet-at-point)]
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
