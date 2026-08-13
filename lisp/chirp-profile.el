;;; chirp-profile.el --- Profile view for chirp -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Fetch and render profile summaries, timelines, and account lists.

;;; Code:

(require 'cl-lib)
(require 'appkit-projection)
(require 'chirp-core)
(require 'chirp-backend)
(require 'chirp-media)
(require 'chirp-render)
(require 'chirp-timeline)

(defconst chirp-profile--base-modes '(posts replies highlights media)
  "Profile subviews shown for all profiles.")
(defvar-local chirp-profile--user nil
  "Buffer-local cached profile plist for the active profile view.")
(defvar-local chirp-profile--tweets nil
  "Buffer-local cached tweet list for the active profile view.")
(defvar-local chirp-profile--available-modes chirp-profile--base-modes
  "Buffer-local list of available profile subview modes.")

(defun chirp-profile--list-title (kind handle)
  "Return a title for KIND list belonging to HANDLE."
  (format "%s: @%s"
          (pcase kind
            ('followers "Followers")
            ('following "Following")
            (_ "Users"))
          (string-remove-prefix "@" handle)))

(defun chirp-profile--mode-label (mode)
  "Return a human-readable label for profile MODE."
  (pcase mode
    ('posts "Posts")
    ('replies "Replies")
    ('highlights "Highlights")
    ('media "Media")
    ('likes "Likes")
    (_ (capitalize (symbol-name mode)))))

(defun chirp-profile--paginated-mode-p (mode)
  "Return non-nil when profile MODE supports cursor-based pagination."
  (memq mode '(posts replies highlights media)))

(defun chirp-profile--title (handle mode)
  "Return a title for HANDLE profile subview MODE."
  (let ((base (format "@%s" (string-remove-prefix "@" handle))))
    (if (eq mode 'posts)
        base
      (format "%s · %s" base (chirp-profile--mode-label mode)))))

(defun chirp-profile--next-mode (current modes)
  "Return the mode following CURRENT inside MODES."
  (let* ((modes (or modes '(posts)))
         (index (or (cl-position current modes) 0)))
    (nth (mod (1+ index) (length modes)) modes)))

(defun chirp-profile--print-user-row (row)
  "Insert one projected user ROW."
  (chirp-render-insert-user-summary (appkit-projection-row-payload row)))

(defun chirp-profile--project-users (users)
  "Project USERS into keyed rows."
  (appkit-projection-project
   users
   (lambda (user)
     (list 'user (or (plist-get user :id) (plist-get user :handle))))))

(defun chirp-profile--header-text (state)
  "Return generated profile chrome and empty/error text for STATE."
  (with-temp-buffer
    (when-let* ((user (plist-get state :user)))
      (chirp-render-insert-user-summary user)
      (chirp-render-insert-profile-view-strip
       (plist-get state :mode)
       (plist-get state :modes)))
    (let* ((status (plist-get state :status))
           (phase (plist-get status :phase))
           (message (plist-get status :message))
           (mode (plist-get state :mode)))
      (cond
       ((plist-get state :items) nil)
       ((eq phase 'error)
        (insert (or message "Unable to load data."))
        (insert "\n"))
       ((plist-get state :timeline-ready)
        (insert (format "No %s returned.\n"
                        (downcase (chirp-profile--mode-label mode)))))
       ((plist-get state :user)
        (insert (propertize
                 (format "Loading %s..."
                         (downcase (chirp-profile--mode-label mode)))
                 'face 'shadow))
        (insert "\n"))))
    (buffer-string)))

(defun chirp-profile--sync (view invalidations)
  "Synchronize profile VIEW from INVALIDATIONS."
  (let ((state (appkit-view-state view)))
    (chirp-sync-projection
     view invalidations
     (chirp-timeline--project-rows (plist-get state :items))
     (chirp-profile--header-text state))))

(defun chirp-profile--users-sync (view invalidations)
  "Synchronize user-list VIEW from INVALIDATIONS."
  (let ((state (appkit-view-state view)))
    (chirp-sync-projection
     view invalidations
     (chirp-profile--project-users (plist-get state :items))
     (pcase (plist-get (plist-get state :status) :phase)
       ('initial (format "Loading %s...\n" (plist-get state :title)))
       ('error
        (format "Unable to load data.\n\n%s\n"
                (plist-get (plist-get state :status) :message)))
       (_ (and (null (plist-get state :items)) "No users returned.\n"))))))

(defun chirp-profile--bind-locals (view)
  "Mirror VIEW state into the profile buffer's command locals."
  (let ((state (appkit-view-state view))
        (buffer (appkit-view-buffer view)))
    (with-current-buffer buffer
      (setq-local chirp-profile--user (plist-get state :user))
      (setq-local chirp-profile--tweets (plist-get state :items))
      (setq-local chirp--profile-view-mode (plist-get state :mode))
      (setq-local chirp-profile--available-modes (plist-get state :modes))
      (setq-local chirp--profile-handle
                  (plist-get (plist-get state :user) :handle))
      (setq-local chirp--profile-switch-mode-function
                  (lambda (target)
                    (chirp-profile--switch-mode
                     chirp--profile-handle target buffer)))
      (setq-local chirp--timeline-count (length (plist-get state :items)))
      (setq-local chirp--timeline-next-cursor
                  (plist-get (plist-get state :page) :next-cursor))
      (setq-local chirp--timeline-load-more-function
                  (and (plist-get state :timeline-ready)
                       (chirp-profile--paginated-mode-p (plist-get state :mode))
                       (plist-get (plist-get state :page) :next-cursor)
                       #'chirp-profile-load-more))
      (setq-local chirp--timeline-exhausted-p
                  (and (plist-get state :timeline-ready)
                       (not (plist-get (plist-get state :page) :next-cursor))))
      (setq-local chirp--timeline-loading-more
                  (plist-get state :loading-more)))))

(defun chirp-profile--ensure-view (handle title refresh mode)
  "Open or reuse HANDLE's profile view titled TITLE.
REFRESH reloads the selected MODE."
  (chirp-open-projection-view
   :id (list 'profile handle)
   :title title
   :state (list :type 'profile
                :query (list :handle handle :mode mode)
                :user nil
                :items nil
                :mode mode
                :modes chirp-profile--base-modes
                :title title
                :refresh refresh
                :page (list :next-cursor nil)
                :status (list :phase 'initial :message nil)
                :timeline-ready nil
                :loading-more nil
                :expanded-tweet-ids (make-hash-table :test #'equal))
   :sync-function #'chirp-profile--sync
   :printer #'chirp-timeline--print-row
   :select t))

(defun chirp-profile--present (view)
  "Request a projection update for profile VIEW."
  (chirp-profile--bind-locals view)
  (appkit-invalidate view :structure t :part 'frame :position t)
  (appkit-sync-invalidations view))

(defun chirp-profile--open-user-list (kind handle &optional _buffer)
  "Open KIND user list for HANDLE."
  (let* ((clean-handle (string-remove-prefix "@" handle))
         (title (chirp-profile--list-title kind clean-handle))
         (refresh (lambda ()
                    (chirp-backend-invalidate-user clean-handle)
                    (chirp-profile--open-user-list kind clean-handle)))
         (view (chirp-open-projection-view
                :id (list 'users kind clean-handle)
                :title title
                :state (list :type 'users
                             :query (list :kind kind :handle clean-handle)
                             :items nil
                             :title title
                             :refresh refresh
                             :status (list :phase 'initial :message nil)
                             :wrap-navigation nil)
                :sync-function #'chirp-profile--users-sync
                :printer #'chirp-profile--print-user-row
                :select t))
         (buffer (appkit-view-buffer view))
         (fetch-fn (pcase kind
                     ('followers #'chirp-backend-followers)
                     ('following #'chirp-backend-following-users)
                     (_ (error "Unknown profile list kind: %S" kind))))
         (token (chirp-begin-background-request buffer title)))
    (funcall
     fetch-fn
     clean-handle
     (lambda (users _envelope)
       (when (chirp-request-current-p buffer token)
         (let ((state (appkit-view-state view)))
           (setf (plist-get state :items) users
                 (plist-get (plist-get state :status) :phase) 'idle)
           (appkit-invalidate view :structure t :part 'frame :position t)
           (appkit-sync-invalidations view)
           (chirp-clear-status buffer)
           (dolist (user users)
             (chirp-media-prefetch-user user buffer)))))
     (lambda (message)
       (when (chirp-request-current-p buffer token)
         (chirp-show-error buffer title refresh message))))
    buffer))

(cl-defun chirp-profile--render
    (buffer title refresh user tweets current-mode modes
            &key _anchor-id display-p timeline-ready next-cursor status-message)
  "Install USER and TWEETS in BUFFER's profile view titled TITLE.

REFRESH reloads the view.  CURRENT-MODE selects one of MODES.  DISPLAY-P
shows the buffer.  TIMELINE-READY, NEXT-CURSOR, and STATUS-MESSAGE update
pagination and empty-state chrome."
  (let* ((handle (or (plist-get user :handle)
                     (with-current-buffer buffer chirp--profile-handle)))
         (view (or (and (buffer-live-p buffer)
                        (with-current-buffer buffer
                          (chirp--live-projection-view)))
                   (chirp-profile--ensure-view
                    handle title refresh current-mode)))
         (state (appkit-view-state view)))
    (setf (plist-get state :user) user
          (plist-get state :items) tweets
          (plist-get state :mode) current-mode
          (plist-get state :modes) modes
          (plist-get state :title) title
          (plist-get state :refresh) refresh
          (plist-get state :timeline-ready) timeline-ready
          (plist-get (plist-get state :page) :next-cursor) next-cursor
          (plist-get (plist-get state :status) :phase)
          (if status-message 'error 'idle)
          (plist-get (plist-get state :status) :message) status-message)
    (chirp-profile--present view)
    (when display-p
      (chirp-display-buffer (appkit-view-buffer view)))
    (appkit-view-buffer view)))

(defun chirp-profile-load-more (&optional _anchor-id)
  "Load older items for the current profile view."
  (interactive)
  (let* ((view (chirp--live-projection-view))
         (state (and view (appkit-view-state view))))
    (unless (and state (eq (plist-get state :type) 'profile)
                 (plist-get (plist-get state :user) :handle))
      (user-error "Current view does not support loading more items"))
    (cond
     ((plist-get state :loading-more)
      (message "Already loading older posts..."))
     ((or (not (plist-get (plist-get state :page) :next-cursor))
          (and (plist-get state :timeline-ready)
               (not (plist-get (plist-get state :page) :next-cursor))))
      (message "No older posts."))
     (t
      (let* ((buffer (appkit-view-buffer view))
             (handle (plist-get (plist-get state :user) :handle))
             (saved-mode (plist-get state :mode))
             (current (plist-get state :items))
             (mode-label (downcase (chirp-profile--mode-label saved-mode)))
             (cursor (plist-get (plist-get state :page) :next-cursor))
             (token (chirp-begin-request buffer)))
        (setf (plist-get state :loading-more) t)
        (chirp-set-status buffer (format "Loading older %s..." mode-label))
        (message "Loading older %s..." mode-label)
        (chirp-profile--fetch-content
         saved-mode
         handle
         (lambda (tweets envelope)
           (when (chirp-request-current-p buffer token)
             (let* ((next-cursor (chirp-backend-envelope-next-cursor envelope))
                    (merged (chirp-append-unique-tweets current tweets))
                    (added-p (> (length merged) (length current))))
               (setf (plist-get state :items) merged
                     (plist-get (plist-get state :page) :next-cursor) next-cursor
                     (plist-get state :timeline-ready) t
                     (plist-get state :loading-more) nil)
               (appkit-view-enqueue-event view (list :position 'preserve))
               (chirp-profile--present view)
               (chirp-clear-status buffer)
               (chirp-media-prefetch-tweets tweets buffer)
               (chirp-enrich-quoted-tweets tweets buffer)
               (unless added-p
                 (message "No older %s." mode-label)))))
         (lambda (message)
           (when (chirp-request-current-p buffer token)
             (setf (plist-get state :loading-more) nil)
             (chirp-set-status buffer "Load more failed" 'error)
             (message "%s" (replace-regexp-in-string "[\r\n]+" "  " message))))
         chirp-profile-post-limit
         cursor))))))

(defun chirp-profile--fetch-content (mode handle callback errback &optional max-results cursor)
  "Fetch profile MODE content for HANDLE and call CALLBACK.

Call ERRBACK on failure.  MAX-RESULTS limits the response.  CURSOR is only used
for paginated modes."
  (pcase mode
    ('posts
     (chirp-backend-user-posts handle callback errback max-results cursor))
    ('replies
     (chirp-backend-user-replies handle callback errback max-results cursor))
    ('highlights
     (chirp-backend-user-highlights handle callback errback max-results cursor))
    ('media
     (chirp-backend-user-media handle callback errback max-results cursor))
    ('likes
     (chirp-backend-likes handle callback errback))
    (_
     (funcall (or errback
                  (lambda (message)
                    (message "%s" message)))
              (format "Unsupported profile mode: %S" mode)))))

(defun chirp-profile--switch-mode (handle target &optional buffer)
  "Switch HANDLE profile BUFFER to TARGET mode.

When TARGET is `:next', cycle through the available profile modes."
  (let* ((buffer (or buffer (current-buffer)))
         (mode (with-current-buffer buffer
                 (if (eq target :next)
                     (chirp-profile--next-mode
                      chirp--profile-view-mode
                      chirp-profile--available-modes)
                   target))))
    (unless (eq mode (with-current-buffer buffer chirp--profile-view-mode))
      (chirp-profile-open handle buffer mode))))

(defun chirp-profile-open (handle &optional _buffer mode)
  "Open HANDLE's profile.

MODE selects the active profile subview and defaults to `posts'."
  (interactive "sProfile handle: ")
  (let* ((clean-handle (string-remove-prefix "@" handle))
         (mode (or mode 'posts))
         (title (chirp-profile--title clean-handle mode))
         (refresh (lambda ()
                    (chirp-backend-invalidate-user clean-handle)
                    (chirp-profile-open clean-handle nil mode)))
         (view (chirp-profile--ensure-view clean-handle title refresh mode))
         (buffer (appkit-view-buffer view))
         (token nil)
         (saved-user nil)
         (saved-tweets nil)
         (available-modes chirp-profile--base-modes)
         (timeline-next-cursor nil)
         (user-ready nil)
         (timeline-ready nil)
         (whoami-ready nil)
         (timeline-error-message nil)
         (content-prefetched nil))
    (cl-labels
        ((present-current ()
           (when user-ready
             (let ((state (appkit-view-state view)))
               (setf (plist-get state :user) saved-user
                     (plist-get state :items) saved-tweets
                     (plist-get state :mode) mode
                     (plist-get state :modes) available-modes
                     (plist-get state :title) title
                     (plist-get state :refresh) refresh
                     (plist-get state :timeline-ready) timeline-ready
                     (plist-get (plist-get state :page) :next-cursor)
                     timeline-next-cursor
                     (plist-get (plist-get state :status) :phase)
                     (if timeline-error-message 'error 'idle)
                     (plist-get (plist-get state :status) :message)
                     timeline-error-message)
               (chirp-profile--present view)))
           (when (and user-ready
                      timeline-ready
                      (not content-prefetched))
             (setq content-prefetched t)
             (when saved-tweets
               (chirp-media-prefetch-tweets saved-tweets buffer)
               (chirp-enrich-quoted-tweets saved-tweets buffer)))
           (when (and user-ready timeline-ready whoami-ready)
             (with-current-buffer buffer
               (setq-local chirp--request-token nil))))
         (update-status ()
           (cond
            (timeline-error-message
             (chirp-set-status
              buffer
              (format "%s failed" (chirp-profile--mode-label mode))
              'error))
            ((and user-ready timeline-ready)
             (chirp-clear-status buffer))
            (user-ready
             (chirp-set-status
              buffer
              (format "Profile ready · loading %s..."
                      (downcase (chirp-profile--mode-label mode)))))
            (timeline-ready
             (chirp-set-status
              buffer
              (format "%s ready · loading profile..."
                      (chirp-profile--mode-label mode)))))))
      (setq token (chirp-begin-background-request buffer title))
      (chirp-set-status buffer "Loading profile...")
      (chirp-profile--bind-locals view)
      (chirp-backend-user
       clean-handle
       (lambda (user _envelope)
         (when (chirp-request-current-p buffer token)
           (setq saved-user user
                 user-ready t)
           (plist-put saved-user :self-p (memq 'likes available-modes))
           (update-status)
           (present-current)
           (chirp-media-prefetch-user saved-user buffer)))
       (lambda (message)
         (when (chirp-request-current-p buffer token)
           (with-current-buffer buffer
             (setq-local chirp--request-token nil))
           (chirp-show-error buffer title refresh message))))
      (chirp-backend-whoami
       (lambda (user _envelope)
         (when (chirp-request-current-p buffer token)
           (let* ((self-handle (plist-get user :handle))
                  (modes (if (and self-handle
                                  (string-equal
                                   (downcase (string-remove-prefix "@" self-handle))
                                   (downcase clean-handle)))
                             (append chirp-profile--base-modes '(likes))
                           chirp-profile--base-modes)))
             (setq available-modes modes
                   whoami-ready t)
             (when saved-user
               (plist-put saved-user :self-p (memq 'likes modes)))
             (when (not (memq mode modes))
               (setq mode 'posts
                     title (chirp-profile--title clean-handle mode)))
             (when user-ready
               (update-status)
               (present-current)))))
       (lambda (_message)
         (when (chirp-request-current-p buffer token)
           (setq whoami-ready t)
           (present-current))))
      (chirp-profile--fetch-content
       mode
       clean-handle
       (lambda (tweets timeline-envelope)
         (when (chirp-request-current-p buffer token)
           (setq saved-tweets tweets
                 timeline-ready t
                 timeline-error-message nil
                 timeline-next-cursor
                 (and (chirp-profile--paginated-mode-p mode)
                      (chirp-backend-envelope-next-cursor timeline-envelope)))
           (update-status)
           (present-current)))
       (lambda (message)
         (when (chirp-request-current-p buffer token)
           (setq saved-tweets nil
                 timeline-ready t
                 timeline-next-cursor nil
                 timeline-error-message
                 (format "Unable to load %s.\n\n%s"
                         (downcase (chirp-profile--mode-label mode))
                         message))
           (update-status)
           (present-current)))
       chirp-profile-post-limit)
      buffer)))

;;;###autoload
(defun chirp-profile-open-followers (handle &optional buffer)
  "Open followers for HANDLE in BUFFER."
  (interactive "sProfile handle: ")
  (chirp-profile--open-user-list 'followers handle buffer))

;;;###autoload
(defun chirp-profile-open-following-users (handle &optional buffer)
  "Open followed accounts for HANDLE in BUFFER."
  (interactive "sProfile handle: ")
  (chirp-profile--open-user-list 'following handle buffer))

(provide 'chirp-profile)

;;; chirp-profile.el ends here
