;;; chirp-media.el --- Media fetching and display for chirp -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Fetch, cache, prefetch, render, and open media attached to Chirp entries.

;;; Code:

(require 'cl-lib)
(require 'image)
(require 'image-mode)
(require 'subr-x)
(require 'svg)
(require 'url)
(require 'url-parse)
(require 'warnings)
(require 'plz)
(require 'appkit-core)
(require 'appkit-evil)
(require 'appkit-invalidation)
(require 'appkit-projection)
(require 'appkit-media-image)
(require 'appkit-media-video)
(require 'appkit-media-resource)
(require 'appkit-chat-avatar)
(require 'appkit-task-queue)
(require 'chirp-core)
(require 'chirp-backend)
(require 'chirp-xchat-native)

;;; Options

(defcustom chirp-cache-directory
  (locate-user-emacs-file "chirp/")
  "Directory used for cached Chirp media files."
  :type 'directory
  :group 'chirp)

(defcustom chirp-media-download-directory
  "~/Downloads/"
  "Directory used as the default target for downloaded media."
  :type 'directory
  :group 'chirp)

(defcustom chirp-avatar-size 28
  "Baseline avatar size relative to one text line.

28 means exactly one default-face line at the current text scale.
Larger values grow the avatar relative to that line."
  :type 'integer
  :group 'chirp)

(defcustom chirp-media-thumbnail-size 256
  "Preferred maximum cell size for timeline media previews."
  :type 'integer
  :group 'chirp)

(defcustom chirp-media-track-height 384
  "Maximum pixel height for natural-ratio media in focused thread tracks."
  :type 'integer
  :group 'chirp)

(defcustom chirp-media-view-max-width 1200
  "Maximum image width in the media viewer."
  :type 'integer
  :group 'chirp)

(defcustom chirp-media-view-max-height 900
  "Maximum image height in the media viewer."
  :type 'integer
  :group 'chirp)

(defcustom chirp-media-render-from-cache-only t
  "When non-nil, list views render avatars and thumbnails only from cache.

Missing files are prefetched in the background instead of blocking first paint."
  :type 'boolean
  :group 'chirp)

(defcustom chirp-media-prefetch-images t
  "When non-nil, Chirp prefetches missing list-view images in the background."
  :type 'boolean
  :group 'chirp)

(defcustom chirp-media-prefetch-concurrency 4
  "Maximum number of concurrent background media downloads."
  :type 'integer
  :group 'chirp)

(defcustom chirp-media-thumbnail-concurrency 1
  "Maximum number of concurrent video/GIF thumbnail extraction jobs."
  :type 'integer
  :group 'chirp)

(defcustom chirp-link-card-prefetch-enabled t
  "When non-nil, Chirp fetches lightweight link-card metadata for external URLs."
  :type 'boolean
  :group 'chirp)

(defcustom chirp-link-card-prefetch-concurrency 2
  "Maximum number of concurrent background link-card metadata fetches."
  :type 'integer
  :group 'chirp)

(defcustom chirp-link-card-max-per-tweet 1
  "Maximum number of external link cards rendered for one tweet."
  :type 'integer
  :group 'chirp)

(defcustom chirp-link-card-fetch-timeout 10
  "Maximum seconds allowed for one background link-card fetch."
  :type 'integer
  :group 'chirp)

(defcustom chirp-media-prefetch-video-remote-thumbnail t
  "When non-nil, try extracting video/GIF thumbnails directly from media URLs.

This restores list thumbnails for video-like media without immediately caching
the full file locally.  When remote extraction fails, Chirp can still fall back
to downloading the whole media file if
`chirp-media-prefetch-video-fallback-download' is non-nil."
  :type 'boolean
  :group 'chirp)

(defcustom chirp-media-prefetch-video-fallback-download nil
  "When non-nil, download full video/GIF files to synthesize list thumbnails.

This can consume substantial bandwidth when upstream data does not include a
lightweight preview image.  When nil, Chirp keeps the text placeholder instead."
  :type 'boolean
  :group 'chirp)

(defcustom chirp-media-prefetch-command
  (executable-find "curl")
  "Command used for background media prefetching.

When nil, Chirp skips background prefetch and only uses cached list-view images."
  :type '(choice (const :tag "Disabled" nil) string)
  :group 'chirp)

(defcustom chirp-video-player-command
  (executable-find "mpv")
  "External video player command used for tweet videos.
When nil, Chirp opens video URLs in a browser."
  :type '(choice (const :tag "Browser" nil) string)
  :group 'chirp)

(defcustom chirp-video-player-window-size nil
  "Preferred initial mpv window size for tweet videos.

When non-nil and `chirp-video-player-command' points to `mpv', Chirp adds
`--geometry=WIDTHxHEIGHT' when launching external playback.  Other players
ignore this setting."
  :type '(choice (const :tag "Player default" nil)
                 (cons :tag "Width x Height" integer integer))
  :group 'chirp)

(defcustom chirp-video-playback-max-bitrate 2176000
  "Maximum preferred MP4 bitrate for direct external playback.

When non-nil, Chirp picks the highest video variant at or below this bitrate.
If all known variants exceed the limit, Chirp falls back to the lowest bitrate
variant to keep playback responsive."
  :type '(choice (const :tag "Highest available" nil) integer)
  :group 'chirp)

(defcustom chirp-video-thumbnail-command
  (executable-find "ffmpeg")
  "Command used to extract video and animated GIF thumbnails.
When nil, Chirp falls back to a text placeholder for video-like media."
  :type '(choice (const :tag "Disabled" nil) string)
  :group 'chirp)

(defcustom chirp-video-thumbnail-offset 0.0
  "Seconds into a video-like media item used for thumbnail extraction."
  :type 'number
  :group 'chirp)

;;; View State

(defvar-local chirp--media-list nil
  "Media list displayed by the current Chirp media buffer.")

(defvar-local chirp--media-index 0
  "Currently selected media index in the current media buffer.")

(defvar-local chirp--media-title nil
  "Base title used by the current media buffer.")

(defvar-local chirp--media-source-buffer nil
  "Source Chirp buffer that opened the current media buffer.")

(defvar-local chirp--media-source-anchor nil
  "Saved source point anchor used when closing the current media buffer.")

(defvar-local chirp--media-source-window-state nil
  "Saved source window state used when closing the current media buffer.")

;;; Runtime

(cl-defstruct (chirp-media--runtime
               (:constructor chirp-media--runtime-create))
  "Media state owned by one Chirp application session."
  prefetch-tasks
  prefetch-pending
  thumbnail-tasks
  thumbnail-pending
  link-card-tasks
  link-card-pending
  link-card-cache)

(defconst chirp-media--link-card-fetch-failed :chirp-link-card-fetch-failed
  "Sentinel stored for failed link-card fetches.")

(defconst chirp-media--link-card-source-limit (* 256 1024)
  "Maximum bytes accepted from one background link-card response.")


(defun chirp-media--curl-protocols (url)
  "Return curl's allowed protocols for uncredentialed URL."
  (if (string-prefix-p "http://" (downcase url))
      "=http,https"
    "=https"))

(defun chirp-media--curl-default-args (max-bytes max-time protocols)
  "Return bounded curl arguments for MAX-BYTES, MAX-TIME, and PROTOCOLS."
  (append
   (list "--disable" "--silent" "--fail"
         "--proto" protocols
         "--max-redirs" "0"
         "--connect-timeout" "10"
         "--max-time" (number-to-string max-time))
   (when max-bytes
     (list "--max-filesize" (number-to-string max-bytes)))))

(defconst chirp-media--image-source-limit (* 25 1024 1024)
  "Maximum bytes accepted from one background image URL.")

(defconst chirp-media--safe-curl-default-args
  (chirp-media--curl-default-args
   chirp-media--image-source-limit 60 "=https")
  "Fixed curl defaults for uncredentialed background image reads.")

(defun chirp-media--runtime ()
  "Return media state for the current Chirp session."
  (let ((session (chirp--session)))
    (or (chirp--session-media-runtime session)
        (setf (chirp--session-media-runtime session)
              (chirp-media--runtime-create
               :prefetch-pending (make-hash-table :test #'equal)
               :thumbnail-pending (make-hash-table :test #'equal)
               :link-card-pending (make-hash-table :test #'equal)
               :link-card-cache (make-hash-table :test #'equal))))))

(defun chirp-media--pending-table (kind)
  "Return the current media pending table for KIND."
  (let ((runtime (chirp-media--runtime)))
    (pcase kind
      ('prefetch (chirp-media--runtime-prefetch-pending runtime))
      ('thumbnail (chirp-media--runtime-thumbnail-pending runtime))
      ('link-card (chirp-media--runtime-link-card-pending runtime))
      (_ (error "Unknown Chirp media task kind: %S" kind)))))

(defun chirp-media--task-queue (kind limit)
  "Return the current session's task queue for KIND and LIMIT."
  (let* ((runtime (chirp-media--runtime))
         (queue
          (pcase kind
            ('prefetch (chirp-media--runtime-prefetch-tasks runtime))
            ('thumbnail (chirp-media--runtime-thumbnail-tasks runtime))
            ('link-card (chirp-media--runtime-link-card-tasks runtime))
            (_ (error "Unknown Chirp media task kind: %S" kind)))))
    (if (appkit-task-queue-live-p queue)
        (progn
          (unless (= (appkit-task-queue-limit queue) limit)
            (appkit-task-queue-set-limit queue limit))
          queue)
      (setq queue (appkit-task-queue-create (chirp-app) limit))
      (pcase kind
        ('prefetch
         (setf (chirp-media--runtime-prefetch-tasks runtime) queue))
        ('thumbnail
         (setf (chirp-media--runtime-thumbnail-tasks runtime) queue))
        ('link-card
         (setf (chirp-media--runtime-link-card-tasks runtime) queue)))
      queue)))

(defun chirp-media-quit ()
  "Close the current media buffer."
  (interactive)
  (let ((source-buffer chirp--media-source-buffer)
        (source-anchor chirp--media-source-anchor)
        (source-window-state chirp--media-source-window-state))
    (chirp-quit-current-buffer)
    (when (buffer-live-p source-buffer)
      (chirp-display-buffer source-buffer)
      (or (chirp-restore-window-state source-window-state)
          (with-current-buffer source-buffer
            (when source-anchor
              (chirp-restore-point-anchor source-anchor)))))))

;;; Modes

(defvar-keymap chirp-media-view-mode-map
  :doc "Keymap for `chirp-media-view-mode'."
  :parent special-mode-map
  "n" #'chirp-media-next
  "p" #'chirp-media-previous
  "D" #'chirp-media-download-at-point
  "v" #'chirp-media-play
  "o" #'chirp-media-browse
  "q" #'chirp-media-quit)

(define-derived-mode chirp-media-view-mode special-mode "Chirp-Media"
  "Major mode for large media in Chirp."
  (appkit-evil-normalize-keymaps))

(defvar-keymap chirp-media-image-mode-map
  :doc "Keymap for `chirp-media-image-mode'."
  :parent image-mode-map
  "n" #'chirp-media-next
  "p" #'chirp-media-previous
  "D" #'chirp-media-download-at-point
  "v" #'chirp-media-play
  "o" #'chirp-media-browse
  "q" #'chirp-media-quit)

(define-derived-mode chirp-media-image-mode image-mode "Chirp-Image"
  "Image mode used for Chirp photo viewing."
  (setq-local header-line-format nil)
  (appkit-evil-normalize-keymaps))

(defun chirp-media--setup-evil ()
  "Install optional Evil bindings for Chirp media views."
  (when appkit-evil-enable-integration
    (appkit-evil-set-initial-states
     '(chirp-media-view-mode chirp-media-image-mode) 'normal)
    (appkit-evil-define-readonly-keys 'chirp-media-view-mode-map)
    (appkit-evil-define-readonly-keys 'chirp-media-image-mode-map)
    (appkit-evil-map
      (:map chirp-media-view-mode-map
       :nm
       "g j" #'chirp-media-next
       "g k" #'chirp-media-previous
       "g d" #'chirp-media-download-at-point
       "RET" #'chirp-media-play
       "g o" #'chirp-media-browse)
      (:map chirp-media-image-mode-map
       :nm
       "g j" #'chirp-media-next
       "g k" #'chirp-media-previous
       "g d" #'chirp-media-download-at-point
       "RET" #'chirp-media-play
       "g o" #'chirp-media-browse))
    (appkit-evil-normalize-buffers
     '(chirp-media-view-mode chirp-media-image-mode))))

(chirp-media--setup-evil)

(with-eval-after-load 'evil
  (chirp-media--setup-evil))

;;; Media Identity and Cache

(defun chirp-media-video-like-p (media)
  "Return non-nil when MEDIA is video-like."
  (member (plist-get media :type) '("video" "animated_gif")))

(defun chirp-media-at-point ()
  "Return the media item stored at point, or nil."
  (or (get-text-property (point) 'chirp-media-item)
      (and (> (point) (point-min))
           (get-text-property (1- (point)) 'chirp-media-item))))

(defun chirp-media-index-at-point ()
  "Return the media index stored at point, or nil."
  (or (get-text-property (point) 'chirp-media-index)
      (and (> (point) (point-min))
           (get-text-property (1- (point)) 'chirp-media-index))))

(defun chirp-media-list-at-point ()
  "Return the media list stored at point, or nil."
  (or (get-text-property (point) 'chirp-media-list)
      (and (> (point) (point-min))
           (get-text-property (1- (point)) 'chirp-media-list))))

(defun chirp-media--cache-subdir (kind)
  "Return the cache subdirectory for KIND."
  (let ((dir (expand-file-name kind chirp-cache-directory)))
    (make-directory dir t)
    dir))

(defun chirp-media--url-extension (url fallback)
  "Return a file extension for URL, or FALLBACK."
  (let* ((parsed (ignore-errors (url-generic-parse-url url)))
         (path (and parsed (url-filename parsed)))
         (base (and path
                    (car (split-string (file-name-nondirectory path) "\\?"))))
         (ext (and base (file-name-extension base))))
    (downcase (or ext fallback))))

(defun chirp-media--photo-original-url (url)
  "Return the original-resolution photo URL for URL when possible."
  (if (and (stringp url)
           (string-match-p "\\`https://pbs\\.twimg\\.com/" url))
      (if (string-match-p "[?&]name=" url)
          (replace-regexp-in-string
           "\\([?&]name=\\)[^&#]*"
           "\\1orig"
           url
           t
           nil)
        (concat url (if (string-match-p "\\?" url) "&" "?") "name=orig"))
    url))

(defun chirp-media-cache-base (identity kind)
  "Return an extensionless cache path for IDENTITY in media KIND."
  (unless (and (stringp identity) (not (string-empty-p identity))
               (stringp kind) (not (string-empty-p kind)))
    (error "Chirp media cache identity and kind must be non-empty strings"))
  (expand-file-name
   (secure-hash 'sha1 identity)
   (chirp-media--cache-subdir kind)))

(defun chirp-media--cache-file (url kind fallback-ext)
  "Return a cache file path for URL of KIND using FALLBACK-EXT when needed."
  (format "%s.%s"
          (chirp-media-cache-base url kind)
          (chirp-media--url-extension url fallback-ext)))

(defconst chirp-media--image-cache-extensions
  '("bmp" "gif" "heic" "heif" "img" "jpeg" "jpg" "png" "svg" "svgz"
    "tif" "tiff" "webp")
  "Extensions whose cache files must contain recognizable image data.")

(defun chirp-media--valid-cache-file-p (path)
  "Return non-nil when PATH contains valid data for its cache extension."
  (and (file-regular-p path)
       (> (file-attribute-size (file-attributes path)) 0)
       (or (not (member (downcase (or (file-name-extension path) ""))
                        chirp-media--image-cache-extensions))
           (ignore-errors (image-type-from-file-header path)))))

(defun chirp-media-cached-file (url kind &optional fallback-ext)
  "Return the valid cached local file for URL of KIND, or nil when absent.

Use FALLBACK-EXT when URL has no recognizable extension."
  (when (and (stringp url)
             (not (string-empty-p url)))
    (let ((path (chirp-media--cache-file url kind (or fallback-ext "bin"))))
      (when (chirp-media--valid-cache-file-p path)
        path))))

(defun chirp-media--download-file (url path)
  "Download URL to PATH unless PATH already contains valid cache data."
  (unless (chirp-media--valid-cache-file-p path)
    (when (file-exists-p path)
      (ignore-errors (delete-file path)))
    (condition-case nil
        (let ((inhibit-message t))
          (url-copy-file url path t))
      (error nil)))
  (when (chirp-media--valid-cache-file-p path)
    path))

(defun chirp-media-local-file (url kind &optional fallback-ext)
  "Return a local cached file for URL of KIND.

Use FALLBACK-EXT when URL has no recognizable extension."
  (when (and (stringp url)
             (not (string-empty-p url)))
    (chirp-media--download-file
     url
     (chirp-media--cache-file url kind (or fallback-ext "bin")))))

(defun chirp-media--prefetch-enabled-p ()
  "Return non-nil when background prefetching can run."
  (and chirp-media-prefetch-images
       (display-images-p)
       chirp-media-prefetch-command
       (> chirp-media-prefetch-concurrency 0)))

(defun chirp-media--link-card-enabled-p ()
  "Return non-nil when external link-card fetching can run."
  (and chirp-link-card-prefetch-enabled
       chirp-media-prefetch-command
       (> chirp-link-card-prefetch-concurrency 0)))

;;; Image Resources

(defun chirp-media--finish-image-resource
    (app resource-key entry status &optional file)
  "Finish APP image ENTRY for RESOURCE-KEY with STATUS and optional FILE."
  (when (appkit-app-live-p app)
    (when-let* ((handle (plist-get entry :handle)))
      (appkit-retire-handle handle)
      (setf (plist-get entry :handle) nil))
    (when (eq entry (gethash resource-key
                             (appkit-app-resource-store app)))
      (when (and (eq status 'ready)
                 (not (chirp-media--valid-cache-file-p file)))
        (when (and (stringp file) (file-exists-p file))
          (ignore-errors (delete-file file)))
        (setq status 'failed
              file nil))
      (setf (plist-get entry :status) status
            (plist-get entry :file) file)
      (dolist (view (plist-get entry :views))
        (when (appkit-view-live-p view)
          (appkit-request-sync view :resource resource-key :position t))))))

(cl-defun chirp-media-request-image-resource
    (view resource-key source &key name)
  "Acquire image SOURCE for RESOURCE-KEY on behalf of Appkit VIEW.

The view's app owns and shares the transfer.  Completion invalidates only
requesting live views.  A later request retries a failed acquisition.  NAME
optionally supplies the source filename used for media type hints."
  (when (and (appkit-view-live-p view)
             resource-key
             (stringp source)
             (not (string-empty-p source))
             (chirp-media--prefetch-enabled-p))
    (let* ((app (appkit-view-app view))
           (store (appkit-app-resource-store app))
           (current (gethash resource-key store)))
      (cond
       ((and (eq (plist-get current :status) 'ready)
             (equal (plist-get current :source) source)
             (chirp-media--valid-cache-file-p
              (plist-get current :file)))
        current)
       ((and (eq (plist-get current :status) 'pending)
             (equal (plist-get current :source) source))
        (cl-pushnew view (plist-get current :views) :test #'eq)
        current)
       (t
        (when (eq (plist-get current :status) 'pending)
          (remhash resource-key store)
          (when-let* ((handle (plist-get current :handle)))
            (setf (plist-get current :handle) nil)
            (appkit-cancel-handle handle)))
        (let* ((cache-base (chirp-media-cache-base source "media"))
               (cached (appkit-media-image-cache-existing-file cache-base))
               (entry (list :source source :status 'pending :file nil
                            :handle nil
                            :views (cl-adjoin view (plist-get current :views)
                                              :test #'eq)))
               transfer)
          (puthash resource-key entry store)
          (if (and cached (chirp-media--valid-cache-file-p cached))
              (setf (plist-get entry :status) 'ready
                    (plist-get entry :file) cached)
            (when (and cached (file-exists-p cached))
              (ignore-errors (delete-file cached)))
            (let ((plz-curl-program chirp-media-prefetch-command)
                  (plz-curl-default-args chirp-media--safe-curl-default-args))
              (setq transfer
                    (appkit-media-cache-image-resource-async
                     (appkit-media-resource-create :url source :name name)
                     cache-base
                     (lambda (file)
                       (chirp-media--finish-image-resource
                        app resource-key entry 'ready file))
                     (lambda (_message)
                       (chirp-media--finish-image-resource
                        app resource-key entry 'failed))))))
          (when (and (eq (plist-get entry :status) 'pending)
                     (appkit-media-transfer-p transfer))
            (setf (plist-get entry :handle)
                  (appkit-register-handle
                   app 'function transfer #'appkit-media-cancel-transfer)))
          entry))))))

(defun chirp-media--xchat-attachment-extension (attachment)
  "Return a safe cache extension hint for verified ATTACHMENT."
  (let ((extension
         (and-let* ((name (plist-get attachment :name)))
           (downcase (or (file-name-extension name) "")))))
    (if (and extension
             (string-match-p "\\`[[:alnum:]]\\{1,16\\}\\'" extension))
        extension
      (pcase (plist-get attachment :kind)
        ('image "jpg")
        ('gif "gif")
        ('svg "svg")
        (_ "bin")))))

(defun chirp-media--write-xchat-attachment (bytes path)
  "Atomically write unibyte XChat attachment BYTES to cache PATH."
  (let ((temporary (make-temp-file (concat path ".") nil ".tmp")))
    (unwind-protect
        (let ((coding-system-for-write 'binary))
          (write-region bytes nil temporary nil 'silent)
          (rename-file temporary path t)
          path)
      (when (file-exists-p temporary)
        (delete-file temporary)))))

(defun chirp-media--finish-xchat-attachment
    (app resource-key entry attachment encrypted)
  "In APP, finish RESOURCE-KEY's ENTRY for ATTACHMENT from ENCRYPTED bytes."
  (condition-case _err
      (let* ((plaintext
              (chirp-xchat-native-decrypt-media-bytes
               (plist-get entry :conversation-id)
               (plist-get entry :key-version)
               encrypted))
             (hint (chirp-media--xchat-attachment-extension attachment))
             (extension
              (if (memq (plist-get attachment :kind)
                        '(image gif svg media))
                  (appkit-media-bytes-to-extension plaintext hint)
                hint))
             (file (format "%s.%s" (plist-get entry :cache-base) extension)))
        (unwind-protect
            (progn
              (chirp-media--write-xchat-attachment plaintext file)
              (chirp-media--finish-image-resource
               app resource-key entry 'ready file))
          (clear-string plaintext)))
    (error
     (chirp-media--finish-image-resource
      app resource-key entry 'failed))))

(cl-defun chirp-media-request-xchat-attachment-resource
    (view resource-key attachment &key conversation-id key-version)
  "Acquire encrypted XChat ATTACHMENT for RESOURCE-KEY in VIEW.

CONVERSATION-ID and KEY-VERSION select the native media decryption key.
Return RESOURCE-KEY only when the verified media metadata is usable."
  (let ((media-hash (plist-get attachment :media-hash)))
    (when (and (appkit-view-live-p view)
               resource-key
               (stringp conversation-id)
               (stringp key-version)
               (stringp media-hash)
               (not (string-empty-p media-hash))
               (chirp-media--prefetch-enabled-p))
      (let* ((app (appkit-view-app view))
             (store (appkit-app-resource-store app))
             (source (list conversation-id key-version media-hash))
             (current (gethash resource-key store))
             (cache-base
              (chirp-media-cache-base
               (mapconcat #'identity source ":") "xchat"))
             (hint-file
              (format "%s.%s" cache-base
                      (chirp-media--xchat-attachment-extension attachment)))
             (cached
              (or (and (chirp-media--valid-cache-file-p hint-file) hint-file)
                  (appkit-media-image-cache-existing-file cache-base))))
        (cond
         ((and (eq (plist-get current :status) 'ready)
               (equal (plist-get current :source) source)
               (chirp-media--valid-cache-file-p
                (plist-get current :file))))
         ((and (eq (plist-get current :status) 'pending)
               (equal (plist-get current :source) source))
          (cl-pushnew view (plist-get current :views) :test #'eq))
         ((and (eq (plist-get current :status) 'failed)
               (equal (plist-get current :source) source)))
         (t
          (let ((entry
                 (list :source source :status 'pending :file nil :handle nil
                       :conversation-id conversation-id
                       :key-version key-version :cache-base cache-base
                       :views (cl-adjoin view (plist-get current :views)
                                         :test #'eq))))
            (puthash resource-key entry store)
            (if cached
                (setf (plist-get entry :status) 'ready
                      (plist-get entry :file) cached)
              (chirp-backend-dm-media
               conversation-id media-hash
               (lambda (encrypted)
                 (unwind-protect
                     (chirp-media--finish-xchat-attachment
                      app resource-key entry attachment encrypted)
                   (clear-string encrypted)))
               :errback
               (lambda (_message)
                 (chirp-media--finish-image-resource
                  app resource-key entry 'failed))
               :owner app)))))
        resource-key))))

(defun chirp-media-xchat-resource-status (view resource-key)
  "Return RESOURCE-KEY's XChat media status in VIEW, or nil."
  (when (appkit-view-live-p view)
    (plist-get
     (gethash resource-key
              (appkit-app-resource-store (appkit-view-app view)))
     :status)))

(defun chirp-media-xchat-resource (view resource-key attachment)
  "Return RESOURCE-KEY as a canonical Appkit resource for ATTACHMENT in VIEW."
  (appkit-media-resource-create
   :file
   (when (appkit-view-live-p view)
     (let ((file
            (plist-get
             (gethash resource-key
                      (appkit-app-resource-store (appkit-view-app view)))
             :file)))
       (and (stringp file) (chirp-media--valid-cache-file-p file) file)))
   :name (plist-get attachment :name)))

(defun chirp-media--trusted-xchat-media-url-p (value)
  "Return non-nil when VALUE is an allowlisted HTTPS XChat media URL."
  (and (stringp value)
       (<= (length value) 8192)
       (not (string-match-p "[[:cntrl:]]" value))
       (condition-case nil
           (let* ((parsed (url-generic-parse-url value))
                  (host (downcase (or (url-host parsed) ""))))
             (and (equal (url-type parsed) "https")
                  (null (url-user parsed))
                  (null (url-password parsed))
                  (memq (url-port parsed) '(nil 443))
                  (or (equal host "ton.twitter.com")
                      (string-suffix-p ".twimg.com" host))))
         (error nil))))

(cl-defun chirp-media-request-xchat-image-resource
    (view resource-key source &key name)
  "Acquire allowlisted XChat image SOURCE for RESOURCE-KEY in VIEW.

NAME optionally supplies the filename used for media type hints."
  (when (chirp-media--trusted-xchat-media-url-p source)
    (chirp-media-request-image-resource
     view resource-key source :name name)
    resource-key))

(cl-defun chirp-media-insert-image-resource
    (view resource-key &key alternate-text help-echo)
  "Insert VIEW's cached image RESOURCE-KEY and return its display status.

Return `rendered', `pending', `failed', or `missing'.  ALTERNATE-TEXT and
HELP-ECHO customize the accessible image action."
  (let* ((entry (and (appkit-view-live-p view)
                     (gethash resource-key
                              (appkit-app-resource-store
                               (appkit-view-app view)))))
         (status (plist-get entry :status))
         (file (plist-get entry :file))
         (image (and (eq status 'ready)
                     (chirp-media--valid-cache-file-p file)
                     (appkit-media-preview-image-from-file file))))
    (cond
     (image
      (appkit-media-insert-image-slices
       image (lambda () (appkit-media-open-file file))
       nil (or alternate-text "[image]")
       (or help-echo "Open image in Emacs"))
      'rendered)
     ((eq status 'pending) 'pending)
     ((eq status 'ready) 'failed)
     (status status)
     (t 'missing))))

;;; Task Scheduling

(defun chirp-media--add-pending-callback (key callback table)
  "Add CALLBACK for KEY to pending callback TABLE."
  (puthash key (cons callback (gethash key table)) table))

(defun chirp-media--dispatch-callback (kind key callback &rest args)
  "Run media CALLBACK for KIND and KEY, reporting recoverable errors."
  (condition-case err
      (apply callback args)
    (error
     (display-warning
      'chirp-media
      (format "Chirp media %s callback failed for %s: %s"
              kind key (error-message-string err))
      :warning))))

(defun chirp-media--invalidate-resource (resource)
  "Invalidate live Appkit projections that depend on RESOURCE."
  (when (appkit-app-live-p chirp--app)
    (maphash
     (lambda (_id view)
       (when (and (appkit-projection-view-p view)
                  (appkit-projection-dependent-keys view (list resource)))
         (appkit-request-sync view :resource resource :position t)))
     (appkit-app-view-registry chirp--app))))

(defun chirp-media--prefetch-finish (path success resource)
  "Finish a background prefetch for PATH with SUCCESS and RESOURCE."
  (let* ((pending (chirp-media--pending-table 'prefetch))
         (callbacks (prog1 (gethash path pending)
                      (remhash path pending))))
    (when (and success resource)
      (chirp-media--invalidate-resource resource))
    (dolist (callback callbacks)
      (when callback
        (chirp-media--dispatch-callback 'prefetch path callback success path)))))

(defun chirp-media--thumbnail-finish (path success resource)
  "Finish thumbnail PATH with SUCCESS for RESOURCE."
  (let* ((pending (chirp-media--pending-table 'thumbnail))
         (callbacks (prog1 (gethash path pending)
                      (remhash path pending))))
    (when (and success resource)
      (chirp-media--invalidate-resource resource))
    (dolist (callback callbacks)
      (when callback
        (chirp-media--dispatch-callback 'thumbnail path callback success path)))))

(defun chirp-media--link-card-finish (url card)
  "Finish a background link-card fetch for URL with CARD."
  (let* ((pending (chirp-media--pending-table 'link-card))
         (callbacks (prog1 (gethash url pending)
                      (remhash url pending))))
    (puthash url (or card chirp-media--link-card-fetch-failed)
             (chirp-media--runtime-link-card-cache (chirp-media--runtime)))
    (chirp-media--invalidate-resource url)
    (dolist (callback callbacks)
      (when callback
        (chirp-media--dispatch-callback 'link-card url callback card)))))

(defun chirp-media--cancel-task-process (process buffer &optional output-file)
  "Cancel PROCESS, kill BUFFER, and remove partial OUTPUT-FILE."
  (when (processp process)
    (set-process-sentinel process nil)
    (when (process-live-p process)
      (delete-process process)))
  (when (buffer-live-p buffer)
    (kill-buffer buffer))
  (when (and output-file (file-exists-p output-file))
    (ignore-errors (delete-file output-file))))

(defun chirp-media--start-thumbnail-task (path command complete)
  "Start thumbnail COMMAND for PATH and call COMPLETE with its success state."
  (let ((buffer (generate-new-buffer " *chirp-thumb*"))
        process)
    (setq process
          (make-process
           :name "chirp-thumb"
           :buffer buffer
           :command command
           :noquery t
           :sentinel
           (lambda (finished _event)
             (when (memq (process-status finished) '(exit signal))
               (let ((success (and (zerop (process-exit-status finished))
                                   (file-exists-p path))))
                 (unless success
                   (ignore-errors
                     (when (file-exists-p path)
                       (delete-file path))))
                 (when (buffer-live-p buffer)
                   (kill-buffer buffer))
                 (funcall complete success))))))
    (lambda ()
      (chirp-media--cancel-task-process process buffer path))))

(defun chirp-media--start-link-card-task (url complete)
  "Fetch URL metadata and call COMPLETE with its parsed link card."
  (let ((buffer (generate-new-buffer " *chirp-link-card*"))
        process)
    (setq process
          (make-process
           :name "chirp-link-card"
           :buffer buffer
           :command
           (append
            (list chirp-media-prefetch-command)
            (chirp-media--curl-default-args
             chirp-media--link-card-source-limit
             chirp-link-card-fetch-timeout
             (chirp-media--curl-protocols url))
            (list url))
           :noquery t
           :sentinel
           (lambda (finished _event)
             (when (memq (process-status finished) '(exit signal))
               (let* ((html
                       (when (zerop (process-exit-status finished))
                         (with-current-buffer buffer
                           (buffer-substring-no-properties
                            (point-min)
                            (min (point-max) (+ (point-min) 262144))))))
                      (card (and html
                                 (chirp-media--parse-link-card-html html url))))
                 (when (buffer-live-p buffer)
                   (kill-buffer buffer))
                 (when-let* ((image-url (plist-get card :image-url)))
                   (chirp-media-prefetch-file image-url "media" "jpg"))
                 (funcall complete card))))))
    (lambda ()
      (chirp-media--cancel-task-process process buffer))))

(defun chirp-media--start-prefetch-task (url path complete)
  "Download URL to PATH and call COMPLETE with its success state."
  (let ((buffer (generate-new-buffer " *chirp-prefetch*"))
        (max-bytes
         (and (member (downcase (or (file-name-extension path) ""))
                      chirp-media--image-cache-extensions)
              chirp-media--image-source-limit))
        process)
    (setq process
          (make-process
           :name "chirp-prefetch"
           :buffer buffer
           :command
           (append
            (list chirp-media-prefetch-command)
            (chirp-media--curl-default-args
             max-bytes 60 (chirp-media--curl-protocols url))
            (list "-o" path url))
           :noquery t
           :sentinel
           (lambda (finished _event)
             (when (memq (process-status finished) '(exit signal))
               (let ((success (and (zerop (process-exit-status finished))
                                   (chirp-media--valid-cache-file-p path))))
                 (unless success
                   (ignore-errors
                     (when (file-exists-p path)
                       (delete-file path))))
                 (when (buffer-live-p buffer)
                   (kill-buffer buffer))
                 (funcall complete success))))))
    (lambda ()
      (chirp-media--cancel-task-process process buffer path))))

(defun chirp-media--queue-thumbnail-extraction
    (thumbnail-file command callback resource)
  "Queue COMMAND for THUMBNAIL-FILE and notify CALLBACK and RESOURCE."
  (let ((pending (chirp-media--pending-table 'thumbnail)))
    (cond
     ((file-exists-p thumbnail-file)
      thumbnail-file)
     ((or (not (listp command))
          (<= chirp-media-thumbnail-concurrency 0))
      nil)
     ((gethash thumbnail-file pending)
      (chirp-media--add-pending-callback thumbnail-file callback pending)
      thumbnail-file)
     (t
      (chirp-media--add-pending-callback thumbnail-file callback pending)
      (condition-case err
          (appkit-task-queue-submit
           (chirp-media--task-queue
            'thumbnail chirp-media-thumbnail-concurrency)
           thumbnail-file
           (lambda (complete)
             (chirp-media--start-thumbnail-task
              thumbnail-file command complete))
           :finish
           (lambda (success)
             (chirp-media--thumbnail-finish
              thumbnail-file success resource)))
        (error
         (remhash thumbnail-file pending)
         (signal (car err) (cdr err))))
      thumbnail-file))))

(defun chirp-media-prefetch-file (url kind &optional fallback-ext callback)
  "Prefetch URL of KIND and run CALLBACK when newly available.

Use FALLBACK-EXT when URL has no recognizable extension."
  (when (chirp-media--prefetch-enabled-p)
    (let* ((path (chirp-media--cache-file url kind (or fallback-ext "bin")))
           (pending (chirp-media--pending-table 'prefetch)))
      (cond
       ((gethash path pending)
        (chirp-media--add-pending-callback path callback pending)
        path)
       ((chirp-media--valid-cache-file-p path)
        path)
       (t
        (when (file-exists-p path)
          (ignore-errors (delete-file path)))
        (chirp-media--add-pending-callback path callback pending)
        (condition-case err
            (appkit-task-queue-submit
             (chirp-media--task-queue
              'prefetch chirp-media-prefetch-concurrency)
             path
             (lambda (complete)
               (chirp-media--start-prefetch-task url path complete))
             :finish
             (lambda (success)
               (chirp-media--prefetch-finish path success url)))
          (error
           (remhash path pending)
           (signal (car err) (cdr err))))
        path)))))

;;; Link Cards

(defun chirp-media--legacy-buffer-p (buffer)
  "Return non-nil when BUFFER needs callback-driven media redraws."
  (not (chirp--live-projection-view buffer)))

(defun chirp-media--link-card-rerender-callback (buffer)
  "Return a callback that rerenders legacy BUFFER after link-card fetch."
  (when (chirp-media--legacy-buffer-p buffer)
    (lambda (_card)
      (when (buffer-live-p buffer)
        (chirp-request-rerender buffer)))))

(defun chirp-media--normalize-link-card-field (value)
  "Normalize one link-card field VALUE."
  (when-let* ((text (and (stringp value)
                         (chirp-clean-text value))))
    (unless (string-empty-p text)
      text)))

(defun chirp-media--resolve-link-card-url (candidate base-url)
  "Return CANDIDATE resolved against BASE-URL."
  (when-let* ((raw (chirp-media--normalize-link-card-field candidate)))
    (let ((absolute (ignore-errors (url-expand-file-name raw base-url))))
      (when (and (stringp absolute)
                 (string-match-p "\\`https?://" absolute))
        absolute))))

(defun chirp-media--extract-meta-content (html key)
  "Return content for HTML meta KEY, or nil."
  (let ((case-fold-search t)
        (quoted-key (regexp-quote key)))
    (chirp-media--normalize-link-card-field
     (or (and (string-match
               (format "<meta[^>]+\\(?:property\\|name\\|itemprop\\)=['\"]%s['\"][^>]+content=['\"]\\([^\"']+\\)['\"]"
                       quoted-key)
               html)
              (match-string 1 html))
         (and (string-match
               (format "<meta[^>]+content=['\"]\\([^\"']+\\)['\"][^>]+\\(?:property\\|name\\|itemprop\\)=['\"]%s['\"]"
                       quoted-key)
               html)
              (match-string 1 html))))))

(defun chirp-media--extract-html-title (html)
  "Return the HTML <title> from HTML, or nil."
  (let ((case-fold-search t))
    (chirp-media--normalize-link-card-field
     (and (string-match "<title[^>]*>\\([^<]+\\)</title>" html)
          (match-string 1 html)))))

(defun chirp-media--parse-link-card-html (html url)
  "Parse HTML for URL into a cached link-card plist."
  (let* ((title (or (chirp-media--extract-meta-content html "og:title")
                    (chirp-media--extract-meta-content html "twitter:title")
                    (chirp-media--extract-html-title html)))
         (description (or (chirp-media--extract-meta-content html "og:description")
                          (chirp-media--extract-meta-content html "twitter:description")
                          (chirp-media--extract-meta-content html "description")))
         (image-url (chirp-media--resolve-link-card-url
                     (or (chirp-media--extract-meta-content html "og:image")
                         (chirp-media--extract-meta-content html "twitter:image")
                         (chirp-media--extract-meta-content html "twitter:image:src"))
                     url)))
    (when (or image-url title description)
      (list :url url
            :title title
            :description description
            :image-url image-url))))

(defun chirp-media-link-card (url)
  "Return the current session's cached link-card for URL, or nil."
  (let ((card (gethash url
                       (chirp-media--runtime-link-card-cache
                        (chirp-media--runtime)))))
    (unless (eq card chirp-media--link-card-fetch-failed)
      card)))

(defun chirp-media--link-card-candidate-p (url)
  "Return non-nil when URL is a good external link-card candidate."
  (and (stringp url)
       (string-match-p "\\`https?://" url)
       (not (string-match-p
             "\\`https?://\\(?:x\\.com\\|twitter\\.com\\|t\\.co\\|pbs\\.twimg\\.com\\|video\\.twimg\\.com\\)/"
             url))
       (not (string-match-p
             "\\.\\(?:png\\|jpe?g\\|gif\\|webp\\|svg\\|mp4\\|mov\\|webm\\)\\(?:\\?[^#]*\\)?\\(?:#.*\\)?\\'"
             (downcase url)))))

(defun chirp-media-link-card-urls (tweet)
  "Return external URLs from TWEET that should get a link-card preview."
  (let ((limit (max 0 chirp-link-card-max-per-tweet))
        urls)
    (dolist (url (plist-get tweet :urls) (nreverse urls))
      (when (and (> limit (length urls))
                 (chirp-media--link-card-candidate-p url))
        (push url urls)))))

(defun chirp-media-link-cards-for-tweet (tweet)
  "Return link-card previews for TWEET.

Prefer the website card X already attached to the tweet.  Fall back to
cached Open Graph fetches for tweets that have no card payload."
  (if-let* ((card (plist-get tweet :link-card)))
      (list card)
    (delq nil
          (mapcar #'chirp-media-link-card
                  (chirp-media-link-card-urls tweet)))))

(defun chirp-media--item-resource-keys (media)
  "Return cache resource keys represented by MEDIA."
  (delq nil (list (plist-get media :url)
                  (plist-get media :preview-url))))

(defun chirp-media-resource-keys-for-tweet (tweet)
  "Return media resource keys whose completion can change TWEET rendering."
  (let ((keys
         (append
          (when chirp-show-avatars
            (list (plist-get tweet :author-avatar-url)))
          (when chirp-show-tweet-media
            (mapcan #'chirp-media--item-resource-keys
                    (append (plist-get tweet :media)
                            (chirp-tweet-article-images tweet)))))))
    (dolist (card (chirp-media-link-cards-for-tweet tweet))
      (when-let* ((url (plist-get card :url)))
        (push url keys))
      (when-let* ((image-url (plist-get card :image-url)))
        (push image-url keys)))
    (when-let* ((quoted (plist-get tweet :quoted-tweet)))
      (setq keys
            (append keys
                    (chirp-media-resource-keys-for-tweet quoted))))
    (delete-dups (delq nil keys))))

(defun chirp-media-prefetch-link-card (url buffer)
  "Prefetch external link-card metadata for URL and update BUFFER when ready."
  (when (and (chirp-media--link-card-enabled-p)
             (chirp-media--link-card-candidate-p url))
    (let* ((cache (chirp-media--runtime-link-card-cache
                   (chirp-media--runtime)))
           (pending (chirp-media--pending-table 'link-card))
           (cached (gethash url cache)))
      (cond
       ((eq cached chirp-media--link-card-fetch-failed)
        nil)
       ((listp cached)
        (when-let* ((image-url (plist-get cached :image-url)))
          (when chirp-show-tweet-media
            (chirp-media-prefetch-file
             image-url "media" "jpg"
             (chirp-media--prefetch-callback buffer))))
        cached)
       ((gethash url pending)
        (chirp-media--add-pending-callback
         url (chirp-media--link-card-rerender-callback buffer) pending))
       (t
        (chirp-media--add-pending-callback
         url (chirp-media--link-card-rerender-callback buffer) pending)
        (condition-case err
            (appkit-task-queue-submit
             (chirp-media--task-queue
              'link-card chirp-link-card-prefetch-concurrency)
             url
             (lambda (complete)
               (chirp-media--start-link-card-task url complete))
             :finish (lambda (card)
                       (chirp-media--link-card-finish url card)))
          (error
           (remhash url pending)
           (signal (car err) (cdr err)))))))))

;;; Media Prefetch

(defun chirp-media--prefetch-callback (buffer)
  "Return a callback that requests a redraw of legacy BUFFER."
  (when (chirp-media--legacy-buffer-p buffer)
    (lambda (success _path)
      (when (and success
                 (buffer-live-p buffer))
        (chirp-request-rerender buffer)))))

(defun chirp-media-prefetch-avatar (url buffer)
  "Prefetch avatar URL for BUFFER."
  (when (and (stringp url)
             (not (string-empty-p url)))
    (chirp-media-prefetch-file url "avatars" "jpg"
                               (chirp-media--prefetch-callback buffer))))

(defun chirp-media--prefetch-video-thumbnail-from-file (media video-file buffer)
  "Extract a thumbnail for MEDIA from VIDEO-FILE and rerender BUFFER on success."
  (let* ((thumbnail-file (chirp-media--video-thumbnail-file media))
         (callback (chirp-media--prefetch-callback buffer)))
    (when (and chirp-video-thumbnail-command
               (file-exists-p video-file))
      (chirp-media--queue-thumbnail-extraction
       thumbnail-file
       (list chirp-video-thumbnail-command
             "-y"
             "-loglevel" "error"
             "-ss" (number-to-string chirp-video-thumbnail-offset)
             "-i" video-file
             "-frames:v" "1"
             thumbnail-file)
       callback
       (plist-get media :url)))))

(defun chirp-media--prefetch-video-thumbnail-from-url (media buffer &optional fallback)
  "Extract a thumbnail for MEDIA directly from its remote URL for BUFFER.

When FALLBACK is non-nil, call it if remote extraction fails."
  (let* ((thumbnail-file (chirp-media--video-thumbnail-file media))
         (callback (chirp-media--prefetch-callback buffer))
         (url (plist-get media :url)))
    (when (and chirp-video-thumbnail-command
               chirp-media-prefetch-video-remote-thumbnail
               (stringp url)
               (not (string-empty-p url)))
      (chirp-media--queue-thumbnail-extraction
       thumbnail-file
       (list chirp-video-thumbnail-command
             "-y"
             "-loglevel" "error"
             "-ss" (number-to-string chirp-video-thumbnail-offset)
             "-i" url
             "-frames:v" "1"
             thumbnail-file)
       (lambda (success path)
         (when callback
           (funcall callback success path))
         (when (and (not success) fallback)
           (funcall fallback)))
       url))))

(defun chirp-media--prefetch-video-thumbnail-via-download (media buffer)
  "Download MEDIA in the background and extract a thumbnail for BUFFER."
  (chirp-media-prefetch-file
   (plist-get media :url)
   "media"
   "mp4"
   (lambda (success video-file)
     (when success
       (chirp-media--prefetch-video-thumbnail-from-file media video-file buffer)))))

(defun chirp-media-prefetch-video-thumbnail (media buffer)
  "Prefetch a list-view thumbnail for video-like MEDIA in BUFFER."
  (let* ((preview-url (plist-get media :preview-url))
         (download-fallback
          (lambda ()
            (when chirp-media-prefetch-video-fallback-download
              (chirp-media--prefetch-video-thumbnail-via-download media buffer)))))
    (cond
     ((and (stringp preview-url)
           (not (string-empty-p preview-url)))
      (chirp-media-prefetch-file
       preview-url
       "video-thumbnails"
       "jpg"
       (lambda (success _path)
         (if success
             (when-let* ((callback (chirp-media--prefetch-callback buffer)))
               (funcall callback t nil))
           (or (chirp-media--prefetch-video-thumbnail-from-url media buffer download-fallback)
               (funcall download-fallback))))))
     ((chirp-media--prefetch-video-thumbnail-from-url media buffer download-fallback))
     (t
      (funcall download-fallback)))))

(defun chirp-media-prefetch-media (media buffer)
  "Prefetch list-view MEDIA for BUFFER."
  (when (and (stringp (plist-get media :url))
             (not (string-empty-p (plist-get media :url))))
    (cond
     ((string= (plist-get media :type) "photo")
      (chirp-media-prefetch-file (plist-get media :url) "media" "jpg"
                                 (chirp-media--prefetch-callback buffer)))
     ((chirp-media-video-like-p media)
      (chirp-media-prefetch-video-thumbnail media buffer)))))

(defun chirp-media-prefetch-tweet (tweet buffer)
  "Prefetch list-view assets for TWEET in BUFFER."
  (when chirp-show-avatars
    (chirp-media-prefetch-avatar (plist-get tweet :author-avatar-url) buffer))
  (when chirp-show-tweet-media
    (dolist (media (plist-get tweet :media))
      (chirp-media-prefetch-media media buffer))
    (dolist (media (chirp-tweet-article-images tweet))
      (chirp-media-prefetch-media media buffer)))
  (if-let* ((card (plist-get tweet :link-card)))
      (when-let* ((image-url (plist-get card :image-url)))
        (chirp-media-prefetch-file image-url "media" "jpg"
                                   (chirp-media--prefetch-callback buffer)))
    (dolist (url (chirp-media-link-card-urls tweet))
      (chirp-media-prefetch-link-card url buffer)))
  (when-let* ((quoted (plist-get tweet :quoted-tweet)))
    (chirp-media-prefetch-tweet quoted buffer)))

(defun chirp-media-prefetch-tweets (tweets buffer)
  "Prefetch list-view assets for TWEETS in BUFFER."
  (when (chirp-media--prefetch-enabled-p)
    (dolist (tweet tweets)
      (chirp-media-prefetch-tweet tweet buffer))))

(defun chirp-media-prefetch-user (user buffer)
  "Prefetch list-view assets for USER in BUFFER."
  (when (chirp-media--prefetch-enabled-p)
    (when chirp-show-avatars
      (chirp-media-prefetch-avatar (plist-get user :avatar-url) buffer))))

;;; Image and Video Display

(defun chirp-media--scaled-image (file max-width max-height)
  "Create a FILE image descriptor constrained by MAX-WIDTH and MAX-HEIGHT."
  (when (and file
             (display-images-p))
    (condition-case nil
        (let ((image (create-image file)))
          (when image
            (pcase-let* ((`(,width . ,height) (image-size image t))
                         (scale (min 1.0
                                     (/ (float max-width) (max 1.0 width))
                                     (/ (float max-height) (max 1.0 height)))))
              (when (< scale 1.0)
                (plist-put (cdr image) :scale scale))
              image)))
      (error nil))))

(defun chirp-media-video-placeholder-image (size &optional animated-gif-p)
  "Return a fixed-size placeholder image for video-like media of SIZE.

When ANIMATED-GIF-P is non-nil, add a subtle GIF label to the badge."
  (when (display-images-p)
    (condition-case nil
        (let* ((svg (svg-create size size))
               (dark-p (eq (frame-parameter nil 'background-mode) 'dark))
               (bg (if dark-p "#27303a" "#dfe6ec"))
               (bg-accent (if dark-p "#313b46" "#e8eef3"))
               (stroke (if dark-p "#4d5966" "#bcc7d1"))
               (badge-fill "rgba(0,0,0,0.45)")
               (badge-stroke "rgba(255,255,255,0.82)")
               (center (/ size 2.0))
               (radius (max 10.0 (/ size 6.0)))
               (left (- center (* radius 0.35)))
               (top (- center (* radius 0.55)))
               (bottom (+ center (* radius 0.55)))
               (right (+ center (* radius 0.55))))
          (dom-append-child
           svg
           (dom-node 'rect
                     `((x . 0)
                       (y . 0)
                       (width . ,size)
                       (height . ,size)
                       (rx . 10)
                       (ry . 10)
                       (fill . ,bg)
                       (stroke . ,stroke)
                       (stroke-width . "1"))))
          (dom-append-child
           svg
           (dom-node 'rect
                     `((x . 0)
                       (y . ,(* size 0.58))
                       (width . ,size)
                       (height . ,(* size 0.42))
                       (rx . 10)
                       (ry . 10)
                       (fill . ,bg-accent))))
          (dom-append-child
           svg
           (dom-node 'circle
                     `((cx . ,center)
                       (cy . ,center)
                       (r . ,radius)
                       (fill . ,badge-fill)
                       (stroke . ,badge-stroke)
                       (stroke-width . "1.5"))))
          (dom-append-child
           svg
           (dom-node 'polygon
                     `((points . ,(format "%s,%s %s,%s %s,%s"
                                          left top
                                          left bottom
                                          right center))
                       (fill . "white"))))
          (when animated-gif-p
            (svg-text svg
                      "GIF"
                      :x (* size 0.5)
                      :y (- size 12)
                      :text-anchor "middle"
                      :font-size "12"
                      :font-family "monospace"
                      :font-weight "700"
                      :fill (if dark-p "#d9e2ea" "#52606d")))
          (svg-image svg :ascent 'center))
      (error nil))))

(defun chirp-media-thumbnail-placeholder-image (media)
  "Return a stable thumbnail placeholder image for MEDIA, or nil."
  (when (chirp-media-video-like-p media)
    (chirp-media-video-placeholder-image
     chirp-media-thumbnail-size
     (string= (plist-get media :type) "animated_gif"))))

(defun chirp-media--avatar-pixel-size ()
  "Return avatar pixel size for the current buffer's text scale.

`chirp-avatar-size' is a baseline ratio where 28 is exactly one text line."
  (let* ((line-height (appkit-chat-avatar-line-pixel-height))
         (size-factor (/ (float (max 1 chirp-avatar-size)) 28.0)))
    (max 8 (round (* line-height size-factor)))))

(defun chirp-media-avatar-image (url)
  "Return a small avatar image descriptor for URL."
  (when-let* ((file (if chirp-media-render-from-cache-only
                        (chirp-media-cached-file url "avatars" "jpg")
                      (chirp-media-local-file url "avatars" "jpg")))
              (size (chirp-media--avatar-pixel-size)))
    (or (appkit-media-circular-image-from-file file size)
        (chirp-media--scaled-image file size size))))

(defun chirp-media-xchat-avatar-resource-key (identity url)
  "Return the stable avatar resource key for IDENTITY and URL, or nil."
  (when (and (stringp identity)
             (not (string-empty-p identity))
             (stringp url)
             (not (string-empty-p url)))
    (list 'xchat-avatar identity)))

(defun chirp-media-request-xchat-avatar-resource (view identity url)
  "Request IDENTITY's avatar URL resource on behalf of VIEW.

Return its stable resource key, or nil when IDENTITY or URL is unavailable."
  (when-let* ((resource-key
               (chirp-media-xchat-avatar-resource-key identity url)))
    (chirp-media-request-image-resource
     view resource-key url :name "avatar.jpg")
    resource-key))

(defun chirp-media-avatar-resource-image (view resource-key &optional pixel-size)
  "Return VIEW's cached avatar RESOURCE-KEY as an image descriptor.

PIXEL-SIZE defaults to the current one-line avatar size."
  (when-let* ((entry
               (and (appkit-view-live-p view)
                    (gethash resource-key
                             (appkit-app-resource-store
                              (appkit-view-app view)))))
              ((eq (plist-get entry :status) 'ready))
              (file (plist-get entry :file))
              ((chirp-media--valid-cache-file-p file))
              (size (max 1 (or pixel-size
                               (chirp-media--avatar-pixel-size)))))
    (or (appkit-media-circular-image-from-file file size)
        (chirp-media--scaled-image file size size))))

(defun chirp-media-cached-image (url &optional max-width max-height)
  "Return an Appkit preview image for cached URL, or nil when not ready.

MAX-WIDTH and MAX-HEIGHT default to `chirp-media-thumbnail-size'.  Sizing
and slice metadata come from `appkit-media-preview-image-from-file'."
  (when-let* ((file (chirp-media-cached-file url "media" "jpg")))
    (appkit-media-preview-image-from-file
     file
     (or max-width chirp-media-thumbnail-size)
     (or max-height chirp-media-thumbnail-size))))
(defun chirp-media--preview-image-from-file
    (file crop-spec max-width max-height)
  "Return a preview for FILE within MAX-WIDTH and MAX-HEIGHT.

When CROP-SPEC is non-nil, center-crop to that fixed box instead."
  (let* ((structuredp (and (listp crop-spec)
                           (keywordp (car crop-spec))))
         (width (if structuredp
                    (plist-get crop-spec :width)
                  (car-safe crop-spec)))
         (height (if structuredp
                     (plist-get crop-spec :height)
                   (cdr-safe crop-spec)))
         (insets (and structuredp (plist-get crop-spec :insets))))
    (or (and (numberp width)
             (numberp height)
             (if insets
                 (appkit-media-cropped-preview-image-from-file
                  file width height insets)
               (appkit-media-cropped-preview-image-from-file
                file width height)))
        (appkit-media-preview-image-from-file
         file max-width max-height))))

(defun chirp-media--preview-image
    (media crop-spec max-width max-height)
  "Return a MEDIA preview using CROP-SPEC or natural-ratio bounds.

MAX-WIDTH and MAX-HEIGHT bound the uncropped result."
  (cond
   ((string= (plist-get media :type) "photo")
    (when-let* ((file (if chirp-media-render-from-cache-only
                          (chirp-media-cached-file (plist-get media :url)
                                                   "media"
                                                   "jpg")
                        (chirp-media-local-file (plist-get media :url)
                                                "media"
                                                "jpg"))))
      (chirp-media--preview-image-from-file
       file crop-spec max-width max-height)))
   ((chirp-media-video-like-p media)
    (when-let* ((file (if chirp-media-render-from-cache-only
                          (or (and-let* ((preview-url
                                         (plist-get media :preview-url)))
                                (chirp-media-cached-file preview-url
                                                         "video-thumbnails"
                                                         "jpg"))
                              (let ((thumbnail-file
                                     (chirp-media--video-thumbnail-file media)))
                                (and (file-exists-p thumbnail-file)
                                     thumbnail-file)))
                        (chirp-media-video-thumbnail-file media)))
                (image
                 (chirp-media--preview-image-from-file
                  file crop-spec max-width max-height)))
      (or (appkit-media-video-preview-display-image image 'chirp)
          image)))))

(defun chirp-media-thumbnail-image (media &optional crop-spec)
  "Return a timeline thumbnail descriptor for MEDIA.

CROP-SPEC may be a pixel width-height pair or a plist containing `:width',
`:height', and optional `:insets'.  It fills that fixed tile without
distorting the source aspect ratio."
  (chirp-media--preview-image
   media crop-spec
   chirp-media-thumbnail-size chirp-media-thumbnail-size))

(defun chirp-media-track-image (media)
  "Return a large natural-ratio track image for focused MEDIA."
  (chirp-media--preview-image
   media nil
   (* 4 chirp-media-track-height)
   chirp-media-track-height))

(defun chirp-media-view-image (media)
  "Return a large image descriptor for MEDIA."
  (when-let* ((file (chirp-media-local-file (plist-get media :url)
                                            "media"
                                            "jpg")))
    (appkit-media-preview-image-from-file
     file chirp-media-view-max-width chirp-media-view-max-height)))

(defun chirp-media-browse ()
  "Browse the current media URL."
  (interactive)
  (if-let* ((media (or (chirp-media-at-point)
                       (nth chirp--media-index chirp--media-list)))
            (url (plist-get media :url)))
      (browse-url url)
    (user-error "No media URL available")))

;;; Downloads and Playback

(defun chirp-media--highest-bitrate-variant-url (media)
  "Return the highest bitrate variant URL for MEDIA, or nil."
  (when-let* ((variants
               (cl-remove-if-not
                (lambda (variant)
                  (let ((url (plist-get variant :url)))
                    (and (stringp url)
                         (not (string-empty-p url)))))
                (copy-sequence (plist-get media :variants)))))
    (let ((sorted
           (sort variants
                 (lambda (left right)
                   (> (chirp-media--variant-bitrate left)
                      (chirp-media--variant-bitrate right))))))
      (plist-get (car sorted) :url))))

(defun chirp-media-download-url (media)
  "Return the best download URL for MEDIA."
  (pcase (plist-get media :type)
    ("photo"
     (chirp-media--photo-original-url (plist-get media :url)))
    ((or "video" "animated_gif")
     (or (chirp-media--highest-bitrate-variant-url media)
         (plist-get media :url)))
    (_
     (plist-get media :url))))

(defun chirp-media--download-filename (media)
  "Return a default download file name for MEDIA."
  (let* ((url (chirp-media-download-url media))
         (parsed (and (stringp url) (ignore-errors (url-generic-parse-url url))))
         (path (and parsed (url-filename parsed)))
         (base (and path
                    (car (split-string (file-name-nondirectory path) "\\?"))))
         (fallback-ext
          (pcase (plist-get media :type)
            ("photo" "jpg")
            ((or "video" "animated_gif") "mp4")
            (_ "bin")))
         (ext (chirp-media--url-extension url fallback-ext))
         (name (cond
                ((and base (not (string-empty-p base)) (file-name-extension base))
                 base)
                ((and base (not (string-empty-p base)))
                 (concat base "." ext))
                (t
                 (format "chirp-media-%s.%s"
                         (secure-hash 'sha1 (or url ""))
                         ext)))))
    name))

(defun chirp-media--candidate-label (media index)
  "Return a minibuffer label for MEDIA at INDEX."
  (let ((dims (if-let* ((width (plist-get media :width))
                        (height (plist-get media :height)))
                  (format " %sx%s" width height)
                "")))
    (format "%d. %s %s%s"
            (1+ index)
            (or (plist-get media :type) "media")
            (chirp-media--download-filename media)
            dims)))

(defun chirp-media--select-download-media ()
  "Return the media item that should be downloaded from the current context."
  (or (chirp-media-at-point)
      (and chirp--media-list
           (nth chirp--media-index chirp--media-list))
      (let* ((entry (chirp-entry-at-point))
             (media-list (or (plist-get entry :media)
                             (chirp-tweet-article-images entry))))
        (pcase media-list
          (`nil (user-error "No media available at point"))
          (`(,single) single)
          (_
           (let* ((choices
                   (cl-loop for media in media-list
                            for index from 0
                            collect (cons (chirp-media--candidate-label media index)
                                          media)))
                  (selection
                   (completing-read "Download media: "
                                    choices
                                    nil
                                    t)))
             (cdr (assoc selection choices))))))))

(defun chirp-media--read-download-target (media)
  "Prompt for a target path for MEDIA."
  (let* ((directory (file-name-as-directory
                     (expand-file-name chirp-media-download-directory)))
         (default-name (chirp-media--download-filename media))
         (default-path (expand-file-name default-name directory))
         (target (read-file-name "Save media as: "
                                 directory
                                 default-path
                                 nil
                                 default-name)))
    (expand-file-name target)))

(defun chirp-media--cached-download-file (media)
  "Return a cached local file that matches MEDIA's download URL, or nil."
  (let* ((url (chirp-media-download-url media))
         (kind "media")
         (fallback-ext
          (pcase (plist-get media :type)
            ("photo" "jpg")
            ((or "video" "animated_gif") "mp4")
            (_ "bin"))))
    (chirp-media-cached-file url kind fallback-ext)))

(defun chirp-media--download-sentinel (target)
  "Return a process sentinel that finalizes download into TARGET."
  (lambda (process _event)
    (when (memq (process-status process) '(exit signal))
      (let ((ok (and (zerop (process-exit-status process))
                     (file-exists-p target))))
        (unless ok
          (ignore-errors
            (when (file-exists-p target)
              (delete-file target))))
        (when (buffer-live-p (process-buffer process))
          (kill-buffer (process-buffer process)))
        (message "%s"
                 (if ok
                     (format "Downloaded %s" (abbreviate-file-name target))
                   (format "Failed to download %s" (abbreviate-file-name target))))))))

(defun chirp-media-download-at-point ()
  "Download the current media item at its original or highest-quality URL."
  (interactive)
  (let* ((media (chirp-media--select-download-media))
         (url (chirp-media-download-url media))
         (target (chirp-media--read-download-target media))
         (cached-file (chirp-media--cached-download-file media)))
    (unless (and (stringp url)
                 (not (string-empty-p url)))
      (user-error "No downloadable media URL available"))
    (make-directory (file-name-directory target) t)
    (when (and (file-exists-p target)
               (not (y-or-n-p (format "Overwrite %s? "
                                      (abbreviate-file-name target)))))
      (user-error "Download cancelled"))
    (cond
     (cached-file
      (copy-file cached-file target t)
      (message "Downloaded %s" (abbreviate-file-name target)))
     (chirp-media-prefetch-command
      (let ((buffer (generate-new-buffer " *chirp-download*")))
        (make-process
         :name "chirp-download"
         :buffer buffer
         :command (list chirp-media-prefetch-command
                        "-L" "-f" "-sS"
                        "-o" target
                        url)
         :noquery t
         :sentinel (chirp-media--download-sentinel target))
        (message "Downloading %s..." (file-name-nondirectory target))))
     (t
      (condition-case err
          (progn
            (url-copy-file url target t)
            (message "Downloaded %s" (abbreviate-file-name target)))
        (error
         (ignore-errors
           (when (file-exists-p target)
             (delete-file target)))
         (user-error "Download failed: %s" (error-message-string err))))))))

(defun chirp-media--variant-bitrate (variant)
  "Return numeric bitrate from media VARIANT, or 0."
  (let ((value (plist-get variant :bitrate)))
    (cond
     ((numberp value) value)
     ((and (stringp value)
           (string-match-p "\\`[0-9]+\\'" value))
      (string-to-number value))
     (t 0))))

(defun chirp-media-playback-url (media)
  "Return the preferred playback URL for video-like MEDIA."
  (if-let* (((chirp-media-video-like-p media))
            (variants
             (cl-remove-if-not
              (lambda (variant)
                (let ((url (plist-get variant :url)))
                  (and (stringp url)
                       (not (string-empty-p url)))))
              (copy-sequence (plist-get media :variants)))))
      (let* ((sorted
             (sort variants
                    (lambda (left right)
                      (< (chirp-media--variant-bitrate left)
                         (chirp-media--variant-bitrate right)))))
             (capped
              (and chirp-video-playback-max-bitrate
                   (cl-remove-if
                    (lambda (variant)
                      (> (chirp-media--variant-bitrate variant)
                         chirp-video-playback-max-bitrate))
                    sorted)))
             (selected
              (if chirp-video-playback-max-bitrate
                  (or (car (last capped))
                      (car sorted))
                (car (last sorted)))))
        (or (plist-get selected :url)
            (plist-get media :url)))
    (plist-get media :url)))

(defun chirp-media--play-external (media)
  "Open video-like MEDIA in the configured external player."
  (if-let* (((chirp-media-video-like-p media))
            (url (chirp-media-playback-url media)))
      (if chirp-video-player-command
          (let* ((program chirp-video-player-command)
                 (mpv-p (string-match-p
                         "\\`mpv\\(?:\\.exe\\)?\\'"
                         (file-name-nondirectory program)))
                 (geometry
                  (when (and mpv-p
                             (consp chirp-video-player-window-size)
                             (integerp (car chirp-video-player-window-size))
                             (integerp (cdr chirp-video-player-window-size))
                             (> (car chirp-video-player-window-size) 0)
                             (> (cdr chirp-video-player-window-size) 0))
                    (format "--geometry=%dx%d"
                            (car chirp-video-player-window-size)
                            (cdr chirp-video-player-window-size))))
                 (process-connection-type nil)
                process)
            (setq process
                  (make-process
                   :name "chirp-video"
                   :buffer nil
                   :command (append (list program)
                                    (and geometry (list geometry))
                                    (list url))
                   :connection-type 'pipe
                   :noquery t))
            (set-process-query-on-exit-flag process nil)
            (message "Opening video with %s" program))
        (browse-url url))
    (user-error "Current media is not a video or GIF")))

(defun chirp-media-play ()
  "Play the current video or GIF using the configured external player."
  (interactive)
  (chirp-media--play-external
   (or (chirp-media-at-point)
       (nth chirp--media-index chirp--media-list))))

(defun chirp-media--photo-file (media)
  "Return a local file path for photo MEDIA."
  (chirp-media-local-file (plist-get media :url) "media" "jpg"))

(defun chirp-media--video-file (media)
  "Return a local file path for video-like MEDIA."
  (chirp-media-local-file (plist-get media :url) "media" "mp4"))

(defun chirp-media--video-thumbnail-file (media)
  "Return the cached thumbnail path for video-like MEDIA."
  (expand-file-name
   (format "%s.jpg"
           (secure-hash
            'sha1
            (format "%s@%s"
                    (or (plist-get media :url) "")
                    chirp-video-thumbnail-offset)))
   (chirp-media--cache-subdir "video-thumbnails")))

(defun chirp-media--cached-video-preview-file (media)
  "Return a cached still preview file for video-like MEDIA, or nil."
  (or (and-let* ((preview-url (plist-get media :preview-url)))
        (chirp-media-cached-file preview-url "video-thumbnails" "jpg"))
      (let ((thumbnail-file (chirp-media--video-thumbnail-file media)))
        (and (file-exists-p thumbnail-file)
             thumbnail-file))))

(defun chirp-media--cached-video-preview-image (media)
  "Return a cached still preview image for video-like MEDIA, or nil."
  (when-let* ((file (chirp-media--cached-video-preview-file media)))
    (appkit-media-preview-image-from-file
     file chirp-media-view-max-width chirp-media-view-max-height)))

(defun chirp-media--extract-video-thumbnail (video-file thumbnail-file)
  "Extract a thumbnail from VIDEO-FILE into THUMBNAIL-FILE."
  (when (and chirp-video-thumbnail-command
             (file-exists-p video-file))
    (let ((status (call-process chirp-video-thumbnail-command
                                nil nil nil
                                "-y"
                                "-loglevel" "error"
                                "-ss" (number-to-string chirp-video-thumbnail-offset)
                                "-i" video-file
                                "-frames:v" "1"
                                thumbnail-file)))
      (and (zerop status)
           (file-exists-p thumbnail-file)
           thumbnail-file))))

(defun chirp-media-video-thumbnail-file (media)
  "Return a thumbnail file path for video-like MEDIA, or nil."
  (when (chirp-media-video-like-p media)
    (or (and-let* ((preview-url (plist-get media :preview-url)))
          (chirp-media-local-file preview-url "video-thumbnails" "jpg"))
        (when-let* ((video-file (chirp-media--video-file media)))
          (let ((thumbnail-file (chirp-media--video-thumbnail-file media)))
            (or (and (file-exists-p thumbnail-file)
                     thumbnail-file)
                (chirp-media--extract-video-thumbnail video-file thumbnail-file)))))))

;;; Media Viewer

(defun chirp-media--render-image-buffer (buffer media-list index title)
  "Render photo MEDIA-LIST at INDEX into BUFFER using `image-mode'."
  (let* ((media (nth index media-list))
         (file (chirp-media--photo-file media)))
    (unless file
      (user-error "Image preview unavailable"))
    (if (not (display-images-p))
        (chirp-media--render-buffer buffer media-list index title)
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert-file-contents-literally file))
        (chirp-media-image-mode)
        (use-local-map chirp-media-image-mode-map)
        (setq-local chirp--media-list media-list)
        (setq-local chirp--media-index index)
        (setq-local chirp--media-title title)
        (setq-local chirp--view-title title)
        (setq-local chirp--timeline-kind nil)
        (setq-local chirp--refresh-function nil)
        (setq-local chirp--rerender-function
                    (lambda ()
                      (chirp-media-open media-list index title buffer)))
        (setq-local header-line-format nil)
        (goto-char (point-min)))
      (chirp-display-buffer buffer)
      (message "%s (%d/%d)" title (1+ index) (length media-list)))))

(defun chirp-media--render-buffer (buffer media-list index title)
  "Render MEDIA-LIST at INDEX into BUFFER."
  (let* ((media (nth index media-list))
         (total (length media-list)))
    (with-current-buffer buffer
      (chirp-media-view-mode)
      (setq-local chirp--media-list media-list)
      (setq-local chirp--media-index index)
      (setq-local chirp--media-title title)
      (setq-local chirp--view-title title)
      (setq-local chirp--timeline-kind nil)
      (setq-local chirp--refresh-function nil)
      (setq-local chirp--rerender-function
                  (lambda ()
                    (chirp-media-open media-list index title buffer)))
      (setq-local header-line-format nil)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "%s (%d/%d)\n\n" title (1+ index) total))
        (cond
         ((string= (plist-get media :type) "photo")
          (if-let* ((image (chirp-media-view-image media)))
              (insert-image image (format "[image %d]" (1+ index)))
            (insert "Image preview unavailable.\n"))
          (insert "\n\n"))
         ((chirp-media-video-like-p media)
          (if-let* ((image (chirp-media--cached-video-preview-image media)))
              (progn
                (insert-image image (format "[video %d]" (1+ index)))
                (insert "\n\n"))
            (chirp-media-prefetch-media media buffer)
            (insert "Preview loading...\n\n"))
          (insert (if (string= (plist-get media :type) "animated_gif")
                      "Animated GIF media.\n\n"
                    "Video media.\n\n"))
          (insert "Press `v` to play externally, `D` to download the original media, or `o` to open the source URL.\n\n"))
         (t
          (insert "Unsupported media type.\n\n")))
        (insert (format "Type: %s\n" (or (plist-get media :type) "unknown")))
        (when-let* ((width (plist-get media :width))
                    (height (plist-get media :height)))
          (insert (format "Size: %sx%s\n" width height)))
        (when-let* ((url (plist-get media :url)))
          (insert (format "URL: %s\n" url)))
        (goto-char (point-min))))
    (chirp-display-buffer buffer)))

(defun chirp-media-open (media-list index &optional title buffer)
  "Open MEDIA-LIST at INDEX with TITLE in BUFFER."
  (let* ((safe-index (max 0 (min index (1- (length media-list)))))
         (media (nth safe-index media-list))
         (base-title (or title "Chirp Media")))
    (if (null media)
        (user-error "No media available")
      (if (chirp-media-video-like-p media)
          (chirp-media--play-external media)
        (let* ((buffer (or buffer (chirp-buffer)))
               (source-buffer
                (or (and (buffer-live-p buffer)
                         (with-current-buffer buffer
                           chirp--media-source-buffer))
                    (current-buffer)))
               (source-anchor
                (or (and (buffer-live-p buffer)
                         (with-current-buffer buffer
                           chirp--media-source-anchor))
                    (and (buffer-live-p source-buffer)
                         (with-current-buffer source-buffer
                           (chirp-capture-point-anchor)))))
               (source-window-state
                (or (and (buffer-live-p buffer)
                         (with-current-buffer buffer
                           chirp--media-source-window-state))
                    (chirp-capture-window-state source-buffer))))
          (if (string= (plist-get media :type) "photo")
              (chirp-media--render-image-buffer
               buffer
               media-list
               safe-index
               base-title)
            (chirp-media--render-buffer
             buffer
             media-list
             safe-index
             base-title))
          (with-current-buffer buffer
            (setq-local chirp--media-source-buffer source-buffer)
            (setq-local chirp--media-source-anchor source-anchor)
            (setq-local chirp--media-source-window-state source-window-state)))))))

(defun chirp-media-open-at-point ()
  "Open the media item at point."
  (interactive)
  (let ((media-list (chirp-media-list-at-point))
        (index (or (chirp-media-index-at-point) 0)))
    (if media-list
        (chirp-media-open media-list
                          index
                          (or chirp--view-title "Chirp Media"))
      (user-error "No media at point"))))

(defun chirp-media-next ()
  "Open the next media item in the current viewer."
  (interactive)
  (if (<= (length chirp--media-list) 1)
      (user-error "No next media item")
    (chirp-media-open chirp--media-list
                      (mod (1+ chirp--media-index) (length chirp--media-list))
                      chirp--media-title
                      (current-buffer))))

(defun chirp-media-previous ()
  "Open the previous media item in the current viewer."
  (interactive)
  (if (<= (length chirp--media-list) 1)
      (user-error "No previous media item")
    (chirp-media-open chirp--media-list
                      (mod (1- chirp--media-index) (length chirp--media-list))
                      chirp--media-title
                      (current-buffer))))

(provide 'chirp-media)

;;; chirp-media.el ends here
