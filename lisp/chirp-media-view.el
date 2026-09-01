;;; chirp-media-view.el --- Media viewer sessions for Chirp -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Own interactive media viewer buffers and their current local resource.
;; Fetching, caching, preview construction, and external playback remain in
;; chirp-media.el.  This boundary also provides a home for a future in-Emacs
;; video presentation without coupling it to timeline rendering.

;;; Code:

(require 'cl-lib)
(require 'dired)
(require 'image-mode)
(require 'appkit-evil)
(require 'chirp-core)
(require 'chirp-media)

(defvar-local chirp--media-list nil
  "Media list displayed by the current Chirp media buffer.")

(defvar-local chirp--media-index 0
  "Currently selected media index in the current media buffer.")

(defvar-local chirp--media-title nil
  "Base title used by the current media buffer.")

(defvar-local chirp--media-file nil
  "Local file rendered for the current media item, or nil.")

(defvar-local chirp--media-source-buffer nil
  "Source Chirp buffer that opened the current media buffer.")

(defvar-local chirp--media-source-anchor nil
  "Saved source point anchor used when closing the current media buffer.")

(defvar-local chirp--media-source-window-state nil
  "Saved source window state used when closing the current media buffer.")

(defun chirp-media-quit ()
  "Close the current media buffer and restore its source view."
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

(defun chirp-media-jump-to-file ()
  "Open Dired at the local file rendered by the current media viewer."
  (interactive)
  (unless (and (stringp chirp--media-file)
               (file-regular-p chirp--media-file))
    (user-error "Current media has no local file"))
  (dired-jump nil chirp--media-file))

(defvar-keymap chirp-media-view-mode-map
  :doc "Keymap for `chirp-media-view-mode'."
  :parent special-mode-map
  "n" #'chirp-media-next
  "p" #'chirp-media-previous
  "C-x C-j" #'chirp-media-jump-to-file
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
  "C-x C-j" #'chirp-media-jump-to-file
  "D" #'chirp-media-download-at-point
  "v" #'chirp-media-play
  "o" #'chirp-media-browse
  "q" #'chirp-media-quit)

(define-derived-mode chirp-media-image-mode image-mode "Chirp-Image"
  "Image mode used for Chirp photo viewing."
  (setq-local header-line-format nil)
  (appkit-evil-normalize-keymaps))

(defun chirp-media-view--setup-evil ()
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

(chirp-media-view--setup-evil)

(with-eval-after-load 'evil
  (chirp-media-view--setup-evil))

(defun chirp-media-browse ()
  "Browse the current media URL."
  (interactive)
  (if-let* ((media (or (chirp-media-at-point)
                       (nth chirp--media-index chirp--media-list)))
            (url (plist-get media :url)))
      (browse-url url)
    (user-error "No media URL available")))

(defun chirp-media-play ()
  "Play the current video or GIF using the configured external player."
  (interactive)
  (chirp-media--play-external
   (or (chirp-media-at-point)
       (nth chirp--media-index chirp--media-list))))

(defun chirp-media-view--set-state (media-list index title file)
  "Record MEDIA-LIST, INDEX, TITLE, and rendered FILE in this viewer."
  (setq-local chirp--media-list media-list)
  (setq-local chirp--media-index index)
  (setq-local chirp--media-title title)
  (setq-local chirp--media-file file)
  (setq-local chirp--view-title title)
  (setq-local chirp--timeline-kind nil)
  (setq-local chirp--refresh-function nil))

(defun chirp-media-view--render-image-buffer (buffer media-list index title)
  "Render photo MEDIA-LIST at INDEX into BUFFER using `image-mode'."
  (let* ((media (nth index media-list))
         (file (chirp-media--photo-file media)))
    (unless file
      (user-error "Image preview unavailable"))
    (if (not (display-images-p))
        (chirp-media-view--render-buffer buffer media-list index title file)
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert-file-contents-literally file))
        (chirp-media-image-mode)
        (use-local-map chirp-media-image-mode-map)
        (chirp-media-view--set-state media-list index title file)
        (setq-local chirp--rerender-function
                    (lambda ()
                      (chirp-media-open media-list index title buffer)))
        (setq-local header-line-format nil)
        (goto-char (point-min)))
      (chirp-display-buffer buffer)
      (message "%s (%d/%d)" title (1+ index) (length media-list)))))

(defun chirp-media-view--render-buffer
    (buffer media-list index title &optional rendered-file)
  "Render MEDIA-LIST at INDEX into BUFFER.

RENDERED-FILE is the local resource represented by the preview, when known."
  (let* ((media (nth index media-list))
         (total (length media-list))
         (file
          (or rendered-file
              (and (chirp-media-video-like-p media)
                   (chirp-media--preview-file media)))))
    (with-current-buffer buffer
      (chirp-media-view-mode)
      (chirp-media-view--set-state media-list index title file)
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
              (chirp-media-view--render-image-buffer
               buffer media-list safe-index base-title)
            (chirp-media-view--render-buffer
             buffer media-list safe-index base-title))
          (with-current-buffer buffer
            (setq-local chirp--media-source-buffer source-buffer)
            (setq-local chirp--media-source-anchor source-anchor)
            (setq-local chirp--media-source-window-state
                        source-window-state)))))))

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
                      (mod (1+ chirp--media-index)
                           (length chirp--media-list))
                      chirp--media-title
                      (current-buffer))))

(defun chirp-media-previous ()
  "Open the previous media item in the current viewer."
  (interactive)
  (if (<= (length chirp--media-list) 1)
      (user-error "No previous media item")
    (chirp-media-open chirp--media-list
                      (mod (1- chirp--media-index)
                           (length chirp--media-list))
                      chirp--media-title
                      (current-buffer))))

(provide 'chirp-media-view)

;;; chirp-media-view.el ends here
