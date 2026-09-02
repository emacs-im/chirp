;;; chirp-media-view.el --- Media viewer sessions for Chirp -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Own reader-style media buffers and their application navigation state.
;; Fetching, caching, preview construction, and external playback remain in
;; chirp-media.el; video.el owns Canvas viewports and playback transport.

;;; Code:

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

(defun chirp-media-view--set-state (media-list index title file)
  "Record MEDIA-LIST, INDEX, TITLE, and rendered FILE in this viewer."
  (setq-local chirp--media-list media-list)
  (setq-local chirp--media-index index)
  (setq-local chirp--media-title title)
  (setq-local chirp--media-file file)
  (setq-local chirp--view-title title)
  (setq-local chirp--timeline-kind nil)
  (setq-local chirp--refresh-function nil))

(defun chirp-media-view--selection-at-point ()
  "Return (MEDIA-LIST . INDEX) for point or its containing entry."
  (if-let* ((media-list (chirp-media-list-at-point)))
      (cons media-list (or (chirp-media-index-at-point) 0))
    (let* ((entry (chirp-entry-at-point))
           (media-list
            (or (plist-get entry :media)
                (and (eq (plist-get entry :kind) 'tweet)
                     (chirp-tweet-article-images entry)))))
      (and media-list (cons media-list 0)))))

(defun chirp-media-view--dedicated-source (media)
  "Return (SOURCE . KIND) for dedicated MEDIA viewing."
  (cond
   ((chirp-media-video-like-p media)
    (if-let* ((source (chirp-media-playback-url media)))
        (cons source 'video)
      (user-error "Current media has no playable URL")))
   ((string= (plist-get media :type) "photo")
    (if-let* ((source (chirp-media--photo-file media)))
        (cons source 'image)
      (user-error "Image preview unavailable")))
   (t
    (user-error "Unsupported media type"))))

(defun chirp-media-open-dedicated
    (media-list index &optional title buffer)
  "Open MEDIA-LIST item INDEX in a reader-style dedicated media BUFFER."
  (let* ((safe-index (max 0 (min index (1- (length media-list)))))
         (media (nth safe-index media-list))
         (base-title (or title "Chirp Media"))
         (viewer
          (or (and (buffer-live-p buffer) buffer)
              (generate-new-buffer "*Chirp Media*")))
         (source-buffer
          (or (and (buffer-live-p buffer)
                   (with-current-buffer buffer chirp--media-source-buffer))
              (current-buffer)))
         (source-anchor
          (or (and (buffer-live-p buffer)
                   (with-current-buffer buffer chirp--media-source-anchor))
              (and (buffer-live-p source-buffer)
                   (with-current-buffer source-buffer
                     (chirp-capture-point-anchor)))))
         (source-window-state
          (or (and (buffer-live-p buffer)
                   (with-current-buffer buffer chirp--media-source-window-state))
              (chirp-capture-window-state source-buffer)))
         (source-kind (and media (chirp-media-view--dedicated-source media))))
    (unless media
      (user-error "No media available"))
    (setq viewer
          (video-open
           (car source-kind) :kind (cdr source-kind) :buffer viewer))
    (with-current-buffer viewer
      (chirp-media-view--set-state
       media-list safe-index base-title
       (and (eq (cdr source-kind) 'image) (car source-kind)))
      (setq-local chirp--media-source-buffer source-buffer
                  chirp--media-source-anchor source-anchor
                  chirp--media-source-window-state source-window-state
                  video-next-function
                  (and (> (length media-list) 1) #'chirp-media-next)
                  video-previous-function
                  (and (> (length media-list) 1) #'chirp-media-previous)
                  video-quit-function #'chirp-media-quit))
    (message "%s (%d/%d)" base-title (1+ safe-index) (length media-list))
    viewer))

(defun chirp-media-open-dedicated-at-point ()
  "Open the selected media in a dedicated reader-style media buffer."
  (interactive)
  (if-let* ((selection (chirp-media-view--selection-at-point)))
      (chirp-media-open-dedicated
       (car selection) (cdr selection)
       (or chirp--view-title "Chirp Media"))
    (user-error "No media at point")))

(defun chirp-media-open-external-at-point ()
  "Open the selected video in the configured external player."
  (interactive)
  (if-let* ((selection (chirp-media-view--selection-at-point))
            (media (nth (cdr selection) (car selection))))
      (chirp-media-play-video media t)
    (user-error "No media at point")))

(defun chirp-media-open (media-list index &optional title buffer)
  "Open MEDIA-LIST at INDEX in the dedicated reader-style media BUFFER."
  (chirp-media-open-dedicated media-list index title buffer))

(defun chirp-media-open-at-point ()
  "Open the media item at point."
  (interactive)
  (if-let* ((selection (chirp-media-view--selection-at-point)))
      (chirp-media-open
       (car selection) (cdr selection)
       (or chirp--view-title "Chirp Media"))
    (user-error "No media at point")))

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
