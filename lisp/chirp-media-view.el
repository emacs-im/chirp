;;; chirp-media-view.el --- Media viewer sessions for Chirp -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Own reader-style media buffers and their application navigation state.
;; Fetching, caching, preview construction, and external playback remain in
;; chirp-media.el; video.el owns Canvas viewports and playback transport.

;;; Code:

(declare-function appkit-media-present-video-inline
                  "appkit-media-resource"
                  (surface &optional client-label &rest arguments))

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
  "Return the media selection at point or for its containing entry."
  (let (media-list index)
    (if (setq media-list (chirp-media-list-at-point))
        (setq index (or (chirp-media-index-at-point) 0))
      (let ((entry (chirp-entry-at-point)))
        (setq media-list
              (or (plist-get entry :media)
                  (and (eq (plist-get entry :kind) 'tweet)
                       (chirp-tweet-article-images entry)))
              index 0)))
    (when media-list
      (or (chirp-media-video-selection media-list index)
          (chirp-media-selection-create media-list index)))))

(defun chirp-media-view--dedicated-image-source (media)
  "Return a local image source for dedicated MEDIA viewing."
  (if (string= (plist-get media :type) "photo")
      (or (chirp-media--photo-file media)
          (user-error "Image preview unavailable"))
    (user-error "Unsupported media type")))

(defun chirp-media-open-dedicated (selection &optional title buffer)
  "Open SELECTION in a dedicated reader named by TITLE.

Reuse BUFFER when it is live.  When SELECTION owns an active inline video
surface, the dedicated target borrows that surface's Appkit session and exact
player state."
  (unless (chirp-media-selection-p selection)
    (error "Invalid Chirp media selection"))
  (let* ((media-list (chirp-media-selection-media-list selection))
         (index (chirp-media-selection-index selection))
         (safe-index (max 0 (min index (1- (length media-list)))))
         (media (nth safe-index media-list))
         (video-p (and media (chirp-media-video-like-p media)))
         (video-inline
          (and (= safe-index index)
               (chirp-media-selection-live-video-inline selection)))
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
         (image-source
          (and media (not video-p)
               (chirp-media-view--dedicated-image-source media)))
         (session
          (and video-p
               (not video-inline)
               (or (chirp-media-video-session-create media)
                   (user-error "Current media has no playable URL"))))
         opened-p)
    (unless media
      (user-error "No media available"))
    (unwind-protect
        (progn
          (setq viewer
                (cond
                 (video-inline
                  (appkit-media-present-video-inline
                   video-inline base-title :buffer viewer))
                 (video-p
                  (appkit-media-present-video-session
                   session base-title :buffer viewer :start t))
                 (t
                  (video-open image-source :kind 'image :buffer viewer))))
          (with-current-buffer viewer
            (chirp-media-view--set-state
             media-list safe-index base-title
             (and (not video-p) image-source))
            (setq-local chirp--media-source-buffer source-buffer
                        chirp--media-source-anchor source-anchor
                        chirp--media-source-window-state source-window-state
                        video-next-function
                        (and (> (length media-list) 1) #'chirp-media-next)
                        video-previous-function
                        (and (> (length media-list) 1) #'chirp-media-previous)
                        video-quit-function #'chirp-media-quit))
          (setq opened-p t)
          (message "%s (%d/%d)" base-title (1+ safe-index)
                   (length media-list))
          viewer)
      (unless opened-p
        (when (buffer-live-p viewer)
          (kill-buffer viewer))
        (when session
          (appkit-media-video-session-close session))))))

(defun chirp-media-open-dedicated-at-point ()
  "Open the selected media in a dedicated reader-style media buffer."
  (interactive)
  (if-let* ((selection (chirp-media-view--selection-at-point)))
      (chirp-media-open-dedicated
       selection (or chirp--view-title "Chirp Media"))
    (user-error "No media at point")))

(defun chirp-media-open-external-at-point ()
  "Open the selected video in the configured external player."
  (interactive)
  (if-let* ((selection (chirp-media-view--selection-at-point))
            (media
             (nth (chirp-media-selection-index selection)
                  (chirp-media-selection-media-list selection))))
      (chirp-media-play-video media t)
    (user-error "No media at point")))

(defun chirp-media-open (media-list index &optional title buffer)
  "Open MEDIA-LIST at INDEX using TITLE in dedicated media BUFFER.

Reuse the current Chirp buffer's registered inline presentation when the same
rendered media list and item are already active."
  (chirp-media-open-dedicated
   (or (chirp-media-video-selection media-list index)
       (chirp-media-selection-create media-list index))
   title buffer))

(defun chirp-media-open-at-point ()
  "Open the media item at point."
  (interactive)
  (if-let* ((selection (chirp-media-view--selection-at-point)))
      (chirp-media-open-dedicated
       selection (or chirp--view-title "Chirp Media"))
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
