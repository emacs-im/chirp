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
  (let
      ((source-buffer chirp--media-source-buffer)
       (source-anchor chirp--media-source-anchor)
       (source-window-state chirp--media-source-window-state))
    (progn
      (chirp-quit-current-buffer)
      (when-let*
          ((surface (chirp--live-projection-view source-buffer)))
        (appkit-surface-send surface '(chirp-media cancel))))
    (when (buffer-live-p source-buffer)
      (chirp-display-buffer source-buffer)
      (or (chirp-restore-window-state source-window-state)
          (with-current-buffer source-buffer
            (when source-anchor
              (chirp-restore-point-anchor source-anchor)))))))

(defvar-local chirp--media-close-hook nil
  "Settlement hook owned by the current dedicated presentation.")

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

(defun chirp-media-open-dedicated (selection &optional title buffer external-fallback)
  "Send SELECTION to its exact source Surface for managed presentation."
  (unless (chirp-media-selection-p selection)
    (error "Invalid Chirp media selection"))
  (let* ((source-buffer
          (or (and (buffer-live-p buffer)
                   (buffer-local-value 'chirp--media-source-buffer buffer))
              (current-buffer)))
         (surface (chirp--live-projection-view source-buffer))
         (media-list (chirp-media-selection-media-list selection))
         (index (max 0 (min (chirp-media-selection-index selection)
                            (1- (length media-list)))))
         (media (nth index media-list)))
    (unless (and surface media) (user-error "No live source media Surface"))
    (appkit-surface-send
     surface
     (list 'chirp-media 'open
           (list :selection selection :index index :media media
                 :inline (chirp-media-selection-live-video-inline selection)
                 :title (or title "Chirp Media") :reuse buffer :source surface
                 :external-fallback external-fallback
                 :source-buffer source-buffer
                 :anchor (or (and (buffer-live-p buffer)
                                  (buffer-local-value 'chirp--media-source-anchor buffer))
                             (with-current-buffer source-buffer (chirp-capture-point-anchor)))
                 :window-state
                 (or (and (buffer-live-p buffer)
                          (buffer-local-value 'chirp--media-source-window-state buffer))
                     (chirp-capture-window-state source-buffer)))))))

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

(require 'appkit-media-effect)
(require 'video-view)

(defun chirp-media-view--present-start (_context input _observe resolve reject)
  "Present committed INPUT, borrowing its exact inline video session."
  (let* ((intent (plist-get input :intent))
         (source (plist-get intent :source))
         (selection (plist-get intent :selection))
         (media-list (chirp-media-selection-media-list selection))
         (media (plist-get intent :media))
         (index (plist-get intent :index))
         (inline (plist-get intent :inline))
         (video-p (chirp-media-video-like-p media))
         (title (plist-get intent :title))
         (file (plist-get input :file))
         (reuse (plist-get intent :reuse))
         (viewer (or (and (buffer-live-p reuse) reuse)
                     (generate-new-buffer "*Chirp Media*")))
         session opened-p canceling)
    (condition-case condition
        (progn
          (unless (appkit-surface-live-p source)
            (error "Media source has closed"))
          (when (and video-p (not inline))
            (setq session (or (chirp-media-video-session-create media)
                              (error "Current media has no playable URL"))))
          (setq viewer
                (cond
                 (inline
                   (unless (eq inline (chirp-media-selection-live-video-inline selection))
                     (error "Inline video session has closed"))
                   (appkit-media-present-video-inline inline title :buffer viewer))
                 (video-p
                  (appkit-media-present-video-session session title :buffer viewer :start t))
                 (t (video-open file :kind 'image :buffer viewer))))
          (with-current-buffer viewer
            (chirp-media-view--set-state media-list index title (and (not video-p) file))
            (setq-local chirp--media-source-buffer (plist-get intent :source-buffer)
                        chirp--media-source-anchor (plist-get intent :anchor)
                        chirp--media-source-window-state (plist-get intent :window-state)
                        video-next-function (and (> (length media-list) 1) #'chirp-media-next)
                        video-previous-function (and (> (length media-list) 1) #'chirp-media-previous)
                        video-quit-function #'chirp-media-quit)
            (when chirp--media-close-hook
              (remove-hook 'kill-buffer-hook chirp--media-close-hook t))
            (setq-local chirp--media-close-hook
                        (lambda ()
                          (unless canceling
                            (when (appkit-surface-live-p source)
                              (let ((current (plist-get (appkit-surface-model source) :media-intent)))
                                (when (or (eq current intent) (eq viewer (plist-get current :reuse)))
                                  (appkit-surface-post source '(chirp-media cancel))))))
                          (funcall resolve nil)))
            (add-hook 'kill-buffer-hook chirp--media-close-hook nil t))
          (setq opened-p t)
          (appkit-cancellation-create
           :kind 'logical
           :cancel
           (lambda ()
             (unless (and (appkit-surface-live-p source)
                          (eq viewer (plist-get (plist-get (appkit-surface-model source) :media-intent)
                                                :reuse)))
               (when (buffer-live-p viewer) (setq canceling t) (kill-buffer viewer))))))
      ((error quit)
       (unless opened-p
         (when (buffer-live-p viewer) (setq canceling t) (kill-buffer viewer))
         (when session (appkit-media-video-session-close session)))
       (if (and (plist-get intent :external-fallback) (not inline)
                (appkit-surface-live-p source))
           (progn
             (display-warning 'chirp-media
                              (format "Internal video playback failed: %s"
                                      (error-message-string condition)) :warning)
             (chirp-media--play-external media)
             (funcall resolve nil))
         (funcall reject (error-message-string condition)))
       nil))))

(defun chirp-media-view--acquire-start (context input observe resolve reject)
  "Acquire media without giving transport callbacks presentation authority."
  (let* ((media (plist-get input :media))
         (url (plist-get media :url))
         (video-p (chirp-media-video-like-p media))
         (cached (and (not video-p) (chirp-media-cached-file url "media" "jpg"))))
    (cond
     (video-p (funcall resolve nil) nil)
     ((not (equal (plist-get media :type) "photo"))
      (funcall reject "Unsupported media type") nil)
     (cached (funcall resolve cached) nil)
     (t (appkit-media-image-acquisition-start
         context
         (appkit-media-image-acquisition-create
          (appkit-media-resource-create :url url)
          (chirp-media-cache-base url "media"))
         observe resolve reject)))))

(defun chirp-media-view--failed (_input reason)
  "Map media failure REASON to source model state."
  (list 'chirp-media 'failed (format "%s" reason)))

(defun chirp-media-view--update (_context model message)
  "Commit media state and emit post-commit acquisition or presentation."
  (pcase (cadr message)
    ('open
     (let ((intent (nth 2 message)))
       (setf (plist-get model :media-intent) intent
             (plist-get model :media-phase) 'acquiring
             (plist-get model :media-error) nil)
       (appkit-next
        :model model :render (appkit-projection-change-create :frame-p t)
        :commands
        (list (appkit-command-start-effect
               (appkit-effect-create
                :key 'chirp-media-acquire :input intent
                :start #'chirp-media-view--acquire-start
                :success (lambda (input file) (list 'chirp-media 'acquired input file))
                :failure #'chirp-media-view--failed
                :cancellation-requirement 'transport))))))
    ('acquired
     (let ((intent (nth 2 message)) (file (nth 3 message)))
       (setf (plist-get model :media-phase) 'presenting)
       (appkit-next
        :model model :render (appkit-projection-change-create :frame-p t)
        :commands
        (list (appkit-command-start-effect
               (appkit-effect-create
                :key 'chirp-media-present :input (list :intent intent :file file)
                :start #'chirp-media-view--present-start
                :success (lambda (_input _result) '(chirp-media closed))
                :failure #'chirp-media-view--failed))))))
    ('local
     (let ((file (nth 2 message)) (kind (nth 3 message)))
       (setf (plist-get model :media-intent) nil
             (plist-get model :media-phase) 'presenting
             (plist-get model :media-error) nil)
       (appkit-next
        :model model :render appkit-render-none
        :commands
        (list (appkit-command-cancel-effect 'chirp-media-acquire)
              (appkit-command-start-effect
               (appkit-effect-create
                :key 'chirp-media-present
                :input (if (eq kind 'video)
                           (appkit-media-video-presentation-create
                            (appkit-media-resource-create :file file)
                            :label "Chirp XChat" :start t)
                         file)
                :start (if (eq kind 'video) #'appkit-media-video-presentation-start
                         #'appkit-media-file-presentation-start)
                :success (lambda (_input _result) '(chirp-media closed))
                :failure #'chirp-media-view--failed))))))
    ('closed
     (setf (plist-get model :media-phase) 'idle)
     (appkit-next :model model :render (appkit-projection-change-create :frame-p t)))
    ('failed
     (setf (plist-get model :media-phase) 'failed
           (plist-get model :media-error) (nth 2 message))
     (appkit-next :model model :render (appkit-projection-change-create :frame-p t)))
    ('cancel
     (setf (plist-get model :media-intent) nil
           (plist-get model :media-phase) 'idle)
     (appkit-next :model model :render (appkit-projection-change-create :frame-p t)
                  :commands (list (appkit-command-cancel-effect 'chirp-media-acquire)
                                  (appkit-command-cancel-effect 'chirp-media-present))))
    (_ (error "Unsupported Chirp media message: %S" message))))

(defun chirp-media-open-local (surface file kind)
  "Present decrypted local FILE through its exact initiating SURFACE."
  (unless (and (appkit-surface-live-p surface) (chirp-media--valid-cache-file-p file))
    (user-error "Local media is unavailable"))
  (appkit-surface-send surface (list 'chirp-media 'local file kind)))

(provide 'chirp-media-view)

;;; chirp-media-view.el ends here
