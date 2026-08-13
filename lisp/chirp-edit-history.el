;;; chirp-edit-history.el --- Tweet edit history for Chirp -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Fetch and present the immutable versions of an edited X post.

;;; Code:

(require 'cl-lib)
(require 'appkit-projection)
(require 'chirp-core)
(require 'chirp-backend)
(require 'chirp-media)
(require 'chirp-render)

(defun chirp-edit-history--target-id (tweet-or-id)
  "Return the tweet ID represented by TWEET-OR-ID."
  (let ((id (if (listp tweet-or-id)
                (plist-get tweet-or-id :id)
              tweet-or-id)))
    (unless (and (stringp id)
                 (string-match-p "\\`[0-9]+\\'" id))
      (user-error "Tweet edit history requires a numeric tweet ID"))
    id))

(defun chirp-edit-history--initial-id (tweet-or-id target-id)
  "Return TWEET-OR-ID's initial version ID, defaulting to TARGET-ID."
  (or (and (listp tweet-or-id)
           (plist-get tweet-or-id :edit-history-initial-id))
      target-id))

(defun chirp-edit-history--rows (tweets)
  "Return projected edit-history rows for normalized TWEETS."
  (cl-loop for tweet in tweets
           for index from 0
           for id = (plist-get tweet :id)
           when id
           collect (list :key (list 'edit-version id)
                         :tweet tweet
                         :latest-p (zerop index)
                         :section (cond
                                   ((zerop index) "Latest post")
                                   ((= index 1) "Version history")))))

(defun chirp-edit-history--project (tweets)
  "Project normalized edit-history TWEETS into keyed Appkit rows."
  (appkit-projection-project
   (chirp-edit-history--rows tweets)
   (lambda (row) (plist-get row :key))
   :dependencies-function
   (lambda (row)
     (chirp-media-resource-keys-for-tweet (plist-get row :tweet)))))

(defun chirp-edit-history--print-row (row)
  "Insert one projected edit-history ROW."
  (chirp-render-insert-edit-history-row
   (appkit-projection-row-payload row)))

(defun chirp-edit-history--frame-text (state)
  "Return header text representing edit-history STATE."
  (let* ((status (plist-get state :status))
         (phase (plist-get status :phase))
         (message (plist-get status :message)))
    (pcase phase
      ('initial "Loading edit history...\n\n")
      ('error (format "Unable to load edit history.\n\n%s\n\n" message))
      (_ (and (null (plist-get state :items))
              "No edit history returned.\n")))))

(defun chirp-edit-history--sync (view invalidations)
  "Synchronize edit-history VIEW from INVALIDATIONS."
  (let ((state (appkit-view-state view)))
    (chirp-sync-projection
     view invalidations
     (chirp-edit-history--project (plist-get state :items))
     (chirp-edit-history--frame-text state))))

(defun chirp-edit-history--present (view tweets)
  "Install normalized TWEETS into edit-history VIEW."
  (let ((state (appkit-view-state view))
        (buffer (appkit-view-buffer view)))
    (setf (plist-get state :items) tweets
          (plist-get (plist-get state :status) :phase) 'idle
          (plist-get (plist-get state :status) :message) nil)
    (appkit-view-enqueue-event view (list :position 'first))
    (appkit-invalidate view :structure t :part 'frame :position t)
    (appkit-sync-invalidations view)
    (chirp-clear-status buffer)
    (chirp-media-prefetch-tweets tweets buffer)))

(defun chirp-edit-history--request (view)
  "Fetch and present the versions owned by edit-history VIEW."
  (let* ((state (appkit-view-state view))
         (tweet-id (plist-get (plist-get state :query) :tweet-id))
         (title (plist-get state :title))
         (buffer (appkit-view-buffer view))
         (token (chirp-begin-background-request buffer title)))
    (chirp-backend-edit-history
     tweet-id
     (lambda (tweets _envelope)
       (when (chirp-request-current-p buffer token)
         (chirp-edit-history--present view tweets)))
     (lambda (message)
       (when (chirp-request-current-p buffer token)
         (chirp-show-error buffer title
                           (plist-get state :refresh)
                           message))))
    buffer))

(defun chirp-edit-history-open (tweet-or-id)
  "Open the edit history represented by TWEET-OR-ID."
  (let* ((tweet-id (chirp-edit-history--target-id tweet-or-id))
         (initial-id (chirp-edit-history--initial-id tweet-or-id tweet-id))
         (title (format "Edit history: %s" initial-id))
         (refresh
          (lambda ()
            (chirp-backend-invalidate-edit-history tweet-id)
            (chirp-edit-history-open tweet-or-id)))
         (view
          (chirp-open-projection-view
           :id (list 'edit-history initial-id)
           :title title
           :state (list :type 'edit-history
                        :query (list :tweet-id tweet-id
                                     :initial-id initial-id)
                        :items nil
                        :title title
                        :refresh refresh
                        :status (list :phase 'initial :message nil)
                        :expanded-tweet-ids (make-hash-table :test #'equal))
           :sync-function #'chirp-edit-history--sync
           :printer #'chirp-edit-history--print-row
           :select t)))
    (chirp-edit-history--request view)))

;;;###autoload
(defun chirp-edit-history-open-at-point ()
  "Open the edit history for the edited tweet at point."
  (interactive)
  (let ((tweet (chirp-entry-at-point)))
    (unless (and (eq (plist-get tweet :kind) 'tweet)
                 (plist-get tweet :edited-p))
      (user-error "Current tweet has no edit history"))
    (chirp-edit-history-open tweet)))

(provide 'chirp-edit-history)

;;; chirp-edit-history.el ends here
