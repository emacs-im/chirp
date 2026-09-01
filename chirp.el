;;; chirp.el --- Browse X timelines -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;; Author: lucius
;; Version: 0.1.0
;; Package-Requires: ((emacs "32.0") (appkit "0.3.0") (browser-session "0.1.0") (video "0.1.0") (plz "0.8") (transient "0.4.3") (websocket "1.16"))
;; Keywords: convenience, comm
;; URL: https://github.com/LuciusChen/chirp

;;; Commentary:

;; chirp.el is a lightweight X/Twitter browser for Emacs.  Its authenticated
;; direct X web backend renders timelines, direct messages, media, and writes.

;;; Code:

(eval-and-compile
  (let ((dir (file-name-directory
              (or load-file-name
                  (buffer-file-name)
                  default-directory))))
    (add-to-list 'load-path (expand-file-name "lisp" dir))))

(require 'chirp-url)
(require 'chirp-core)
(require 'chirp-backend)
(require 'chirp-notifications)
(require 'chirp-media)
(require 'chirp-media-view)
(require 'chirp-render)
(require 'chirp-edit-history)
(require 'chirp-actions)
(require 'chirp-unsent)
(require 'chirp-thread)
(require 'chirp-profile)
(require 'chirp-timeline)
(require 'chirp-dm)

(defun chirp--open-url-target (target)
  "Open parsed X URL TARGET in its owning Chirp view."
  (pcase (plist-get target :kind)
    ('home (chirp-timeline-open-home))
    ('bookmarks (chirp-timeline-open-bookmarks))
    ('direct-messages (chirp-dm-open-inbox))
    ('search (chirp-timeline-open-search (plist-get target :query)))
    ('tweet (chirp-thread-open (plist-get target :id)))
    ('edit-history (chirp-edit-history-open (plist-get target :id)))
    ('profile (chirp-profile-open (plist-get target :handle)))
    ('followers (chirp-profile-open-followers (plist-get target :handle)))
    ('following-users
     (chirp-profile-open-following-users (plist-get target :handle)))
    ('likes (chirp-timeline-open-likes (plist-get target :handle)))
    ('list (chirp-timeline-open-list (plist-get target :id)))
    (_ (error "Unsupported Chirp X URL target: %S" target))))

;;;###autoload
(defun chirp-open-url (url &optional _new-window)
  "Open supported X URL inside Chirp.

NEW-WINDOW is accepted for compatibility with `browse-url-handlers'."
  (interactive "sX URL: ")
  (if-let* ((target (chirp-url-parse url)))
      (chirp--open-url-target target)
    (user-error "Unsupported X URL: %s" url)))

;;;###autoload
(defun chirp-login ()
  "Capture an X browser session for Chirp."
  (interactive)
  (chirp-x-capture-browser-session))

;;;###autoload
(defun chirp-forget-browser-session ()
  "Delete Chirp's private browser-imported X session."
  (interactive)
  (chirp-x-clear-auth-file))

;;;###autoload
(defun chirp-home ()
  "Open the home timeline."
  (interactive)
  (chirp-timeline-open-home))

;;;###autoload
(defun chirp-direct-messages ()
  "Unlock XChat and open a fresh read-only direct-message inbox."
  (interactive)
  (chirp-dm-open-inbox))

;;;###autoload
(defun chirp-following ()
  "Open the following timeline."
  (interactive)
  (chirp-timeline-open-following))

;;;###autoload
(defun chirp-bookmarks ()
  "Open bookmarks."
  (interactive)
  (chirp-timeline-open-bookmarks))

;;;###autoload
(defun chirp-likes ()
  "Open liked tweets for the current account."
  (interactive)
  (chirp-timeline-open-likes))

;;;###autoload
(defun chirp-me ()
  "Open the authenticated account's profile."
  (interactive)
  (chirp-backend-whoami
   (lambda (user _envelope)
     (if-let* ((handle (plist-get user :handle)))
         (chirp-profile-open handle)
       (message "X returned an authenticated profile Chirp could not parse.")))
   (lambda (message)
     (message "%s" message))))

;;;###autoload
(defun chirp-list (&optional list-id)
  "Open a list timeline.

When LIST-ID is nil, prompt from the authenticated account's lists."
  (interactive)
  (chirp-timeline-open-list list-id))

;;;###autoload
(defun chirp-search (query)
  "Search X for QUERY."
  (interactive "sSearch X: ")
  (chirp-timeline-open-search query))

;;;###autoload
(defun chirp-thread (tweet-id)
  "Open a thread for TWEET-ID."
  (interactive "sTweet ID: ")
  (chirp-thread-open tweet-id))

;;;###autoload
(defun chirp-profile (handle)
  "Open HANDLE's profile."
  (interactive "sProfile handle: ")
  (chirp-profile-open handle))

;;;###autoload
(defun chirp-profile-followers (handle)
  "Open followers for HANDLE."
  (interactive "sProfile handle: ")
  (chirp-profile-open-followers handle))

;;;###autoload
(defun chirp-profile-following-users (handle)
  "Open followed accounts for HANDLE."
  (interactive "sProfile handle: ")
  (chirp-profile-open-following-users handle))

(provide 'chirp)

;;; chirp.el ends here
