;;; chirp-write-smoke.el --- Opt-in live write smoke for Chirp -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Commentary:

;; Exercise direct post, reply, quote, long-form, image-upload, and delete
;; workflows against the configured X account.  This file is never loaded by
;; the ordinary ERT suite.  Set CHIRP_ALLOW_WRITE_SMOKE=1 and evaluate
;; `chirp-write-smoke-run' explicitly.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'chirp-backend)

(defconst chirp-write-smoke--png-base64
  (concat "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0l"
          "EQVR42mNk+A8AAQUBAScY42YAAAAASUVORK5CYII=")
  "One-pixel PNG used by the live media smoke.")

(defun chirp-write-smoke--await (starter &optional timeout)
  "Run asynchronous STARTER and return its success values before TIMEOUT."
  (let (done result failure)
    (funcall starter
             (lambda (&rest values)
               (setq result values
                     done t))
             (lambda (message)
               (setq failure message
                     done t)))
    (let ((deadline (+ (float-time) (or timeout 180))))
      (while (and (not done) (< (float-time) deadline))
        (accept-process-output nil 0.2)))
    (unless done
      (error "Write smoke request timed out"))
    (when failure
      (error "%s" failure))
    result))

(cl-defun chirp-write-smoke--publish
    (&key kind text target-id attachments)
  "Publish KIND with TEXT, TARGET-ID, and ATTACHMENTS, returning its ID."
  (let* ((values
          (chirp-write-smoke--await
           (lambda (success failure)
             (chirp-backend-compose
              :kind kind
              :text text
              :target-id target-id
              :attachments attachments
              :callback success
              :errback failure))
           240))
         (tweet-id (plist-get (car values) :id)))
    (unless (and (stringp tweet-id)
                 (string-match-p "\\`[0-9]+\\'" tweet-id))
      (error "Write smoke create returned an invalid tweet ID"))
    tweet-id))

(defun chirp-write-smoke--fetch (tweet-id)
  "Fetch and return TWEET-ID without a stale thread cache entry."
  (chirp-backend-invalidate-thread tweet-id)
  (car
   (chirp-write-smoke--await
    (lambda (success failure)
      (chirp-backend-tweet tweet-id success failure)))))

(defun chirp-write-smoke--verify-text (tweet-id marker)
  "Require TWEET-ID to contain MARKER and return the normalized tweet."
  (let ((tweet (chirp-write-smoke--fetch tweet-id)))
    (unless (string-match-p (regexp-quote marker)
                            (or (plist-get tweet :text) ""))
      (error "Write smoke could not verify tweet %s" tweet-id))
    tweet))

(defun chirp-write-smoke--delete (tweet-id)
  "Delete TWEET-ID through Chirp's direct mutation path."
  (chirp-write-smoke--await
   (lambda (success failure)
     (chirp-backend-request
      (list "delete" "--yes" tweet-id) success failure))))

(defun chirp-write-smoke--confirm-deleted (tweet-id)
  "Require TWEET-ID to be unavailable after deletion."
  (chirp-backend-invalidate-thread tweet-id)
  (let (still-present)
    (condition-case nil
        (progn
          (chirp-write-smoke--await
           (lambda (success failure)
             (chirp-backend-tweet tweet-id success failure))
           60)
          (setq still-present t))
      (error nil))
    (when still-present
      (error "Deleted smoke tweet is still readable: %s" tweet-id))))

(defun chirp-write-smoke--image-file ()
  "Create and return the temporary PNG used by the write smoke."
  (let ((file (make-temp-file "chirp-write-smoke-" nil ".png")))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert (base64-decode-string chirp-write-smoke--png-base64))
      (write-region (point-min) (point-max) file nil 'silent))
    file))

(defun chirp-write-smoke-run ()
  "Run destructive direct-X smoke tests and delete every created tweet.

The command refuses to run unless CHIRP_ALLOW_WRITE_SMOKE equals 1.  It never
retries a write mutation.  Uploaded media without a created tweet has no known
delete operation and is left for X to expire."
  (interactive)
  (unless (equal (getenv "CHIRP_ALLOW_WRITE_SMOKE") "1")
    (user-error "Set CHIRP_ALLOW_WRITE_SMOKE=1 to permit live X writes"))
  ;; Resolve credentials before creating any local or remote artifact.
  (chirp-x-credentials)
  (let* ((marker
          (format "chirp-write-smoke-%s-%04x"
                  (format-time-string "%Y%m%dT%H%M%SZ" nil t)
                  (random #xffff)))
         (image-file (chirp-write-smoke--image-file))
         created-ids
         cleanup-failures
         result)
    (unwind-protect
        (let* ((root
                (chirp-write-smoke--publish
                 :kind 'post :text marker :attachments nil))
               (_ (push root created-ids))
               (root-tweet (chirp-write-smoke--verify-text root marker))
               (reply-marker (concat marker " reply"))
               (reply
                (chirp-write-smoke--publish
                 :kind 'reply :text reply-marker :target-id root
                 :attachments nil))
               (_ (push reply created-ids))
               (reply-tweet
                (chirp-write-smoke--verify-text reply reply-marker))
               (quote-marker (concat marker " quote"))
               (quote
                (chirp-write-smoke--publish
                 :kind 'quote :text quote-marker :target-id root
                 :attachments nil))
               (_ (push quote created-ids))
               (quote-tweet
                (chirp-write-smoke--verify-text quote quote-marker))
               (long-marker (concat marker " " (make-string 281 ?x)))
               (long
                (chirp-write-smoke--publish
                 :kind 'post :text long-marker :attachments nil))
               (_ (push long created-ids))
               (long-tweet
                (chirp-write-smoke--verify-text long marker))
               (image-marker (concat marker " image"))
               (image
                (chirp-write-smoke--publish
                 :kind 'post :text image-marker
                 :attachments (list image-file)))
               (_ (push image created-ids))
               (image-tweet
                (chirp-write-smoke--verify-text image image-marker)))
          (unless (equal (plist-get reply-tweet :reply-to-id) root)
            (error "Write smoke reply target did not match its root"))
          (unless (or (equal (plist-get
                              (plist-get quote-tweet :quoted-tweet) :id)
                             root)
                      (string-match-p
                       (regexp-quote root)
                       (or (plist-get quote-tweet :text) "")))
            (error "Write smoke quote target could not be verified"))
          (unless (> (chirp-backend--tweet-weighted-length
                      (plist-get long-tweet :text))
                     chirp-backend--standard-tweet-weight-limit)
            (error "Write smoke long-form text was not preserved"))
          (unless (plist-get image-tweet :media)
            (error "Write smoke image post had no normalized media"))
          (setq result
                (list :marker marker
                      :root (plist-get root-tweet :id)
                      :reply reply
                      :quote quote
                      :long long
                      :image image)))
      (dolist (tweet-id created-ids)
        (condition-case err
            (chirp-write-smoke--delete tweet-id)
          (error
           (push (cons tweet-id (error-message-string err))
                 cleanup-failures))))
      (dolist (tweet-id created-ids)
        (condition-case err
            (chirp-write-smoke--confirm-deleted tweet-id)
          (error
           (push (cons tweet-id (error-message-string err))
                 cleanup-failures))))
      (when (file-exists-p image-file)
        (delete-file image-file)))
    (when cleanup-failures
      (error "Write smoke cleanup failed: %S" (nreverse cleanup-failures)))
    (message "Chirp write smoke passed and cleaned up %d tweets"
             (length created-ids))
    result))

(provide 'chirp-write-smoke)

;;; chirp-write-smoke.el ends here
