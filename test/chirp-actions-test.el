;;; chirp-actions-test.el --- Tests for compose actions -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'transient)
(require 'appkit-ui)
(require 'chirp-actions)
(require 'chirp-render)

(defun chirp-test--make-compose-buffer (body)
  "Return a cons of compose and source buffers seeded with BODY."
  (let ((compose (generate-new-buffer " *chirp-compose-test*"))
        (source (generate-new-buffer " *chirp-source-test*")))
    (with-current-buffer compose
      (chirp-compose-mode)
      (setq-local chirp-compose-kind 'post)
      (setq-local chirp-compose-source-buffer source)
      (setq-local chirp-compose-items (list (list :attachments nil)))
      (setq-local chirp-compose-temp-attachments nil)
      (setq-local chirp-compose-draft-id nil)
      (setq-local chirp-compose-scheduled-id nil)
      (setq-local chirp-compose-execute-at nil)
      (setq-local chirp-compose-unknown-outcome nil)
      (setq-local chirp-compose-reply-audience 'everyone)
      (erase-buffer)
      (insert body)
      (appkit-compose-setup
       :app (chirp-app)
       :context-function #'chirp-compose--header-string
       :status-fields-function #'chirp-compose--status-fields
       :parts-function #'chirp-compose--parts))
    (cons compose source)))

(defun chirp-test--open-compose-from-foreign-current-buffer (kind &optional tweet)
  "Open compose KIND while `current-buffer' is not the displayed source buffer.

Return a list of (compose source foreign)."
  (let ((source (generate-new-buffer " *chirp-source-window*"))
        (foreign (generate-new-buffer " *chirp-transient*"))
        compose)
    (with-current-buffer source
      (chirp-view-mode)
      (setq-local chirp--view-title "For You"))
    (switch-to-buffer source)
    (with-current-buffer foreign
      (chirp-compose-open kind tweet)
      (setq compose (current-buffer)))
    (list compose source foreign)))

(ert-deftest chirp-compose-status-fields-show-media-count ()
  "Compose status fields should reflect the current media count."
  (pcase-let ((`(,compose . ,source)
               (chirp-test--make-compose-buffer "hello")))
    (unwind-protect
        (with-current-buffer compose
          (setq-local chirp-compose-items
                      (list (list :attachments '("/tmp/photo.png"))))
          (appkit-compose-refresh)
          (should (string-match-p "Audience: Everyone"
                                  (appkit-compose-display-string)))
          (should (string-match-p "Length: 5/280"
                                  (appkit-compose-display-string)))
          (should (string-match-p "Media: 1/4"
                                  (appkit-compose-display-string))))
      (when (buffer-live-p compose)
        (kill-buffer compose))
      (when (buffer-live-p source)
        (kill-buffer source)))))

(ert-deftest chirp-compose-attach-video-uses-the-video-slot ()
  "An MP4 should occupy the single video slot and refuse extra media."
  (pcase-let ((`(,compose . ,source)
               (chirp-test--make-compose-buffer "hello")))
    (let ((video (make-temp-file "chirp-compose-video-" nil ".mp4"))
          (photo (make-temp-file "chirp-compose-photo-" nil ".png")))
      (unwind-protect
          (progn
            (write-region "mp4" nil video nil 'silent)
            (write-region "png" nil photo nil 'silent)
            (with-current-buffer compose
              (chirp-compose-attach-image video)
              (should (string-match-p "Media: 1 video"
                                      (appkit-compose-display-string)))
              (should (chirp-compose--video-attachment-p
                       (car (chirp-compose--item-attachments))))
              (should-error (chirp-compose-attach-image photo)
                            :type 'user-error)))
        (when (buffer-live-p compose)
          (kill-buffer compose))
        (when (buffer-live-p source)
          (kill-buffer source))
        (delete-file video)
        (delete-file photo)))))

(ert-deftest chirp-compose-reply-omits-reply-audience-field ()
  "Reply drafts should not expose a reply-audience status field."
  (pcase-let ((`(,compose . ,source)
               (chirp-test--make-compose-buffer "hello")))
    (unwind-protect
        (with-current-buffer compose
          (setq-local chirp-compose-kind 'reply)
          (setq-local chirp-compose-reply-audience nil)
          (appkit-compose-refresh)
          (should-not (string-match-p "Audience:"
                                      (appkit-compose-display-string)))
          (should-not (plist-member (chirp-compose--draft) :reply-audience)))
      (when (buffer-live-p compose)
        (kill-buffer compose))
      (when (buffer-live-p source)
        (kill-buffer source)))))

(ert-deftest chirp-compose-add-post-appends-an-empty-part ()
  "A post draft should be able to grow into an ordered multi-part draft."
  (pcase-let ((`(,compose . ,source)
               (chirp-test--make-compose-buffer "first")))
    (unwind-protect
        (with-current-buffer compose
          (chirp-compose-add-post)
          (should (= (length chirp-compose-items) 2))
          (should (string-match-p "Posts: 2" (appkit-compose-display-string)))
          (should (string-match-p "Post 1/2" (appkit-compose-display-string)))
          (insert "second")
          (let ((items (plist-get (chirp-compose--draft) :items)))
            (should (equal (mapcar (lambda (item) (plist-get item :text))
                                   items)
                           '("first" "second")))))
      (when (buffer-live-p compose)
        (kill-buffer compose))
      (when (buffer-live-p source)
        (kill-buffer source)))))

(ert-deftest chirp-compose-add-and-remove-posts-in-the-middle ()
  "Adding or removing a post should keep the surrounding items in order."
  (pcase-let ((`(,compose . ,source)
               (chirp-test--make-compose-buffer "first")))
    (unwind-protect
        (with-current-buffer compose
          (chirp-compose-add-post)
          (insert "third")
          (appkit-compose-goto-part 0)
          (chirp-compose-add-post)
          (insert "second")
          (should (equal (appkit-compose-bodies)
                         '("first" "second" "third")))
          (appkit-compose-goto-part 1)
          (chirp-compose-remove-post)
          (should (equal (appkit-compose-bodies) '("first" "third")))
          (should (eq (appkit-compose-current-part-index) 1)))
      (when (buffer-live-p compose)
        (kill-buffer compose))
      (when (buffer-live-p source)
        (kill-buffer source)))))

(ert-deftest chirp-compose-save-keeps-buffer-and-draft-id ()
  "Saving should keep the compose buffer and remember the X draft ID."
  (pcase-let ((`(,compose . ,source)
               (chirp-test--make-compose-buffer "hello")))
    (let (requests)
      (unwind-protect
          (cl-letf (((symbol-function 'chirp-backend-save-draft)
                     (lambda (&rest draft)
                       (push draft requests)
                       (funcall (plist-get draft :callback)
                                (list :id "2087") nil))))
            (with-current-buffer compose
              (chirp-compose-save)
              (should (buffer-live-p compose))
              (should-not (appkit-compose-submitting-p))
              (should (equal chirp-compose-draft-id "2087"))
              (should-not buffer-read-only)
              (chirp-compose-save))
            (setq requests (nreverse requests))
            (should (= (length requests) 2))
            (should (eq (plist-get (car requests) :kind) 'post))
            (should (equal (plist-get (car (plist-get (car requests) :items))
                                      :text)
                           "hello"))
            (should-not (plist-get (car requests) :draft-id))
            (should (equal (plist-get (cadr requests) :draft-id) "2087")))
        (when (buffer-live-p compose)
          (kill-buffer compose))
        (when (buffer-live-p source)
          (kill-buffer source))))))

(ert-deftest chirp-compose-save-posts-a-thread-as-one-draft ()
  "A multi-part save should send every item in one draft request."
  (pcase-let ((`(,compose . ,source)
               (chirp-test--make-compose-buffer "first")))
    (let (captured)
      (unwind-protect
          (cl-letf (((symbol-function 'chirp-backend-save-draft)
                     (lambda (&rest draft)
                       (setq captured draft)
                       (funcall (plist-get draft :callback)
                                (list :id "1") nil))))
            (with-current-buffer compose
              (chirp-compose-add-post)
              (insert "second")
              (chirp-compose-save))
            (let ((items (plist-get captured :items)))
              (should (= (length items) 2))
              (should (equal (plist-get (car items) :text) "first"))
              (should (equal (plist-get (cadr items) :text) "second")))
            (should (buffer-live-p compose)))
        (when (buffer-live-p compose)
          (kill-buffer compose))
        (when (buffer-live-p source)
          (kill-buffer source))))))

(ert-deftest chirp-compose-schedule-closes-after-success ()
  "Scheduling should submit one request and close the compose buffer."
  (pcase-let ((`(,compose . ,source)
               (chirp-test--make-compose-buffer "later")))
    (let (captured)
      (unwind-protect
          (cl-letf (((symbol-function 'chirp-backend-schedule)
                     (lambda (&rest draft)
                       (setq captured draft)
                       (funcall (plist-get draft :callback)
                                (list :id "9") nil))))
            (with-current-buffer compose
              (chirp-compose-schedule 1786700000))
            (should (eq (plist-get captured :kind) 'post))
            (should (equal (plist-get captured :execute-at) 1786700000))
            (should (equal (plist-get (car (plist-get captured :items)) :text)
                           "later"))
            (should-not (buffer-live-p compose)))
        (when (buffer-live-p compose)
          (kill-buffer compose))
        (when (buffer-live-p source)
          (kill-buffer source))))))

(ert-deftest chirp-compose-read-schedule-time-parses-local-time ()
  "The schedule prompt should parse a local time as Unix seconds."
  (cl-letf (((symbol-function 'read-string)
             (lambda (&rest _) "2026-12-01 09:30"))
            ((symbol-function 'current-time)
             (lambda () (encode-time 0 0 0 1 1 2026))))
    (should (equal (chirp-compose--read-schedule-time)
                   (time-convert (encode-time 0 30 9 1 12 2026) 'integer)))))

(ert-deftest chirp-compose-send-posts-a-thread-in-order ()
  "Sending a multi-part draft should create the root then reply to it."
  (pcase-let ((`(,compose . ,source)
               (chirp-test--make-compose-buffer "first")))
    (let (requests)
      (unwind-protect
          (cl-letf (((symbol-function 'chirp-backend-compose)
                     (lambda (&rest draft)
                       (push draft requests)
                       (funcall (plist-get draft :callback)
                                (list :id (format "%d" (length requests)))
                                nil))))
            (with-current-buffer compose
              (chirp-compose-add-post)
              (insert "second")
              (chirp-compose-send))
            (setq requests (nreverse requests))
            (should (= (length requests) 2))
            (should (eq (plist-get (car requests) :kind) 'post))
            (should (equal (plist-get (car requests) :text) "first"))
            (should (eq (plist-get (cadr requests) :kind) 'reply))
            (should (equal (plist-get (cadr requests) :text) "second"))
            (should (equal (plist-get (cadr requests) :target-id) "1"))
            (should-not (buffer-live-p compose)))
        (when (buffer-live-p compose)
          (kill-buffer compose))
        (when (buffer-live-p source)
          (kill-buffer source))))))

(ert-deftest chirp-compose-length-field-marks-long-form ()
  "Weighted length over 280 should be labeled as long-form."
  (pcase-let ((`(,compose . ,source)
               (chirp-test--make-compose-buffer (make-string 141 ?你))))
    (unwind-protect
        (with-current-buffer compose
          (appkit-compose-refresh)
          (should (string-match-p "Length: 282 long"
                                  (appkit-compose-display-string))))
      (when (buffer-live-p compose)
        (kill-buffer compose))
      (when (buffer-live-p source)
        (kill-buffer source)))))

(ert-deftest chirp-compose-describe-image-updates-attachment ()
  "Editing alt text should update the current attachment and status row."
  (pcase-let ((`(,compose . ,source)
               (chirp-test--make-compose-buffer "hello")))
    (let ((path (expand-file-name "/tmp/photo.png")))
      (unwind-protect
          (with-current-buffer compose
            (chirp-compose--set-current-item
             (list :attachments (list (list :path path))))
            (cl-letf (((symbol-function 'read-string)
                       (lambda (_prompt &optional _initial)
                         "a black cat")))
              (chirp-compose-describe-image path))
            (should (string-match-p "Alt: a black cat"
                                    (appkit-compose-display-string)))
            (should (equal (chirp-compose--attachment-description
                            (car (chirp-compose--item-attachments)))
                           "a black cat")))
        (when (buffer-live-p compose)
          (kill-buffer compose))
        (when (buffer-live-p source)
          (kill-buffer source))))))

(ert-deftest chirp-compose-describe-image-keeps-media-id ()
  "Alt text edits should not drop a restored X media ID."
  (pcase-let ((`(,compose . ,source)
               (chirp-test--make-compose-buffer "hello")))
    (unwind-protect
        (with-current-buffer compose
          (setq-local chirp-compose-items
                      (list (list :attachments
                                  (list (list :media-id "2087"
                                              :preview-url
                                              "https://pbs.twimg.com/media/x.jpg")))))
          (cl-letf (((symbol-function 'read-string)
                     (lambda (_prompt &optional _initial)
                       "restored cat")))
            (chirp-compose-describe-image "2087"))
          (let ((attachment (car (chirp-compose--item-attachments))))
            (should (equal (plist-get attachment :media-id) "2087"))
            (should (equal (plist-get attachment :preview-url)
                           "https://pbs.twimg.com/media/x.jpg"))
            (should (equal (plist-get attachment :description)
                           "restored cat"))))
      (when (buffer-live-p compose)
        (kill-buffer compose))
      (when (buffer-live-p source)
        (kill-buffer source)))))

(ert-deftest chirp-compose-send-deletes-the-x-draft ()
  "Publishing a restored draft should delete that X draft afterwards."
  (pcase-let ((`(,compose . ,source)
               (chirp-test--make-compose-buffer "hello")))
    (let (deleted)
      (unwind-protect
          (progn
            (with-current-buffer compose
              (setq-local chirp-compose-draft-id "2087"))
            (cl-letf (((symbol-function 'chirp-backend-compose)
                       (lambda (&rest draft)
                         (funcall (plist-get draft :callback)
                                  (list :id "1") nil)))
                      ((symbol-function 'chirp-backend-delete-unsent)
                       (lambda (kind id callback &optional _errback)
                         (setq deleted (list kind id))
                         (funcall callback nil nil))))
              (with-current-buffer compose
                (chirp-compose-send)))
            (should (equal deleted '(draft "2087")))
            (should-not (buffer-live-p compose)))
        (when (buffer-live-p compose)
          (kill-buffer compose))
        (when (buffer-live-p source)
          (kill-buffer source))))))

(ert-deftest chirp-compose-set-reply-audience-updates-draft ()
  "Choosing a reply audience should update the draft and status field."
  (pcase-let ((`(,compose . ,source)
               (chirp-test--make-compose-buffer "hello")))
    (unwind-protect
        (with-current-buffer compose
          (chirp-compose-set-reply-audience 'community)
          (should (eq chirp-compose-reply-audience 'community))
          (should (string-match-p "Audience: People you follow"
                                  (appkit-compose-display-string)))
          (should (eq (plist-get (chirp-compose--draft) :reply-audience)
                      'community)))
      (when (buffer-live-p compose)
        (kill-buffer compose))
      (when (buffer-live-p source)
        (kill-buffer source)))))

(ert-deftest chirp-compose-prefetches-mention-handles-without-blocking-capf ()
  "The installed compose hook should prefetch and cache mention candidates."
  (pcase-let ((`(,compose . ,source)
               (chirp-test--make-compose-buffer "hello @em")))
    (let ((request-count 0))
      (unwind-protect
          (cl-letf (((symbol-function 'run-with-idle-timer)
                     (lambda (_seconds _repeat function &rest args)
                       (apply function args)
                       nil))
                    ((symbol-function 'chirp-backend-search-users)
                     (lambda (query callback &optional _errback _max-results)
                       (setq request-count (1+ request-count))
                       (should (equal query "em"))
                       (funcall callback
                                '((:handle "emacs")
                                  (:handle "emacslife"))
                                nil))))
            (with-current-buffer compose
              (goto-char (appkit-compose-body-end-position))
              (let ((point-before (point)))
                (run-hooks 'post-command-hook)
                (let ((completion
                       (chirp-compose-mention-completion-at-point)))
                  (should (equal (buffer-substring-no-properties
                                  (nth 0 completion) (nth 1 completion))
                                 "em"))
                  (should (equal (nth 2 completion)
                                 '("emacs" "emacslife")))
                  (should (= (point) point-before))))
              (run-hooks 'post-command-hook)
              (should (= request-count 1))))
        (when (buffer-live-p compose)
          (kill-buffer compose))
        (when (buffer-live-p source)
          (kill-buffer source))))))

(ert-deftest chirp-compose-does-not-complete-email-addresses ()
  "An @ inside an email address should not start user completion."
  (pcase-let ((`(,compose . ,source)
               (chirp-test--make-compose-buffer "mail emacs@example")))
    (unwind-protect
        (with-current-buffer compose
          (goto-char (appkit-compose-body-end-position))
          (should-not (chirp-compose-mention-completion-at-point)))
      (when (buffer-live-p compose)
        (kill-buffer compose))
      (when (buffer-live-p source)
        (kill-buffer source)))))

(ert-deftest chirp-compose-send-keeps-buffer-until-success ()
  "Sending should keep the compose buffer until the backend succeeds."
  (pcase-let ((`(,compose . ,source)
               (chirp-test--make-compose-buffer "hello world")))
    (let (captured-draft)
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'chirp-backend-compose)
                       (lambda (&rest draft)
                         (setq captured-draft draft))))
              (with-current-buffer compose
                (chirp-compose-send)
                (should (appkit-compose-submitting-p))
                (should buffer-read-only))
              (should (buffer-live-p compose))
              (should (eq (plist-get captured-draft :kind) 'post))
              (should (equal (plist-get captured-draft :text) "hello world"))
              (should (eq (plist-get captured-draft :reply-audience) 'everyone))
              (should-not (plist-get captured-draft :attachments))
              (funcall (plist-get captured-draft :callback) nil nil)
              (should-not (buffer-live-p compose))))
        (when (buffer-live-p compose)
          (kill-buffer compose))
        (when (buffer-live-p source)
          (kill-buffer source))))))

(ert-deftest chirp-compose-draft-preserves-long-form-text ()
  "Structured long-form drafts should reach the backend without truncation."
  (let ((body (make-string 281 ?x)))
    (pcase-let ((`(,compose . ,source)
                 (chirp-test--make-compose-buffer body)))
      (unwind-protect
          (with-current-buffer compose
            (let* ((draft (chirp-compose--draft))
                   (item (car (plist-get draft :items))))
              (should (eq (plist-get draft :kind) 'post))
              (should (equal (plist-get item :text) body))
              (should-not (plist-get item :attachments))))
        (when (buffer-live-p compose)
          (kill-buffer compose))
        (when (buffer-live-p source)
          (kill-buffer source))))))

(ert-deftest chirp-compose-send-cleans-temp-files-after-success ()
  "Temporary attachments should stay alive until the async send finishes."
  (pcase-let ((`(,compose . ,source)
               (chirp-test--make-compose-buffer "photo post")))
    (let* ((temp-file (make-temp-file "chirp-compose-test-" nil ".png"))
           success-callback)
      (with-temp-file temp-file
        (insert "png"))
      (unwind-protect
          (progn
            (with-current-buffer compose
              (setq-local chirp-compose-items
                          (list (list :attachments (list temp-file))))
              (setq-local chirp-compose-temp-attachments (list temp-file)))
            (cl-letf (((symbol-function 'chirp-backend-compose)
                       (lambda (&rest draft)
                         (setq success-callback
                               (plist-get draft :callback)))))
              (with-current-buffer compose
                (chirp-compose-send)))
            (should (functionp success-callback))
            (should (file-exists-p temp-file))
            (funcall success-callback nil nil)
            (should-not (file-exists-p temp-file)))
        (when (buffer-live-p compose)
          (kill-buffer compose))
        (when (buffer-live-p source)
          (kill-buffer source))
        (when (file-exists-p temp-file)
          (delete-file temp-file))))))

(ert-deftest chirp-compose-send-keeps-draft-after-failure ()
  "A known send failure should keep the draft and its temporary attachments."
  (pcase-let ((`(,compose . ,source)
               (chirp-test--make-compose-buffer "failed photo post")))
    (let* ((temp-file (make-temp-file "chirp-compose-test-" nil ".png"))
           error-callback
           reported)
      (unwind-protect
          (progn
            (with-current-buffer compose
              (setq-local chirp-compose-items
                          (list (list :attachments (list temp-file))))
              (setq-local chirp-compose-temp-attachments (list temp-file)))
            (cl-letf (((symbol-function 'chirp-backend-compose)
                       (lambda (&rest draft)
                         (setq error-callback (plist-get draft :errback))))
                      ((symbol-function 'chirp-actions--show-error)
                       (lambda (message)
                         (setq reported message))))
              (with-current-buffer compose
                (chirp-compose-send)
                (should (appkit-compose-submitting-p))
                (funcall error-callback "upload failed")
                (should (buffer-live-p compose))
                (should-not (appkit-compose-submitting-p))
                (should-not chirp-compose-unknown-outcome)
                (should-not buffer-read-only)
                (should (member temp-file chirp-compose-temp-attachments))
                (should (file-exists-p temp-file))
                (should (equal reported "upload failed")))))
        (when (buffer-live-p compose)
          (kill-buffer compose))
        (when (buffer-live-p source)
          (kill-buffer source))
        (when (file-exists-p temp-file)
          (delete-file temp-file))))))

(ert-deftest chirp-compose-stop-during-upload-keeps-draft ()
  "Stopping during media processing should keep the draft and warn."
  (pcase-let ((`(,compose . ,source)
               (chirp-test--make-compose-buffer "stopped photo post")))
    (let ((temp-file (make-temp-file "chirp-compose-test-" nil ".png"))
          (request-count 0)
          reported)
      (unwind-protect
          (progn
            (write-region "png" nil temp-file nil 'silent)
            (with-current-buffer compose
              (setq-local chirp-compose-items
                          (list (list :attachments (list temp-file))))
              (setq-local chirp-compose-temp-attachments (list temp-file)))
            (cl-letf (((symbol-function 'chirp-x--request)
                       (lambda (_url _method callback &rest _options)
                         (funcall
                          callback
                          (pcase (cl-incf request-count)
                            (1 '(("media_id_string" . "7")))
                            (2 (make-hash-table :test #'equal))
                            (_ '(("processing_info" .
                                  (("state" . "pending")
                                   ("check_after_secs" . 30)))))))))
                      ((symbol-function 'chirp-actions--show-error)
                       (lambda (message)
                         (setq reported message))))
              (with-current-buffer compose
                (chirp-compose-send))
              (should (file-exists-p temp-file))
              (chirp-stop)
              (should (buffer-live-p compose))
              (with-current-buffer compose
                (should-not (appkit-compose-submitting-p))
                (should chirp-compose-unknown-outcome)
                (should (member temp-file chirp-compose-temp-attachments)))
              (should (file-exists-p temp-file))
              (should (string-prefix-p "X write outcome is unknown" reported))))
        (chirp-stop)
        (when (buffer-live-p compose)
          (kill-buffer compose))
        (when (buffer-live-p source)
          (kill-buffer source))
        (when (file-exists-p temp-file)
          (delete-file temp-file))))))

(ert-deftest chirp-compose-send-confirms-after-unknown-outcome ()
  "A later send after an unknown outcome should require confirmation."
  (pcase-let ((`(,compose . ,source)
               (chirp-test--make-compose-buffer "maybe posted")))
    (let ((request-count 0)
          confirmed)
      (unwind-protect
          (cl-letf (((symbol-function 'chirp-backend-compose)
                     (lambda (&rest _draft)
                       (setq request-count (1+ request-count))))
                    ((symbol-function 'yes-or-no-p)
                     (lambda (_prompt)
                       (setq confirmed t)
                       nil)))
            (with-current-buffer compose
              (setq-local chirp-compose-unknown-outcome t)
              (should-error (chirp-compose-send) :type 'user-error)
              (should confirmed)
              (should (= request-count 0))
              (setq confirmed nil)
              (cl-letf (((symbol-function 'yes-or-no-p)
                         (lambda (_prompt)
                           (setq confirmed t)
                           t)))
                (chirp-compose-send)
                (should confirmed)
                (should (= request-count 1))
                (should-not chirp-compose-unknown-outcome))))
        (when (buffer-live-p compose)
          (kill-buffer compose))
        (when (buffer-live-p source)
          (kill-buffer source))))))

(ert-deftest chirp-actions-warns-when-a-write-outcome-is-unknown ()
  "Ambiguous write failures should remain visible and discourage retrying."
  (let (warning)
    (cl-letf (((symbol-function 'display-warning)
               (lambda (type message level &rest _args)
                 (setq warning (list type message level)))))
      (chirp-actions--show-error
       (concat "X write outcome is unknown; the request may have succeeded. "
               "Check X before trying again.")))
    (should (eq (car warning) 'chirp))
    (should (eq (nth 2 warning) :warning))
    (should (string-match-p "may have succeeded" (cadr warning)))))

(ert-deftest chirp-compose-send-passes-reply-audience ()
  "Sending a post should forward the selected reply audience."
  (pcase-let ((`(,compose . ,source)
               (chirp-test--make-compose-buffer "audience post")))
    (let (captured-draft)
      (unwind-protect
          (cl-letf (((symbol-function 'chirp-backend-compose)
                     (lambda (&rest draft)
                       (setq captured-draft draft))))
            (with-current-buffer compose
              (chirp-compose-set-reply-audience 'byinvitation)
              (chirp-compose-send))
            (should (eq (plist-get captured-draft :reply-audience)
                        'byinvitation)))
        (when (buffer-live-p compose)
          (kill-buffer compose))
        (when (buffer-live-p source)
          (kill-buffer source))))))

(ert-deftest chirp-compose-upload-progress-updates-status ()
  "Upload progress events should appear in the compose status strip."
  (pcase-let ((`(,compose . ,source)
               (chirp-test--make-compose-buffer "video post")))
    (unwind-protect
        (with-current-buffer compose
          (chirp-compose--begin-submit "Sending post...")
          (chirp-compose--upload-progress
           compose
           (list :phase 'append
                 :media-type "video/mp4"
                 :index 2
                 :count 4
                 :progress 0.25))
          (should (string-match-p "Uploading video 2/4"
                                  (appkit-compose-display-string)))
          (should (string-match-p "25%" (appkit-compose-display-string)))
          (appkit-compose-finish-submit)
          (setq-local buffer-read-only nil))
      (when (buffer-live-p compose)
        (kill-buffer compose))
      (when (buffer-live-p source)
        (kill-buffer source)))))

(ert-deftest chirp-compose-cancel-requests-in-flight-submit ()
  "Cancel should abort an in-flight submit and keep the draft."
  (pcase-let ((`(,compose . ,source)
               (chirp-test--make-compose-buffer "in flight")))
    (unwind-protect
        (with-current-buffer compose
          (cl-letf (((symbol-function 'chirp-backend-compose)
                     (lambda (&rest _args) 'pending)))
            (chirp-compose-send)
            (should (appkit-compose-submitting-p))
            (chirp-compose-cancel)
            (should (buffer-live-p compose))
            (should-not (appkit-compose-submitting-p))
            (should-not buffer-read-only)))
      (when (buffer-live-p compose)
        (kill-buffer compose))
      (when (buffer-live-p source)
        (kill-buffer source)))))

(ert-deftest chirp-compose-send-rejects-duplicate-submit ()
  "Sending should reject a second submit while a draft is marked in flight."
  (pcase-let ((`(,compose . ,source)
               (chirp-test--make-compose-buffer "duplicate")))
    (let ((request-count 0))
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'chirp-backend-compose)
                       (lambda (&rest _args)
                         (setq request-count (1+ request-count)))))
              (with-current-buffer compose
                (chirp-compose-send)
                (should-error (chirp-compose-send) :type 'user-error)))
            (should (= request-count 1)))
        (when (buffer-live-p compose)
          (kill-buffer compose))
        (when (buffer-live-p source)
          (kill-buffer source))))))

(ert-deftest chirp-compose-paste-image-uses-chirp-temp-directory ()
  "Clipboard pastes should create temporary files under Chirp's own cache tree."
  (pcase-let ((`(,compose . ,source)
               (chirp-test--make-compose-buffer "")))
    (let* ((temp-root (make-temp-file "chirp-compose-dir-" t))
           (chirp-compose-temporary-directory temp-root)
           pasted-path)
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'chirp-compose--clipboard-image-backend)
                       (lambda ()
                         '(:kind stdout :program "fake" :args nil :extension ".png")))
                      ((symbol-function 'chirp-compose--paste-image-to-file)
                       (lambda (_backend file)
                         (setq pasted-path file)
                         (with-temp-file file
                           (insert "png"))
                         t)))
              (with-current-buffer compose
                (chirp-compose-paste-image)
                (should (string-prefix-p (file-name-as-directory temp-root)
                                         pasted-path))
                (should (member pasted-path
                                (chirp-compose--attachment-paths)))
                (should (member pasted-path chirp-compose-temp-attachments)))))
        (when (buffer-live-p compose)
          (kill-buffer compose))
        (when (buffer-live-p source)
          (kill-buffer source))
        (when (file-directory-p temp-root)
          (delete-directory temp-root t))))))

(ert-deftest chirp-compose-empty-body-allows-insert-and-delete ()
  "An empty compose buffer should keep the body editable."
  (let ((buffer (generate-new-buffer " *chirp-compose-empty*")))
    (unwind-protect
        (with-current-buffer buffer
          (chirp-compose-mode)
          (setq-local chirp-compose-kind 'post)
          (setq-local chirp-compose-items (list (list :attachments nil)))
          (setq-local chirp-compose-temp-attachments nil)
          (appkit-compose-setup
           :context-function #'chirp-compose--header-string
           :status-fields-function #'chirp-compose--status-fields
           :parts-function #'chirp-compose--parts)
          (let ((display (appkit-compose-display-string)))
            (should-not (string-match-p "Compose a new post" display))
            (should-not (string-match-p "No images attached" display))
            (should-not (string-match-p "C-c C-a attach" display))
            (should-not (string-match-p "Posts:" display)))
          (goto-char (appkit-compose-body-start-position))
          (insert "abc")
          (should (equal (appkit-compose-body) "abc"))
          (delete-char -1)
          (should (equal (appkit-compose-body) "ab")))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest chirp-compose-q-self-inserts ()
  "Compose buffers should leave `q' available for normal text insertion."
  (let ((buffer (generate-new-buffer " *chirp-compose-q*")))
    (unwind-protect
        (with-current-buffer buffer
          (chirp-compose-mode)
          (should (eq (key-binding (kbd "q")) #'self-insert-command)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest chirp-compose-refresh-keeps-body-editable-at-footer-boundary ()
  "Refreshing compose chrome should not make the body read-only at the end."
  (pcase-let ((`(,compose . ,source)
               (chirp-test--make-compose-buffer "hello")))
    (unwind-protect
        (with-current-buffer compose
          (appkit-compose-refresh)
          (goto-char (appkit-compose-body-end-position))
          (insert "!")
          (should (equal (appkit-compose-body) "hello!"))
          (delete-char -1)
          (should (equal (appkit-compose-body) "hello")))
      (when (buffer-live-p compose)
        (kill-buffer compose))
      (when (buffer-live-p source)
        (kill-buffer source)))))

(ert-deftest chirp-compose-open-uses-displayed-source-buffer-for-post-reply-and-quote ()
  "Compose buffers should capture the displayed Chirp view as their source."
  (dolist (case `((post nil)
                  (reply ,(list :kind 'tweet :id "123" :author-handle "alice"))
                  (quote ,(list :kind 'tweet :id "123" :author-handle "alice"))))
    (pcase-let* ((`(,kind ,tweet) case)
                 (`(,compose ,source ,foreign)
                  (chirp-test--open-compose-from-foreign-current-buffer kind tweet)))
      (unwind-protect
          (with-current-buffer compose
            (should (eq chirp-compose-source-buffer source)))
        (dolist (buffer (list compose source foreign))
          (when (buffer-live-p buffer)
            (kill-buffer buffer)))))))

(ert-deftest chirp-compose-source-buffer-prefers-minibuffer-origin-view ()
  "Source buffer lookup should prefer the minibuffer-origin Chirp view."
  (let ((thread (generate-new-buffer " *chirp-thread-source*"))
        (home (generate-new-buffer " *chirp-home-source*"))
        (foreign (generate-new-buffer " *chirp-transient*")))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (with-current-buffer home
            (chirp-view-mode)
            (setq-local chirp--view-title "For You"))
          (with-current-buffer thread
            (chirp-view-mode)
            (setq-local chirp--view-title "Thread"))
          (switch-to-buffer home)
          (let ((origin-window (split-window)))
            (set-window-buffer origin-window thread)
            (with-current-buffer foreign
              (cl-letf (((symbol-function 'active-minibuffer-window)
                         (lambda ()
                           t))
                        ((symbol-function 'minibuffer-selected-window)
                         (lambda ()
                           origin-window)))
                (should (eq (chirp-compose--source-buffer) thread))))))
      (dolist (buffer (list thread home foreign))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest chirp-quote-send-closes-buffer-and-restores-source-view ()
  "Quote sending should close the compose buffer and return to the source view."
  (pcase-let* ((`(,compose ,source ,foreign)
                (chirp-test--open-compose-from-foreign-current-buffer
                 'quote
                 (list :kind 'tweet
                       :id "123"
                       :author-handle "alice"
                       :url "https://x.com/alice/status/123"))))
    (let (captured-draft)
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'chirp-backend-compose)
                       (lambda (&rest draft)
                         (setq captured-draft draft))))
              (with-current-buffer compose
                (goto-char (appkit-compose-body-start-position))
                (insert "hello quote")
                (chirp-compose-send)))
            (should (buffer-live-p compose))
            (should (eq (plist-get captured-draft :kind) 'quote))
            (should (equal (plist-get captured-draft :target-id) "123"))
            (should (equal (plist-get captured-draft :text) "hello quote"))
            (should (eq (plist-get captured-draft :reply-audience) 'everyone))
            (funcall (plist-get captured-draft :callback) nil nil)
            (should-not (buffer-live-p compose))
            (should (eq (window-buffer (selected-window)) source)))
        (dolist (buffer (list compose source foreign))
          (when (buffer-live-p buffer)
            (kill-buffer buffer)))))))

(ert-deftest chirp-compose-send-deletes-compose-window-when-source-is-still-visible ()
  "Sending should remove the extra compose window instead of duplicating the source view."
  (let ((source (generate-new-buffer " *chirp-source-window*"))
        (compose (generate-new-buffer " *chirp-compose-window*")))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (let ((source-window (selected-window))
                (compose-window (split-window-below)))
            (with-current-buffer source
              (chirp-view-mode)
              (setq-local chirp--view-title "For You"))
            (set-window-buffer source-window source)
            (set-window-buffer compose-window compose)
            (select-window compose-window)
            (with-current-buffer compose
              (chirp-compose-mode)
              (setq-local chirp-compose-kind 'post)
              (setq-local chirp-compose-source-buffer source)
              (setq-local chirp-compose-items (list (list :attachments nil)))
              (setq-local chirp-compose-temp-attachments nil)
              (setq-local chirp-compose-unknown-outcome nil)
              (setq-local chirp-compose-reply-audience 'everyone)
              (erase-buffer)
              (insert "hello world")
              (appkit-compose-setup
               :context-function #'chirp-compose--header-string
               :status-fields-function #'chirp-compose--status-fields
               :parts-function #'chirp-compose--parts))
            (let (success-callback)
              (cl-letf (((symbol-function 'chirp-backend-compose)
                         (lambda (&rest draft)
                           (setq success-callback
                                 (plist-get draft :callback)))))
                (with-current-buffer compose
                  (chirp-compose-send)))
              (should (buffer-live-p compose))
              (funcall success-callback nil nil))
            (should-not (buffer-live-p compose))
            (should (= (length (window-list nil 'no-minibuf)) 1))
            (should (eq (window-buffer (selected-window)) source))))
      (when (buffer-live-p compose)
        (kill-buffer compose))
      (when (buffer-live-p source)
        (kill-buffer source)))))

(defun chirp-test--with-tweet-buffer (tweet fn)
  "Create a temporary Chirp view buffer with TWEET and call FN inside it."
  (let ((buffer (generate-new-buffer " *chirp-action-tweet*")))
    (unwind-protect
        (with-current-buffer buffer
          (chirp-view-mode)
          (let ((inhibit-read-only t))
            (insert "tweet\n")
            (add-text-properties
             (point-min) (point-max)
             `(chirp-entry-item ,tweet))
            (put-text-property (point-min) (1+ (point-min)) 'chirp-entry-start t)
            (goto-char (point-min)))
          (funcall fn buffer))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(defun chirp-test--with-user-buffer (user fn)
  "Create a temporary Chirp view buffer with USER and call FN inside it."
  (let ((buffer (generate-new-buffer " *chirp-action-user*")))
    (unwind-protect
        (with-current-buffer buffer
          (chirp-view-mode)
          (let ((inhibit-read-only t))
            (insert "user\n")
            (add-text-properties
             (point-min) (point-max)
             `(chirp-entry-item ,user))
            (put-text-property (point-min) (1+ (point-min)) 'chirp-entry-start t)
            (goto-char (point-min)))
          (funcall fn buffer))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest chirp-reply-at-point-rejects-limited-conversations ()
  "Reply should refuse tweets that X marks as reply-limited."
  (chirp-test--with-tweet-buffer
   '(:kind tweet :id "123" :author-handle "alice" :reply-limited-p t)
   (lambda (_buffer)
     (let ((err (should-error (chirp-reply-at-point) :type 'user-error)))
       (should (string-match-p "cannot reply" (error-message-string err)))))))

(ert-deftest chirp-reply-at-point-opens-compose-for-allowed-tweets ()
  "Reply should open a compose draft when the conversation allows it."
  (let (compose)
    (unwind-protect
        (chirp-test--with-tweet-buffer
         '(:kind tweet :id "123" :author-handle "alice")
         (lambda (_buffer)
           (chirp-reply-at-point)
           (setq compose (current-buffer))
           (should (derived-mode-p 'chirp-compose-mode))
           (should (eq chirp-compose-kind 'reply))
           (should (equal chirp-compose-target-id "123"))
           (should-not chirp-compose-reply-audience)))
      (when (buffer-live-p compose)
        (kill-buffer compose)))))

(ert-deftest chirp-actions-mouse-action-targets-clicked-tweet ()
  "Mouse tweet metrics should dispatch their action for the clicked tweet."
  (let ((buffer (generate-new-buffer " *chirp-mouse-action-test*"))
        (first-tweet '(:kind tweet
                       :id "first"
                       :text "First tweet"
                       :author-name "Alice"
                       :author-handle "alice"
                       :reply-count 1
                       :retweet-count 2
                       :like-count 3
                       :quote-count 4
                       :bookmark-count 5
                       :view-count 6))
        (clicked-tweet '(:kind tweet
                         :id "clicked"
                         :text "Clicked tweet"
                         :author-name "Bob"
                         :author-handle "bob"
                         :reply-count 7
                         :retweet-count 8
                         :like-count 9
                         :quote-count 10
                         :bookmark-count 11
                         :view-count 12))
        dispatched
        event-position)
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (set-window-buffer (selected-window) buffer)
          (with-current-buffer buffer
            (chirp-view-mode)
            (cl-letf (((symbol-function 'chirp-media-avatar-image)
                       (lambda (&rest _args) nil)))
              (let ((inhibit-read-only t))
                (chirp-render-insert-tweet first-tweet)
                (chirp-render-insert-tweet clicked-tweet)))
            (let (positions)
              (dolist (spec
                       `((reply . ,#'chirp-reply-at-point)
                         (retweet . ,#'chirp-toggle-retweet-at-point)
                         (like . ,#'chirp-toggle-like-at-point)
                         (quote . ,#'chirp-quote-at-point)
                         (bookmark . ,#'chirp-toggle-bookmark-at-point)))
                (goto-char (point-min))
                (let (position)
                  (while (< (point) (point-max))
                    (when (eq (get-text-property
                               (point) appkit-ui-action-property)
                              (cdr spec))
                      (setq position (point)))
                    (goto-char
                     (or (next-single-property-change
                          (point) appkit-ui-action-property nil (point-max))
                         (point-max))))
                  (push (cons (car spec) position) positions)))
              (cl-letf (((symbol-function 'event-start)
                         (lambda (_event) event-position))
                        ((symbol-function 'chirp-reply-at-point)
                         (lambda ()
                           (setq dispatched
                                 (list 'reply (chirp-entry-id-at-point)))))
                        ((symbol-function 'chirp-toggle-retweet-at-point)
                         (lambda ()
                           (setq dispatched
                                 (list 'retweet (chirp-entry-id-at-point)))))
                        ((symbol-function 'chirp-toggle-like-at-point)
                         (lambda ()
                           (setq dispatched
                                 (list 'like (chirp-entry-id-at-point)))))
                        ((symbol-function 'chirp-quote-at-point)
                         (lambda ()
                           (setq dispatched
                                 (list 'quote (chirp-entry-id-at-point)))))
                        ((symbol-function 'chirp-toggle-bookmark-at-point)
                         (lambda ()
                           (setq dispatched
                                 (list 'bookmark (chirp-entry-id-at-point))))))
                (dolist (action '(reply retweet like quote bookmark))
                  (setq event-position
                        (list (selected-window)
                              nil
                              (cons 0 0)
                              nil
                              nil
                              (cdr (assq action positions))
                              nil
                              nil
                              nil
                              nil))
                  (let* ((position (posn-point event-position))
                         (keymap (get-text-property position 'keymap)))
                    (should (eq (lookup-key keymap [mouse-1])
                                #'appkit-ui-activate))
                    (funcall (lookup-key keymap [mouse-1]) '(mouse-1)))
                  (should (equal dispatched (list action "clicked"))))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest chirp-toggle-like-at-point-uses-like-and-unlike-commands ()
  "Toggle like should choose the backend command from the current local state."
  (clrhash (chirp--tweet-state-table))
  (let (captured-args rerendered)
    (unwind-protect
        (progn
          (chirp-test--with-tweet-buffer
           '(:kind tweet :id "123" :liked-p nil :like-count 10)
           (lambda (_buffer)
             (cl-letf (((symbol-function 'chirp-actions--perform)
                        (lambda (args on-success &optional _on-error)
                          (setq captured-args args)
                          (funcall on-success nil nil)))
                       ((symbol-function 'chirp-request-rerender)
                        (lambda (&optional _buffer _delay)
                          (setq rerendered t))))
               (chirp-toggle-like-at-point)
               (should (equal captured-args '("like" "123")))
               (should rerendered)
               (should (plist-get (chirp-entry-at-point) :liked-p))
               (should (= (plist-get (chirp-entry-at-point) :like-count) 11)))))
          (setq captured-args nil
                rerendered nil)
          (chirp-test--with-tweet-buffer
           '(:kind tweet :id "123" :liked-p t :like-count 10)
           (lambda (_buffer)
             (cl-letf (((symbol-function 'chirp-actions--perform)
                        (lambda (args on-success &optional _on-error)
                          (setq captured-args args)
                          (funcall on-success nil nil)))
                       ((symbol-function 'chirp-request-rerender)
                        (lambda (&optional _buffer _delay)
                          (setq rerendered t))))
               (chirp-toggle-like-at-point)
               (should (equal captured-args '("unlike" "123")))
               (should rerendered)
               (should-not (plist-get (chirp-entry-at-point) :liked-p))
               (should (= (plist-get (chirp-entry-at-point) :like-count) 9))))))
      (clrhash (chirp--tweet-state-table)))))

(ert-deftest chirp-delete-at-point-removes-primary-canonical-state ()
  "Successful deletion should remove primary state without a feed refresh."
  (let (captured-args removed refreshed)
    (chirp-test--with-tweet-buffer
     '(:kind tweet :id "123")
     (lambda (buffer)
       (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                 ((symbol-function 'chirp-actions--perform)
                  (lambda (args on-success &optional _on-error)
                    (setq captured-args args)
                    (funcall on-success nil nil)))
                 ((symbol-function 'chirp-clear-tweet-state-overrides) #'ignore)
                 ((symbol-function 'chirp--remove-tweet-from-primary-feeds)
                  (lambda (target tweet-id)
                    (should (eq target buffer))
                    (should (equal tweet-id "123"))
                    (setq removed t)))
                 ((symbol-function 'chirp-actions--refresh-buffer)
                  (lambda (_target)
                    (setq refreshed t))))
         (chirp-delete-at-point)
         (should (equal captured-args '("delete" "--yes" "123")))
         (should removed)
         (should-not refreshed))))))

(ert-deftest chirp-follow-and-unfollow-user-at-point-use-explicit-commands ()
  "User follow actions should dispatch follow and unfollow commands."
  (let (captured-args refreshed)
    (chirp-test--with-user-buffer
     '(:kind user :handle "alice")
     (lambda (_buffer)
       (cl-letf (((symbol-function 'chirp-actions--perform)
                  (lambda (args on-success &optional _on-error)
                    (setq captured-args args)
                    (funcall on-success nil nil)))
                 ((symbol-function 'chirp-actions--refresh-buffer)
                  (lambda (_target)
                    (setq refreshed t))))
         (chirp-follow-user-at-point)
         (should (equal captured-args '("follow" "alice")))
         (should refreshed)
         (setq captured-args nil
               refreshed nil)
         (chirp-unfollow-user-at-point)
         (should (equal captured-args '("unfollow" "alice")))
         (should refreshed))))))

(ert-deftest chirp-follow-success-refreshes-profile-after-point-moves ()
  "Profile refresh should not depend on point remaining on the user summary."
  (let (refreshed)
    (chirp-test--with-tweet-buffer
     '(:kind tweet :id "123")
     (lambda (buffer)
       (with-current-buffer buffer
         (setq-local chirp--profile-handle "alice"))
       (cl-letf (((symbol-function 'chirp-actions--refresh-buffer)
                  (lambda (target)
                    (should (eq target buffer))
                    (setq refreshed t))))
         (chirp-actions--refresh-user-buffer-if-needed buffer))))
    (should refreshed)))

(ert-deftest chirp-translate-at-point-caches-and-renders-result ()
  "Translation should be stored on the current tweet and trigger a rerender."
  (clrhash (chirp--tweet-state-table))
  (let ((chirp-translation-language "zh")
        rerendered)
    (unwind-protect
        (chirp-test--with-tweet-buffer
         '(:kind tweet :id "123" :translation nil :translation-language nil)
         (lambda (_buffer)
           (cl-letf (((symbol-function 'chirp-backend-translate)
                      (lambda (tweet-id language callback &optional _errback)
                        (should (equal tweet-id "123"))
                        (should (equal language "zh"))
                        (funcall callback
                                 '(("translation" . "你好")
                                   ("destinationLanguage" . "zh"))
                                 nil)))
                     ((symbol-function 'chirp-request-rerender)
                      (lambda (&optional _buffer _delay)
                        (setq rerendered t))))
             (chirp-translate-at-point)
             (should rerendered)
             (should (equal (plist-get (chirp-entry-at-point) :translation)
                            "你好"))
             (should (equal
                      (plist-get (gethash "123" (chirp--tweet-state-table))
                                 :translation-language)
                      "zh")))))
      (clrhash (chirp--tweet-state-table)))))

(ert-deftest chirp-dispatch-uses-toggle-actions-for-stateful-tweet-actions ()
  "The Chirp transient should expose only the intended action bindings."
  (dolist (binding
           '(("h" . chirp-timeline-open-home)
             ("f" . chirp-timeline-open-following)
             ("u" . chirp-me)
             ("b" . chirp-timeline-open-bookmarks)
             ("L" . chirp-timeline-open-likes)
             ("s" . chirp-timeline-open-list)
             ("+" . chirp-follow-user-at-point)
             ("-" . chirp-unfollow-user-at-point)
             ("R" . chirp-toggle-retweet-at-point)
             ("l" . chirp-toggle-like-at-point)
             ("B" . chirp-toggle-bookmark-at-point)
             ("T" . chirp-translate-at-point)
             ("y" . chirp-copy-fixupx-url-at-point)))
    (should
     (equal (transient-get-suffix 'chirp-dispatch (car binding))
            (transient-get-suffix 'chirp-dispatch (cdr binding)))))
  (should-error (transient-get-suffix 'chirp-dispatch "U")))

(ert-deftest chirp-copy-fixupx-url-at-point-copies-rewritten-url ()
  "Copy action should rewrite tweet URLs from x.com to fixupx.com."
  (let (captured-url last-message)
    (chirp-test--with-tweet-buffer
     '(:kind tweet
       :id "123"
       :author-handle "alice"
       :url "https://x.com/alice/status/123")
     (lambda (_buffer)
       (cl-letf (((symbol-function 'kill-new)
                  (lambda (text &optional _replace)
                    (setq captured-url text)))
                 ((symbol-function 'message)
                  (lambda (format-string &rest args)
                    (setq last-message (apply #'format format-string args)))))
         (chirp-copy-fixupx-url-at-point)
         (should (equal captured-url "https://fixupx.com/alice/status/123"))
         (should (equal last-message
                        "Copied https://fixupx.com/alice/status/123")))))))

(provide 'chirp-actions-test)

;;; chirp-actions-test.el ends here
