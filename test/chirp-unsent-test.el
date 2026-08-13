;;; chirp-unsent-test.el --- Tests for unsent draft lists -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'chirp-backend)
(require 'chirp-unsent)
(require 'chirp-actions)

(unless (fboundp 'chirp-test--make-compose-buffer)
  (load (expand-file-name "chirp-actions-test.el"
                          (file-name-directory (or load-file-name
                                                   default-directory)))
        nil t))
(declare-function chirp-test--make-compose-buffer
                  "chirp-actions-test" (body))

(defun chirp-unsent-test--payload-at-path (path value)
  "Return a JSON-style payload containing VALUE at PATH."
  (dolist (key (reverse (copy-sequence path)) value)
    (setq value (list (cons key value)))))

(defun chirp-unsent-test--draft-payload
    (status &optional thread-status media-id preview-url)
  "Return a FetchDraftTweets payload with STATUS and optional THREAD-STATUS.

When MEDIA-ID is set, include that media ID and optional PREVIEW-URL."
  (let* ((thread (if thread-status
                     (vector `(("status" . ,thread-status)
                               ("media_ids" . [])))
                   []))
         (media-ids (if media-id (vector media-id) []))
         (entities
          (if media-id
              (vector
               `(("media_key" . ,(format "3_%s" media-id))
                 ("media_info" .
                  (("__typename" . "ApiImage")
                   ("original_img_url"
                    . ,(or preview-url
                           "https://pbs.twimg.com/media/x.jpg"))
                   ("original_img_width" . 1200)
                   ("original_img_height" . 800)))))
            [])))
    (chirp-unsent-test--payload-at-path
     '("data" "viewer" "draft_list" "response_data")
     (vector
      `(("rest_id" . "2087")
        ("media_entities" . ,entities)
        ("tweet_create_request" .
         (("status" . ,status)
          ("media_ids" . ,media-ids)
          ("thread_tweets" . ,thread))))))))

(defun chirp-unsent-test--scheduled-payload (status execute-at)
  "Return a FetchScheduledTweets payload with STATUS and EXECUTE-AT."
  (chirp-unsent-test--payload-at-path
   '("data" "viewer" "scheduled_tweet_list")
   (vector
    `(("rest_id" . "2090")
      ("scheduling_info" .
       (("execute_at" . ,execute-at)
        ("state" . "Scheduled")))
      ("tweet_create_request" .
       (("status" . ,status)
        ("media_ids" . [])))))))

(ert-deftest chirp-backend-fetch-unsent-normalizes-a-thread-draft ()
  "FetchDraftTweets should expose root text, thread texts, and IDs."
  (let (entries)
    (cl-letf (((symbol-function 'chirp-x-graphql-request)
               (lambda (operation variables callback &rest _options)
                 (should (equal (plist-get operation :name) "FetchDraftTweets"))
                 (should (eq (chirp-get variables "ascending") :json-false))
                 (funcall callback
                          (chirp-unsent-test--draft-payload
                           "Nice" "Two Nices")))))
      (chirp-backend-fetch-unsent
       'draft
       (lambda (result _envelope)
         (setq entries result))))
    (should (= (length entries) 1))
    (should (equal (plist-get (car entries) :id) "2087"))
    (should (eq (plist-get (car entries) :kind) 'draft))
    (should (eq (plist-get (car entries) :compose-kind) 'post))
    (should (equal (plist-get (car entries) :texts)
                   '("Nice" "Two Nices")))))

(ert-deftest chirp-backend-fetch-unsent-keeps-media-ids-and-previews ()
  "Draft media_entities should become reusable compose attachments."
  (let (entries)
    (cl-letf (((symbol-function 'chirp-x-graphql-request)
               (lambda (_operation _variables callback &rest _options)
                 (funcall callback
                          (chirp-unsent-test--draft-payload
                           "Nice" nil "2087634413287100416"
                           "https://pbs.twimg.com/media/HPjEcCjb0AA6uAy.jpg")))))
      (chirp-backend-fetch-unsent
       'draft
       (lambda (result _envelope)
         (setq entries result))))
    (let* ((item (car (plist-get (car entries) :items)))
           (attachment (car (plist-get item :attachments))))
      (should (equal (plist-get (car entries) :media-count) 1))
      (should (equal (plist-get attachment :media-id) "2087634413287100416"))
      (should (equal (plist-get attachment :preview-url)
                     "https://pbs.twimg.com/media/HPjEcCjb0AA6uAy.jpg")))))

(ert-deftest chirp-backend-fetch-unsent-normalizes-a-scheduled-post ()
  "FetchScheduledTweets should keep execute_at and status."
  (let (entries)
    (cl-letf (((symbol-function 'chirp-x-graphql-request)
               (lambda (operation variables callback &rest _options)
                 (should (equal (plist-get operation :name)
                                "FetchScheduledTweets"))
                 (should (eq (chirp-get variables "ascending") t))
                 (funcall callback
                          (chirp-unsent-test--scheduled-payload
                           "later" 1786700000000)))))
      (chirp-backend-fetch-unsent
       'scheduled
       (lambda (result _envelope)
         (setq entries result))))
    (should (equal (plist-get (car entries) :id) "2090"))
    (should (eq (plist-get (car entries) :kind) 'scheduled))
    (should (equal (plist-get (car entries) :execute-at) 1786700000))
    (should (equal (plist-get (car entries) :texts) '("later")))))

(ert-deftest chirp-backend-unix-seconds-accepts-milliseconds ()
  "Scheduled execute_at values from X are milliseconds."
  (should (equal (chirp-backend--unix-seconds 1786572521000) 1786572521))
  (should (equal (chirp-backend--unix-seconds 1786572521) 1786572521)))

(ert-deftest chirp-backend-delete-unsent-uses-kind-specific-ids ()
  "Deletes should send draft_tweet_id or scheduled_tweet_id."
  (let (operation variables)
    (cl-letf (((symbol-function 'chirp-x-graphql-request)
               (lambda (request-operation request-variables callback
                        &rest _options)
                 (setq operation request-operation
                       variables request-variables)
                 (funcall callback '(("data" . nil))))))
      (chirp-backend-delete-unsent 'draft "2087" #'ignore)
      (should (equal (plist-get operation :name) "DeleteDraftTweet"))
      (should (equal (chirp-get variables "draft_tweet_id") "2087"))
      (chirp-backend-delete-unsent 'scheduled "2090" #'ignore)
      (should (equal (plist-get operation :name) "DeleteScheduledTweet"))
      (should (equal (chirp-get variables "scheduled_tweet_id") "2090")))))

(ert-deftest chirp-unsent-rows-use-buffer-menu-mark-column ()
  "The unsent list should keep a Buffer Menu style mark column."
  (let ((buffer (generate-new-buffer " *chirp-unsent-test*")))
    (unwind-protect
        (with-current-buffer buffer
          (chirp-unsent-mode)
          (setq-local chirp-unsent-kind 'draft)
          (chirp-unsent--apply-entries
           (list (list :id "2087"
                       :kind 'draft
                       :texts '("Nice" "Two Nices")
                       :media-count 0)))
          (goto-char (point-min))
          (should (equal (tabulated-list-get-id) "2087"))
          (chirp-unsent-flag-delete)
          (goto-char (point-min))
          (should (equal (chirp-unsent--flagged-ids ?D) '("2087")))
          (chirp-unsent-unmark-all)
          (should-not (chirp-unsent--flagged-ids ?D)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest chirp-compose-open-unsent-restores-thread-text ()
  "Opening an unsent draft should restore every part and the draft ID."
  (pcase-let ((`(,compose . ,source)
               (chirp-test--make-compose-buffer "ignore")))
    (unwind-protect
        (with-current-buffer compose
          (chirp-compose--apply-unsent
           (list :id "2087"
                 :kind 'draft
                 :compose-kind 'post
                 :texts '("Nice" "Two Nices")
                 :media-count 0))
          (should (equal chirp-compose-draft-id "2087"))
          (should (= (length chirp-compose-items) 2))
          (should (equal (appkit-compose-bodies) '("Nice" "Two Nices"))))
      (when (buffer-live-p compose)
        (kill-buffer compose))
      (when (buffer-live-p source)
        (kill-buffer source)))))

(ert-deftest chirp-compose-open-unsent-restores-media-ids ()
  "Opening an unsent draft should keep X media IDs for the next submit."
  (pcase-let ((`(,compose . ,source)
               (chirp-test--make-compose-buffer "ignore")))
    (unwind-protect
        (cl-letf (((symbol-function 'chirp-media-prefetch-file)
                   (lambda (&rest _args) nil)))
          (with-current-buffer compose
            (chirp-compose--apply-unsent
             (list :id "2087"
                   :kind 'draft
                   :compose-kind 'post
                   :items
                   (list (list :text "Nice"
                               :attachments
                               (list (list :media-id "2087634413287100416"
                                           :preview-url
                                           "https://pbs.twimg.com/media/x.jpg"))))))
            (should (equal (plist-get
                            (car (chirp-compose--item-attachments))
                            :media-id)
                           "2087634413287100416"))
            (let ((draft (chirp-compose--draft)))
              (should (equal (plist-get
                              (car (plist-get (car (plist-get draft :items))
                                              :attachments))
                              :media-id)
                             "2087634413287100416")))))
      (when (buffer-live-p compose)
        (kill-buffer compose))
      (when (buffer-live-p source)
        (kill-buffer source)))))

(provide 'chirp-unsent-test)

;;; chirp-unsent-test.el ends here
