;;; chirp-edit-history-test.el --- Tests for Chirp edit history -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'chirp-edit-history)

(ert-deftest chirp-edit-history-rows-label-latest-and-version-history ()
  "Edit-history rows should label the two X timeline sections once."
  (let ((rows
         (chirp-edit-history--rows
          '((:kind tweet :id "200" :text "Latest")
            (:kind tweet :id "150" :text "Middle")
            (:kind tweet :id "100" :text "Initial")))))
    (should (equal (mapcar (lambda (row) (plist-get row :key)) rows)
                   '((edit-version "200")
                     (edit-version "150")
                     (edit-version "100"))))
    (should (plist-get (car rows) :latest-p))
    (should (equal (plist-get (car rows) :section) "Latest post"))
    (should (equal (plist-get (cadr rows) :section) "Version history"))
    (should-not (plist-get (caddr rows) :section))))

(ert-deftest chirp-edit-history-open-renders-versions-through-public-path ()
  "Opening edit history should create one reusable Appkit projection view."
  (let ((chirp--app nil)
        (tweet
         '(:kind tweet :id "200" :text "Latest"
           :edit-history-ids ("100" "200")
           :edit-history-initial-id "100" :edited-p t))
        buffers)
    (unwind-protect
        (cl-letf (((symbol-function 'chirp-backend-edit-history)
                   (lambda (_tweet-id callback &optional _errback)
                     (funcall
                      callback
                      '((:kind tweet :id "200" :text "Latest"
                         :author-name "Alice" :created-at "LATEST-TIME"
                         :edited-p t :edit-history-ids ("100" "200"))
                        (:kind tweet :id "100" :text "Initial"
                         :author-name "Alice" :created-at "INITIAL-TIME"
                         :edited-p t :edit-history-ids ("100" "200")))
                      nil)))
                  ((symbol-function 'chirp-media-avatar-image)
                   (lambda (&rest _args) nil))
                  ((symbol-function 'chirp-media-prefetch-tweets) #'ignore))
          (let ((first (chirp-edit-history-open-tweet tweet))
                second)
            (push first buffers)
            (setq second (chirp-edit-history-open "100"))
            (push second buffers)
            (should (eq first second))
            (with-current-buffer first
              (should (derived-mode-p 'chirp-view-mode))
              (should buffer-read-only)
              (should (string-match-p "Latest post" (buffer-string)))
              (should (string-match-p "Latest" (buffer-string)))
              (should (string-match-p "Version history" (buffer-string)))
              (should (string-match-p "Initial" (buffer-string)))
              (should-not (string-match-p "Edited" (buffer-string)))
              (let ((view (appkit-current-view)))
                (should (equal (appkit-view-id view)
                               '(edit-history "100")))))))
      (chirp-stop)
      (dolist (buffer buffers)
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest chirp-edit-history-open-at-point-routes-edited-tweet ()
  "The public point command should route an edited tweet to its view."
  (let ((tweet
         '(:kind tweet :id "200" :text "Latest"
           :edit-history-ids ("100" "200")
           :edit-history-initial-id "100" :edited-p t))
        opened)
    (with-temp-buffer
      (chirp-view-mode)
      (let ((inhibit-read-only t))
        (insert (propertize "Edited"
                            'chirp-entry-item tweet)))
      (goto-char (point-min))
      (cl-letf (((symbol-function 'chirp-edit-history-open-tweet)
                 (lambda (value) (setq opened value))))
        (chirp-edit-history-open-at-point))
      (should (eq opened tweet)))))

(ert-deftest chirp-edit-history-open-at-point-requires-edited-tweet ()
  "The public point command should reject an unedited tweet."
  (with-temp-buffer
    (chirp-view-mode)
    (let ((inhibit-read-only t))
      (insert (propertize "Original"
                          'chirp-entry-item
                          '(:kind tweet :id "300" :text "Original"))))
    (goto-char (point-min))
    (should-error (chirp-edit-history-open-at-point) :type 'user-error)))

(provide 'chirp-edit-history-test)

;;; chirp-edit-history-test.el ends here
