;;; chirp-media-view-test.el --- Tests for Chirp media viewers -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'chirp-media-view)

(ert-deftest chirp-media-view-jumps-to-rendered-local-file ()
  "C-x C-j should select the actual file represented by the viewer."
  (let ((file (make-temp-file "chirp-view-file-"))
        jumped-file)
    (unwind-protect
        (with-temp-buffer
          (chirp-media-view-mode)
          (setq-local chirp--media-file file)
          (cl-letf (((symbol-function 'dired-jump)
                     (lambda (_other-window file-name)
                       (setq jumped-file file-name))))
            (call-interactively
             (lookup-key (current-local-map) (kbd "C-x C-j"))))
          (should (equal jumped-file file)))
      (delete-file file))))

(ert-deftest chirp-media-view-jump-rejects-missing-local-file ()
  "A viewer without a rendered local resource should fail explicitly."
  (with-temp-buffer
    (chirp-media-view-mode)
    (setq-local chirp--media-file "/missing/chirp-media")
    (should-error (chirp-media-jump-to-file) :type 'user-error)))

(ert-deftest chirp-media-image-render-tracks-corresponding-file ()
  "Rendering another image should update the viewer's current local file."
  (let ((first (make-temp-file "chirp-view-first-"))
        (second (make-temp-file "chirp-view-second-"))
        (buffer (generate-new-buffer " *chirp-media-view-test*"))
        (media-list '((:type "photo" :file first)
                      (:type "photo" :file second))))
    (unwind-protect
        (cl-letf (((symbol-function 'chirp-media--photo-file)
                   (lambda (media)
                     (pcase (plist-get media :file)
                       ('first first)
                       ('second second))))
                  ((symbol-function 'display-images-p) (lambda () t))
                  ((symbol-function 'chirp-media-image-mode)
                   (lambda () (special-mode)))
                  ((symbol-function 'chirp-display-buffer) #'ignore))
          (chirp-media-view--render-image-buffer
           buffer media-list 0 "Media")
          (with-current-buffer buffer
            (should (equal chirp--media-file first)))
          (chirp-media-view--render-image-buffer
           buffer media-list 1 "Media")
          (with-current-buffer buffer
            (should (equal chirp--media-file second))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (delete-file first)
      (delete-file second))))

(provide 'chirp-media-view-test)

;;; chirp-media-view-test.el ends here
