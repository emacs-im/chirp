;;; chirp-time-test.el --- Tests for Chirp timestamps -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Code:

(require 'ert)
(require 'time-date)
(require 'chirp-time)

(ert-deftest chirp-time-formats-x-style-compact-times ()
  "Compact times should follow X's Chinese and English forms."
  (let* ((now (encode-time 0 0 12 13 8 2026))
         (stamp
          (lambda (seconds)
            (format-time-string
             "%Y-%m-%dT%H:%M:%S%z"
             (time-subtract now (seconds-to-time seconds)))))
         (same-year
          (format-time-string
           "%Y-%m-%dT%H:%M:%S%z"
           (encode-time 0 0 12 11 8 2026)))
         (previous-year
          (format-time-string
           "%Y-%m-%dT%H:%M:%S%z"
           (encode-time 0 0 12 13 8 2025))))
    (let ((chirp-language "zh-CN"))
      (should (equal (chirp-time--format-compact
                      (funcall stamp 30) now)
                     "现在"))
      (should (equal (chirp-time--format-compact
                      (funcall stamp (* 7 60)) now)
                     "7分钟"))
      (should (equal (chirp-time--format-compact
                      (funcall stamp (* 6 3600)) now)
                     "6小时"))
      (should (equal (chirp-time--format-compact same-year now)
                     "8月11日"))
      (should (equal (chirp-time--format-compact previous-year now)
                     "2025年8月13日")))
    (let ((chirp-language "en"))
      (should (equal (chirp-time--format-compact
                      (funcall stamp (* 7 60)) now)
                     "7m"))
      (should (equal (chirp-time--format-compact
                      (funcall stamp (* 6 3600)) now)
                     "6h"))
      (should (equal (chirp-time--format-compact same-year now)
                     "Aug 11"))
      (should (equal (chirp-time--format-compact previous-year now)
                     "Aug 13, 2025")))))

(ert-deftest chirp-time-formats-full-focus-times ()
  "Full times should preserve the exact time and calendar date."
  (let ((time (encode-time 0 36 16 13 8 2026)))
    (let ((chirp-language "zh-CN"))
      (should (equal (chirp-time--format-full time)
                     "下午4:36 · 2026年8月13日")))
    (let ((chirp-language "en"))
      (should (equal (chirp-time--format-full time)
                     "4:36 PM · Aug 13, 2026")))))

(ert-deftest chirp-time-preserves-unknown-values-and-rejects-invalid-language ()
  "Opaque timestamps should survive, but configured language must be valid."
  (should (equal (chirp-time--format-compact "UNKNOWN") "UNKNOWN"))
  (should (equal (chirp-time--format-full "UNKNOWN") "UNKNOWN"))
  (let ((chirp-language "not a tag")
        (time (encode-time 0 0 12 13 8 2026)))
    (should-error (chirp-time--format-compact time) :type 'error)
    (should-error (chirp-time--format-full time) :type 'error)))

(provide 'chirp-time-test)

;;; chirp-time-test.el ends here
