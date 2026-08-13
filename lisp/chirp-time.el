;;; chirp-time.el --- Localized timestamps for Chirp -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Parse and format the localized compact and full timestamps shown by Chirp.

;;; Code:

(require 'subr-x)
(require 'time-date)
(require 'chirp-core)

(defun chirp-time--chinese-language-p ()
  "Return non-nil when `chirp-language' selects Chinese."
  (unless (chirp--language-tag-p chirp-language)
    (error "Invalid Chirp language tag: %S" chirp-language))
  (let ((case-fold-search t))
    (string-match-p "\\`zh\\(?:-\\|\\'\\)" chirp-language)))

(defun chirp-time--parse (value)
  "Return VALUE as an Emacs time, or nil when a string cannot be parsed."
  (if (stringp value)
      (condition-case nil
          (date-to-time value)
        (error nil))
    value))

(defun chirp-time--format-compact (value &optional now)
  "Return X-style compact VALUE relative to NOW.

VALUE may be an Emacs time value or a date-time string.  Return an
unparseable string unchanged."
  (if-let* ((time (chirp-time--parse value)))
      (let* ((chinese-p (chirp-time--chinese-language-p))
             (now (or now (current-time)))
             (seconds (max 0 (float-time (time-subtract now time)))))
        (cond
         ((< seconds 60) (if chinese-p "现在" "now"))
         ((< seconds 3600)
          (format (if chinese-p "%d分钟" "%dm")
                  (floor seconds 60)))
         ((< seconds 86400)
          (format (if chinese-p "%d小时" "%dh")
                  (floor seconds 3600)))
         ((equal (format-time-string "%Y" time)
                 (format-time-string "%Y" now))
          (if chinese-p
              (format-time-string "%-m月%-d日" time)
            (let ((system-time-locale "C"))
              (format-time-string "%b %-d" time))))
         (chinese-p
          (format-time-string "%Y年%-m月%-d日" time))
         (t
          (let ((system-time-locale "C"))
            (format-time-string "%b %-d, %Y" time)))))
    value))

(defun chirp-time--format-full (value)
  "Return X-style full timestamp VALUE.

VALUE may be an Emacs time value or a date-time string.  Return an
unparseable string unchanged."
  (if-let* ((time (chirp-time--parse value)))
      (if (chirp-time--chinese-language-p)
          (pcase-let* ((`(,_second ,minute ,hour . ,_) (decode-time time))
                       (display-hour (mod hour 12)))
            (format "%s%d:%02d · %s"
                    (if (< hour 12) "上午" "下午")
                    (if (zerop display-hour) 12 display-hour)
                    minute
                    (format-time-string "%Y年%-m月%-d日" time)))
        (let ((system-time-locale "C"))
          (format-time-string "%-I:%M %p · %b %-d, %Y" time)))
    value))

(provide 'chirp-time)

;;; chirp-time.el ends here
