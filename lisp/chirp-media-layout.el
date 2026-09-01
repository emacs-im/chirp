;;; chirp-media-layout.el --- Media layout plans for Chirp -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Compute presentation-neutral geometry for X-style cover grids and the
;; horizontal media track used by focused posts.  Rendering and media loading
;; remain owned by their respective Chirp modules.

;;; Code:

(require 'cl-lib)

(defconst chirp-media-layout-cover-gap 2
  "Pixel gutter between adjacent cover-grid cells.")

(defconst chirp-media-layout-track-gap 8
  "Pixel gutter between adjacent focused-post track items.")

(defun chirp-media-layout-cover-topology (count)
  "Return X Web's cover-grid bands for COUNT media items.

The official component displays at most six items.  A repeated index spans
the corresponding bands."
  (pcase (min count 6)
    (1 '((0)))
    (2 '((0 1)))
    (3 '((0 1) (0 2)))
    (4 '((0 1) (2 3)))
    (5 '((0 1) (2 3 4)))
    (_ '((0 1 2) (3 4 5)))))

(defun chirp-media-layout--equal-widths (group-width count)
  "Split GROUP-WIDTH into COUNT cells around cover-grid gutters."
  (let* ((available (- group-width
                       (* (1- count) chirp-media-layout-cover-gap)))
         (base (max 1 (/ available count)))
         (remainder (max 0 (% available count))))
    (cl-loop for index below count
             collect (+ base (if (< index remainder) 1 0)))))

(defun chirp-media-layout-cover-plan (count cell-size line-height)
  "Return a line-aligned X Web cover plan for COUNT media items.

CELL-SIZE is the preferred maximum cell dimension in pixels.  LINE-HEIGHT is
one default-face line in pixels.  The result contains `:bands',
`:band-slices', and one `:crop-specs' vector entry per visible item."
  (let* ((visible-count (min count 6))
         (bands (chirp-media-layout-cover-topology visible-count))
         (band-count (length bands))
         (max-columns (apply #'max (mapcar #'length bands)))
         (safe-cell-size (max 1 cell-size))
         (safe-line-height (max 1 line-height))
         (aspect (/ 16.0 9.0))
         (target-width
          (+ (* max-columns safe-cell-size)
             (* (1- max-columns) chirp-media-layout-cover-gap)))
         (band-slices
          (max 1
               (round
                (/ target-width aspect band-count safe-line-height))))
         (band-height (* band-slices safe-line-height))
         (group-width (max 1 (round (* aspect band-height band-count))))
         (crop-specs (make-vector visible-count nil)))
    (dotimes (index visible-count)
      (let* ((first-band
              (cl-position-if (lambda (band) (memq index band)) bands))
             (band (nth first-band bands))
             (column (cl-position index band))
             (occurrences
              (cl-count-if (lambda (candidate) (memq index candidate))
                           bands))
             (width
              (nth column
                   (chirp-media-layout--equal-widths
                    group-width (length band))))
             (height (* occurrences band-height))
             (insets
              (and (= occurrences 1)
                   (< first-band (1- band-count))
                   (list 0 0 chirp-media-layout-cover-gap 0))))
        (aset crop-specs index
              (append (list :width width :height height)
                      (and insets (list :insets insets))))))
    (list :bands bands
          :band-slices band-slices
          :crop-specs crop-specs)))

(provide 'chirp-media-layout)

;;; chirp-media-layout.el ends here
