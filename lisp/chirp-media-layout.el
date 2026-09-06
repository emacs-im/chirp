;;; chirp-media-layout.el --- Media layout plans for Chirp -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Compute presentation-neutral geometry for X-style condensed cover grids and
;; the shared non-condensed media carousel.  Rendering and media loading remain
;; owned by their respective Chirp modules.

;;; Code:

(require 'cl-lib)

(defconst chirp-media-layout-cover-gap 2
  "Pixel gutter between adjacent cover-grid cells.")

(defconst chirp-media-layout-carousel-gap 4
  "Pixel gutter between current X Web carousel items.")

(defconst chirp-media-layout-carousel-max-height-ratio 1.24446
  "Maximum non-condensed media height relative to its reference width.")

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

(defun chirp-media-layout--lower-ratio (left right)
  "Return the lower-ratio candidate of LEFT and RIGHT."
  (if (<= (plist-get left :ratio) (plist-get right :ratio))
      left
    right))

(defun chirp-media-layout--carousel-width-ratio (height-ratio aspect)
  "Return an X Web carousel item's width ratio.

HEIGHT-RATIO is relative to the carousel width.  ASPECT is the item's natural
width divided by its height."
  (min (* height-ratio (or aspect 1.0)) 0.8))

(defun chirp-media-layout--carousel-visible-ratio (height-ratio aspects)
  "Return visible fraction of the second item in ASPECTS at HEIGHT-RATIO."
  (let* ((first
          (chirp-media-layout--carousel-width-ratio
           height-ratio (nth 0 aspects)))
         (second
          (chirp-media-layout--carousel-width-ratio
           height-ratio (nth 1 aspects))))
    (if (<= second 0)
        0
      (/ (min (max (- 1 first) 0) second) second))))

(defun chirp-media-layout--carousel-fit-candidate (aspects)
  "Return X Web's all-item fit candidate for ASPECTS, or nil."
  (when (and (> (length aspects) 1)
             (cl-every (lambda (aspect)
                         (and (numberp aspect) (> aspect 0)))
                       aspects))
    (let ((remaining aspects)
          (capped-count 0)
          (gap-pixels
           (* chirp-media-layout-carousel-gap
              (1- (length aspects))))
          candidate)
      (while (and remaining (not candidate))
        (let ((sum (apply #'+ remaining)))
          (if (<= sum 0)
              (setq remaining nil)
            (let* ((ratio (/ (- 1.0 (* 0.8 capped-count)) sum))
                   (fitting
                    (cl-remove-if
                     (lambda (aspect) (> (* ratio aspect) 0.8))
                     remaining)))
              (if (<= ratio 0)
                  (setq remaining nil)
                (if (= (length fitting) (length remaining))
                    (setq candidate
                          (list :ratio ratio
                                :offset (/ (- gap-pixels) sum)))
                  (setq capped-count
                        (+ capped-count
                           (- (length remaining) (length fitting)))
                        remaining fitting)))))))
      candidate)))

(defun chirp-media-layout--carousel-portrait-candidate (aspects)
  "Return X Web's multi-item portrait candidate for ASPECTS."
  (let* ((first-known (numberp (nth 0 aspects)))
         (second-known (numberp (nth 1 aspects)))
         (first (or (nth 0 aspects) 1.0))
         (second (or (nth 1 aspects) 1.0))
         candidates)
    (cl-labels
        ((add-candidate
           (numerator denominator)
           (when (> denominator 0)
             (let ((ratio (/ numerator denominator)))
               (when (> ratio 0)
                 (push
                  (list :ratio ratio
                        :offset
                        (/ (- chirp-media-layout-carousel-gap)
                           denominator))
                  candidates))))))
      (let* ((denominator (+ first (* 0.67 second)))
             (ratio (and (> denominator 0) (/ 1.0 denominator))))
        (when (and ratio
                   (or (not first-known) (<= (* ratio first) 0.8))
                   (or (not second-known) (<= (* ratio second) 0.8)))
          (add-candidate 1.0 denominator)))
      (when first-known
        (let* ((denominator (* 0.67 second))
               (ratio (and (> denominator 0) (/ 0.2 denominator))))
          (when (and ratio
                     (>= (* ratio first) 0.8)
                     (or (not second-known)
                         (<= (* ratio second) 0.8)))
            (add-candidate 0.2 denominator))))
      (when second-known
        (let* ((ratio (and (> first 0) (/ 0.464 first))))
          (when (and ratio
                     (>= (* ratio second) 0.8)
                     (or (not first-known)
                         (<= (* ratio first) 0.8)))
            (add-candidate 0.464 first)))))
    (when candidates
      (cl-reduce #'chirp-media-layout--lower-ratio candidates))))

(defun chirp-media-layout-carousel-plan (aspects container-width)
  "Return the current X Web non-condensed media plan for ASPECTS.

ASPECTS contains natural width/height ratios.  CONTAINER-WIDTH is the
reference width in pixels.  The result contains `:height', `:widths',
`:overflow-p', and `:fit'.  A single item uses its natural ratio within the
same large media bounds; multiple items use carousel cover boxes."
  (when (and aspects
             (numberp container-width)
             (> container-width 0))
    (if (= (length aspects) 1)
        (let* ((aspect
                (let ((candidate (car aspects)))
                  (if (and (numberp candidate) (> candidate 0))
                      candidate
                    1.0)))
               (height
                (max 1
                     (round
                      (min (/ container-width aspect)
                           (* container-width
                              chirp-media-layout-carousel-max-height-ratio)))))
               (width
                (max 1 (min container-width (round (* height aspect))))))
          (list :height height
                :widths (list width)
                :overflow-p nil
                :fit nil))
      (let* ((default (list :ratio 0.68 :offset 0))
             (maximum
              (list :ratio chirp-media-layout-carousel-max-height-ratio
                    :offset 0))
             (fit (chirp-media-layout--carousel-fit-candidate aspects))
             (base
              (if (or (null fit)
                      (< (plist-get fit :ratio)
                         (plist-get default :ratio)))
                  default
                (chirp-media-layout--lower-ratio fit maximum)))
             (candidate
              (cond
               ((= (length aspects) 2)
                (let ((two-fit
                       (chirp-media-layout--carousel-fit-candidate aspects)))
                  (if (or (null two-fit)
                          (>= (plist-get two-fit :ratio)
                              (plist-get base :ratio))
                          (< (chirp-media-layout--carousel-visible-ratio
                              (plist-get base :ratio) aspects)
                             0.33))
                      base
                    two-fit)))
               ((>= (- 1
                       (chirp-media-layout--carousel-visible-ratio
                        (plist-get base :ratio) aspects))
                    0.33)
                base)
               (t
                (let ((portrait
                       (chirp-media-layout--carousel-portrait-candidate
                        aspects)))
                  (if (null portrait)
                      base
                    (setq portrait
                          (chirp-media-layout--lower-ratio portrait maximum))
                    (when-let* ((first-two
                                 (cl-remove-if-not
                                  (lambda (aspect)
                                    (and (numberp aspect) (> aspect 0)))
                                  (seq-take aspects 2)))
                                ((not (null first-two))))
                      (let ((cap
                             (apply #'min
                                    (mapcar
                                     (lambda (aspect) (/ 0.8 aspect))
                                     first-two))))
                        (setq portrait
                              (chirp-media-layout--lower-ratio
                               portrait
                               (list :ratio cap :offset 0)))))
                    (if (> (plist-get portrait :ratio)
                           (plist-get base :ratio))
                        portrait
                      base))))))
             (height-ratio (plist-get candidate :ratio))
             (height
              (max 1
                   (round
                    (+ (* container-width height-ratio)
                       (plist-get candidate :offset)))))
             (item-aspects
              (mapcar
               (lambda (aspect)
                 (/ (chirp-media-layout--carousel-width-ratio
                     height-ratio aspect)
                    height-ratio))
               aspects))
             (widths
              (mapcar (lambda (aspect)
                        (max 1 (round (* height aspect))))
                      item-aspects)))
        (list :height height
              :widths widths
              :overflow-p
              (> (apply #'+ widths) container-width)
              :fit 'cover)))))

(provide 'chirp-media-layout)

;;; chirp-media-layout.el ends here
