;;; chirp-media-layout-test.el --- Tests for Chirp media layouts -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'chirp-media-layout)

(ert-deftest chirp-media-layout-cover-topology-matches-x-web ()
  "Cover grids should retain X Web's one-through-six item topology."
  (should
   (equal
    (cl-loop for count from 1 to 6
             collect (chirp-media-layout-cover-topology count))
    '(((0))
      ((0 1))
      ((0 1) (0 2))
      ((0 1) (2 3))
      ((0 1) (2 3 4))
      ((0 1 2) (3 4 5))))))

(ert-deftest chirp-media-layout-cover-plan-aligns-large-grid-cells ()
  "Larger cover cells should retain 16:9 groups and pixel gutters."
  (let ((two (chirp-media-layout-cover-plan 2 256 18))
        (three (chirp-media-layout-cover-plan 3 256 18))
        (four (chirp-media-layout-cover-plan 4 256 18))
        (five (chirp-media-layout-cover-plan 5 256 18))
        (six (chirp-media-layout-cover-plan 6 256 18)))
    (should
     (equal
      (plist-get two :crop-specs)
      [(:width 255 :height 288)
       (:width 255 :height 288)]))
    (should
     (equal
      (plist-get three :crop-specs)
      [(:width 255 :height 288)
       (:width 255 :height 144 :insets (0 0 2 0))
       (:width 255 :height 144)]))
    (should
     (equal
      (plist-get four :crop-specs)
      [(:width 255 :height 144 :insets (0 0 2 0))
       (:width 255 :height 144 :insets (0 0 2 0))
       (:width 255 :height 144)
       (:width 255 :height 144)]))
    (should
     (equal
      (plist-get five :crop-specs)
      [(:width 383 :height 216 :insets (0 0 2 0))
       (:width 383 :height 216 :insets (0 0 2 0))
       (:width 255 :height 216)
       (:width 255 :height 216)
       (:width 254 :height 216)]))
    (should
     (equal
      (plist-get six :crop-specs)
      [(:width 255 :height 216 :insets (0 0 2 0))
       (:width 255 :height 216 :insets (0 0 2 0))
       (:width 254 :height 216 :insets (0 0 2 0))
       (:width 255 :height 216)
       (:width 255 :height 216)
       (:width 254 :height 216)]))))


(ert-deftest chirp-media-layout-uses-distinct-grid-and-track-gutters ()
  "Focused tracks should keep the wider carousel separation."
  (should (= chirp-media-layout-cover-gap 2))
  (should (= chirp-media-layout-track-gap 8)))

(provide 'chirp-media-layout-test)

;;; chirp-media-layout-test.el ends here
