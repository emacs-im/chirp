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

(ert-deftest chirp-media-layout-uses-current-x-web-gutters ()
  "Cover grids and non-condensed carousels use their official gutters."
  (should (= chirp-media-layout-cover-gap 2))
  (should (= chirp-media-layout-carousel-gap 4)))

(ert-deftest chirp-media-layout-carousel-matches-x-web-portrait-track ()
  "Three portrait items should use X Web's tall overflowing carousel."
  (let ((plan
         (chirp-media-layout-carousel-plan '(0.5 0.5 0.5) 512)))
    (should (= (plist-get plan :height) 608))
    (should (equal (plist-get plan :widths) '(304 304 304)))
    (should (plist-get plan :overflow-p))))

(ert-deftest chirp-media-layout-carousel-caps-landscape-item-widths ()
  "Wide carousel items should each occupy at most 80% of the container."
  (let ((plan
         (chirp-media-layout-carousel-plan
          '(1.7777778 1.7777778) 512)))
    (should (= (plist-get plan :height) 348))
    (should (equal (plist-get plan :widths) '(409 409)))
    (should (plist-get plan :overflow-p))))

(ert-deftest chirp-media-layout-single-media-uses-large-natural-ratio ()
  "Single media should use the carousel bounds without cover cropping."
  (let ((landscape
         (chirp-media-layout-carousel-plan '(1.6) 512))
        (portrait
         (chirp-media-layout-carousel-plan (list (/ 43.0 60)) 512)))
    (should (= (plist-get landscape :height) 320))
    (should (equal (plist-get landscape :widths) '(512)))
    (should-not (plist-get landscape :fit))
    (should-not (plist-get landscape :overflow-p))
    (should (= (plist-get portrait :height) 637))
    (should (equal (plist-get portrait :widths) '(457)))
    (should-not (plist-get portrait :fit))))

(provide 'chirp-media-layout-test)

;;; chirp-media-layout-test.el ends here
