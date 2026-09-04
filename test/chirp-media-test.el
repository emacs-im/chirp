;;; chirp-media-test.el --- Tests for Chirp media prefetch helpers -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'face-remap)
(require 'chirp-core)
(require 'chirp-media)
(require 'chirp-media-view)
(require 'chirp-timeline)

(ert-deftest chirp-media-prefetch-video-thumbnail-tries-remote-extraction-without-preview ()
  "Video/GIF thumbnail prefetch should try remote extraction before full download."
  (let ((chirp-media-prefetch-video-fallback-download nil)
        (chirp-media-prefetch-video-remote-thumbnail t)
        remote-called
        download-called
        prefetch-called)
    (cl-letf (((symbol-function 'chirp-media--prefetch-video-thumbnail-from-url)
               (lambda (&rest _args)
                 (setq remote-called t)
                 t))
              ((symbol-function 'chirp-media--prefetch-video-thumbnail-via-download)
               (lambda (&rest _args)
                 (setq download-called t)))
              ((symbol-function 'chirp-media-prefetch-file)
               (lambda (&rest _args)
                 (setq prefetch-called t))))
      (chirp-media-prefetch-video-thumbnail
       '(:type "animated_gif" :url "https://example.com/anim.mp4")
       (current-buffer)))
    (should remote-called)
    (should-not download-called)
    (should-not prefetch-called)))

(ert-deftest chirp-media-prefetch-video-thumbnail-falls-back-to-download-after-remote-failure ()
  "Full download fallback should run only after remote extraction fails."
  (let ((chirp-media-prefetch-video-fallback-download t)
        (chirp-media-prefetch-video-remote-thumbnail t)
        download-called)
    (cl-letf (((symbol-function 'chirp-media--prefetch-video-thumbnail-from-url)
               (lambda (_media _buffer fallback)
                 (funcall fallback)
                 t))
              ((symbol-function 'chirp-media--prefetch-video-thumbnail-via-download)
               (lambda (&rest _args)
                 (setq download-called t))))
      (chirp-media-prefetch-video-thumbnail
       '(:type "video" :url "https://example.com/video.mp4")
       (current-buffer)))
    (should download-called)))

(ert-deftest chirp-media-prefetch-video-thumbnail-prefers-preview-url ()
  "Video thumbnail prefetch should use preview images when available."
  (let ((chirp-media-prefetch-video-fallback-download nil)
        captured)
    (cl-letf (((symbol-function 'chirp-media-prefetch-file)
               (lambda (url kind ext callback)
                 (setq captured (list url kind ext (functionp callback)))))
              ((symbol-function 'chirp-media--prefetch-video-thumbnail-via-download)
               (lambda (&rest _args)
                 (ert-fail "unexpected full download fallback"))))
      (chirp-media-prefetch-video-thumbnail
       '(:type "video"
         :url "https://example.com/video.mp4"
         :preview-url "https://example.com/preview.jpg")
       (current-buffer)))
    (should (equal captured
                   '("https://example.com/preview.jpg" "video-thumbnails" "jpg" t)))))

(ert-deftest chirp-media-queue-thumbnail-extraction-skips-callback-when-cached ()
  "Existing thumbnails should not retrigger render callbacks.

Timeline rendering calls prefetch after every draw.  If cached video thumbnails
invoke callbacks synchronously, each draw schedules another timer-driven full
rerender and creates a CPU loop."
  (let ((thumbnail-file (make-temp-file "chirp-thumb-" nil ".jpg"))
        callback-called)
    (unwind-protect
        (progn
          (should (equal (chirp-media--queue-thumbnail-extraction
                          thumbnail-file
                          '("ffmpeg")
                          (lambda (&rest _args)
                            (setq callback-called t))
                          nil)
                         thumbnail-file))
          (should-not callback-called))
      (when (file-exists-p thumbnail-file)
        (delete-file thumbnail-file)))))

(ert-deftest chirp-media-prefetch-replaces-html-masquerading-as-an-image ()
  "An HTML response must not become a persistent image-cache hit."
  (let* ((chirp--app nil)
         (chirp-cache-directory (make-temp-file "chirp-media-cache-" t))
         (url "https://x.com/alice/status/1/photo/1")
         (path (chirp-media--cache-file url "media" "jpg"))
         queued-key)
    (unwind-protect
        (progn
          (with-temp-file path
            (insert "<!DOCTYPE html><title>X post</title>"))
          (should-not (chirp-media-cached-file url "media" "jpg"))
          (cl-letf (((symbol-function 'chirp-media--prefetch-enabled-p)
                     (lambda () t))
                    ((symbol-function 'chirp-media--task-queue)
                     (lambda (&rest _args) 'queue))
                    ((symbol-function 'appkit-task-queue-submit)
                     (lambda (_queue key _starter &rest _options)
                       (setq queued-key key))))
            (should (equal (chirp-media-prefetch-file url "media" "jpg")
                           path)))
          (should (equal queued-key path))
          (should-not (file-exists-p path)))
      (chirp-stop)
      (delete-directory chirp-cache-directory t))))

(ert-deftest chirp-media-prefetch-commands-are-bounded ()
  "Background curl commands should bound sources and redirects."
  (let ((chirp-media-prefetch-command "/usr/bin/curl")
        (chirp-link-card-fetch-timeout 7)
        link-card-command
        image-command)
    (cl-labels
        ((option (command name)
           (when-let* ((position (cl-position name command :test #'equal)))
             (nth (1+ position) command))))
      (cl-letf (((symbol-function 'make-process)
                 (lambda (&rest args)
                   (let ((name (plist-get args :name)))
                     (cond
                      ((equal name "chirp-link-card")
                       (setq link-card-command (plist-get args :command)))
                      ((equal name "chirp-prefetch")
                       (setq image-command (plist-get args :command)))))
                   nil)))
        (funcall
         (chirp-media--start-link-card-task
          "http://example.com" #'ignore))
        (funcall
         (chirp-media--start-prefetch-task
          "https://pbs.twimg.com/media/photo.jpg"
          "/tmp/chirp-prefetch-test.jpg"
          #'ignore)))
      (should (equal (option link-card-command "--max-filesize")
                     (number-to-string (* 256 1024))))
      (should (equal (option link-card-command "--max-time") "7"))
      (should (equal (option link-card-command "--proto") "=http,https"))
      (should-not (member "-L" link-card-command))
      (should (equal (option image-command "--max-filesize")
                     (number-to-string (* 25 1024 1024))))
      (should (equal (option image-command "--max-time") "60"))
      (should (equal (option image-command "--proto") "=https"))
      (should-not (member "-L" image-command)))))

(ert-deftest chirp-media-image-resource-retries-and-binds-cache-to-source ()
  "Replacement and explicit renewed interest fence stale or invalid image bytes."
  (let ((chirp--app nil)
        (directory (make-temp-file "chirp-resource-" t))
        view callbacks canceled invalid)
    (cl-labels
        ((drain ()
           (let ((app-loop (appkit-app-loop (chirp-app)))
                 (loop (appkit-surface-loop view)))
             (while (> (+ (appkit-loop-pending-count app-loop)
                          (appkit-loop-pending-count loop)) 0)
               (appkit-loop-run-pass app-loop)
               (appkit-loop-run-pass loop))))
         (source (url)
           (appkit-surface-post view (list 'chirp-model (list :type 'test :source url)))
           (drain)))
      (unwind-protect
          (cl-letf (((symbol-function 'chirp-media--prefetch-enabled-p) (lambda () t))
                    ((symbol-function 'appkit-media-image-cache-existing-file) (lambda (_base) nil))
                    ((symbol-function 'appkit-media-image-resource-load)
                     (lambda (_context input resolve reject)
                       (let ((url (alist-get 'url (appkit-media-image-acquisition-resource input))))
                         (push (list url resolve reject) callbacks)
                         (appkit-cancellation-create
                          :kind 'transport :cancel (lambda () (push url canceled)))))))
            (setq view
                  (chirp-open-projection-view
                   :id (make-symbol "image-resource-test") :title "Image resource"
                   :state '(:type test :source "https://pbs.twimg.com/media/one.jpg")
                   :setup #'ignore
                   :render-function
                   (lambda (surface _app model _change)
                     (let ((demand (chirp-media-image-demand
                                    surface 'photo (plist-get model :source))))
                       (appkit-render-result-create
                        :resource-demands (and demand (list demand))
                        :resource-interest-update
                        (appkit-resource-interest-update-create
                         :mode 'replace :entries
                         (and demand (list (appkit-resource-interest-create
                                            :key 'photo :row-keys '(photo))))))))))
            (source "https://pbs.twimg.com/media/two.jpg")
            (should (= (length callbacks) 2))
            (should (member "https://pbs.twimg.com/media/one.jpg" canceled))
            (funcall (nth 1 (cadr callbacks)) (expand-file-name "stale-source.jpg" directory))
            (drain)
            (should (eq (appkit-resource-state-status (appkit-resource-state view 'photo)) 'pending))
            (funcall (nth 2 (car callbacks)) "transient failure")
            (drain)
            (should (eq (appkit-resource-state-status (appkit-resource-state view 'photo)) 'failed))
            (source nil)
            (source "https://pbs.twimg.com/media/two.jpg")
            (should (= (length callbacks) 3))
            (setq invalid (make-temp-file (expand-file-name "invalid-" directory) nil ".img"))
            (with-temp-file invalid (insert "<!DOCTYPE html><title>not an image</title>"))
            (funcall (nth 1 (car callbacks)) invalid)
            (drain)
            (should-not (file-exists-p invalid))
            (should (eq (appkit-resource-state-status (appkit-resource-state view 'photo)) 'failed))
            (source nil)
            (source "https://pbs.twimg.com/media/two.jpg")
            (chirp-stop)
            (funcall (nth 1 (car callbacks)) (expand-file-name "stale-image.jpg" directory))
            (should-not (appkit-surface-live-p view)))
        (chirp-stop)
        (when (file-directory-p directory) (delete-directory directory t))
        (when (and view (buffer-live-p (appkit-surface-buffer view)))
          (kill-buffer (appkit-surface-buffer view)))))))

(ert-deftest chirp-media-prefetch-callback-errors-are-reported-and-isolated ()
  "A failed media callback should warn without blocking later callbacks."
  (let ((chirp--app nil)
        (path "/tmp/chirp-prefetch-test.jpg")
        warning
        later-called)
    (puthash path
             (list (lambda (&rest _args)
                     (error "prefetch callback failed"))
                   (lambda (_success _path)
                     (setq later-called t)))
             (chirp-media--pending-table 'prefetch))
    (cl-letf (((symbol-function 'display-warning)
               (lambda (type message &rest _args)
                 (setq warning (list type message)))))
      (chirp-media--prefetch-finish path t nil))
    (should later-called)
    (should (eq (car warning) 'chirp-media))
    (should (string-match-p
             "prefetch callback failed for /tmp/chirp-prefetch-test.jpg"
             (cadr warning)))
    (chirp-stop)))

(ert-deftest chirp-media-task-queues-belong-to-the-lazy-appkit-runtime ()
  "Media queues should share Appkit lifecycle ownership and adjustable limits."
  (let ((chirp--app nil))
    (unwind-protect
        (let* ((queue (chirp-media--task-queue 'prefetch 1))
               (runtime (chirp--session-media-runtime (chirp--session))))
          (should (appkit-task-queue-live-p queue))
          (should (eq (appkit-task-queue-owner queue) (chirp-app)))
          (should (eq queue (chirp-media--task-queue 'prefetch 2)))
          (should (= (appkit-task-queue-limit queue) 2))
          (should (eq queue
                      (chirp-media--runtime-prefetch-tasks runtime)))
          (chirp-stop)
          (should-not (appkit-task-queue-live-p queue))
          (should-not
           (eq runtime
               (chirp-media--runtime))))
      (chirp-stop))))

(ert-deftest chirp-media-prefetch-tweet-recurses-into-quoted-tweet ()
  "Quoted tweet media should also be prefetched."
  (let (avatars media-urls)
    (cl-letf (((symbol-function 'chirp-media-prefetch-avatar)
               (lambda (url _buffer)
                 (push url avatars)))
              ((symbol-function 'chirp-media-prefetch-media)
               (lambda (media _buffer)
                 (push (plist-get media :url) media-urls))))
      (chirp-media-prefetch-tweet
       '(:author-avatar-url "https://example.com/main-avatar.jpg"
         :media ((:type "photo" :url "https://example.com/main.jpg"))
         :quoted-tweet (:author-avatar-url "https://example.com/quoted-avatar.jpg"
                        :media ((:type "photo" :url "https://example.com/quoted.jpg"))))
       (current-buffer)))
    (should (equal avatars
                   '("https://example.com/quoted-avatar.jpg"
                     "https://example.com/main-avatar.jpg")))
    (should (equal media-urls
                   '("https://example.com/quoted.jpg"
                     "https://example.com/main.jpg")))))

(ert-deftest chirp-media-prefetch-tweet-skips-hidden-avatars-and-media ()
  "Hidden avatars/media should not be prefetched in the background."
  (let ((chirp-show-avatars nil)
        (chirp-show-tweet-media nil)
        avatars
        media-urls)
    (cl-letf (((symbol-function 'chirp-media-prefetch-avatar)
               (lambda (url _buffer)
                 (push url avatars)))
              ((symbol-function 'chirp-media-prefetch-media)
               (lambda (media _buffer)
                 (push (plist-get media :url) media-urls))))
      (chirp-media-prefetch-tweet
       '(:author-avatar-url "https://example.com/avatar.jpg"
         :media ((:type "photo" :url "https://example.com/main.jpg"))
         :article-text "Intro.\n\n![Cover](https://example.com/cover.jpg)"
         :quoted-tweet (:author-avatar-url "https://example.com/quoted-avatar.jpg"
                        :media ((:type "photo" :url "https://example.com/quoted.jpg"))))
       (current-buffer)))
    (should-not avatars)
    (should-not media-urls)))

(ert-deftest chirp-media-prefetch-tweet-prefetches-article-images-and-link-cards ()
  "Article images and external link cards should join normal media prefetch."
  (let (media-urls card-urls)
    (cl-letf (((symbol-function 'chirp-media-prefetch-avatar)
               (lambda (&rest _args)))
              ((symbol-function 'chirp-media-prefetch-media)
               (lambda (media _buffer)
                 (push (plist-get media :url) media-urls)))
              ((symbol-function 'chirp-media-prefetch-link-card)
               (lambda (url _buffer)
                 (push url card-urls))))
      (chirp-media-prefetch-tweet
       '(:author-avatar-url "https://example.com/avatar.jpg"
         :urls ("https://github.com/example/project")
         :article-text "Intro.\n\n![Cover](https://example.com/cover.jpg)")
       (current-buffer)))
    (should (equal media-urls
                   '("https://example.com/cover.jpg")))
    (should (equal card-urls
                   '("https://github.com/example/project")))))

(ert-deftest chirp-media-parse-link-card-html-extracts-opengraph-fields ()
  "Link-card HTML parsing should extract title, description, and image."
  (let ((card
         (chirp-media--parse-link-card-html
          "<html><head><meta property=\"og:title\" content=\"microsoft/RD-Agent\"><meta property=\"og:description\" content=\"Research &amp; development\"><meta property=\"og:image\" content=\"/preview.png\"></head></html>"
          "https://github.com/microsoft/RD-Agent")))
    (should (equal (plist-get card :url)
                   "https://github.com/microsoft/RD-Agent"))
    (should (equal (plist-get card :title)
                   "microsoft/RD-Agent"))
    (should (equal (plist-get card :description)
                   "Research & development"))
    (should (equal (plist-get card :image-url)
                   "https://github.com/preview.png"))))

(ert-deftest chirp-media-thumbnail-image-uses-appkit-video-decoration ()
  "Photo and video thumbnails should use Appkit sizing and video decoration."
  (let ((chirp-media-render-from-cache-only t)
        rendered
        decorated)
    (cl-letf (((symbol-function 'chirp-media-cached-file)
               (lambda (&rest _args)
                 "/tmp/chirp-thumb.jpg"))
              ((symbol-function 'appkit-media-preview-image-from-file)
               (lambda (file max-width max-height)
                 (setq rendered (list file max-width max-height))
                 'preview-image))
              ((symbol-function 'appkit-media-video-preview-display-image)
               (lambda (image namespace)
                 (setq decorated (list image namespace))
                 'decorated-preview)))
      (should (eq (chirp-media-thumbnail-image
                   '(:type "photo"
                     :url "https://example.com/photo.jpg"))
                  'preview-image))
      (should (equal rendered
                     '("/tmp/chirp-thumb.jpg" 256 256)))
      (should-not decorated)
      (should (eq (chirp-media-thumbnail-image
                   '(:type "video"
                     :preview-url "https://example.com/preview.jpg"))
                  'decorated-preview))
      (should (equal decorated '(preview-image chirp)))
      (cl-letf (((symbol-function 'appkit-media-video-preview-display-image)
                 (lambda (&rest _args) nil)))
        (should (eq (chirp-media-thumbnail-image
                     '(:type "video"
                       :preview-url "https://example.com/preview.jpg"))
                    'preview-image))))))

(ert-deftest chirp-media-carousel-image-builds-configured-montage ()
  "Carousel media should form one configured horizontal SVG."
  (let (captured-items captured-height captured-gap captured-offset)
    (cl-letf (((symbol-function 'chirp-media--preview-file)
               (lambda (media) (plist-get media :file)))
              ((symbol-function 'appkit-media-horizontal-strip-image)
               (lambda (items height gap &optional offset)
                 (setq captured-items items
                       captured-height height
                       captured-gap gap
                       captured-offset offset)
                 'track-image)))
      (should
       (eq
        (chirp-media-carousel-image
         '((:type "photo" :file "/tmp/a.jpg"
            :width 430 :height 600)
           (:type "video" :file "/tmp/b.jpg"
            :width 600 :height 375))
         384 8 28)
        'track-image))
      (should (= captured-height 384))
      (should (= captured-gap 8))
      (should (= captured-offset 28))
      (should
       (equal
        captured-items
        '((:file "/tmp/a.jpg" :width 430 :height 600
           :id chirp-media-0)
          (:file "/tmp/b.jpg" :width 600 :height 375
           :id chirp-media-1)))))))

(ert-deftest chirp-media-side-by-side-thumbnail-prefers-fixed-crop ()
  "Grid thumbnails should crop to one fixed tile and retain a decoder fallback."
  (let ((chirp-media-render-from-cache-only t)
        cropped-arguments)
    (cl-letf (((symbol-function 'chirp-media-cached-file)
               (lambda (&rest _args) "/tmp/chirp-thumb.jpg"))
              ((symbol-function
                'appkit-media-cropped-preview-image-from-file)
               (lambda (&rest arguments)
                 (setq cropped-arguments arguments)
                 'cropped-image))
              ((symbol-function 'appkit-media-preview-image-from-file)
               (lambda (&rest _args)
                 (ert-fail "fixed crop unexpectedly used its fallback"))))
      (should
       (eq (chirp-media-thumbnail-image
            '(:type "photo" :url "https://example.com/photo.jpg")
            '(127 . 144))
           'cropped-image))
      (should
       (equal cropped-arguments
              '("/tmp/chirp-thumb.jpg" 127 144)))
      (setq cropped-arguments nil)
      (should
       (eq (chirp-media-thumbnail-image
            '(:type "photo" :url "https://example.com/photo.jpg")
            '(:width 127 :height 72 :insets (0 0 2 0)))
           'cropped-image))
      (should
       (equal cropped-arguments
              '("/tmp/chirp-thumb.jpg" 127 72 (0 0 2 0)))))
    (cl-letf (((symbol-function 'chirp-media-cached-file)
               (lambda (&rest _args) "/tmp/chirp-thumb.jpg"))
              ((symbol-function
                'appkit-media-cropped-preview-image-from-file)
               (lambda (&rest _args) nil))
              ((symbol-function 'appkit-media-preview-image-from-file)
               (lambda (&rest _args) 'ordinary-image)))
      (should
       (eq (chirp-media-thumbnail-image
            '(:type "photo" :url "https://example.com/photo.jpg")
            '(127 . 144))
           'ordinary-image)))))

(ert-deftest chirp-media-thumbnail-placeholder-image-exists-for-video-like-media ()
  "Video-like media should reserve thumbnail space before the real preview arrives."
  (cl-letf (((symbol-function 'display-images-p)
             (lambda () t)))
    (should (chirp-media-thumbnail-placeholder-image
             '(:type "video" :url "https://example.com/video.mp4")))
    (should (chirp-media-thumbnail-placeholder-image
             '(:type "animated_gif" :url "https://example.com/anim.mp4")))
    (should-not (chirp-media-thumbnail-placeholder-image
                 '(:type "photo" :url "https://example.com/photo.jpg")))))

(ert-deftest chirp-media-from-x-item-preserves-preview-and-variants ()
  "Structured media payloads should keep preview URLs and variant lists."
  (let* ((media
          (chirp--media-item-from-x
           '(("type" . "video")
             ("url" . "https://high.mp4")
             ("previewUrl" . "https://preview.jpg")
             ("altText" . "A demo video")
             ("variants"
              . ((("url" . "https://high.mp4")
                  ("bitrate" . 2176000))
                 (("url" . "https://low.mp4")
                  ("bitrate" . 832000))))))))
    (should (equal (plist-get media :preview-url) "https://preview.jpg"))
    (should (equal (plist-get media :alt) "A demo video"))
    (should (equal (mapcar (lambda (variant) (plist-get variant :url))
                           (plist-get media :variants))
                   '("https://high.mp4" "https://low.mp4")))))

(defun chirp-test--card-binding (key value)
  "Return one GraphQL-style card binding for KEY and VALUE."
  (list (cons "key" key)
        (cons "value" value)))

(defun chirp-test--card-object (name bindings)
  "Return a tweet card object named NAME with BINDINGS."
  `(("legacy" . (("name" . ,name)
                 ("binding_values" . ,bindings)))))

(ert-deftest chirp-tweet-from-x-reads-summary-link-card ()
  "Website cards should come from X binding_values, not a later HTML fetch."
  (let* ((bindings
          (list
           (chirp-test--card-binding
            "title" '(("string_value" . "GitHub - antirez/h3.c")))
           (chirp-test--card-binding
            "description"
            '(("string_value" . "MiniMax H3 inference engine for Mac computers.")))
           (chirp-test--card-binding
            "vanity_url" '(("string_value" . "github.com")))
           (chirp-test--card-binding
            "thumbnail_image"
            '(("image_value"
               . (("url" . "https://pbs.twimg.com/card_img/demo.jpg")))))))
         (tweet
          (chirp--tweet-from-x
           (list (cons "rest_id" "2086764219433660463")
                 (cons "urls" (list "https://github.com/antirez/h3.c"))
                 (cons "card" (chirp-test--card-object "summary" bindings)))))
         (card (plist-get tweet :link-card)))
    (should (equal (plist-get card :title) "GitHub - antirez/h3.c"))
    (should (equal (plist-get card :description)
                   "MiniMax H3 inference engine for Mac computers."))
    (should (equal (plist-get card :domain) "github.com"))
    (should (equal (plist-get card :url) "https://github.com/antirez/h3.c"))
    (should (equal (plist-get card :image-url)
                   "https://pbs.twimg.com/card_img/demo.jpg"))))

(ert-deftest chirp-tweet-from-x-ignores-poll-cards ()
  "Poll cards should not be rendered as website previews."
  (let ((tweet
         (chirp--tweet-from-x
          (list (cons "id" "1")
                (cons "text" "poll")
                (cons "card"
                      (chirp-test--card-object
                       "poll2choice_text_only"
                       (list (chirp-test--card-binding
                              "title"
                              '(("string_value" . "A or B"))))))))))
    (should-not (plist-get tweet :link-card))))

(ert-deftest chirp-media-prefetch-tweet-prefetches-x-link-card-image ()
  "An X website card should prefetch its thumbnail instead of Open Graph HTML."
  (let (file-urls card-urls)
    (cl-letf (((symbol-function 'chirp-media-prefetch-avatar) #'ignore)
              ((symbol-function 'chirp-media-prefetch-media) #'ignore)
              ((symbol-function 'chirp-media-prefetch-file)
               (lambda (url &rest _args)
                 (push url file-urls)))
              ((symbol-function 'chirp-media-prefetch-link-card)
               (lambda (url _buffer)
                 (push url card-urls))))
      (chirp-media-prefetch-tweet
       '(:author-avatar-url "https://example.com/avatar.jpg"
         :urls ("https://github.com/antirez/h3.c")
         :link-card (:url "https://github.com/antirez/h3.c"
                     :title "GitHub - antirez/h3.c"
                     :image-url "https://pbs.twimg.com/card_img/demo.jpg"))
       (current-buffer)))
    (should (equal file-urls '("https://pbs.twimg.com/card_img/demo.jpg")))
    (should-not card-urls)))

(ert-deftest chirp-tweet-from-x-reads-unified-card-video-media ()
  "Tweet conversion should expose playable unified-card video variants."
  (let* ((card
          (concat
           "{\"media_entities\":{\"13_1\":{"
           "\"type\":\"video\","
           "\"media_url_https\":\"https://example.com/poster.jpg\","
           "\"original_info\":{\"width\":720,\"height\":1280},"
           "\"video_info\":{\"variants\":[{"
           "\"bitrate\":2176000,"
           "\"url\":\"https://example.com/video.mp4\"}]}}}}"))
         (tweet
          (chirp--tweet-from-x
           `(("rest_id" . "1")
             ("legacy" . (("full_text" . "video")))
             ("card" .
              (("legacy" .
                (("binding_values" .
                  ((("key" . "unified_card")
                    ("value" . (("string_value" . ,card)))))))))))))
         (media (car (plist-get tweet :media))))
    (should (equal (plist-get media :type) "video"))
    (should (equal (chirp-media-playback-url media)
                   "https://example.com/video.mp4"))))

(ert-deftest chirp-media-quit-restores-source-buffer-point ()
  "Closing a managed viewer restores the exact source viewport."
  (let* ((chirp--app nil) (surface (chirp-media-test--source))
         (source (appkit-surface-buffer surface))
         (viewer (generate-new-buffer " *chirp-media-viewer*")))
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer source)
          (let ((inhibit-read-only t))
            (dotimes (index 80) (insert (format "line %02d\n" index))))
          (goto-char (point-min)) (forward-line 40) (recenter 0)
          (let ((source-point (point)) (source-start (window-start)))
            (cl-letf (((symbol-function 'chirp-media-cached-file) (lambda (&rest _) "/tmp/photo.jpg"))
                      ((symbol-function 'video-open)
                       (lambda (_file &rest args)
                         (let ((target (plist-get args :buffer)))
                           (with-current-buffer target (special-mode))
                           (chirp-display-buffer target) target))))
              (chirp-media-open '((:type "photo" :url "https://example.com/photo.jpg")) 0 "Media" viewer)
              (appkit-loop-run-pass (appkit-surface-loop surface))
              (with-current-buffer viewer (chirp-media-quit)))
            (should (eq (window-buffer) source))
            (should (= (window-point) source-point))
            (should (= (window-start) source-start))))
      (chirp-stop)
      (dolist (buffer (list source viewer))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest chirp-media-open-video-reuses-inline-appkit-session ()
  "Committed dedicated presentation preserves the exact inline player state."
  (let* ((chirp--app nil) (source (chirp-media-test--source))
         (buffer (appkit-surface-buffer source))
         (viewer (generate-new-buffer " *chirp-inline-handoff*"))
         (media-list '((:type "video" :url "https://example.com/video.mp4")))
         (session (list :player (make-symbol "player") :position 23.5 :paused t))
         (inline (appkit-media--video-inline-create :session session :inline 'inline))
         presented)
    (unwind-protect
        (cl-letf (((symbol-function 'appkit-media-video-inline-closed-p) (lambda (actual) (not (eq actual inline))))
                  ((symbol-function 'appkit-media-video-session-live-p) (lambda (actual) (eq actual session)))
                  ((symbol-function 'chirp-media-video-session-create)
                   (lambda (&rest _) (ert-fail "Inline handoff must not create a fresh session")))
                  ((symbol-function 'appkit-media-present-video-inline)
                   (lambda (actual _label &rest keys)
                     (setq presented (appkit-media-video-inline-session actual))
                     (plist-get keys :buffer))))
          (with-current-buffer buffer
            (chirp-media-register-video-inline media-list 0 inline)
            (chirp-media-open media-list 0 "Media" viewer))
          (should-not presented)
          (appkit-loop-run-pass (appkit-surface-loop source))
          (should (eq presented session))
          (should (= (plist-get presented :position) 23.5))
          (should (plist-get presented :paused)))
      (chirp-stop)
      (dolist (target (list buffer viewer)) (when (buffer-live-p target) (kill-buffer target))))))

(ert-deftest chirp-media-play-launches-configured-player ()
  "Media viewer playback should launch the configured external player on demand."
  (let ((chirp-video-player-command "/usr/bin/mpv")
        (chirp-video-use-internal-player nil)
        (chirp-video-playback-max-bitrate 2176000)
        captured-command
        captured-query-flag)
    (with-temp-buffer
      (setq-local chirp--media-list '((:type "animated_gif"
                                       :url "https://example.com/anim-high.mp4"
                                       :variants ((:url "https://example.com/anim-high.mp4" :bitrate 4096000)
                                                  (:url "https://example.com/anim-low.mp4" :bitrate 832000)))))
      (setq-local chirp--media-index 0)
      (cl-letf (((symbol-function 'make-process)
                 (lambda (&rest args)
                   (setq captured-command (plist-get args :command))
                   'fake-process))
                ((symbol-function 'set-process-query-on-exit-flag)
                 (lambda (_process flag)
                   (setq captured-query-flag flag))))
        (chirp-media-play-video (car chirp--media-list) t)))
    (should (equal captured-command
                   '("/usr/bin/mpv" "https://example.com/anim-low.mp4")))
    (should (eq captured-query-flag nil))))

(ert-deftest chirp-media-play-launches-mpv-with-configured-window-size ()
  "mpv playback should honor `chirp-video-player-window-size'."
  (let ((chirp-video-player-command "/usr/bin/mpv")
        (chirp-video-use-internal-player nil)
        (chirp-video-player-window-size '(1280 . 720))
        captured-command)
    (with-temp-buffer
      (setq-local chirp--media-list '((:type "video"
                                       :url "https://example.com/video.mp4")))
      (setq-local chirp--media-index 0)
      (cl-letf (((symbol-function 'make-process)
                 (lambda (&rest args)
                   (setq captured-command (plist-get args :command))
                   'fake-process))
                ((symbol-function 'set-process-query-on-exit-flag)
                 (lambda (&rest _args)
                   nil)))
        (chirp-media-play-video (car chirp--media-list) t)))
    (should (equal captured-command
                   '("/usr/bin/mpv" "--geometry=1280x720" "https://example.com/video.mp4")))))

(ert-deftest chirp-media-play-falls-back-to-browser-when-player-is-disabled ()
  "When no external player is configured, Chirp should browse the media URL."
  (let ((chirp-video-player-command nil)
        (chirp-video-use-internal-player nil)
        browsed-url)
    (with-temp-buffer
      (setq-local chirp--media-list '((:type "animated_gif"
                                       :url "https://example.com/anim.mp4")))
      (setq-local chirp--media-index 0)
      (cl-letf (((symbol-function 'browse-url)
                 (lambda (url &rest _args)
                   (setq browsed-url url))))
        (chirp-media-play-video (car chirp--media-list) t)))
    (should (equal browsed-url "https://example.com/anim.mp4"))))

(ert-deftest chirp-media-download-url-prefers-original-photo-and-highest-video-variant ()
  "Downloads should prefer original photos and the highest bitrate video URL."
  (should
   (equal
    (chirp-media-download-url
     '(:type "photo"
       :url "https://pbs.twimg.com/media/abc123.jpg?name=small"))
    "https://pbs.twimg.com/media/abc123.jpg?name=orig"))
  (should
   (equal
    (chirp-media-download-url
     '(:type "video"
       :url "https://example.com/mid.mp4"
       :variants ((:url "https://example.com/low.mp4" :bitrate 832000)
                  (:url "https://example.com/high.mp4" :bitrate 4096000)
                  (:url "https://example.com/mid.mp4" :bitrate 2176000))))
    "https://example.com/high.mp4")))

(ert-deftest chirp-media-download-at-point-starts-async-download ()
  "Downloading media should prompt for a target path and spawn curl asynchronously."
  (let ((chirp-media-prefetch-command "/usr/bin/curl")
        (chirp-media-download-directory "~/Downloads/")
        captured-command
        start-message)
    (with-temp-buffer
      (setq-local chirp--media-list '((:type "video"
                                       :url "https://example.com/mid.mp4"
                                       :variants ((:url "https://example.com/high.mp4" :bitrate 4096000)))))
      (setq-local chirp--media-index 0)
      (cl-letf (((symbol-function 'read-file-name)
                 (lambda (&rest _args)
                   "/tmp/chirp-download.mp4"))
                ((symbol-function 'file-exists-p)
                 (lambda (_path)
                   nil))
                ((symbol-function 'make-directory) #'ignore)
                ((symbol-function 'make-process)
                 (lambda (&rest args)
                   (setq captured-command (plist-get args :command))
                   'fake-process))
                ((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (setq start-message (apply #'format format-string args)))))
        (chirp-media-download-at-point)))
    (should (equal captured-command
                   '("/usr/bin/curl"
                     "-L" "-f" "-sS"
                     "-o" "/tmp/chirp-download.mp4"
                     "https://example.com/high.mp4")))
    (should (equal start-message "Downloading chirp-download.mp4..."))))

(ert-deftest chirp-media-download-at-point-falls-back-to-browserless-copy-when-no-curl ()
  "Downloading media without curl should use `url-copy-file'."
  (let ((chirp-media-prefetch-command nil)
        copied-url
        copied-target
        final-message)
    (with-temp-buffer
      (setq-local chirp--media-list '((:type "photo"
                                       :url "https://pbs.twimg.com/media/abc123.jpg")))
      (setq-local chirp--media-index 0)
      (cl-letf (((symbol-function 'read-file-name)
                 (lambda (&rest _args)
                   "/tmp/chirp-photo.jpg"))
                ((symbol-function 'file-exists-p)
                 (lambda (_path)
                   nil))
                ((symbol-function 'make-directory) #'ignore)
                ((symbol-function 'url-copy-file)
                 (lambda (url target &optional _ok-if-exists)
                   (setq copied-url url
                         copied-target target)))
                ((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (setq final-message (apply #'format format-string args)))))
        (chirp-media-download-at-point)))
    (should (equal copied-url
                   "https://pbs.twimg.com/media/abc123.jpg?name=orig"))
    (should (equal copied-target "/tmp/chirp-photo.jpg"))
    (should (equal final-message "Downloaded /tmp/chirp-photo.jpg"))))

(ert-deftest chirp-media-avatar-pixel-size-follows-current-line-height ()
  "Avatar pixels should track the current line height and `chirp-avatar-size'."
  (let ((chirp-avatar-size 28)
        (line-height 21))
    (cl-letf (((symbol-function 'appkit-chat-avatar-line-pixel-height)
               (lambda () line-height)))
      (should (= 21 (chirp-media--avatar-pixel-size)))
      (setq line-height 35)
      (should (= 35 (chirp-media--avatar-pixel-size)))
      (setq chirp-avatar-size 14)
      (should (= 18 (chirp-media--avatar-pixel-size))))))

(ert-deftest chirp-media-avatar-image-uses-current-line-pixel-size ()
  "Avatar descriptors should be built at the current line pixel size."
  (let ((chirp-avatar-size 28)
        (chirp-media-render-from-cache-only t)
        captured-size)
    (cl-letf (((symbol-function 'appkit-chat-avatar-line-pixel-height)
               (lambda () 35))
              ((symbol-function 'chirp-media-cached-file)
               (lambda (&rest _args) "/tmp/chirp-avatar.jpg"))
              ((symbol-function 'appkit-media-circular-image-from-file)
               (lambda (_file size)
                 (setq captured-size size)
                 '(image :type svg :data "avatar"))))
      (should (equal (chirp-media-avatar-image "https://example.com/a.jpg")
                     '(image :type svg :data "avatar")))
      (should (= captured-size 35)))))

(ert-deftest chirp-view-mode-rebuilds-geometry-after-text-scale ()
  "Text scale should request a cached redraw, not a network refresh."
  (with-temp-buffer
    (chirp-view-mode)
    (should (memq #'chirp--on-text-scale-change text-scale-mode-hook))
    (let (rerender-args)
      (cl-letf (((symbol-function 'chirp-request-rerender)
                 (lambda (&optional buffer delay)
                   (setq rerender-args (list buffer delay))))
                ((symbol-function 'chirp-refresh)
                 (lambda ()
                   (ert-fail "text-scale must not refetch"))))
        (chirp--on-text-scale-change)
        (should (equal rerender-args '(nil 0)))))))

(defun chirp-media-test--source (&optional identity)
  "Open a real generated source Surface without multimedia dependencies."
  (chirp-open-projection-view :id (or identity (make-symbol "media-test"))
                              :title "Media test" :state (list :type 'test)
                              :setup #'ignore :render-function #'ignore))

(ert-deftest chirp-media-replaced-source-rejects-late-acquisition ()
  "Replacing a source model revokes the old acquisition's viewer authority."
  (let* ((chirp--app nil) (source (chirp-media-test--source))
         (buffer (appkit-surface-buffer source)) resolve canceled opened)
    (unwind-protect
        (cl-letf (((symbol-function 'chirp-media-view--acquire-start)
                   (lambda (_context _input _observe success _reject)
                     (setq resolve success)
                     (appkit-cancellation-create :kind 'transport :cancel (lambda () (setq canceled t)))))
                  ((symbol-function 'video-open)
                   (lambda (&rest _) (setq opened t) (ert-fail "Late acquisition opened a viewer"))))
          (with-current-buffer buffer
            (chirp-media-open '((:type "photo" :url "https://example.com/photo.jpg")) 0))
          (appkit-surface-send source (list 'chirp-model (list :type 'replacement)))
          (should canceled)
          (funcall resolve "/tmp/photo.jpg")
          (appkit-loop-run-pass (appkit-surface-loop source))
          (should-not opened)
          (should (eq (plist-get (appkit-surface-model source) :type) 'replacement)))
      (chirp-stop)
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(provide 'chirp-media-test)

;;; chirp-media-test.el ends here
