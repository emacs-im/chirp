;;; chirp-media-test.el --- Tests for Chirp media prefetch helpers -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'face-remap)
(require 'chirp-core)
(require 'chirp-media)
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
  "Image retries should be explicit, source-bound, and lifecycle-safe."
  (let ((chirp--app nil)
        (buffer (generate-new-buffer " *chirp-media-resource*"))
        callbacks cache-bases curl-defaults view store entry)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (chirp-view-mode)
            (setq view
                  (appkit-attach-view
                   :app (chirp-app)
                   :id '(test media-resource)
                   :state '(:type test)
                   :mode 'chirp-view-mode
                   :sync-function #'ignore
                   :parts nil)))
          (setq store (appkit-app-resource-store (chirp-app)))
          (cl-letf (((symbol-function 'chirp-media--prefetch-enabled-p)
                     (lambda () t))
                    ((symbol-function 'appkit-media-image-cache-existing-file)
                     (lambda (_cache-base) nil))
                    ((symbol-function 'appkit-media-cache-image-resource-async)
                     (lambda (_resource cache-base success error &rest _options)
                       (push (cons success error) callbacks)
                       (push cache-base cache-bases)
                       (push (copy-sequence plz-curl-default-args)
                             curl-defaults)
                       nil)))
            (chirp-media-request-image-resource
             view 'photo "https://pbs.twimg.com/media/one.jpg")
            (chirp-media-request-image-resource
             view 'photo "https://pbs.twimg.com/media/two.jpg")
            (should (= (length callbacks) 2))
            (should-not (equal (car cache-bases) (cadr cache-bases)))
            (funcall (car (cadr callbacks)) "/tmp/stale-source.jpg")
            (should (eq (plist-get (gethash 'photo store) :status) 'pending))
            (funcall (cdar callbacks) "transient failure")
            (should (eq (plist-get (gethash 'photo store) :status) 'failed))
            (chirp-media-request-image-resource
             view 'photo "https://pbs.twimg.com/media/two.jpg")
            (should (= (length callbacks) 3))
            (let ((invalid (make-temp-file "chirp-image-" nil ".img")))
              (with-temp-file invalid
                (insert "<!DOCTYPE html><title>not an image</title>"))
              (funcall (caar callbacks) invalid)
              (should-not (file-exists-p invalid))
              (should (eq (plist-get (gethash 'photo store) :status)
                          'failed)))
            (chirp-media-request-image-resource
             view 'photo "https://pbs.twimg.com/media/two.jpg")
            (should (= (length callbacks) 4))
            (should (equal (caar curl-defaults) "--disable"))
            (should-not (member "--location" (car curl-defaults)))
            (should-not (member "--cookie" (car curl-defaults)))
            (setq entry (gethash 'photo store))
            (chirp-stop)
            (funcall (caar callbacks) "/tmp/stale-image.jpg")
            (should (eq (plist-get entry :status) 'pending))))
      (chirp-stop)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

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
                     '("/tmp/chirp-thumb.jpg" 128 128)))
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
  "Closing media should restore point and scroll state in the source buffer."
  (let ((source (generate-new-buffer " *chirp-media-source*"))
        (viewer (generate-new-buffer " *chirp-media-viewer*")))
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer source)
          (with-current-buffer source
            (chirp-view-mode)
            (let ((inhibit-read-only t))
              (dotimes (index 80)
                (insert (format "line %02d\n" index))))
            (goto-char (point-min))
            (forward-line 40)
            (set-window-start (selected-window)
                              (save-excursion
                                (goto-char (point-min))
                                (forward-line 34)
                                (point)))
            (recenter 0))
          (let ((source-point (with-current-buffer source (point)))
                (source-window-state (chirp-capture-window-state source)))
            (cl-letf (((symbol-function 'chirp-media--render-image-buffer)
                       (lambda (buffer media-list index title)
                         (with-current-buffer buffer
                           (chirp-media-view-mode)
                           (setq-local chirp--media-list media-list)
                           (setq-local chirp--media-index index)
                           (setq-local chirp--media-title title)
                           (setq-local chirp--view-title title))
                         (chirp-display-buffer buffer))))
              (chirp-media-open
               '((:type "photo" :url "https://example.com/photo.jpg"))
               0
               "Media"
               viewer))
            (with-current-buffer viewer
              (chirp-media-quit))
            (should (eq (window-buffer (selected-window)) source))
            (with-current-buffer source
              (should (= (point) source-point)))
            (should (= (window-point (selected-window)) source-point))
            (should
             (= (window-start (selected-window))
                (with-current-buffer source
                  (chirp-point-position-from-anchor
                   (plist-get source-window-state :start-anchor)))))))
      (dolist (buffer (list source viewer))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest chirp-media-open-video-launches-player-with-pipe-connection ()
  "Opening video media should launch the external player without a PTY."
  (let ((chirp-video-player-command "/usr/bin/mpv")
        (chirp-video-playback-max-bitrate 2176000)
        (source (generate-new-buffer " *chirp-video-source*"))
        captured-command
        captured-connection-type
        captured-query-flag)
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer source)
          (with-current-buffer source
            (chirp-view-mode))
          (cl-letf (((symbol-function 'make-process)
                     (lambda (&rest args)
                       (setq captured-command (plist-get args :command))
                       (setq captured-connection-type (plist-get args :connection-type))
                       'fake-process))
                    ((symbol-function 'set-process-query-on-exit-flag)
                     (lambda (_process flag)
                       (setq captured-query-flag flag))))
            (chirp-media-open
             '((:type "video"
                :url "https://example.com/high.mp4"
                :variants ((:url "https://example.com/high.mp4" :bitrate 4096000)
                           (:url "https://example.com/mid.mp4" :bitrate 2176000)
                           (:url "https://example.com/low.mp4" :bitrate 832000))))
             0
             "Media"))
          (should (equal captured-command
                         '("/usr/bin/mpv" "https://example.com/mid.mp4")))
          (should (eq captured-connection-type 'pipe))
          (should (eq captured-query-flag nil)))
      (dolist (buffer (list source))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest chirp-media-play-launches-configured-player ()
  "Media viewer playback should launch the configured external player on demand."
  (let ((chirp-video-player-command "/usr/bin/mpv")
        (chirp-video-playback-max-bitrate 2176000)
        captured-command
        captured-query-flag)
    (with-temp-buffer
      (chirp-media-view-mode)
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
        (chirp-media-play)))
    (should (equal captured-command
                   '("/usr/bin/mpv" "https://example.com/anim-low.mp4")))
    (should (eq captured-query-flag nil))))

(ert-deftest chirp-media-play-launches-mpv-with-configured-window-size ()
  "mpv playback should honor `chirp-video-player-window-size'."
  (let ((chirp-video-player-command "/usr/bin/mpv")
        (chirp-video-player-window-size '(1280 . 720))
        captured-command)
    (with-temp-buffer
      (chirp-media-view-mode)
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
        (chirp-media-play)))
    (should (equal captured-command
                   '("/usr/bin/mpv" "--geometry=1280x720" "https://example.com/video.mp4")))))

(ert-deftest chirp-media-play-falls-back-to-browser-when-player-is-disabled ()
  "When no external player is configured, Chirp should browse the media URL."
  (let ((chirp-video-player-command nil)
        browsed-url)
    (with-temp-buffer
      (chirp-media-view-mode)
      (setq-local chirp--media-list '((:type "animated_gif"
                                       :url "https://example.com/anim.mp4")))
      (setq-local chirp--media-index 0)
      (cl-letf (((symbol-function 'browse-url)
                 (lambda (url &rest _args)
                   (setq browsed-url url))))
        (chirp-media-play)))
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
      (chirp-media-view-mode)
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
      (chirp-media-view-mode)
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

(ert-deftest chirp-projection-text-scale-requests-sync ()
  "Text scale should invalidate an Appkit projection instead of refetching."
  (let (view)
    (unwind-protect
        (let ((state (list :type 'collection
                           :query (list :kind 'bookmarks)
                           :items nil
                           :title "Bookmarks"
                           :refresh #'ignore
                           :status (list :phase 'idle :message nil)
                           :expanded-tweet-ids (make-hash-table :test #'equal))))
          (setq view (chirp-open-projection-view
                      :id (list 'collection 'scale-test)
                      :title "Bookmarks"
                      :state state
                      :sync-function #'chirp-timeline--sync
                      :printer #'chirp-render-print-tweet-row))
          (with-current-buffer (appkit-view-buffer view)
            (text-scale-increase 2)
            (let ((amount text-scale-mode-amount)
                  requested)
              (cl-letf (((symbol-function 'appkit-request-sync)
                         (lambda (live &rest _args)
                           (setq requested live))))
                (chirp--on-text-scale-change))
              (should (eq requested view))
              (should (equal amount text-scale-mode-amount))
              (should (bound-and-true-p text-scale-mode)))))
      (chirp-stop))))

(provide 'chirp-media-test)

;;; chirp-media-test.el ends here
