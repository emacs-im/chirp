;;; chirp-render-test.el --- Tests for Chirp rendering helpers -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'appkit-ui)
(require 'chirp-core)
(require 'chirp-render)
(require 'chirp-actions)
(require 'chirp-thread)

(defun chirp-test--face-member-p (face value)
  "Return non-nil when FACE appears in text property VALUE."
  (cond
   ((eq value face) t)
   ((listp value)
    (or (memq face value)
        (cl-some (lambda (item)
                   (chirp-test--face-member-p face item))
                 value)))
   (t nil)))

(defun chirp-test--slice-displays ()
  "Return (POSITION . DISPLAY) pairs for image slices in the current buffer."
  (let (result)
    (dotimes (offset (buffer-size))
      (let* ((position (1+ offset))
             (display (get-text-property position 'display)))
        (when (eq (car-safe (car-safe display)) 'slice)
          (push (cons position display) result))))
    (nreverse result)))

(defun chirp-test--discussion-row (tweet &optional focus-p parent-key depth role)
  "Return a discussion row for TWEET in render tests."
  (list :key (list 'tweet (plist-get tweet :id))
        :parent-key parent-key
        :depth (or depth 0)
        :role (or role (and focus-p 'focus) 'tree)
        :focus-p focus-p
        :tweet tweet))

(defun chirp-test--insert-tweet-list (tweets)
  "Insert TWEETS through the production row renderer."
  (let (previous)
    (dolist (tweet tweets)
      (chirp-render-insert-tweet-row tweet previous)
      (setq previous tweet))))

(defun chirp-test--sample-article-tweet ()
  "Return a normalized tweet payload with article metadata."
  (chirp-normalize-tweet
   '(("id" . "123")
     ("text" . "Read this https://t.co/demo")
     ("urls" . ("https://example.com/article"))
     ("articleTitle" . "Longform title")
     ("articleText" . "First paragraph with [details](https://example.com/article).\n\nSecond paragraph.")
     ("author" . (("screenName" . "alice")
                  ("name" . "Alice"))))))

(defun chirp-test--sample-article-tweet-with-image ()
  "Return a normalized article tweet that includes one inline image."
  (chirp-normalize-tweet
   '(("id" . "124")
     ("text" . "Longform https://t.co/demo")
     ("urls" . ("https://example.com/article"))
     ("articleTitle" . "Longform title")
     ("articleText" . "First paragraph.\n\n![Cover](https://example.com/cover.jpg)\n\nSecond paragraph.")
     ("author" . (("screenName" . "alice")
                  ("name" . "Alice"))))))

(defun chirp-test--sample-quoted-tweet ()
  "Return a normalized tweet payload with a quoted tweet."
  (chirp-normalize-tweet
   '(("id" . "999")
     ("text" . "Commentary https://t.co/quoted")
     ("urls" . ("https://x.com/bob/status/456"))
     ("author" . (("screenName" . "alice")
                  ("name" . "Alice")))
     ("quotedTweet" . (("id" . "456")
                       ("text" . "Quoted body text that is intentionally long enough to be shown as a short preview instead of the entire post verbatim.")
                       ("createdAt" . "QUOTE-TIME")
                       ("inReplyToScreenName" . "parent")
                       ("author" . (("screenName" . "bob")
                                    ("name" . "Bob"))))))))

(defun chirp-test--sample-quoted-tweet-with-media ()
  "Return a normalized tweet payload whose quoted tweet has media."
  (chirp-normalize-tweet
   '(("id" . "998")
     ("text" . "Commentary https://t.co/quoted")
     ("urls" . ("https://x.com/bob/status/456"))
     ("author" . (("screenName" . "alice")
                  ("name" . "Alice")))
     ("quotedTweet" . (("id" . "456")
                       ("text" . "")
                       ("author" . (("screenName" . "bob")
                                    ("name" . "Bob")))
                       ("media" . ((("type" . "photo")
                                    ("url" . "https://example.com/quoted.jpg")))))))))

(defun chirp-test--sample-retweeted-tweet ()
  "Return a normalized tweet payload with retweet social context."
  (chirp-normalize-tweet
   '(("id" . "321")
     ("text" . "Boosted post")
     ("retweetedBy" . "dotey")
     ("author" . (("screenName" . "alice")
                  ("name" . "Alice"))))))

(defun chirp-test--sample-adjacent-reply-tweets ()
  "Return two tweets where the second replies to the first."
  (list
   (chirp-normalize-tweet
    '(("id" . "100")
      ("text" . "Parent body text")
      ("author" . (("screenName" . "dingyi")
                   ("name" . "Ding")))))
   (chirp-normalize-tweet
    '(("id" . "101")
      ("text" . "Reply body text")
      ("inReplyToStatusId" . "100")
      ("inReplyToScreenName" . "dingyi")
      ("author" . (("screenName" . "nowazhu")
                   ("name" . "Nowa")))))))

(defun chirp-test--sample-adjacent-reply-tweets-with-handle-fallback ()
  "Return two tweets linked by handle and conversation metadata."
  (list
   (chirp-normalize-tweet
    '(("id" . "200")
      ("conversationId" . "200")
      ("text" . "Parent body text")
      ("author" . (("screenName" . "dingyi")
                   ("name" . "Ding")))))
   (chirp-normalize-tweet
    '(("id" . "201")
      ("conversationId" . "200")
      ("text" . "Reply body text")
      ("inReplyToScreenName" . "dingyi")
      ("author" . (("screenName" . "nowazhu")
                   ("name" . "Nowa")))))))

(defun chirp-test--sample-note-tweet-with-entity-links ()
  "Return a normalized note tweet whose expanded URLs live in entity metadata."
  (chirp-normalize-tweet
   '(("id" . "777")
     ("author" . (("screenName" . "alice")
                  ("name" . "Alice")))
     ("note_tweet" . (("note_tweet_results" . (("result" . (("text" . "GitHub仓库 https://t.co/repo\n在线阅读 https://t.co/read")
                                                            ("entity_set" . (("urls" . ((("expanded_url" . "https://github.com/example/project"))
                                                                                        (("expanded_url" . "https://example.com/read")))))))))))))))

(defun chirp-test--sample-tweet-with-incomplete-expanded-urls ()
  "Return a normalized tweet whose short links outnumber expanded URLs."
  (chirp-normalize-tweet
   '(("id" . "778")
     ("text" . "GitHub仓库 https://t.co/repo 在线阅读 https://t.co/read")
     ("urls" . ("https://github.com/example/project"))
     ("author" . (("screenName" . "alice")
                  ("name" . "Alice"))))))

(ert-deftest chirp-normalize-tweet-handles-x-web-graphql-shape ()
  "X web timeline results should retain authors and legacy media."
  (let* ((media
          '(("type" . "photo")
            ("url" . "https://t.co/photo-link")
            ("media_url_https" .
             "https://pbs.twimg.com/media/photo.jpg?format=jpg")
            ("ext_alt_text" . "Alt text")
            ("original_info" . (("width" . 1200)
                                ("height" . 800)))))
         (author
          '(("id" . "VXNlcjo0Mg==")
            ("rest_id" . "42")
            ("core" . (("name" . "Alice")
                       ("screen_name" . "alice")))
            ("avatar" .
             (("image_url" .
               "https://pbs.twimg.com/profile_images/alice.jpg")))))
         (tweet
          (chirp-normalize-tweet
           `(("id" . "VHdlZXQ6MTIz")
             ("rest_id" . "123")
             ("legacy" . (("full_text" . "Direct GraphQL payload")
                          ("bookmark_count" . 16)
                          ("extended_entities" . (("media" . (,media))))))
             ("views" . (("count" . 10286969)
                         ("state" . "EnabledWithCount")))
             ("core" . (("user_results" . (("result" . ,author)))))))))
    (should (equal (plist-get tweet :id) "123"))
    (should (equal (plist-get tweet :text) "Direct GraphQL payload"))
    (should (equal (plist-get tweet :author-name) "Alice"))
    (should (equal (plist-get tweet :author-handle) "alice"))
    (should (equal (plist-get tweet :author-avatar-url)
                   "https://pbs.twimg.com/profile_images/alice.jpg"))
    (let ((normalized-media (car (plist-get tweet :media))))
      (should (equal (plist-get normalized-media :url)
                     "https://pbs.twimg.com/media/photo.jpg?format=jpg"))
      (should (equal (plist-get normalized-media :alt) "Alt text"))
      (should (= (plist-get normalized-media :width) 1200))
      (should (= (plist-get normalized-media :height) 800)))
    (should (= (plist-get tweet :bookmark-count) 16))
    (should (= (plist-get tweet :view-count) 10286969))))

(ert-deftest chirp-normalize-tweet-preserves-reply-control-envelope ()
  "Tweet visibility wrappers should retain reply-control metadata."
  (let* ((raw
          '(("__typename" . "TweetWithVisibilityResults")
            ("tweet" . (("rest_id" . "123")
                        ("legacy" . (("full_text" . "Restricted")
                                     ("conversation_control" .
                                      (("mode" . "ByInvitation")))))))
            ("limitedActionResults" .
             (("limited_actions" . ((("action" . "Reply"))))))))
         (tweet (chirp-normalize-tweet raw)))
    (should (chirp-tweet-like-p raw))
    (should (equal (plist-get tweet :reply-control-mode) "ByInvitation"))
    (should (plist-get tweet :reply-limited-p))))

(ert-deftest chirp-normalize-tweet-preserves-edit-history-metadata ()
  "Edited tweets should expose stable version IDs from both X shapes."
  (let ((latest
         (chirp-normalize-tweet
          '(("rest_id" . "200")
            ("legacy" . (("full_text" . "Latest")))
            ("edit_control" .
             (("edit_control_initial" .
               (("edit_tweet_ids" . ("100" "200"))))
              ("initial_tweet_id" . "100"))))))
        (initial
         (chirp-normalize-tweet
          '(("rest_id" . "100")
            ("legacy" . (("full_text" . "Initial")))
            ("edit_control" .
             (("edit_tweet_ids" . ("100" "200")))))))
        (single-version
         (chirp-normalize-tweet
          '(("rest_id" . "300")
            ("legacy" . (("full_text" . "Original")))
            ("edit_control" .
             (("edit_tweet_ids" . ("300")))))))
        (unedited
         (chirp-normalize-tweet
          '(("rest_id" . "400")
            ("legacy" . (("full_text" . "No metadata")))))))
    (dolist (tweet (list latest initial))
      (should (equal (plist-get tweet :edit-history-ids)
                     '("100" "200")))
      (should (equal (plist-get tweet :edit-history-initial-id) "100"))
      (should (plist-get tweet :edited-p)))
    (should (equal (plist-get single-version :edit-history-ids)
                   '("300")))
    (should-not (plist-get single-version :edited-p))
    (should-not (plist-get unedited :edit-history-ids))
    (should-not (plist-get unedited :edited-p))))

(ert-deftest chirp-normalize-tweet-strips-short-urls-and-keeps-article-fields ()
  "Short links should be removed from display text while article data survives."
  (let ((tweet (chirp-test--sample-article-tweet)))
    (should (equal (plist-get tweet :text) "Read this"))
    (should (equal (plist-get tweet :raw-text) "Read this https://t.co/demo"))
    (should (equal (plist-get tweet :urls) '("https://example.com/article")))
    (should (equal (plist-get tweet :article-title) "Longform title"))
    (should (equal (chirp-tweet-article-preview tweet 80)
                   "First paragraph with details."))))

(ert-deftest chirp-normalize-tweet-renders-x-article-rich-content ()
  "Direct X article content should preserve structure, links, and images."
  (let* ((raw
          '(("rest_id" . "123")
            ("legacy" . (("full_text" . "Article preview")))
            ("article" .
             (("article_results" .
               (("result" .
                 (("title" . "Longform title")
                  ("content_state" .
                   (("blocks" .
                     ((("type" . "header-one") ("text" . "Introduction"))
                      (("type" . "unstyled")
                       ("text" . "Read docs")
                       ("entityRanges" .
                        ((("key" . 0) ("offset" . 5) ("length" . 4)))))
                      (("type" . "unordered-list-item")
                       ("text" . "First item"))
                      (("type" . "atomic")
                       ("text" . "")
                       ("entityRanges" .
                        ((("key" . 1) ("offset" . 0) ("length" . 0)))))))
                    ("entityMap" .
                     (("0" . (("type" . "LINK")
                              ("data" .
                               (("url" . "https://example.com/docs")))))
                      ("1" . (("type" . "IMAGE")
                              ("data" .
                               (("caption" . "Cover")
                                ("mediaItems" .
                                 ((("mediaId" . "media-1"))
                                  (("mediaId" . "media-2")
                                   ("caption" . "Detail"))))))))))))
                  ("media_entities" .

                   ((("media_id" . "media-1")
                     ("media_info" .
                      (("original_img_url" .
                        "https://pbs.twimg.com/media/cover.jpg"))))
                    (("media_id" . "media-2")
                     ("media_info" .
                      (("original_img_url" .
                        "https://pbs.twimg.com/media/detail.jpg"))))))))))))))
         (tweet (chirp-normalize-tweet raw)))
    (should (equal (plist-get tweet :article-title) "Longform title"))
    (should
     (equal (plist-get tweet :article-text)
            (concat "# Introduction\n\n"
                    "Read [docs](https://example.com/docs)\n\n"
                    "- First item\n\n"
                    "![Cover](https://pbs.twimg.com/media/cover.jpg)\n\n"
                    "![Detail](https://pbs.twimg.com/media/detail.jpg)")))))

(ert-deftest chirp-render-insert-tweet-shows-cached-translation ()
  "A cached translation should render directly below the original text."
  (clrhash (chirp--session-tweet-state-overrides (chirp--session)))
  (unwind-protect
      (progn
        (chirp-set-tweet-state-override "123" :translation "你好")
        (chirp-set-tweet-state-override "123" :translation-language "zh")
        (let ((tweet
               (chirp-apply-tweet-state-overrides
                (chirp-normalize-tweet
                 '(("id" . "123")
                   ("text" . "Hello")
                   ("author" . (("screenName" . "alice")
                                ("name" . "Alice"))))))))
          (with-temp-buffer
            (chirp-render-insert-tweet tweet)
            (should (string-match-p "Hello\nTranslation · zh\n你好"
                                    (buffer-string))))))
    (clrhash (chirp--session-tweet-state-overrides (chirp--session)))))

(ert-deftest chirp-article-segments-split-inline-images-out-of-body-text ()
  "Article helpers should split Markdown image paragraphs into media items."
  (let* ((tweet (chirp-test--sample-article-tweet-with-image))
         (segments (chirp-article-segments (plist-get tweet :article-text)))
         (images (chirp-tweet-article-images tweet)))
    (should (= (length segments) 3))
    (should (equal (mapcar (lambda (segment) (plist-get segment :type)) segments)
                   '(text image text)))
    (should (equal (plist-get (car images) :url)
                   "https://example.com/cover.jpg"))))

(ert-deftest chirp-normalize-tweet-keeps-quoted-tweet-and-filters-quote-link ()
  "Quoted tweets should survive normalization without duplicate permalinks."
  (let* ((tweet (chirp-test--sample-quoted-tweet))
         (quoted (plist-get tweet :quoted-tweet)))
    (should quoted)
    (should (equal (plist-get quoted :id) "456"))
    (should (equal (plist-get tweet :text) "Commentary"))
    (should (string-match-p "Quoted body text" (plist-get quoted :text)))
    (should-not (plist-get tweet :urls))))

(ert-deftest chirp-normalize-tweet-hides-leading-reply-mentions ()
  "Reply-chain @handles should not appear in the visible tweet text."
  (let ((tweet (chirp-normalize-tweet
                '(("id" . "1")
                  ("text" . "@alice @bob hello there")
                  ("inReplyToStatusId" . "0")
                  ("inReplyToScreenName" . "alice")))))
    (should (equal (plist-get tweet :text) "hello there"))
    (should (equal (plist-get tweet :raw-text) "@alice @bob hello there"))))

(ert-deftest chirp-normalize-tweet-uses-display-text-range ()
  "X display_text_range should win over stripping every leading mention."
  (let ((tweet (chirp-normalize-tweet
                '(("id" . "1")
                  ("full_text" . "@alice @bob check this")
                  ("display_text_range" . (7 22))
                  ("inReplyToScreenName" . "alice")))))
    (should (equal (plist-get tweet :text) "@bob check this"))))

(ert-deftest chirp-normalize-tweet-keeps-leading-mention-on-original-posts ()
  "An original post may start with an @mention that the author typed."
  (let ((tweet (chirp-normalize-tweet
                '(("id" . "1")
                  ("text" . "@alice hello")))))
    (should (equal (plist-get tweet :text) "@alice hello"))))

(ert-deftest chirp-normalize-tweet-preserves-related-timeline-context-only ()
  "Known timeline context should normalize without interning arbitrary values."
  (let ((related
         (chirp-normalize-tweet
          '(("id" . "123")
            ("text" . "Related body")
            ("timelineContext" . "related"))))
        (snake-related
         (chirp-normalize-tweet
          '(("id" . "234")
            ("text" . "Related body")
            ("timeline_context" . "related"))))
        (unknown
         (chirp-normalize-tweet
          '(("id" . "456")
            ("text" . "Unknown body")
            ("timelineContext" . "future-context")))))
    (should (eq (plist-get related :timeline-context) 'related))
    (should (eq (plist-get snake-related :timeline-context) 'related))
    (should-not (plist-get unknown :timeline-context))))

(ert-deftest chirp-normalize-tweet-filters-own-permalink ()
  "A tweet's own permalink should not be rendered as an expanded link."
  (dolist (permalink '("https://twitter.com/alice/status/123?ref_src=twsrc"
                       "https://x.com/i/web/status/123"))
    (let ((tweet (chirp-normalize-tweet
                  `(("id" . "123")
                    ("text" . "Original body https://t.co/self")
                    ("urls" . (,permalink))
                    ("author" . (("screenName" . "alice")
                                 ("name" . "Alice")))))))
      (should (equal (plist-get tweet :text) "Original body"))
      (should-not (plist-get tweet :urls)))))

(ert-deftest chirp-normalize-tweet-hides-photo-and-video-links ()
  "Media placeholders and resource URLs should not be displayed as links."
  (dolist (media '((("type" . "photo")
                    ("url" . "https://pbs.twimg.com/media/example.jpg"))
                   (("type" . "video")
                    ("url" . "https://video.twimg.com/ext_tw_video/example.mp4"))))
    (let* ((media-url (cdr (assoc "url" media)))
           (tweet (chirp-normalize-tweet
                   `(("id" . "123")
                     ("text" . "External https://t.co/site Media https://t.co/media")
                     ("urls" . ("https://example.com/article" ,media-url))
                     ("media" . (,media))
                     ("author" . (("screenName" . "alice")
                                  ("name" . "Alice")))))))
      (should (equal (plist-get tweet :text) "External Media"))
      (should (equal (plist-get tweet :urls)
                     '("https://example.com/article"))))))

(ert-deftest chirp-normalize-tweet-strips-unexpanded-media-placeholder ()
  "A rendered media item should cover its otherwise unexpanded short URL."
  (let ((tweet (chirp-normalize-tweet
                '(("id" . "123")
                  ("text" . "Photo https://t.co/media")
                  ("media" . ((("type" . "photo")
                               ("url" . "https://pbs.twimg.com/media/example.jpg"))))
                  ("author" . (("screenName" . "alice")
                               ("name" . "Alice")))))))
    (should (equal (plist-get tweet :text) "Photo"))
    (should-not (plist-get tweet :urls))))

(ert-deftest chirp-normalize-tweet-hides-known-media-host-without-metadata ()
  "A known media host should stay hidden without structured media metadata."
  (let ((tweet (chirp-normalize-tweet
                '(("id" . "123")
                  ("text" . "Photo https://t.co/media")
                  ("urls" . ("https://pic.x.com/example"))
                  ("author" . (("screenName" . "alice")
                               ("name" . "Alice")))))))
    (should (equal (plist-get tweet :text) "Photo"))
    (should-not (plist-get tweet :urls))))

(ert-deftest chirp-normalize-tweet-extracts-multiple-note-tweet-links ()
  "Expanded URLs should survive even when they only appear in note-tweet entities."
  (let ((tweet (chirp-test--sample-note-tweet-with-entity-links)))
    (should (equal (plist-get tweet :text) "GitHub仓库\n在线阅读"))
    (should (equal (plist-get tweet :urls)
                   '("https://github.com/example/project"
                     "https://example.com/read")))))

(ert-deftest chirp-normalize-tweet-keeps-short-urls-when-expanded-links-are-incomplete ()
  "Display text should keep `t.co` placeholders when expansion coverage is incomplete."
  (let ((tweet (chirp-test--sample-tweet-with-incomplete-expanded-urls)))
    (should (equal (plist-get tweet :text)
                   "GitHub仓库 https://t.co/repo 在线阅读 https://t.co/read"))
    (should (equal (plist-get tweet :urls)
                   '("https://github.com/example/project")))))

(ert-deftest chirp-normalize-tweet-preserves-retweeted-by-handle ()
  "Structured tweets should preserve retweet social context handles."
  (let ((tweet (chirp-test--sample-retweeted-tweet)))
    (should (equal (plist-get tweet :retweeted-by) "dotey"))))

(ert-deftest chirp-normalize-tweet-unwraps-x-retweet-results ()
  "X retweet wrappers should render the original tweet with social context."
  (let* ((retweeter
          '(("rest_id" . "10")
            ("legacy" . (("screen_name" . "alice")
                         ("name" . "Alice")))))
         (author
          '(("rest_id" . "20")
            ("legacy" . (("screen_name" . "bob")
                         ("name" . "Bob")))))
         (original
          `(("rest_id" . "200")
            ("core" . (("user_results" . (("result" . ,author)))))
            ("legacy" . (("full_text" . "Original post")))))
         (tweet
          (chirp-normalize-tweet
           `(("rest_id" . "100")
             ("isPromoted" . t)
             ("core" . (("user_results" . (("result" . ,retweeter)))))
             ("legacy" .
              (("full_text" . "RT @bob: Original post")
               ("retweeted_status_result" . (("result" . ,original)))))))))
    (should (equal (plist-get tweet :id) "200"))
    (should (equal (plist-get tweet :text) "Original post"))
    (should (equal (plist-get tweet :author-handle) "bob"))
    (should (equal (plist-get tweet :retweeted-by) "alice"))
    (should (equal (plist-get tweet :retweeted-by-name) "Alice"))
    (should (plist-get tweet :promoted-p))))

(ert-deftest chirp-normalize-user-parses-structured-profile-payload-with-blank-name ()
  "Structured profile payloads should survive blank display-name fields."
  (let ((user (chirp-normalize-user
               '(("id" . "50683")
                 ("name" . "")
                 ("screenName" . "dingyi")
                 ("bio" . "promote")
                 ("followers" . 148033)
                 ("following" . 4908)
                 ("tweets" . 59745)
                 ("profileImageUrl" . "")
                 ("viewerFollowing" . t)
                 ("viewerFollowedBy" . chirp-json-false)))))
    (should user)
    (should (equal (plist-get user :handle) "dingyi"))
    (should (equal (plist-get user :name) "dingyi"))
    (should (equal (plist-get user :bio) "promote"))
    (should (= (plist-get user :followers) 148033))
    (should (= (plist-get user :posts) 59745))
    (should (plist-get user :viewer-following-p))
    (should-not (plist-get user :viewer-followed-by-p))))

(ert-deftest chirp-normalize-user-handles-current-x-profile-shape ()
  "Current X profile fields should retain biography and account counts."
  (let ((user
         (chirp-normalize-user
          '(("id" . "VXNlcjo0Mg==")
            ("rest_id" . "42")
            ("core" . (("name" . "Alice")
                       ("screen_name" . "alice")
                       ("created_at" . "Mon Jan 01 00:00:00 +0000 2024")))
            ("profile_bio" . (("description" . "Emacs user")))
            ("relationship_counts" . (("followers" . 120)
                                      ("following" . 30)))
            ("tweet_counts" . (("tweets" . 450)))
            ("avatar" . (("image_url" . "https://example.com/avatar.jpg")))
            ("relationship_perspectives" . (("following" . t)))))))
    (should (equal (plist-get user :id) "42"))
    (should (equal (plist-get user :handle) "alice"))
    (should (equal (plist-get user :bio) "Emacs user"))
    (should (= (plist-get user :followers) 120))
    (should (= (plist-get user :following) 30))
    (should (= (plist-get user :posts) 450))
    (should (plist-get user :viewer-following-p))))

(ert-deftest chirp-render-insert-tweet-renders-expanded-links-and-article-preview ()
  "Tweet rendering should show expanded links and article metadata."
  (let ((tweet (chirp-test--sample-article-tweet)))
    (with-temp-buffer
      (chirp-view-mode)
      (cl-letf (((symbol-function 'chirp-media-avatar-image) (lambda (&rest _args) nil))
                ((symbol-function 'chirp-media-thumbnail-image) (lambda (&rest _args) nil)))
        (let ((inhibit-read-only t))
          (chirp-render-insert-tweet tweet)))
      (let ((rendered (buffer-string)))
        (should (string-match-p "Longform title" rendered))
        (should (string-match-p "First paragraph with details\\." rendered))
        (should (string-match-p "https://example.com/article" rendered))
        (should-not (string-match-p "https://t\\.co/demo" rendered))))))

(ert-deftest chirp-open-at-point-expands-only-from-show-more ()
  "RET should expand Show more while tweet-body RET opens the thread."
  (let ((tweet (chirp-test--sample-article-tweet))
        opened-thread
        (rerender-count 0))
    (with-temp-buffer
      (chirp-view-mode)
      (cl-letf (((symbol-function 'chirp-media-avatar-image)
                 (lambda (&rest _args) nil))
                ((symbol-function 'chirp-media-thumbnail-image)
                 (lambda (&rest _args) nil))
                ((symbol-function 'chirp-thread-open-tweet)
                 (lambda (tweet)
                   (setq opened-thread (plist-get tweet :id)))))
        (let ((inhibit-read-only t))
          (chirp-render-insert-tweet tweet))
        (setq-local
         chirp--rerender-function
         (lambda ()
           (cl-incf rerender-count)
           (let ((inhibit-read-only t))
             (erase-buffer)
             (chirp-render-insert-tweet tweet))))
        (goto-char (point-min))
        (search-forward "Read this")
        (goto-char (match-beginning 0))
        (chirp-open-at-point)
        (should (equal opened-thread "123"))
        (should (zerop rerender-count))
        (setq opened-thread nil)
        (goto-char (point-min))
        (should (search-forward "Show more" nil t))
        (goto-char (match-beginning 0))
        (should (equal (get-text-property (point) 'chirp-expand-tweet-id)
                       "123"))
        (chirp-open-at-point)
        (should (= rerender-count 1))
        (should-not opened-thread)
        (should (gethash "123" chirp--expanded-tweet-ids))
        (should (string-match-p "Second paragraph" (buffer-string)))
        (should-not (string-match-p "Show more" (buffer-string)))))))

(ert-deftest chirp-render-insert-tweet-highlights-genuine-external-links ()
  "Genuine external links should highlight on hover and open themselves."
  (let ((tweet (chirp-normalize-tweet
                '(("id" . "123")
                  ("text" . "Read https://t.co/article")
                  ("urls" . ("https://example.com/article"))
                  ("author" . (("screenName" . "alice")
                               ("name" . "Alice")))))))
    (with-temp-buffer
      (chirp-view-mode)
      (cl-letf (((symbol-function 'chirp-media-avatar-image) (lambda (&rest _args) nil))
                ((symbol-function 'chirp-media-thumbnail-image) (lambda (&rest _args) nil)))
        (let ((inhibit-read-only t))
          (chirp-render-insert-tweet tweet)))
      (goto-char (point-min))
      (search-forward "https://example.com/article")
      (let ((position (match-beginning 0)))
        (should (eq (get-text-property position 'face) 'chirp-link-face))
        (should (eq (get-text-property position 'mouse-face) 'highlight))
        (should (equal (get-text-property position 'chirp-subentry-url)
                       "https://example.com/article"))))))

(ert-deftest chirp-render-insert-tweet-renders-multiple-note-tweet-links ()
  "Tweet rendering should show multiple expanded links extracted from note entities."
  (let ((tweet (chirp-test--sample-note-tweet-with-entity-links)))
    (with-temp-buffer
      (chirp-view-mode)
      (cl-letf (((symbol-function 'chirp-media-avatar-image) (lambda (&rest _args) nil))
                ((symbol-function 'chirp-media-thumbnail-image) (lambda (&rest _args) nil)))
        (let ((inhibit-read-only t))
          (chirp-render-insert-tweet tweet)))
      (let ((rendered (buffer-string)))
        (should (string-match-p "GitHub仓库" rendered))
        (should (string-match-p "在线阅读" rendered))
        (should (string-match-p "https://github.com/example/project" rendered))
        (should (string-match-p "https://example.com/read" rendered))
        (should-not (string-match-p "https://t\\.co/repo" rendered))
        (should-not (string-match-p "https://t\\.co/read" rendered))))))

(ert-deftest chirp-render-insert-tweet-keeps-short-urls-when-expanded-links-are-incomplete ()
  "Rendering should prefer visible short links over silently swallowing them."
  (let ((tweet (chirp-test--sample-tweet-with-incomplete-expanded-urls)))
    (with-temp-buffer
      (chirp-view-mode)
      (cl-letf (((symbol-function 'chirp-media-avatar-image) (lambda (&rest _args) nil))
                ((symbol-function 'chirp-media-thumbnail-image) (lambda (&rest _args) nil)))
        (let ((inhibit-read-only t))
          (chirp-render-insert-tweet tweet)))
      (let ((rendered (buffer-string)))
        (should (string-match-p "https://t\\.co/repo" rendered))
        (should (string-match-p "https://t\\.co/read" rendered))
        (should (string-match-p "https://github.com/example/project" rendered))))))

(defun chirp-test--sample-inline-url-tweet ()
  "Return a tweet whose URL entities replace list-item `t.co` placeholders."
  (let* ((text "See\n- https://t.co/aaa\n- https://t.co/bbb\n- ASD-STE100")
         (first (string-match "https://t.co/aaa" text))
         (second (string-match "https://t.co/bbb" text)))
    (chirp-normalize-tweet
     `(("id" . "2087")
       ("text" . ,text)
       ("entities" .
        (("urls" .
          ((("url" . "https://t.co/aaa")
            ("expanded_url" . "https://tbaggery.com/2008/04/19/a-note-about-git-commit-messages.html")
            ("display_url" . "tbaggery.com/2008/04/19/a-n...")
            ("indices" . (,first ,(+ first (length "https://t.co/aaa")))))
           (("url" . "https://t.co/bbb")
            ("expanded_url" . "https://cbea.ms/git-commit/")
            ("display_url" . "cbea.ms/git-commit/")
            ("indices" . (,second ,(+ second (length "https://t.co/bbb")))))))))
       ("author" . (("screenName" . "zackkanter")
                    ("name" . "Zack Kanter")))))))

(ert-deftest chirp-normalize-tweet-uses-code-point-entity-indices ()
  "GraphQL entity indices are Unicode code points, not UTF-16 units."
  (let* ((text (concat "FuckCraft Episode 4 Pinke's Gym Arc! [Pinke Anims] "
                       "Minecraft: pinke gym anal gangbang deepthroat sweaty "
                       "heat 30:06 fire! 🍑🍆💦🔥🥵\nFull uncut here 👇\n"
                       "https://t.co/ZC1eHiIdEj\n\n"
                       "#Rule34 #Minecraft #FuckCraft #PinkeAnims #NSFW "
                       "https://t.co/YqNlKANgce"))
         (tweet (chirp-normalize-tweet
                 `(("id" . "2087131921880871390")
                   ("full_text" . ,text)
                   ("display_text_range" . (0 217))
                   ("entities" .
                    (("urls" .
                      ((("url" . "https://t.co/ZC1eHiIdEj")
                        ("expanded_url" . "https://t.me/r34videoss")
                        ("display_url" . "t.me/r34videoss")
                        ("indices" . (145 168)))))
                     ("hashtags" .
                      ((("text" . "Rule34") ("indices" . (170 177)))
                       (("text" . "Minecraft") ("indices" . (178 188)))
                       (("text" . "FuckCraft") ("indices" . (189 199)))
                       (("text" . "PinkeAnims") ("indices" . (200 211)))
                       (("text" . "NSFW") ("indices" . (212 217)))))
                     ("timestamps" .
                      ((("text" . "30:06")
                        ("seconds" . 1806)
                        ("indices" . (109 114)))))
                     ("media" .
                      ((("url" . "https://t.co/YqNlKANgce")
                        ("expanded_url" . "https://x.com/Rule34XXX34/status/2087131921880871390/video/1")
                        ("display_url" . "pic.x.com/YqNlKANgce")
                        ("indices" . (218 241))
                        ("type" . "video"))))))
                   ("author" . (("screenName" . "Rule34XXX34")
                                ("name" . "Rule34XXX")))))))
    (should (string-match-p "Full uncut here" (plist-get tweet :text)))
    (should (string-match-p "t\\.me/r34videoss" (plist-get tweet :text)))
    (should-not (string-match-p "het\\.me" (plist-get tweet :text)))
    (should-not (string-match-p "HiIdEj" (plist-get tweet :text)))
    (should (string-match-p "#NSFW" (plist-get tweet :text)))
    (should (string-match-p "30:06" (plist-get tweet :text)))
    (should (member "NSFW" (plist-get tweet :hashtags)))
    (should (cl-find-if (lambda (entity)
                          (and (eq (plist-get entity :kind) 'timestamp)
                               (equal (plist-get entity :tag) "30:06")))
                        (plist-get tweet :text-entities)))))

(ert-deftest chirp-normalize-tweet-inlines-display-urls-from-entities ()
  "URL entities should replace `t.co` in place with X's display_url."
  (let ((tweet (chirp-test--sample-inline-url-tweet)))
    (should (equal (plist-get tweet :text)
                   "See\n- tbaggery.com/2008/04/19/a-n...\n- cbea.ms/git-commit/\n- ASD-STE100"))
    (let ((entities (plist-get tweet :text-entities)))
      (should (equal (mapcar (lambda (entity)
                               (list (plist-get entity :kind)
                                     (plist-get entity :url)))
                             entities)
                     '((url "https://tbaggery.com/2008/04/19/a-note-about-git-commit-messages.html")
                       (url "https://cbea.ms/git-commit/"))))
      (should (equal (substring (plist-get tweet :text)
                                (plist-get (car entities) :start)
                                (plist-get (car entities) :end))
                     "tbaggery.com/2008/04/19/a-n..."))
      (should (equal (substring (plist-get tweet :text)
                                (plist-get (cadr entities) :start)
                                (plist-get (cadr entities) :end))
                     "cbea.ms/git-commit/")))))

(ert-deftest chirp-render-insert-tweet-keeps-inline-display-urls-in-order ()
  "Inline display URLs should stay on their list lines instead of moving below."
  (let ((tweet (chirp-test--sample-inline-url-tweet))
        opened-url)
    (with-temp-buffer
      (chirp-view-mode)
      (cl-letf (((symbol-function 'chirp-media-avatar-image)
                 (lambda (&rest _args) nil))
                ((symbol-function 'browse-url)
                 (lambda (url &rest _args)
                   (setq opened-url url))))
        (let ((inhibit-read-only t))
          (chirp-render-insert-tweet tweet))
        (let ((rendered (buffer-string)))
          (should (string-match-p
                   "- tbaggery\\.com/2008/04/19/a-n\\.\\.\\.\n- cbea\\.ms/git-commit/\n- ASD-STE100"
                   rendered))
          (should-not (string-match-p "https://tbaggery\\.com" rendered))
          (should-not (string-match-p "https://t\\.co/" rendered)))
        (goto-char (point-min))
        (search-forward "cbea.ms/git-commit/")
        (goto-char (match-beginning 0))
        (chirp-open-at-point)
        (should (equal opened-url "https://cbea.ms/git-commit/"))))))

(ert-deftest chirp-render-insert-tweet-renders-retweet-social-context ()
  "Tweet rendering should show who retweeted the current post."
  (let ((tweet (chirp-test--sample-retweeted-tweet)))
    (with-temp-buffer
      (chirp-view-mode)
      (cl-letf (((symbol-function 'chirp-media-avatar-image) (lambda (&rest _args) nil))
                ((symbol-function 'chirp-media-thumbnail-image) (lambda (&rest _args) nil)))
        (let ((inhibit-read-only t))
          (chirp-render-insert-tweet tweet)))
      (goto-char (point-min))
      (should (search-forward "retweeted by dotey" nil t))
      (should (chirp-test--face-member-p
               'chirp-social-context-face
               (get-text-property (match-beginning 0) 'face)))
      (should (functionp (appkit-ui-action-at (match-beginning 0)))))))

(ert-deftest chirp-open-at-point-opens-retweeter-profile-from-social-context ()
  "RET on the retweeted-by line should open the retweeter, not the tweet."
  (let ((tweet (chirp-test--sample-retweeted-tweet))
        opened-profile
        opened-thread)
    (with-temp-buffer
      (chirp-view-mode)
      (cl-letf (((symbol-function 'chirp-media-avatar-image) (lambda (&rest _args) nil))
                ((symbol-function 'chirp-profile-open)
                 (lambda (handle &optional _mode)
                   (setq opened-profile handle)))
                ((symbol-function 'chirp-thread-open-tweet)
                 (lambda (&rest args)
                   (setq opened-thread args))))
        (let ((inhibit-read-only t))
          (chirp-render-insert-tweet tweet))
        (goto-char (point-min))
        (search-forward "retweeted by")
        (chirp-open-at-point)))
    (should (equal opened-profile "dotey"))
    (should-not opened-thread)))

(ert-deftest chirp-render-shows-edit-history-action-only-for-edited-tweets ()
  "Only a tweet with multiple X versions should expose edit history."
  (let ((edited
         '(:kind tweet :id "200" :text "Latest"
           :author-name "Alice" :edit-history-ids ("100" "200")
           :edit-history-initial-id "100" :edited-p t))
        (original
         '(:kind tweet :id "300" :text "Original"
           :author-name "Alice"))
        opened)
    (with-temp-buffer
      (chirp-view-mode)
      (cl-letf (((symbol-function 'chirp-media-avatar-image)
                 (lambda (&rest _args) nil))
                ((symbol-function 'chirp-edit-history-open-tweet)
                 (lambda (tweet) (setq opened tweet))))
        (let ((inhibit-read-only t))
          (chirp-render-insert-tweet edited)
          (chirp-render-insert-tweet original))
        (goto-char (point-min))
        (search-forward "Edited · 2 versions")
        (let ((edited-start (match-beginning 0))
              (edited-end (match-end 0)))
          (goto-char edited-start)
          (chirp-open-at-point)
          (should (eq opened edited))
          (goto-char edited-end)
          (should-not (search-forward "Edited" nil t)))))))

(ert-deftest chirp-render-edit-history-stale-row-has-no-write-actions ()
  "A stale version should retain read actions but expose no mutation action."
  (let ((tweet
         '(:kind tweet :id "100" :text "Visit example.com"
           :author-name "Alice" :author-handle "alice"
           :created-at "Thu Aug 13 08:35:37 +0000 2026"
           :text-entities
           ((:kind url :start 6 :end 17 :url "https://example.com"))
           :reply-count 1 :retweet-count 2 :like-count 3
           :quote-count 4 :bookmark-count 5 :view-count 6))
        opened-url)
    (with-temp-buffer
      (chirp-view-mode)
      (cl-letf (((symbol-function 'chirp-media-avatar-image)
                 (lambda (&rest _args) nil))
                ((symbol-function 'browse-url)
                 (lambda (url &rest _args) (setq opened-url url))))
        (let ((inhibit-read-only t))
          (chirp-render-insert-edit-history-row
           (list :key '(edit-version "100")
                 :tweet tweet :latest-p nil
                 :section "Version history")))
        (goto-char (point-min))
        (search-forward "example.com")
        (goto-char (match-beginning 0))
        (chirp-open-at-point)
        (should (equal opened-url "https://example.com"))
        (dolist (label '("Replies" "RT" "Likes" "Quotes" "Bookmarks"))
          (goto-char (point-min))
          (search-forward label)
          (should-not (appkit-ui-action-at (match-beginning 0))))
        (goto-char (point-min))
        (should (search-forward "Version history" nil t))
        (should-not (search-forward "Edited" nil t))))))

(ert-deftest chirp-normalize-tweet-extracts-mentions-and-hashtags ()
  "Tweet entities should keep mentions and hashtags for inline actions."
  (let ((tweet (chirp-normalize-tweet
                '(("id" . "55")
                  ("text" . "Hi @bob see #emacs")
                  ("author" . (("screenName" . "alice")
                               ("name" . "Alice")))
                  ("entities" .
                   (("user_mentions" . ((("screen_name" . "bob")
                                         ("name" . "Bob")
                                         ("indices" . (3 7)))))
                    ("hashtags" . ((("text" . "emacs")
                                    ("indices" . (12 18)))))))))))
    (should (equal (plist-get (car (plist-get tweet :mentions)) :handle) "bob"))
    (should (equal (plist-get (car (plist-get tweet :mentions)) :name) "Bob"))
    (should (equal (plist-get tweet :hashtags) '("emacs")))
    (should (equal (mapcar (lambda (entity)
                             (list (plist-get entity :kind)
                                   (or (plist-get entity :handle)
                                       (plist-get entity :tag))
                                   (plist-get entity :start)
                                   (plist-get entity :end)))
                           (plist-get tweet :text-entities))
                   '((mention "bob" 3 7)
                     (hashtag "emacs" 12 18))))))

(ert-deftest chirp-open-at-point-follows-mentions-and-hashtags ()
  "RET on an inline @handle or #hashtag should follow that target."
  (let ((tweet '(:kind tweet
                 :id "55"
                 :text "Hi @bob see #emacs"
                 :author-name "Alice"
                 :author-handle "alice"
                 :text-entities
                 ((:kind mention :handle "bob" :name "Bob" :start 3 :end 7)
                  (:kind hashtag :tag "emacs" :start 12 :end 18))))
        opened-profile
        opened-search
        opened-thread)
    (with-temp-buffer
      (chirp-view-mode)
      (cl-letf (((symbol-function 'chirp-media-avatar-image) (lambda (&rest _args) nil))
                ((symbol-function 'chirp-profile-open)
                 (lambda (handle &optional _mode)
                   (setq opened-profile handle)))
                ((symbol-function 'chirp-timeline-open-search)
                 (lambda (query)
                   (setq opened-search query)))
                ((symbol-function 'chirp-thread-open-tweet)
                 (lambda (&rest args)
                   (setq opened-thread args))))
        (let ((inhibit-read-only t))
          (chirp-render-insert-tweet tweet))
        (goto-char (point-min))
        (search-forward "@bob")
        (goto-char (match-beginning 0))
        (chirp-open-at-point)
        (should (equal opened-profile "bob"))
        (should-not opened-thread)
        (goto-char (point-min))
        (search-forward "#emacs")
        (goto-char (match-beginning 0))
        (chirp-open-at-point)
        (should (equal opened-search "#emacs"))
        (should-not opened-thread)))))

(ert-deftest chirp-open-at-point-ignores-unparsed-at-words ()
  "A visible @word is not a profile action unless X sent a mention entity."
  (let ((tweet '(:kind tweet
                 :id "56"
                 :text "email foo@bar.com and @notalink"
                 :author-name "Alice"
                 :author-handle "alice"))
        opened-profile)
    (with-temp-buffer
      (chirp-view-mode)
      (cl-letf (((symbol-function 'chirp-media-avatar-image)
                 (lambda (&rest _args) nil))
                ((symbol-function 'chirp-profile-open)
                 (lambda (handle &optional _mode)
                   (setq opened-profile handle)))
                ((symbol-function 'chirp-thread-open-tweet)
                 (lambda (&rest _args) nil)))
        (let ((inhibit-read-only t))
          (chirp-render-insert-tweet tweet))
        (goto-char (point-min))
        (search-forward "@notalink")
        (goto-char (match-beginning 0))
        (should-not (appkit-ui-action-at))
        (chirp-open-at-point)
        (should-not opened-profile)))))

(ert-deftest chirp-render-insert-tweet-right-aligns-compact-time ()
  "Tweet headings should align localized time to the view's right edge."
  (let* ((chirp-language "zh-CN")
         (chirp-show-avatars nil)
         (now (encode-time 0 0 12 13 8 2026))
         (created-at
          (format-time-string
           "%Y-%m-%dT%H:%M:%S%z"
           (time-subtract now (seconds-to-time (* 6 3600)))))
         (tweet
          (list :kind 'tweet :id "time-1" :text "Body"
                :author-name "Alice" :author-handle "alice"
                :created-at created-at
                :reply-count 0 :retweet-count 0 :like-count 0
                :quote-count 0 :bookmark-count 0 :view-count 0)))
    (with-temp-buffer
      (chirp-view-mode)
      (setq-local fill-column 40)
      (cl-letf (((symbol-function 'current-time) (lambda () now)))
        (let ((inhibit-read-only t))
          (chirp-render-insert-tweet tweet)))
      (goto-char (point-min))
      (search-forward "6小时")
      (let ((spacer (1- (match-beginning 0))))
        (should
         (equal
          (get-text-property spacer 'display)
          `(space :align-to
                  (- right (,(string-width "6小时") . width)))))))))

(ert-deftest chirp-render-insert-tweet-can-hide-avatar-and-keep-author-text ()
  "Hiding avatars should leave the display name and handle visible."
  (let ((chirp-show-avatars nil)
        (tweet (chirp-test--sample-quoted-tweet)))
    (with-temp-buffer
      (chirp-view-mode)
      (cl-letf (((symbol-function 'chirp-media-avatar-image)
                 (lambda (&rest _args)
                   (ert-fail "avatar image should not be requested")))
                ((symbol-function 'chirp-media-thumbnail-image) (lambda (&rest _args) nil)))
        (let ((inhibit-read-only t))
          (chirp-render-insert-tweet tweet)))
      (goto-char (point-min))
      (should (search-forward "Alice @alice" nil t)))))

(ert-deftest chirp-render-insert-tweet-can-hide-media-and-show-alt-text ()
  "Hidden media should render as a compact alt-aware text entry."
  (let ((chirp-show-tweet-media nil)
        (tweet '(:kind tweet
                 :id "media-1"
                 :text "Photo post"
                 :author-name "Alice"
                 :author-handle "alice"
                 :media ((:type "photo"
                          :url "https://example.com/cat.jpg"
                          :alt "A black cat looking out the window")
                         (:type "video"
                          :url "https://example.com/cat.mp4"
                          :width 640
                          :height 360))
                 :reply-count 0
                 :retweet-count 0
                 :like-count 0
                 :quote-count 0
                 :bookmark-count 0
                 :view-count 0)))
    (with-temp-buffer
      (chirp-view-mode)
      (cl-letf (((symbol-function 'chirp-media-avatar-image) (lambda (&rest _args) nil))
                ((symbol-function 'chirp-media-thumbnail-image)
                 (lambda (&rest _args)
                   (ert-fail "thumbnail image should not be requested")))
                ((symbol-function 'chirp-media-thumbnail-placeholder-image)
                 (lambda (&rest _args)
                   (ert-fail "thumbnail placeholder should not be requested"))))
        (let ((inhibit-read-only t))
          (chirp-render-insert-tweet tweet)))
      (goto-char (point-min))
      (should (search-forward "[image: A black cat looking out the window]" nil t))
      (let ((image-start (match-beginning 0)))
        (should (looking-at "\n\\[video 640x360\\]\n\n"))
        (should-not (looking-at "\n\\[video 640x360\\]\n\n\n"))
        (should (get-text-property image-start 'chirp-media-item))))))

(ert-deftest chirp-render-tweet-rows-link-adjacent-replies ()
  "List rendering should indent replies to the previous visible tweet."
  (pcase-let ((`(,parent ,reply) (chirp-test--sample-adjacent-reply-tweets)))
    (with-temp-buffer
      (chirp-view-mode)
      (cl-letf (((symbol-function 'chirp-media-avatar-image) (lambda (&rest _args) nil))
                ((symbol-function 'chirp-media-thumbnail-image) (lambda (&rest _args) nil)))
        (let ((inhibit-read-only t))
          (chirp-test--insert-tweet-list (list parent reply))))
      (goto-char (point-min))
      (should (search-forward "↳ replying to @dingyi above" nil t))
      (should (equal (get-text-property (match-beginning 0) 'chirp-reply-parent-id)
                     "100"))
      (goto-char (point-min))
      (search-forward "@dingyi")
      (should (eq (get-text-property (match-beginning 0) 'face)
                  'chirp-handle-face))
      (goto-char (point-min))
      (search-forward "Reply body text")
      (let* ((needle "Reply body text")
             (pos (- (point) (length needle)))
             (wrap-prefix (get-text-property pos 'wrap-prefix)))
        (should (stringp wrap-prefix))
        (should (string-match-p "^  " wrap-prefix))))))

(ert-deftest chirp-open-at-point-jumps-to-visible-reply-parent ()
  "RET on an inline reply context should jump to the visible parent tweet."
  (pcase-let ((`(,parent ,reply) (chirp-test--sample-adjacent-reply-tweets)))
    (let (opened-thread)
      (with-temp-buffer
        (chirp-view-mode)
        (cl-letf (((symbol-function 'chirp-media-avatar-image) (lambda (&rest _args) nil))
                  ((symbol-function 'chirp-media-thumbnail-image) (lambda (&rest _args) nil))
                  ((symbol-function 'chirp-thread-open-tweet)
                   (lambda (&rest args)
                     (setq opened-thread args))))
          (let ((inhibit-read-only t))
            (chirp-test--insert-tweet-list (list parent reply)))
          (goto-char (point-min))
          (search-forward "↳ replying to @dingyi above")
          (goto-char (match-beginning 0))
          (chirp-open-at-point)
          (should (equal (plist-get (chirp-entry-at-point) :id) "100"))
          (should-not opened-thread))))))

(ert-deftest chirp-open-at-point-opens-unseen-reply-parent-thread ()
  "RET on an unseen reply parent should open that parent's thread."
  (pcase-let ((`(,parent ,reply) (chirp-test--sample-adjacent-reply-tweets)))
    (let (opened-thread)
      (with-temp-buffer
        (chirp-view-mode)
        (cl-letf (((symbol-function 'chirp-media-avatar-image)
                   (lambda (&rest _args) nil))
                  ((symbol-function 'chirp-media-thumbnail-image)
                   (lambda (&rest _args) nil))
                  ((symbol-function 'chirp-thread-open)
                   (lambda (tweet-id)
                     (setq opened-thread tweet-id))))
          (let ((inhibit-read-only t))
            (chirp-render--insert-tweet reply :reply-parent parent))
          (goto-char (point-min))
          (search-forward "↳ replying to @dingyi above")
          (goto-char (match-beginning 0))
          (chirp-open-at-point)
          (should (equal opened-thread "100")))))))

(ert-deftest chirp-render-tweet-rows-link-replies-via-handle-fallback ()
  "List rendering should also catch replies linked by handle and conversation."
  (pcase-let ((`(,parent ,reply) (chirp-test--sample-adjacent-reply-tweets-with-handle-fallback)))
    (with-temp-buffer
      (chirp-view-mode)
      (cl-letf (((symbol-function 'chirp-media-avatar-image) (lambda (&rest _args) nil))
                ((symbol-function 'chirp-media-thumbnail-image) (lambda (&rest _args) nil)))
        (let ((inhibit-read-only t))
          (chirp-test--insert-tweet-list (list parent reply))))
      (goto-char (point-min))
      (should (search-forward "↳ replying to @dingyi above" nil t))
      (should (equal (get-text-property (match-beginning 0) 'chirp-reply-parent-id)
                     "200")))))

(ert-deftest chirp-render-tweet-rows-insert-customizable-separator ()
  "List rendering should place a non-entry separator between tweets."
  (let ((tweets (list
                 '(:kind tweet :id "100" :text "First" :author-name "Alice" :author-handle "alice"
                   :reply-count 0 :retweet-count 0 :like-count 0 :quote-count 0 :bookmark-count 0 :view-count 0)
                 '(:kind tweet :id "101" :text "Second" :author-name "Bob" :author-handle "bob"
                   :reply-count 0 :retweet-count 0 :like-count 0 :quote-count 0 :bookmark-count 0 :view-count 0))))
    (with-temp-buffer
      (chirp-view-mode)
      (cl-letf (((symbol-function 'chirp-media-avatar-image) (lambda (&rest _args) nil)))
        (let ((inhibit-read-only t))
          (chirp-test--insert-tweet-list tweets)))
      (goto-char (point-min))
      (should (search-forward chirp-tweet-separator nil t))
      (let ((pos (match-beginning 0)))
        (should (chirp-test--face-member-p
                 'chirp-tweet-separator-face
                 (get-text-property pos 'face)))
        (should-not (get-text-property pos 'chirp-entry-item))))))

(ert-deftest chirp-render-tweet-rows-indent-separator-from-left ()
  "List separators should use a stable left indent."
  (let ((chirp-tweet-separator "|")
        (chirp-tweet-separator-indent 6)
        (tweets (list
                 '(:kind tweet :id "100" :text "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
                   :author-name "Alice" :author-handle "alice"
                   :reply-count 0 :retweet-count 0 :like-count 0 :quote-count 0
                   :bookmark-count 0 :view-count 0)
                 '(:kind tweet :id "101" :text "Second" :author-name "Bob" :author-handle "bob"
                   :reply-count 0 :retweet-count 0 :like-count 0 :quote-count 0
                   :bookmark-count 0 :view-count 0))))
    (with-temp-buffer
      (chirp-view-mode)
      (cl-letf (((symbol-function 'chirp-media-avatar-image) (lambda (&rest _args) nil))
                ((symbol-function 'chirp-render--metric-string) (lambda (&rest _args) "")))
        (let ((inhibit-read-only t))
          (chirp-test--insert-tweet-list tweets)))
      (goto-char (point-min))
      (should (search-forward "|" nil t))
      (should (= (save-excursion
                   (goto-char (match-beginning 0))
                   (current-column))
                 6)))))

(ert-deftest chirp-render-tweet-rows-can-disable-separator ()
  "Setting `chirp-tweet-separator' to nil should disable list separators."
  (let ((chirp-tweet-separator nil)
        (tweets (list
                 '(:kind tweet :id "100" :text "First" :author-name "Alice" :author-handle "alice"
                   :reply-count 0 :retweet-count 0 :like-count 0 :quote-count 0 :bookmark-count 0 :view-count 0)
                 '(:kind tweet :id "101" :text "Second" :author-name "Bob" :author-handle "bob"
                   :reply-count 0 :retweet-count 0 :like-count 0 :quote-count 0 :bookmark-count 0 :view-count 0))))
    (with-temp-buffer
      (chirp-view-mode)
      (cl-letf (((symbol-function 'chirp-media-avatar-image) (lambda (&rest _args) nil)))
        (let ((inhibit-read-only t))
          (chirp-test--insert-tweet-list tweets)))
      (should-not (string-match-p "- - - -" (buffer-string))))))

(ert-deftest chirp-render-insert-discussion-entry-renders-full-article-body ()
  "Thread focus rendering should include the full article text."
  (let ((tweet (chirp-test--sample-article-tweet)))
    (with-temp-buffer
      (chirp-view-mode)
      (cl-letf (((symbol-function 'chirp-media-avatar-image) (lambda (&rest _args) nil))
                ((symbol-function 'chirp-media-thumbnail-image) (lambda (&rest _args) nil)))
        (let ((inhibit-read-only t))
          (chirp-render-insert-discussion-entry
           (chirp-test--discussion-row tweet t))))
      (let ((rendered (buffer-string)))
        (should (string-match-p "Longform title" rendered))
        (should (string-match-p "First paragraph with \\[details\\]" rendered))
        (should (string-match-p "Second paragraph\\." rendered))))))

(ert-deftest chirp-render-insert-discussion-entry-renders-article-images ()
  "Thread focus rendering should show inline article images instead of raw Markdown."
  (let ((tweet (chirp-test--sample-article-tweet-with-image)))
    (with-temp-buffer
      (chirp-view-mode)
      (cl-letf (((symbol-function 'chirp-media-avatar-image) (lambda (&rest _args) nil))
                ((symbol-function 'chirp-media-thumbnail-image) (lambda (&rest _args) nil)))
        (let ((inhibit-read-only t))
          (chirp-render-insert-discussion-entry
           (chirp-test--discussion-row tweet t))))
      (let ((rendered (buffer-string)))
        (should (string-match-p "First paragraph\\." rendered))
        (should (string-match-p "Second paragraph\\." rendered))
        (should (string-match-p "\\[image\\]" rendered))
        (should-not (string-match-p "!\\[Cover\\]" rendered))))))

(ert-deftest chirp-render-insert-tweet-renders-link-card-preview ()
  "Tweet rendering should include X website-card previews."
  (let ((tweet
         (list :kind 'tweet
               :id "125"
               :text "Repo github.com/example/project"
               :author-name "Alice"
               :author-handle "alice"
               :link-card
               (list :url "https://github.com/example/project"
                     :title "microsoft/RD-Agent"
                     :description "Research and development agent"
                     :domain "github.com"
                     :image-url "https://opengraph.githubassets.com/demo"))))
    (with-temp-buffer
      (chirp-view-mode)
      (cl-letf (((symbol-function 'chirp-media-avatar-image)
                 (lambda (&rest _args) nil))
                ((symbol-function 'chirp-media-thumbnail-image)
                 (lambda (&rest _args) nil)))
        (let ((inhibit-read-only t))
          (chirp-render-insert-tweet tweet)))
      (let ((rendered (buffer-string)))
        (should (string-match-p "microsoft/RD-Agent" rendered))
        (should (string-match-p "Research and development agent" rendered))
        (should (string-match-p "github\\.com" rendered)))
      (goto-char (point-min))
      (search-forward "microsoft/RD-Agent")
      (let ((position (match-beginning 0))
            opened)
        (should (chirp-test--face-member-p
                 'chirp-quoted-tweet-block-face
                 (get-text-property position 'face)))
        (should (stringp (get-text-property position 'line-prefix)))
        (cl-letf (((symbol-function 'browse-url)
                   (lambda (url &rest _args)
                     (setq opened url))))
          (goto-char position)
          (chirp-open-at-point))
        (should (equal opened "https://github.com/example/project"))))))

(ert-deftest chirp-render-link-card-slices-cached-preview-image ()
  "A cached website-card image should be inserted as Appkit slices."
  (let ((tweet
         (list :kind 'tweet
               :id "126"
               :text "Card"
               :author-name "Alice"
               :author-handle "alice"
               :link-card
               (list :url "https://example.com/post"
                     :title "Example"
                     :domain "example.com"
                     :image-url "https://pbs.twimg.com/card_img/demo.jpg")))
        sliced)
    (with-temp-buffer
      (chirp-view-mode)
      (cl-letf (((symbol-function 'chirp-media-avatar-image)
                 (lambda (&rest _args) nil))
                ((symbol-function 'chirp-media-cached-image)
                 (lambda (&rest _args)
                   '(image :type png :data "x")))
                ((symbol-function 'appkit-media-insert-image-slices)
                 (lambda (image &rest _args)
                   (setq sliced image)
                   (insert "[slice]"))))
        (let ((inhibit-read-only t))
          (chirp-render-insert-tweet tweet)))
      (should (equal sliced '(image :type png :data "x")))
      (should (string-match-p "\\[slice\\]" (buffer-string)))
      (goto-char (point-min))
      (search-forward "[slice]")
      (should (stringp (get-text-property (match-beginning 0) 'line-prefix))))))

(ert-deftest chirp-normalize-tweet-decodes-html-entities-without-shifting-urls ()
  "HTML entities in tweet text should decode without moving URL spans."
  (let* ((raw "Scala &amp; Java https://t.co/aaa")
         (url-beg (string-match "https://t.co/aaa" raw))
         (url-end (+ url-beg (length "https://t.co/aaa")))
         (tweet
          (chirp-normalize-tweet
           `(("id" . "amp")
             ("text" . ,raw)
             ("entities"
              . (("urls"
                  . ((("url" . "https://t.co/aaa")
                      ("expanded_url" . "https://example.com/x")
                      ("display_url" . "example.com/x")
                      ("indices" . (,url-beg ,url-end)))))))))))
    (should (string-match-p "Scala & Java" (plist-get tweet :text)))
    (should-not (string-match-p "&amp;" (plist-get tweet :text)))
    (let ((entity (car (plist-get tweet :text-entities))))
      (should (eq (plist-get entity :kind) 'url))
      (should (equal (substring (plist-get tweet :text)
                                (plist-get entity :start)
                                (plist-get entity :end))
                     "example.com/x")))))

(ert-deftest chirp-render-keeps-url-entity-aligned-around-ampersand ()
  "Re-cleaning emitted tweet text must not shift entity offsets past `&'."
  (let* ((text "Scala & Java see example.com/path")
         (start (string-match "example\\.com/path" text))
         (end (+ start (length "example.com/path")))
         (tweet
          (list :kind 'tweet
                :id "amp-1"
                :text text
                :author-name "Alice"
                :author-handle "alice"
                :text-entities
                (list (list :kind 'url
                            :url "https://example.com/path"
                            :start start
                            :end end))))
        opened)
    (should (equal (substring text start end) "example.com/path"))
    (with-temp-buffer
      (chirp-view-mode)
      (cl-letf (((symbol-function 'chirp-media-avatar-image)
                 (lambda (&rest _args) nil))
                ((symbol-function 'browse-url)
                 (lambda (url &rest _args)
                   (setq opened url))))
        (let ((inhibit-read-only t))
          (chirp-render-insert-tweet tweet))
        (goto-char (point-min))
        (search-forward "example.com/path")
        (goto-char (match-beginning 0))
        (should (eq (get-text-property (point) 'face) 'chirp-link-face))
        (chirp-open-at-point)
        (should (equal opened "https://example.com/path"))))))

(ert-deftest chirp-render-metric-string-uses-action-specific-active-faces ()
  "Liked, bookmarked, and retweeted metrics should use distinct active faces."
  (should (eq (get-text-property 0 'face
                                 (chirp-render--metric-string 'like 12 t))
              'chirp-liked-metric-face))
  (should (eq (get-text-property 0 'face
                                 (chirp-render--metric-string 'bookmark 3 t))
              'chirp-bookmarked-metric-face))
  (should (eq (get-text-property 0 'face
                                 (chirp-render--metric-string 'retweet 5 t))
              'chirp-retweeted-metric-face))
  (should (eq (get-text-property 0 'face
                                 (chirp-render--metric-string 'reply 1 nil))
              'chirp-meta-face)))

(ert-deftest chirp-render-insert-tweet-marks-metric-actions ()
  "Tweet metrics should be Appkit actions with the matching commands."
  (let ((tweet '(:kind tweet
                 :id "mouse-1"
                 :text "Clickable actions"
                 :author-name "Alice"
                 :author-handle "alice"
                 :reply-count 1
                 :retweet-count 2
                 :like-count 3
                 :quote-count 4
                 :bookmark-count 5
                 :view-count 6
                 :retweeted-p t
                 :liked-p t
                 :bookmarked-p t)))
    (with-temp-buffer
      (chirp-view-mode)
      (let ((inhibit-read-only t))
        (chirp-render-insert-tweet tweet))
      (dolist (spec
               `((reply 1 nil ,#'chirp-reply-at-point
                        chirp-meta-face "Reply")
                 (retweet 2 t ,#'chirp-toggle-retweet-at-point
                          chirp-retweeted-metric-face "Repost")
                 (like 3 t ,#'chirp-toggle-like-at-point
                       chirp-liked-metric-face "Like")
                 (quote 4 nil ,#'chirp-quote-at-point
                        chirp-meta-face "Quote")
                 (bookmark 5 t ,#'chirp-toggle-bookmark-at-point
                           chirp-bookmarked-metric-face "Bookmark")))
        (pcase-let ((`(,label ,count ,active ,command ,face ,help) spec))
          (goto-char (point-min))
          (search-forward (chirp-render--metric-string label count active))
          (let ((position (match-beginning 0)))
            (should (eq (get-text-property position 'face) face))
            (should (eq (get-text-property position 'mouse-face) 'highlight))
            (should (eq (get-text-property position 'pointer) 'hand))
            (should (equal (get-text-property position 'help-echo) help))
            (should (eq (appkit-ui-action-at position) command))
            (should (eq (lookup-key (get-text-property position 'keymap)
                                    [mouse-1])
                        #'appkit-ui-activate))
            (goto-char position)
            (should (eq (key-binding (kbd "RET")) #'appkit-ui-activate))
            (should (equal (get-text-property position 'chirp-entry-item)
                           tweet)))))
      (goto-char (point-min))
      (search-forward (chirp-render--metric-string 'view 6))
      (let ((position (match-beginning 0)))
        (should-not (appkit-ui-action-at position))
        (should-not (get-text-property position 'mouse-face)))
      (should buffer-read-only))))

(ert-deftest chirp-render-insert-tweet-renders-reply-control ()
  "Restricted reply audiences should be visible above tweet metrics."
  (let ((tweet '(:kind tweet
                 :id "reply-control-1"
                 :text "Restricted replies"
                 :author-name "Alice"
                 :author-handle "alice"
                 :reply-control-mode "ByInvitation"
                 :reply-count 1
                 :retweet-count 2
                 :like-count 3
                 :quote-count 4
                 :bookmark-count 5
                 :view-count 6)))
    (with-temp-buffer
      (chirp-view-mode)
      (let ((inhibit-read-only t))
        (chirp-render-insert-tweet tweet))
      (should (string-match-p
               "Accounts @alice mentioned can reply"
               (buffer-string))))))

(ert-deftest chirp-render-insert-tweet-omits-limited-reply-action ()
  "A viewer-limited reply should remain visible but not be actionable."
  (let ((tweet '(:kind tweet
                 :id "reply-control-2"
                 :text "No reply action"
                 :author-name "Alice"
                 :author-handle "alice"
                 :reply-limited-p t
                 :reply-count 1
                 :retweet-count 2
                 :like-count 3
                 :quote-count 4
                 :bookmark-count 5
                 :view-count 6)))
    (with-temp-buffer
      (chirp-view-mode)
      (let ((inhibit-read-only t))
        (chirp-render-insert-tweet tweet))
      (should (string-match-p
               "You cannot reply to this conversation"
               (buffer-string)))
      (goto-char (point-min))
      (search-forward (chirp-render--metric-string 'reply 1))
      (should-not (appkit-ui-action-at (match-beginning 0))))))

(ert-deftest chirp-render-metric-string-omits-missing-count-placeholder ()
  "Metrics with unavailable counts should retain only their action icon."
  (cl-letf (((symbol-function 'nerd-icons-mdicon)
             (lambda (&rest _args) "bookmark-icon")))
    (should (equal (substring-no-properties
                    (chirp-render--metric-string 'bookmark nil t))
                   "bookmark-icon"))))

(ert-deftest chirp-render-insert-tweet-renders-quoted-tweet-preview ()
  "Tweet rendering should show a normal quoted tweet card instead of a label."
  (let ((tweet (chirp-test--sample-quoted-tweet)))
    (with-temp-buffer
      (chirp-view-mode)
      (cl-letf (((symbol-function 'chirp-media-avatar-image) (lambda (&rest _args) nil))
                ((symbol-function 'chirp-media-thumbnail-image) (lambda (&rest _args) nil)))
        (let ((inhibit-read-only t))
          (chirp-render-insert-tweet tweet)))
      (let ((rendered (buffer-string)))
        (should (string-match-p "Bob @bob" rendered))
        (should (string-match-p "QUOTE-TIME" rendered))
        (should (string-match-p "replying to @parent" rendered))
        (should (string-match-p "Quoted body text" rendered))
        (should-not (string-match-p "Quoted @bob" rendered))
        (should-not (string-match-p "https://x\\.com/bob/status/456" rendered)))
      (goto-char (point-min))
      (search-forward "Bob @bob")
      (should (equal (plist-get (chirp-entry-at-point) :id) "456")))))

(ert-deftest chirp-open-at-point-opens-quoted-tweet-from-card-body ()
  "RET on quoted body opens that tweet; card metrics stay local actions."
  (let ((tweet (chirp-test--sample-quoted-tweet))
        opened-thread
        liked)
    (with-temp-buffer
      (chirp-view-mode)
      (cl-letf (((symbol-function 'chirp-media-avatar-image)
                 (lambda (&rest _args) nil))
                ((symbol-function 'chirp-media-thumbnail-image)
                 (lambda (&rest _args) nil))
                ((symbol-function 'chirp-thread-open-tweet)
                 (lambda (tweet)
                   (setq opened-thread (plist-get tweet :id))))
                ((symbol-function 'chirp-toggle-like-at-point)
                 (lambda ()
                   (setq liked (plist-get (chirp-entry-at-point) :id)))))
        (let ((inhibit-read-only t))
          (chirp-render-insert-tweet tweet))
        (goto-char (point-min))
        (search-forward "Quoted body text")
        (goto-char (match-beginning 0))
        (chirp-open-at-point)
        (should (equal opened-thread "456"))
        (search-forward (chirp-render--metric-string 'like nil))
        (goto-char (match-beginning 0))
        (should (eq (appkit-ui-action-at)
                    #'chirp-toggle-like-at-point))
        (chirp-open-at-point)
        (should (equal liked "456"))
        (should (equal opened-thread "456"))))))

(ert-deftest chirp-render-quoted-tweet-follows-own-media ()
  "A tweet's own media should render before its quoted tweet card."
  (let ((tweet (chirp-test--sample-quoted-tweet))
        (chirp-show-tweet-media nil))
    (setf (plist-get tweet :media)
          '((:type "video"
             :url "https://example.com/outer.mp4"
             :width 640
             :height 360)))
    (with-temp-buffer
      (chirp-view-mode)
      (cl-letf (((symbol-function 'chirp-media-avatar-image)
                 (lambda (&rest _args) nil)))
        (let ((inhibit-read-only t))
          (chirp-render-insert-tweet tweet)))
      (let* ((rendered (buffer-string))
             (media-position (string-match "\\[video 640x360\\]"
                                           rendered))
             (quote-position (string-match "Bob @bob" rendered)))
        (should media-position)
        (should quote-position)
        (should (< media-position quote-position))))))

(ert-deftest chirp-render-insert-discussion-entry-labels-related-tweet ()
  "Related context should precede the original author heading."
  (let ((tweet
         (chirp-normalize-tweet
          '(("id" . "related-1")
            ("text" . "Related body")
            ("timelineContext" . "related")
            ("author" . (("screenName" . "alice")
                         ("name" . "Alice")))))))
    (with-temp-buffer
      (chirp-view-mode)
      (cl-letf (((symbol-function 'chirp-media-avatar-image)
                 (lambda (&rest _args) nil)))
        (let ((inhibit-read-only t))
          (chirp-render-insert-discussion-entry
           (chirp-test--discussion-row tweet))))
      (goto-char (point-min))
      (search-forward "Related tweet")
      (let ((label-position (match-beginning 0)))
        (should (chirp-test--face-member-p
                 'chirp-thread-related-context
                 (get-text-property label-position 'face)))
        (search-forward "Alice @alice")
        (should (< label-position (match-beginning 0)))))))

(ert-deftest chirp-render-insert-discussion-entry-puts-retweeter-before-author ()
  "Retweet attribution should remain actionable above the original author."
  (let ((tweet (chirp-test--sample-retweeted-tweet)))
    (with-temp-buffer
      (chirp-view-mode)
      (cl-letf (((symbol-function 'chirp-media-avatar-image)
                 (lambda (&rest _args) nil)))
        (let ((inhibit-read-only t))
          (chirp-render-insert-discussion-entry
           (chirp-test--discussion-row tweet))))
      (goto-char (point-min))
      (search-forward "retweeted by dotey")
      (let ((context-position (match-beginning 0)))
        (should (chirp-test--face-member-p
                 'chirp-social-context-face
                 (get-text-property context-position 'face)))
        (should (functionp (appkit-ui-action-at context-position)))
        (search-forward "Alice @alice")
        (should (< context-position (match-beginning 0)))))))

(ert-deftest chirp-render-insert-discussion-entry-highlights-reply-handle ()
  "Thread reply context should highlight only the target handle."
  (let ((tweet '(:kind tweet
                 :id "reply-1"
                 :text "Reply body"
                 :reply-to-handle "bob"
                 :author-name "Alice"
                 :author-handle "alice")))
    (with-temp-buffer
      (chirp-view-mode)
      (cl-letf (((symbol-function 'chirp-media-avatar-image)
                 (lambda (&rest _args) nil)))
        (let ((inhibit-read-only t))
          (chirp-render-insert-discussion-entry
           (chirp-test--discussion-row tweet nil '(tweet "parent") 2))))
      (goto-char (point-min))
      (search-forward "replying to ")
      (should (eq (get-text-property (match-beginning 0) 'face)
                  'chirp-thread-reply-context-face))
      (search-forward "@bob")
      (should (eq (get-text-property (match-beginning 0) 'face)
                  'chirp-link-face)))))

(ert-deftest chirp-render-insert-tweet-highlights-quoted-tweet-block ()
  "Quoted tweet cards should carry a distinct block face and prefix."
  (let ((tweet (chirp-test--sample-quoted-tweet)))
    (with-temp-buffer
      (chirp-view-mode)
      (cl-letf (((symbol-function 'chirp-media-avatar-image) (lambda (&rest _args) nil))
                ((symbol-function 'chirp-media-thumbnail-image) (lambda (&rest _args) nil)))
        (let ((inhibit-read-only t))
          (chirp-render-insert-tweet tweet)))
      (goto-char (point-min))
      (search-forward "Bob @bob")
      (let ((heading-position (match-beginning 0)))
        (should (chirp-test--face-member-p
                 'chirp-quoted-tweet-block-face
                 (get-text-property heading-position 'face)))
        (should (stringp (get-text-property heading-position 'line-prefix))))
      (goto-char (point-min))
      (search-forward "Quoted body text")
      (should (chirp-test--face-member-p
               'chirp-quoted-tweet-block-face
               (get-text-property (match-beginning 0) 'face)))
      (should (stringp (get-text-property (match-beginning 0) 'line-prefix))))))
(ert-deftest chirp-render-quoted-tweet-lines-use-wrap-prefix ()
  "Quoted tweet body lines should keep the card prefix on visual wraps."
  (let ((tweet (chirp-test--sample-quoted-tweet)))
    (with-temp-buffer
      (chirp-view-mode)
      (cl-letf (((symbol-function 'chirp-media-avatar-image) (lambda (&rest _args) nil))
                ((symbol-function 'chirp-media-thumbnail-image) (lambda (&rest _args) nil)))
        (let ((inhibit-read-only t))
          (chirp-render-insert-tweet tweet)))
      (goto-char (point-min))
      (search-forward "Quoted body text")
      (let* ((needle "Quoted body text")
             (pos (- (point) (length needle)))
             (wrap-prefix (get-text-property pos 'wrap-prefix)))
        (should (stringp wrap-prefix))
        (should (>= (string-width wrap-prefix) 3))))))
(ert-deftest chirp-render-quoted-tweet-media-uses-gapless-image-slices ()
  "Quoted tweet media should use the card prefix on gapless image slices."
  (let ((tweet (chirp-test--sample-quoted-tweet-with-media))
        (fake-image '(image :type png :file "/tmp/fake.png"
                            :appkit-media-nslices 4)))
    (with-temp-buffer
      (chirp-view-mode)
      (cl-letf (((symbol-function 'chirp-media-avatar-image) (lambda (&rest _args) nil))
                ((symbol-function 'chirp-media-thumbnail-image) (lambda (&rest _args) fake-image))
                ((symbol-function 'chirp-media-thumbnail-placeholder-image) (lambda (&rest _args) nil))
                ((symbol-function 'image-size)
                 (lambda (&rest _args) '(8 . 4))))
        (let ((inhibit-read-only t))
          (chirp-render-insert-tweet tweet)))
      (let ((slices (chirp-test--slice-displays)))
        (should (eq line-spacing 0))
        (should (= (length slices) 4))
        (should
         (cl-every
          (lambda (item)
            (stringp (get-text-property (car item) 'line-prefix)))
          slices))
        (let ((slice-height (nth 4 (car (cdr (car slices))))))
          (should (equal
                   (mapcar (lambda (item) (nth 2 (car (cdr item)))) slices)
                   (list 0 slice-height (* 2 slice-height)
                         (* 3 slice-height)))))
        (cl-loop for (position . display) in slices
                 for finalp = (= position (caar (last slices)))
                 do (should (plist-get
                             (get-text-property position 'chirp-media-item)
                             :url))
                 do (unless finalp
                      (save-excursion
                        (goto-char position)
                        (should (eq (char-after (line-end-position)) ?\n))
                        (should (eq (get-text-property
                                     (line-end-position) 'line-height)
                                    t)))))))))

(ert-deftest chirp-render-media-grid-uses-appkit-slice-rows ()
  "Tweet media cells should use Appkit slice rows for current-line geometry."
  (let ((source '(image :type png :file "/tmp/fake.png"
                        :height (3 . ch)
                        :appkit-media-nslices 3))
        (media '(:type "photo" :url "https://example.com/a.jpg")))
    (cl-letf (((symbol-function 'chirp-media-thumbnail-image)
               (lambda (&rest _args) source))
              ((symbol-function 'image-size)
               (lambda (&rest _args) '(8 . 3)))
              ((symbol-function 'appkit-media--char-pixel-height)
               (lambda () 10)))
      (let* ((cell (chirp-render--media-grid-cell media 0))
             (rows (plist-get cell :rows)))
        (should (= (length rows) 3))
        (should (equal (get-text-property 0 'display
                                         (plist-get cell :padding))
                       '(space :width 8)))
        (cl-loop for row in rows
                 for index from 0
                 for display = (get-text-property 0 'display row)
                 do (should (equal (car display)
                                   (list 'slice 0 (* index 10) 1.0 10)))
                 do (should (= (plist-get (cdr (cadr display)) :height) 30)))
        (should (equal source
                       '(image :type png :file "/tmp/fake.png"
                               :height (3 . ch)
                               :appkit-media-nslices 3)))))))

(ert-deftest chirp-render-video-placeholder-cover-is-sliced ()
  "Video placeholders should use the same sliced cover path as photos."
  (let ((media '(:type "video" :url "https://example.com/video.mp4"))
        (placeholder '(image :type svg :data "video-cover"
                             :appkit-media-nslices 3)))
    (with-temp-buffer
      (chirp-view-mode)
      (cl-letf (((symbol-function 'chirp-media-thumbnail-image)
                 (lambda (&rest _args) nil))
                ((symbol-function 'chirp-media-thumbnail-placeholder-image)
                 (lambda (&rest _args) placeholder))
                ((symbol-function 'image-size)
                 (lambda (&rest _args) '(8 . 3))))
        (let ((inhibit-read-only t))
          (chirp-render-insert-media-strip (list media))))
      (let ((slices (chirp-test--slice-displays)))
        (should (= (length slices) 3))
        (dolist (item slices)
          (should (equal (get-text-property (car item) 'chirp-media-item)
                         media)))))))

(ert-deftest chirp-render-sliced-media-grid-reserves-shorter-image-column ()
  "Later slice rows should keep shorter images from shifting the media grid."
  (let ((media-list '((:type "photo" :url "short")
                      (:type "photo" :url "tall"))))
    (with-temp-buffer
      (chirp-view-mode)
      (cl-letf (((symbol-function 'chirp-media-thumbnail-image)
                 (lambda (media)
                   `(image :type png
                           :file ,(plist-get media :url)
                           :appkit-media-nslices
                           ,(if (equal (plist-get media :url) "short") 2 3))))
                ((symbol-function 'chirp-media-thumbnail-placeholder-image)
                 (lambda (&rest _args) nil))
                ((symbol-function 'image-size)
                 (lambda (_image &rest _args) '(8 . 3))))
        (let ((inhibit-read-only t))
          (chirp-render-insert-media-strip media-list)))
      (goto-char (point-min))
      (forward-line 2)
      (should (equal (get-text-property (point) 'display)
                     '(space :width 8)))
      (should (= (get-text-property (point) 'chirp-media-index) 0))
      (forward-char 1)
      (should (eq (car-safe (car-safe (get-text-property (point) 'display)))
                  'slice))
      (should (= (get-text-property (point) 'chirp-media-index) 1)))))

(ert-deftest chirp-open-at-point-opens-profile-when-point-is-on-avatar ()
  "RET on an avatar should open the author profile, not the tweet thread."
  (let ((tweet (chirp-test--sample-quoted-tweet))
        opened-profile
        opened-thread)
    (with-temp-buffer
      (chirp-view-mode)
      (cl-letf (((symbol-function 'chirp-media-avatar-image) (lambda (&rest _args) nil))
                ((symbol-function 'chirp-media-thumbnail-image) (lambda (&rest _args) nil))
                ((symbol-function 'chirp-profile-open)
                 (lambda (handle &optional _mode)
                   (setq opened-profile handle)))
                ((symbol-function 'chirp-thread-open-tweet)
                 (lambda (&rest args)
                   (setq opened-thread args))))
        (let ((inhibit-read-only t))
          (chirp-render-insert-tweet tweet))
        (goto-char (point-min))
        (chirp-open-at-point)))
    (should (equal opened-profile "alice"))
    (should-not opened-thread)))

(ert-deftest chirp-open-at-point-opens-profile-when-point-is-on-author-handle ()
  "RET on the author name or handle should open the profile."
  (let ((tweet (chirp-test--sample-quoted-tweet))
        opened-profile
        opened-thread)
    (with-temp-buffer
      (chirp-view-mode)
      (cl-letf (((symbol-function 'chirp-media-avatar-image) (lambda (&rest _args) nil))
                ((symbol-function 'chirp-media-thumbnail-image) (lambda (&rest _args) nil))
                ((symbol-function 'chirp-profile-open)
                 (lambda (handle &optional _mode)
                   (setq opened-profile handle)))
                ((symbol-function 'chirp-thread-open-tweet)
                 (lambda (&rest args)
                   (setq opened-thread args))))
        (let ((inhibit-read-only t))
          (chirp-render-insert-tweet tweet))
        (goto-char (point-min))
        (search-forward "@alice")
        (goto-char (match-beginning 0))
        (chirp-open-at-point)))
    (should (equal opened-profile "alice"))
    (should-not opened-thread)))

(ert-deftest chirp-render-insert-user-summary-marks-followers-and-following-regions ()
  "Profile summaries should expose clickable followers/following regions."
  (let ((user '(:kind user
                :name "Alice"
                :handle "alice"
                :followers 34
                :following 12
                :posts 56)))
    (with-temp-buffer
      (chirp-view-mode)
      (cl-letf (((symbol-function 'chirp-media-avatar-image) (lambda (&rest _args) nil)))
        (let ((inhibit-read-only t))
          (chirp-render-insert-user-summary user)))
      (goto-char (point-min))
      (search-forward "Following 12")
      (should (eq (get-text-property (match-beginning 0) 'chirp-profile-list-kind)
                  'following))
      (should (equal (get-text-property (match-beginning 0) 'chirp-profile-list-handle)
                     "alice"))
      (goto-char (point-min))
      (search-forward "Followers 34")
      (should (eq (get-text-property (match-beginning 0) 'chirp-profile-list-kind)
                  'followers))
      (should (equal (get-text-property (match-beginning 0) 'chirp-profile-list-handle)
                     "alice")))))

(ert-deftest chirp-render-insert-user-summary-adds-follow-action-region ()
  "Profile summaries should expose a clickable follow-state action."
  (let ((user '(:kind user
                :name "Alice"
                :handle "alice"
                :followers 34
                :following 12
                :posts 56
                :viewer-followed-by-p t)))
    (with-temp-buffer
      (chirp-view-mode)
      (cl-letf (((symbol-function 'chirp-media-avatar-image) (lambda (&rest _args) nil)))
        (let ((inhibit-read-only t))
          (chirp-render-insert-user-summary user)))
      (goto-char (point-min))
      (search-forward "Follow back")
      (should (eq (get-text-property (match-beginning 0) 'chirp-profile-action)
                  'toggle-follow)))))

(ert-deftest chirp-open-at-point-opens-followers-list-from-profile-summary ()
  "RET on profile follower/following counts should open the matching list."
  (let ((user '(:kind user
                :name "Alice"
                :handle "alice"
                :followers 34
                :following 12
                :posts 56))
        opened-followers
        opened-following)
    (with-temp-buffer
      (chirp-view-mode)
      (cl-letf (((symbol-function 'chirp-media-avatar-image) (lambda (&rest _args) nil))
                ((symbol-function 'chirp-profile-open-followers)
                 (lambda (handle)
                   (setq opened-followers handle)))
                ((symbol-function 'chirp-profile-open-following-users)
                 (lambda (handle)
                   (setq opened-following handle))))
        (let ((inhibit-read-only t))
          (chirp-render-insert-user-summary user))
        (goto-char (point-min))
        (search-forward "Followers 34")
        (goto-char (match-beginning 0))
        (chirp-open-at-point)
        (should (equal opened-followers "alice"))
        (goto-char (point-min))
        (search-forward "Following 12")
        (goto-char (match-beginning 0))
        (chirp-open-at-point)
        (should (equal opened-following "alice"))))))

(ert-deftest chirp-open-at-point-toggles-follow-from-profile-summary ()
  "RET on the profile follow button should toggle follow state."
  (let ((user '(:kind user
                :name "Alice"
                :handle "alice"
                :followers 34
                :following 12
                :posts 56
                :viewer-following-p t))
        toggled)
    (with-temp-buffer
      (chirp-view-mode)
      (cl-letf (((symbol-function 'chirp-media-avatar-image) (lambda (&rest _args) nil))
                ((symbol-function 'chirp-toggle-follow-user-at-point)
                 (lambda ()
                   (setq toggled t))))
        (let ((inhibit-read-only t))
          (chirp-render-insert-user-summary user))
        (goto-char (point-min))
        (search-forward "Following")
        (goto-char (match-beginning 0))
        (chirp-open-at-point)
        (should toggled)))))

(ert-deftest chirp-open-at-point-opens-profile-post-thread-in-composite-profile-buffer ()
  "RET on a recent post inside a profile buffer should open the tweet thread."
  (let ((user '(:kind user
                :name "Alice"
                :handle "alice"
                :followers 34
                :following 12
                :posts 56))
        (tweet '(:kind tweet
                 :id "123"
                 :text "Hello world"
                 :author-name "Alice"
                 :author-handle "alice"
                 :reply-count 0
                 :retweet-count 0
                 :like-count 0
                 :quote-count 0
                 :bookmark-count 0
                 :view-count 0))
        opened-thread
        opened-profile)
    (with-temp-buffer
      (chirp-view-mode)
      (cl-letf (((symbol-function 'chirp-media-avatar-image) (lambda (&rest _args) nil))
                ((symbol-function 'chirp-thread-open-tweet)
                 (lambda (tweet)
                   (setq opened-thread (plist-get tweet :id))))
                ((symbol-function 'chirp-profile-open)
                 (lambda (handle &optional _mode)
                   (setq opened-profile handle))))
        (let ((inhibit-read-only t))
          (chirp-render-insert-user-summary user)
          (chirp-test--insert-tweet-list (list tweet)))
        (goto-char (point-min))
        (search-forward "Hello world")
        (goto-char (match-beginning 0))
        (chirp-open-at-point)
        (should (equal opened-thread "123"))
        (should-not opened-profile)))))

(ert-deftest chirp-open-at-point-opens-profile-from-author-on-that-profile ()
  "RET on an author handle opens the profile even on that user's profile view."
  (let ((tweet '(:kind tweet
                 :id "123"
                 :text "Hello world"
                 :author-name "Alice"
                 :author-handle "alice"
                 :reply-count 0
                 :retweet-count 0
                 :like-count 0
                 :quote-count 0
                 :bookmark-count 0
                 :view-count 0))
        opened-thread
        opened-profile)
    (with-temp-buffer
      (chirp-view-mode)
      (setq-local chirp--profile-handle "alice")
      (cl-letf (((symbol-function 'chirp-media-avatar-image) (lambda (&rest _args) nil))
                ((symbol-function 'chirp-thread-open-tweet)
                 (lambda (tweet)
                   (setq opened-thread (plist-get tweet :id))))
                ((symbol-function 'chirp-profile-open)
                 (lambda (handle &optional _mode)
                   (setq opened-profile handle))))
        (let ((inhibit-read-only t))
          (chirp-render-insert-tweet tweet))
        (goto-char (point-min))
        (search-forward "@alice")
        (goto-char (match-beginning 0))
        (chirp-open-at-point)
        (should (equal opened-profile "alice"))
        (should-not opened-thread)))))

(ert-deftest chirp-entry-navigation-can-disable-wraparound ()
  "List-style buffers should be able to stop at the ends instead of wrapping."
  (let ((tweets (list
                 '(:kind tweet :id "100" :text "First" :author-name "Alice" :author-handle "alice"
                   :reply-count 0 :retweet-count 0 :like-count 0 :quote-count 0 :bookmark-count 0 :view-count 0)
                 '(:kind tweet :id "101" :text "Second" :author-name "Bob" :author-handle "bob"
                   :reply-count 0 :retweet-count 0 :like-count 0 :quote-count 0 :bookmark-count 0 :view-count 0))))
    (with-temp-buffer
      (chirp-view-mode)
      (setq-local chirp--entry-wrap-navigation nil)
      (cl-letf (((symbol-function 'chirp-media-avatar-image) (lambda (&rest _args) nil)))
        (let ((inhibit-read-only t))
          (chirp-test--insert-tweet-list tweets)))
      (goto-char (point-min))
      (search-forward "Second")
      (goto-char (match-beginning 0))
      (should-error (chirp-next-entry) :type 'user-error)
      (goto-char (point-min))
      (search-forward "First")
      (goto-char (match-beginning 0))
      (should-error (chirp-previous-entry) :type 'user-error))))

(ert-deftest chirp-enrich-quoted-tweets-upgrades-preview-and-prefetches-media ()
  "Quoted tweet enrichment should replace the preview and kick media prefetch."
  (let ((chirp--app nil)
        (tweet (chirp-test--sample-quoted-tweet))
        rerendered-buffer
        prefetched-media-url)
    (unwind-protect
        (let ((buffer (generate-new-buffer " *chirp-quote-enrich-test*")))
          (with-current-buffer buffer
            (chirp-view-mode))
          (cl-letf (((symbol-function 'chirp-backend-tweet)
                     (lambda (_tweet-id callback &optional _errback)
                       (funcall
                        callback
                        (chirp-normalize-tweet
                         '(("id" . "456")
                           ("text" . "Quoted body text with image")
                           ("author" . (("screenName" . "bob")
                                        ("name" . "Bob")))
                           ("media" . ((("type" . "photo")
                                        ("url" . "https://example.com/quoted.jpg"))))))
                        nil)))
                    ((symbol-function 'chirp-request-tweet-rerender)
                     (lambda (_tweet-id target &optional _delay)
                       (setq rerendered-buffer target)))
                    ((symbol-function 'chirp-media-prefetch-tweet)
                     (lambda (quoted _buffer)
                       (setq prefetched-media-url
                             (plist-get (car (plist-get quoted :media)) :url)))))
            (chirp-enrich-quoted-tweets (list tweet) buffer))
          (let ((quoted (plist-get tweet :quoted-tweet)))
            (should (plist-get quoted :chirp-enriched-p))
            (should (equal rerendered-buffer buffer))
            (should (equal prefetched-media-url "https://example.com/quoted.jpg"))
            (should (equal (plist-get (car (plist-get quoted :media)) :url)
                           "https://example.com/quoted.jpg"))))
      (chirp-stop)
      (dolist (name '(" *chirp-quote-enrich-test*"))
        (when-let* ((buffer (get-buffer name)))
          (kill-buffer buffer))))))

(ert-deftest chirp-quoted-tweet-callback-errors-are-reported-and-isolated ()
  "A failed quoted-tweet callback should not block later pending callbacks."
  (let ((chirp--app nil)
        warning
        later-payload)
    (puthash "456"
             (list (lambda (_payload)
                     (error "quoted-tweet callback failed"))
                   (lambda (payload)
                     (setq later-payload payload)))
             (chirp--session-quoted-tweet-pending (chirp--session)))
    (cl-letf (((symbol-function 'display-warning)
               (lambda (type message &rest _args)
                 (setq warning (list type message)))))
      (chirp--dispatch-quoted-tweet-callbacks "456" :payload))
    (should (eq later-payload :payload))
    (should-not
     (gethash "456" (chirp--session-quoted-tweet-pending (chirp--session))))
    (should (eq (car warning) 'chirp-core))
    (should (string-match-p "Quoted-tweet callback failed for 456"
                            (cadr warning)))
    (chirp-stop)))

(ert-deftest chirp-entry-navigation-jumps-between-top-level-tweets ()
  "Entry navigation should move between top-level tweets from nested regions."
  (let ((tweet-a (chirp-test--sample-quoted-tweet))
        (tweet-b (chirp-test--sample-article-tweet)))
    (with-temp-buffer
      (chirp-view-mode)
      (cl-letf (((symbol-function 'chirp-media-avatar-image) (lambda (&rest _args) nil))
                ((symbol-function 'chirp-media-thumbnail-image) (lambda (&rest _args) nil)))
        (let ((inhibit-read-only t))
          (chirp-render-insert-tweet tweet-a)
          (chirp-render-insert-tweet tweet-b)))
      (goto-char (point-min))
      (search-forward "Bob @bob")
      (chirp-next-entry)
      (should (equal (plist-get (chirp-entry-at-point) :id) "123"))
      (search-forward "First paragraph")
      (chirp-previous-entry)
      (should (equal (plist-get (chirp-entry-at-point) :id) "999")))))

(ert-deftest chirp-thread-article-fetch-needed-for-link-only-or-preview-tweets ()
  "Thread views should enrich article-like tweets when body text is missing."
  (should (chirp-thread--article-fetch-needed-p
           '(:id "123"
             :text ""
             :urls ("https://example.com/article"))))
  (should (chirp-thread--article-fetch-needed-p
           '(:id "123"
             :text "Read this"
             :article-title "Longform title")))
  (should-not (chirp-thread--article-fetch-needed-p
               '(:id "123"
                 :text "Read this"
                 :article-text "Full article body."))))


(provide 'chirp-render-test)

;;; chirp-render-test.el ends here
