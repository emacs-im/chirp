;;; chirp-backend-test.el --- Tests for Chirp backend caching -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Commentary:

;; Exercise backend caching, direct request shaping, and response adaptation.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'chirp-backend)

(defun chirp-backend-test--timeline-entry (tweet &optional promoted-p)
  "Return one HomeTimeline entry carrying raw TWEET.

When PROMOTED-P is non-nil, include the item-level promoted marker used by X."
  (let ((item `(("tweet_results" . (("result" . ,tweet))))))
    (when promoted-p
      (push '("promotedMetadata" . t) item))
    `(("entryId" . "tweet-1")
      ("content" . (("itemContent" . ,item))))))

(defun chirp-backend-test--module-entry (tweet &optional nested-entry-id)
  "Return one nested timeline module entry carrying TWEET.

NESTED-ENTRY-ID identifies the tweet's occurrence inside the module."
  (let ((item-content `(("tweet_results" . (("result" . ,tweet))))))
    `(("entryId" . "conversation-thread-1")
      ("content" .
       (("items" .
         ((("entryId" .
            ,(or nested-entry-id "conversation-thread-1-tweet-1"))
           ("item" . (("itemContent" . ,item-content)))))))))))

(defun chirp-backend-test--module-item (tweet)
  "Return one instruction-level module item carrying TWEET."
  `(("entryId" . "module-item-1")
    ("item" .
     (("itemContent" .
       (("tweet_results" . (("result" . ,tweet)))))))))

(defun chirp-backend-test--bottom-cursor (cursor)
  "Return an X timeline bottom cursor carrying CURSOR."
  `(("entryId" . "cursor-bottom")
    ("content" . (("cursorType" . "Bottom")
                  ("value" . ,cursor)))))

(defun chirp-backend-test--instructions (entries)
  "Return minimal X timeline instructions containing ENTRIES."
  `(("instructions" .
     ((("type" . "TimelineAddEntries")
       ("entries" . ,entries))))))

(defun chirp-backend-test--payload-at-path (path value)
  "Return a JSON-style payload containing VALUE at PATH."
  (dolist (key (reverse (copy-sequence path)) value)
    (setq value (list (cons key value)))))

(defun chirp-backend-test--timeline-payload (path entries)
  "Return a minimal timeline payload at PATH containing ENTRIES."
  (chirp-backend-test--payload-at-path
   path (chirp-backend-test--instructions entries)))

(defun chirp-backend-test--home-timeline-payload (entries)
  "Return a minimal HomeTimeline GraphQL payload containing ENTRIES."
  (chirp-backend-test--timeline-payload
   '("data" "home" "home_timeline_urt") entries))

(defun chirp-backend-test--user-payload (&optional handle user-id)
  "Return a minimal profile payload for HANDLE and USER-ID."
  `(("data" .
     (("user" .
       (("result" .
         (("rest_id" . ,(or user-id "42"))
          ("legacy" . (("screen_name" . ,(or handle "alice"))
                       ("name" . "Alice")))))))))))

(defun chirp-backend-test--viewer-payload (&optional handle user-id)
  "Return a minimal Viewer payload for HANDLE and USER-ID."
  `(("data" .
     (("viewer" .
       (("user_results" .
         (("result" .
           (("rest_id" . ,(or user-id "42"))
            ("core" . (("screen_name" . ,(or handle "alice"))
                       ("name" . "Alice")))))))))))))

(defun chirp-backend-test--user-timeline-payload (entries)
  "Return a minimal user timeline payload containing ENTRIES."
  (chirp-backend-test--timeline-payload
   '("data" "user" "result" "timeline" "timeline") entries))

(defun chirp-backend-test--thread-payload (entries)
  "Return a minimal TweetDetail payload containing ENTRIES."
  (chirp-backend-test--timeline-payload
   '("data" "threaded_conversation_with_injections_v2") entries))

(defun chirp-backend-test--edit-history-entry (entry-id tweet)
  "Return edit-history ENTRY-ID carrying raw TWEET."
  `(("entryId" . ,entry-id)
    ("content" .
     (("items" .
       ((("item" .
          (("itemContent" .
            (("tweet_results" . (("result" . ,tweet))))))))))))))

(defun chirp-backend-test--edit-history-payload (entries)
  "Return a minimal TweetEditHistory payload containing ENTRIES."
  (chirp-backend-test--timeline-payload
   '("data" "tweet_result_by_rest_id" "result"
     "edit_history_timeline" "timeline")
   entries))

(ert-deftest chirp-stop-discards-session-owned-backend-state ()
  "Restarting Chirp should use new completed and in-flight cache tables."
  (let ((chirp--app nil))
    (unwind-protect
        (let ((old-cache (chirp--session-backend-read-cache (chirp--session)))
              (old-pending (chirp--session-backend-pending-reads
                            (chirp--session))))
          (puthash '(thread "1") 'cached old-cache)
          (puthash '(thread "2") '(callback) old-pending)
          (chirp-stop)
          (let ((new-cache
                 (chirp--session-backend-read-cache (chirp--session)))
                (new-pending
                 (chirp--session-backend-pending-reads (chirp--session))))
            (should-not (eq old-cache new-cache))
            (should-not (eq old-pending new-pending))
            (should (zerop (hash-table-count new-cache)))
            (should (zerop (hash-table-count new-pending)))))
      (chirp-stop))))

(ert-deftest chirp-stop-destroys-session-owned-native-state ()
  "Stopping Chirp should destroy and forget its native XChat session."
  (let ((chirp--app nil) (destroy-count 0) observed-state)
    (unwind-protect
        (let* ((app (chirp-app)) (state (appkit-app-model app)))
          (setf (chirp--session-xchat-native-session state)
                'native-session
                (chirp--session-xchat-native-epoch state) 7)
          (cl-letf
              (((symbol-function 'chirp-xchat-native-session-destroy)
                (lambda (session)
                  (setq destroy-count (1+ destroy-count)
                        observed-state
                        (list
                         (chirp--session-xchat-native-session state)
                         (chirp--session-xchat-native-epoch state)))
                  (should (eq session 'native-session)) t)))
            (chirp-stop))
          (should (= destroy-count 1))
          (should (equal observed-state '(nil nil)))
          (should-not chirp--app))
      (chirp-stop))))

(ert-deftest chirp-backend-thread-cache-reuses-fresh-results ()
  "Fresh cached thread results should avoid a second backend request."
  (let ((chirp-backend-read-cache-ttl 15)
        (now 1000)
        (request-count 0)
        first second third)
    (unwind-protect
        (progn
          (chirp-backend-clear-cache)
          (cl-letf (((symbol-function 'float-time)
                     (lambda (&rest _args)
                       now))
                    ((symbol-function 'chirp-x-graphql-request)
                     (lambda (_operation _variables callback &rest _options)
                       (setq request-count (1+ request-count))
                       (funcall
                        callback
                        (chirp-backend-test--thread-payload
                         (list
                          (chirp-backend-test--timeline-entry
                           '(("rest_id" . "123")
                             ("legacy" . (("full_text" . "hello")))))))))))
            (chirp-backend-thread "123"
                                  (lambda (tweets _envelope)
                                    (setq first tweets)))
            (chirp-backend-thread "123"
                                  (lambda (tweets _envelope)
                                    (setq second tweets)))
            (should (= request-count 1))
            (should (equal first second))
            (should-not (eq first second))
            (setf (plist-get (car first) :text) "changed")
            (chirp-backend-thread "123"
                                  (lambda (tweets _envelope)
                                    (setq third tweets)))
            (should (equal (plist-get (car third) :text) "hello"))
            (chirp-backend-invalidate-thread "123")
            (chirp-backend-thread "123" #'ignore)
            (should (= request-count 2))
            (setq now 1016)
            (chirp-backend-thread "123" #'ignore)
            (should (= request-count 3))))
      (chirp-backend-clear-cache))))

(ert-deftest chirp-backend-user-cache-coalesces-inflight-requests ()
  "Concurrent profile requests for the same handle should share one backend call."
  (let ((chirp-backend-read-cache-ttl 15)
        (request-count 0)
        success-callback
        results)
    (unwind-protect
        (progn
          (chirp-backend-clear-cache)
          (cl-letf (((symbol-function 'chirp-x-graphql-request)
                     (lambda (_operation _variables callback &rest _options)
                       (setq request-count (1+ request-count)
                             success-callback callback))))
            (chirp-backend-user "@Alice"
                                (lambda (user _envelope)
                                  (push user results)))
            (chirp-backend-user "alice"
                                (lambda (user _envelope)
                                  (push user results)))
            (should (= request-count 1))
            (should (functionp success-callback))
            (funcall success-callback
                     (chirp-backend-test--user-payload "alice" "42"))
            (should (= (length results) 2))
            (should (equal (plist-get (car results) :handle) "alice"))
            (should (equal (plist-get (cadr results) :handle) "alice"))
            (should-not (eq (car results) (cadr results)))
            (chirp-backend-invalidate-user "alice")
            (chirp-backend-user "alice" #'ignore)
            (should (= request-count 2))))
      (chirp-backend-clear-cache))))

(ert-deftest chirp-backend-user-cache-isolates-success-callback-errors ()
  "One coalesced profile callback failure should not starve later callers."
  (let ((chirp-backend-read-cache-ttl 15)
        success-callback
        second-result
        warnings)
    (unwind-protect
        (progn
          (chirp-backend-clear-cache)
          (cl-letf (((symbol-function 'chirp-x-graphql-request)
                     (lambda (_operation _variables callback &rest _options)
                       (setq success-callback callback)))
                    ((symbol-function 'display-warning)
                     (lambda (_type message &rest _args)
                       (push message warnings))))
            (chirp-backend-user
             "success-callback-user"
             (lambda (_user _envelope)
               (error "first callback failed")))
            (chirp-backend-user
             "success-callback-user"
             (lambda (user _envelope)
               (setq second-result user)))
            (funcall success-callback
                     (chirp-backend-test--user-payload
                      "success-callback-user" "42"))
            (should (equal (plist-get second-result :handle)
                           "success-callback-user"))
            (should (= (length warnings) 1))
            (should (string-match-p "first callback failed" (car warnings)))))
      (chirp-backend-clear-cache))))

(ert-deftest chirp-backend-user-cache-isolates-error-callback-errors ()
  "One coalesced profile errback failure should not starve later callers."
  (let ((chirp-backend-read-cache-ttl 15)
        error-callback
        second-message
        warnings)
    (unwind-protect
        (progn
          (chirp-backend-clear-cache)
          (cl-letf (((symbol-function 'chirp-x-graphql-request)
                     (lambda (_operation _variables _callback &rest options)
                       (setq error-callback (plist-get options :errback))))
                    ((symbol-function 'display-warning)
                     (lambda (_type message &rest _args)
                       (push message warnings))))
            (chirp-backend-user
             "error-callback-user" #'ignore
             (lambda (_message)
               (error "first errback failed")))
            (chirp-backend-user
             "error-callback-user" #'ignore
             (lambda (message)
               (setq second-message message)))
            (funcall error-callback "remote failure")
            (should (equal second-message "remote failure"))
            (should (= (length warnings) 1))
            (should (string-match-p "first errback failed" (car warnings)))))
      (chirp-backend-clear-cache))))

(ert-deftest chirp-backend-feed-uses-x-graphql-and-preserves-pagination ()
  "Home feeds should use direct X GraphQL with normalized pagination metadata."
  (let (operation variables raw-tweets next-cursor tweets)
    (cl-letf (((symbol-function 'chirp-x-graphql-request)
               (lambda (request-operation request-variables callback &rest _options)
                 (setq operation request-operation
                       variables request-variables)
                 (funcall
                  callback
                  (chirp-backend-test--home-timeline-payload
                   (list
                    (chirp-backend-test--timeline-entry
                     '(("rest_id" . "1")
                       ("legacy" . (("full_text" . "hello")))))
                    (chirp-backend-test--bottom-cursor "cursor-next"))))))
              ((symbol-function 'chirp--top-level-tweets-from-x)
               (lambda (items)
                 (setq raw-tweets items)
                 '((:id "1")))))
      (chirp-backend-feed
       (lambda (items envelope)
         (setq tweets items
               next-cursor (chirp-backend-envelope-next-cursor envelope)))
       nil nil 20 "cursor-prev"))
    (should (equal (plist-get operation :name) "HomeTimeline"))
    (should (equal (alist-get "count" variables nil nil #'string=) 20))
    (should (equal (alist-get "cursor" variables nil nil #'string=)
                   "cursor-prev"))
    (should (equal (length raw-tweets) 1))
    (should (equal tweets '((:id "1" :timeline-entry-id "tweet-1"))))
    (should (equal next-cursor "cursor-next"))))

(ert-deftest chirp-backend-timeline-preserves-reply-control-envelope ()
  "Timeline adaptation should retain viewer-specific reply controls."
  (let* ((raw
          '(("__typename" . "TweetWithVisibilityResults")
            ("tweet" . (("rest_id" . "123")
                        ("legacy" . (("full_text" . "Restricted")
                                     ("conversation_control" .
                                      (("mode" . "ByInvitation")))))))
            ("limitedActionResults" .
             (("limited_actions" . ((("action" . "Reply"))))))))
         (page
          (chirp-backend--timeline-page
           (chirp-backend-test--home-timeline-payload
            (list (chirp-backend-test--timeline-entry raw)))
           '(("data" "home" "home_timeline_urt"))
           10
           "the home timeline"))
         (tweet (car (car page))))
    (should (equal (plist-get tweet :reply-control-mode) "ByInvitation"))
    (should (plist-get tweet :reply-limited-p))))

(ert-deftest chirp-backend-feed-selects-the-following-operation ()
  "Following feeds should use the chronological X timeline operation."
  (let (operation)
    (cl-letf (((symbol-function 'chirp-x-graphql-request)
               (lambda (request-operation _variables callback &rest _options)
                 (setq operation request-operation)
                 (funcall callback
                          (chirp-backend-test--home-timeline-payload nil))))
              ((symbol-function 'chirp--top-level-tweets-from-x)
               (lambda (_items) nil)))
      (chirp-backend-feed #'ignore t))
    (should (equal (plist-get operation :name) "HomeLatestTimeline"))))

(ert-deftest chirp-backend-feed-reports-an-invalid-x-timeline ()
  "Malformed direct timeline responses should not reach feed callbacks."
  (let (success failure)
    (cl-letf (((symbol-function 'chirp-x-graphql-request)
               (lambda (_operation _variables callback &rest _options)
                 (funcall callback '(("data" . nil))))))
      (chirp-backend-feed
       (lambda (&rest _args)
         (setq success t))
       nil
       (lambda (message)
         (setq failure message))))
    (should-not success)
    (should (equal failure "X did not return a home timeline"))))

(ert-deftest chirp-backend-bookmarks-uses-direct-v2-timeline ()
  "Bookmarks should accept X's v2 timeline path without a CLI process."
  (let (operation variables tweets)
    (cl-letf (((symbol-function 'chirp-x-graphql-request)
               (lambda (request-operation request-variables callback &rest _options)
                 (setq operation request-operation
                       variables request-variables)
                 (funcall
                  callback
                  (chirp-backend-test--timeline-payload
                   '("data" "bookmark_timeline_v2" "timeline")
                   (list
                    (chirp-backend-test--timeline-entry
                     '(("rest_id" . "1")
                       ("legacy" . (("full_text" . "saved")))))))))))
      (chirp-backend-bookmarks
       (lambda (items _envelope)
         (setq tweets items))))
    (should (equal (plist-get operation :name) "Bookmarks"))
    (should (equal (alist-get "count" variables nil nil #'string=) 20))
    (should (equal (mapcar (lambda (tweet) (plist-get tweet :id)) tweets)
                   '("1")))))

(ert-deftest chirp-backend-search-uses-post-and-normalizes-modules ()
  "Search should use direct POST and retain tweets nested in modules."
  (let (operation variables tweets)
    (cl-letf (((symbol-function 'chirp-x-graphql-request)
               (lambda (request-operation request-variables callback &rest _options)
                 (setq operation request-operation
                       variables request-variables)
                 (funcall
                  callback
                  (chirp-backend-test--timeline-payload
                   '("data" "search_by_raw_query" "search_timeline" "timeline")
                   (list
                    (chirp-backend-test--module-entry
                     '(("rest_id" . "2")
                       ("legacy" . (("full_text" . "nested")))))))))))
      (chirp-backend-search
       "emacs"
       (lambda (items _envelope)
         (setq tweets items))))
    (should (equal (plist-get operation :name) "SearchTimeline"))
    (should (eq (plist-get operation :method) 'post))
    (should (equal (alist-get "rawQuery" variables nil nil #'string=)
                   "emacs"))
    (should (equal (mapcar (lambda (tweet) (plist-get tweet :id)) tweets)
                   '("2")))))

(ert-deftest chirp-backend-timeline-keeps-module-items-and-pinned-entry ()
  "Shared timeline adaptation should retain instruction-level special entries."
  (let* ((module-item
          (chirp-backend-test--module-item
           '(("rest_id" . "1")
             ("legacy" . (("full_text" . "module"))))))
         (pinned-entry
          (chirp-backend-test--timeline-entry
           '(("rest_id" . "2")
             ("legacy" . (("full_text" . "pinned"))))))
         (instructions
          (list
           (list '("type" . "TimelineAddToModule")
                 (cons "moduleItems" (list module-item)))
           (list '("type" . "TimelinePinEntry")
                 (cons "entry" pinned-entry))))
         (payload
          (chirp-backend-test--payload-at-path
           '("data" "timeline")
           (list (cons "instructions" instructions))))
         (page (chirp-backend--timeline-page
                payload '(("data" "timeline")) 10 "the test timeline")))
    (should (equal (mapcar (lambda (tweet) (plist-get tweet :id)) (car page))
                   '("1" "2")))))

(ert-deftest chirp-backend-timeline-preserves-pinned-tweet-in-module ()
  "Timeline adaptation should preserve distinct occurrences of one tweet."
  (let* ((tweet-id "2094737512484970656")
         (nested-entry-id
          "profile-conversation-2094846962946801665-tweet-2094737512484970656")
         (raw
          `(("rest_id" . ,tweet-id)
            ("legacy" . (("full_text" . "Pinned and nested")))))
         (pinned-entry (chirp-backend-test--timeline-entry raw))
         (conversation-entry
          (chirp-backend-test--module-entry raw nested-entry-id))
         (instructions
          (list
           (list '("type" . "TimelinePinEntry")
                 (cons "entry" pinned-entry))
           (list '("type" . "TimelineAddEntries")
                 (cons "entries" (list conversation-entry)))))
         (payload
          (chirp-backend-test--payload-at-path
           '("data" "timeline")
           (list (cons "instructions" instructions))))
         (tweets
          (car (chirp-backend--timeline-page
                payload '(("data" "timeline")) 10 "the test timeline"))))
    (should (equal (mapcar (lambda (tweet) (plist-get tweet :id)) tweets)
                   (list tweet-id tweet-id)))
    (should
     (equal (mapcar (lambda (tweet)
                      (plist-get tweet :timeline-entry-id))
                    tweets)
            (list "tweet-1" nested-entry-id)))
    (should
     (equal (mapcar (lambda (tweet)
                      (plist-get tweet :timeline-context))
                    tweets)
            '(pinned nil)))))

(ert-deftest chirp-backend-simple-mutations-map-to-direct-x-operations ()
  "Simple state and delete actions should map directly to X mutations."
  (dolist (case '(("like" "FavoriteTweet" "tweet_id")
                  ("unlike" "UnfavoriteTweet" "tweet_id")
                  ("retweet" "CreateRetweet" "tweet_id")
                  ("unretweet" "DeleteRetweet" "source_tweet_id")
                  ("bookmark" "CreateBookmark" "tweet_id")
                  ("unbookmark" "DeleteBookmark" "tweet_id")
                  ("delete" "DeleteTweet" "tweet_id")))
    (let (operation variables success)
      (cl-letf (((symbol-function 'chirp-x-graphql-request)
                 (lambda (request-operation request-variables callback &rest _options)
                   (setq operation request-operation
                         variables request-variables)
                   (funcall callback '(("data" . nil))))))
        (chirp-backend-request
         (if (equal (car case) "delete")
             '("delete" "--yes" "123")
           (list (car case) "123"))
         (lambda (_payload _envelope)
           (setq success t))))
      (should success)
      (should (equal (plist-get operation :name) (cadr case)))
      (should (equal (alist-get (nth 2 case) variables nil nil #'string=)
                     "123")))))

(ert-deftest chirp-backend-follow-mutations-resolve-user-and-use-rest ()
  "Follow state changes should use direct non-retried REST form requests."
  (dolist (case '(("follow" . "create") ("unfollow" . "destroy")))
    (let (path form success)
      (cl-letf (((symbol-function 'chirp-backend-user)
                 (lambda (handle callback &optional _errback)
                   (should (equal handle "Alice"))
                   (funcall callback '(:id "42") nil)))
                ((symbol-function 'chirp-x-api-request)
                 (lambda (service request-path callback &rest options)
                   (should (eq service 'web))
                   (should (eq (plist-get options :method) 'post))
                   (setq path request-path
                         form (plist-get options :form))
                   (funcall callback '(("id_str" . "42"))))))
        (chirp-backend-request
         (list (car case) "Alice")
         (lambda (_payload _envelope)
           (setq success t))))
      (should success)
      (should (equal path (format "1.1/friendships/%s.json" (cdr case))))
      (should (equal (alist-get "user_id" form nil nil #'string=) "42")))))

(ert-deftest chirp-backend-user-posts-passes-cursor-without-cache ()
  "Profile post pagination should forward the next X cursor directly."
  (let (operation variables tweets next-cursor)
    (cl-letf (((symbol-function 'chirp-backend-user)
               (lambda (handle callback &optional _errback)
                 (should (equal handle "Alice"))
                 (funcall callback '(:id "42") nil)))
              ((symbol-function 'chirp-x-graphql-request)
               (lambda (request-operation request-variables callback &rest _options)
                 (setq operation request-operation
                       variables request-variables)
                 (funcall
                  callback
                  (chirp-backend-test--user-timeline-payload
                   (list
                    (chirp-backend-test--timeline-entry
                     '(("rest_id" . "1")
                       ("legacy" . (("full_text" . "hello")))))
                    (chirp-backend-test--bottom-cursor "cursor-next")))))))
      (chirp-backend-user-posts
       "@Alice"
       (lambda (items envelope)
         (setq tweets items
               next-cursor (chirp-backend-envelope-next-cursor envelope)))
       nil 15 "cursor-prev"))
    (should (equal (plist-get operation :name) "UserTweets"))
    (should (equal (alist-get "userId" variables nil nil #'string=) "42"))
    (should (equal (alist-get "count" variables nil nil #'string=) 15))
    (should (equal (alist-get "cursor" variables nil nil #'string=)
                   "cursor-prev"))
    (should (equal (mapcar (lambda (tweet) (plist-get tweet :id)) tweets)
                   '("1")))
    (should (equal next-cursor "cursor-next"))))

(ert-deftest chirp-backend-lists-merge-direct-collections-and-use-cache ()
  "Accessible lists should merge direct REST collections and reuse the cache."
  (let ((chirp-backend-read-cache-ttl 15)
        (now 1000)
        (request-count 0)
        first second)
    (unwind-protect
        (progn
          (chirp-backend-clear-cache)
          (cl-letf
              (((symbol-function 'float-time)
                (lambda (&rest _args)
                  now))
               ((symbol-function 'chirp-backend-whoami)
                (lambda (callback &optional _errback)
                  (funcall callback '(:id "42") nil)))
               ((symbol-function 'chirp-x-api-request)
                (lambda (_service path callback &rest _options)
                  (setq request-count (1+ request-count))
                  (funcall
                   callback
                   (cond
                    ((string-match-p "ownerships" path)
                     '(("lists" .
                        ((("id_str" . "1") ("name" . "Owned"))
                         (("id_str" . "2") ("name" . "Shared"))))
                       ("next_cursor_str" . "0")))
                    ((string-match-p "subscriptions" path)
                     '(("lists" .
                        ((("id_str" . "2") ("name" . "Shared"))
                         (("id_str" . "3") ("name" . "Subscribed"))))
                       ("next_cursor_str" . "0")))
                    (t
                     '(("lists" .
                        ((("id_str" . "4") ("name" . "Member"))))
                       ("next_cursor_str" . "0"))))))))
            (chirp-backend-lists
             (lambda (lists _envelope)
               (setq first lists)))
            (chirp-backend-lists
             (lambda (lists _envelope)
               (setq second lists)))
            (should (= request-count 3))
            (should (equal first second))
            (should-not (eq first second))
            (should
             (equal (chirp-get
                     (cl-find "2" first
                              :key (lambda (item) (chirp-get item "id"))
                              :test #'equal)
                     "sources")
                    '("owned" "subscribed")))
            (should
             (equal (chirp-get
                     (cl-find "4" first
                              :key (lambda (item) (chirp-get item "id"))
                              :test #'equal)
                     "sources")
                    '("member")))
            (setq now 1016)
            (chirp-backend-lists #'ignore)
            (should (= request-count 6))))
      (chirp-backend-clear-cache))))

(ert-deftest chirp-backend-search-users-uses-direct-typeahead ()
  "User completion should normalize and deduplicate direct typeahead results."
  (let (service path query users)
    (cl-letf (((symbol-function 'chirp-x-api-request)
               (lambda (request-service request-path callback &rest options)
                 (setq service request-service
                       path request-path
                       query (plist-get options :query))
                 (funcall callback
                          '(("users" .
                             ((("id_str" . "1")
                               ("screen_name" . "emacs")
                               ("name" . "Emacs"))
                              (("id_str" . "1")
                               ("screen_name" . "duplicate")
                               ("name" . "Duplicate"))
                              (("id_str" . "2")
                               ("screen_name" . "emacslife")
                               ("name" . "Emacs Life")))))))))
      (chirp-backend-search-users
       "@em"
       (lambda (items _envelope)
         (setq users items))
       nil 5))
    (should (eq service 'web))
    (should (equal path "1.1/search/typeahead.json"))
    (should (equal (alist-get "q" query nil nil #'string=) "em"))
    (should (equal (mapcar (lambda (user) (plist-get user :handle)) users)
                   '("emacs" "emacslife")))))

(ert-deftest chirp-backend-translate-uses-direct-strato-response ()
  "Tweet translation should adapt the authenticated X Strato response."
  (let (service path result failure)
    (cl-letf (((symbol-function 'chirp-x-api-request)
               (lambda (request-service request-path callback &rest _options)
                 (setq service request-service
                       path request-path)
                 (funcall callback
                          '(("id_str" . "123")
                            ("translation" . "你好")
                            ("destinationLanguage" . "zh")
                            ("translationState" . "Success"))))))
      (chirp-backend-translate
       "123" "zh"
       (lambda (data _envelope)
         (setq result data))
       (lambda (message)
         (setq failure message))))
    (should-not failure)
    (should (eq service 'legacy))
    (should (string-match-p
             (concat "tweetId=123,destinationLanguage=Some(zh),"
                     "translationSource=Some(Google)")
             path))
    (should (equal (chirp-get result "id") "123"))
    (should (equal (chirp-get result "translation") "你好"))
    (should (equal (chirp-get result "destinationLanguage") "zh"))))

(ert-deftest chirp-backend-whoami-cache-reuses-fresh-results ()
  "Fresh cached whoami results should avoid a second backend request."
  (let ((chirp-backend-read-cache-ttl 15)
        (request-count 0)
        first second)
    (unwind-protect
        (progn
          (chirp-backend-clear-cache)
          (cl-letf (((symbol-function 'chirp-x-graphql-request)
                     (lambda (_operation _variables callback &rest _options)
                       (setq request-count (1+ request-count))
                       (funcall callback
                                (chirp-backend-test--viewer-payload
                                 "alice" "42")))))
            (chirp-backend-whoami
             (lambda (user _envelope)
               (setq first user)))
            (chirp-backend-whoami
             (lambda (user _envelope)
               (setq second user)))
            (should (= request-count 1))
            (should (equal first second))
            (should-not (eq first second))))
      (chirp-backend-clear-cache))))

(ert-deftest chirp-backend-likes-resolves-user-id-and-marks-results ()
  "Likes should resolve the handle and mark every direct timeline result."
  (let (operation variables tweets)
    (cl-letf (((symbol-function 'chirp-backend-user)
               (lambda (handle callback &optional _errback)
                 (should (equal handle "@Alice"))
                 (funcall callback '(:id "42") nil)))
              ((symbol-function 'chirp-x-graphql-request)
               (lambda (request-operation request-variables callback &rest _options)
                 (setq operation request-operation
                       variables request-variables)
                 (funcall
                  callback
                  (chirp-backend-test--user-timeline-payload
                   (list
                    (chirp-backend-test--timeline-entry
                     '(("rest_id" . "1")
                       ("legacy" . (("full_text" . "one")))))
                    (chirp-backend-test--timeline-entry
                     '(("rest_id" . "2")
                       ("legacy" . (("full_text" . "two")))))))))))
      (chirp-backend-likes
       "@Alice"
       (lambda (items _envelope)
         (setq tweets items))))
    (should (equal (plist-get operation :name) "Likes"))
    (should (equal (alist-get "userId" variables nil nil #'string=) "42"))
    (should (equal (mapcar (lambda (tweet) (plist-get tweet :liked-p)) tweets)
                   '(t t)))))

(ert-deftest chirp-backend-user-replies-shapes-direct-request ()
  "Replies should use X search with a handle filter and cursor."
  (let (operation variables)
    (cl-letf (((symbol-function 'chirp-x-graphql-request)
               (lambda (request-operation request-variables callback &rest _options)
                 (setq operation request-operation
                       variables request-variables)
                 (funcall
                  callback
                  (chirp-backend-test--timeline-payload
                   '("data" "search_by_raw_query" "search_timeline" "timeline")
                   nil)))))
      (chirp-backend-user-replies "@Alice" #'ignore nil 10 "cursor-prev"))
    (should (equal (plist-get operation :name) "SearchTimeline"))
    (should (equal (alist-get "rawQuery" variables nil nil #'string=)
                   "from:Alice filter:replies"))
    (should (equal (alist-get "count" variables nil nil #'string=) 10))
    (should (equal (alist-get "cursor" variables nil nil #'string=)
                   "cursor-prev"))))

(ert-deftest chirp-backend-user-highlights-shapes-direct-request ()
  "Highlights should select their X operation and requested count."
  (let (operation variables)
    (cl-letf (((symbol-function 'chirp-backend-user)
               (lambda (_handle callback &optional _errback)
                 (funcall callback '(:id "42") nil)))
              ((symbol-function 'chirp-x-graphql-request)
               (lambda (request-operation request-variables callback &rest _options)
                 (setq operation request-operation
                       variables request-variables)
                 (funcall callback
                          (chirp-backend-test--user-timeline-payload nil)))))
      (chirp-backend-user-highlights "@Alice" #'ignore nil 7))
    (should (equal (plist-get operation :name) "UserHighlightsTweets"))
    (should (equal (alist-get "count" variables nil nil #'string=) 7))))

(ert-deftest chirp-backend-user-media-shapes-direct-request ()
  "Media should select its X operation and disable promoted content."
  (let (operation variables)
    (cl-letf (((symbol-function 'chirp-backend-user)
               (lambda (_handle callback &optional _errback)
                 (funcall callback '(:id "42") nil)))
              ((symbol-function 'chirp-x-graphql-request)
               (lambda (request-operation request-variables callback &rest _options)
                 (setq operation request-operation
                       variables request-variables)
                 (funcall callback
                          (chirp-backend-test--user-timeline-payload nil)))))
      (chirp-backend-user-media "@Alice" #'ignore nil 6))
    (should (equal (plist-get operation :name) "UserMedia"))
    (should (equal (alist-get "count" variables nil nil #'string=) 6))
    (should (eq (alist-get "includePromotedContent" variables nil nil #'string=)
                :json-false))))

(ert-deftest chirp-backend-followers-resolve-user-and-normalize-rest-users ()
  "Followers should resolve the handle before requesting the direct REST list."
  (let ((chirp-backend-read-cache-ttl 0)
        service path query users envelope)
    (cl-letf (((symbol-function 'chirp-backend-user)
               (lambda (handle callback &optional _errback)
                 (should (equal handle "@Bob"))
                 (funcall callback '(:id "42") nil)))
              ((symbol-function 'chirp-x-api-request)
               (lambda (request-service request-path callback &rest options)
                 (setq service request-service
                       path request-path
                       query (plist-get options :query))
                 (funcall callback
                          '(("users" .
                             ((("id_str" . "1")
                               ("screen_name" . "alice")
                               ("name" . "Alice"))))
                            ("next_cursor_str" . "next"))))))
      (chirp-backend-followers
       "@Bob"
       (lambda (items response-envelope)
         (setq users items
               envelope response-envelope))))
    (should (eq service 'legacy))
    (should (equal path "followers/list.json"))
    (should (equal (alist-get "user_id" query nil nil #'string=) "42"))
    (should (equal (plist-get (car users) :handle) "alice"))
    (should (equal (chirp-backend-envelope-next-cursor envelope) "next"))))

(ert-deftest chirp-backend-following-users-use-the-friends-rest-list ()
  "Following users should share the direct relationship-list adapter."
  (let ((chirp-backend-read-cache-ttl 0)
        path users)
    (cl-letf (((symbol-function 'chirp-backend-user)
               (lambda (_handle callback &optional _errback)
                 (funcall callback '(:id "42") nil)))
              ((symbol-function 'chirp-x-api-request)
               (lambda (_service request-path callback &rest _options)
                 (setq path request-path)
                 (funcall callback
                          '(("users" .
                             ((("id_str" . "1")
                               ("screen_name" . "alice")
                               ("name" . "Alice")))))))))
      (chirp-backend-following-users
       "@Bob"
       (lambda (items _envelope)
         (setq users items))))
    (should (equal path "friends/list.json"))
    (should (equal (plist-get (car users) :handle) "alice"))))

(ert-deftest chirp-backend-list-passes-id-to-direct-x-graphql ()
  "List requests should pass their ID to direct X GraphQL."
  (let (operation variables)
    (cl-letf (((symbol-function 'chirp-x-graphql-request)
               (lambda (request-operation request-variables callback &rest _options)
                 (setq operation request-operation
                       variables request-variables)
                 (funcall
                  callback
                  (chirp-backend-test--timeline-payload
                   '("data" "list" "tweets_timeline" "timeline") nil)))))
      (chirp-backend-list "1956792682412345678" #'ignore))
    (should (equal (plist-get operation :name) "ListLatestTweetsTimeline"))
    (should (equal (alist-get "listId" variables nil nil #'string=)
                   "1956792682412345678"))))

(ert-deftest chirp-backend-thread-passes-explicit-max-results ()
  "Thread requests should pass Chirp's explicit limit and focus id to X."
  (let ((chirp-thread-max-results 20)
        operation
        variables)
    (cl-letf (((symbol-function 'chirp-x-graphql-request)
               (lambda (request-operation request-variables callback &rest _options)
                 (setq operation request-operation
                       variables request-variables)
                 (funcall
                  callback
                  (chirp-backend-test--thread-payload
                   (list
                    (chirp-backend-test--timeline-entry
                     '(("rest_id" . "123")
                       ("legacy" . (("full_text" . "hello")))))))))))
      (chirp-backend-thread "123" #'ignore))
    (should (equal (plist-get operation :name) "TweetDetail"))
    (should (equal (alist-get "focalTweetId" variables nil nil #'string=)
                   "123"))
    (should (equal (alist-get "count" variables nil nil #'string=) 20))
    (should (alist-get "withDisallowedReplyControls"
                       (plist-get operation :field-toggles)
                       nil nil #'string=))))

(ert-deftest chirp-backend-thread-accepts-a-retweet-wrapper-id ()
  "A retweet permalink should match its raw wrapper after normalization."
  (let ((chirp-backend-read-cache-ttl 0)
        result
        failure)
    (cl-letf (((symbol-function 'chirp-x-graphql-request)
               (lambda (_operation _variables callback &rest _options)
                 (let* ((original
                         '(("rest_id" . "200")
                           ("legacy" . (("full_text" . "original")))))
                        (wrapper
                         `(("rest_id" . "100")
                           ("legacy" .
                            (("full_text" . "RT @user: original")
                             ("retweeted_status_result" .
                              (("result" . ,original))))))))
                   (funcall
                    callback
                    (chirp-backend-test--thread-payload
                     (list (chirp-backend-test--timeline-entry wrapper))))))))
      (chirp-backend-thread
       "100"
       (lambda (tweets _envelope)
         (setq result tweets))
       (lambda (message)
         (setq failure message))))
    (should-not failure)
    (should (equal (plist-get (car result) :id) "200"))))

(ert-deftest chirp-backend-edit-history-normalizes-versions-and-caches-read ()
  "Edit history should preserve version order and reuse one fresh X read."
  (let* ((chirp-backend-read-cache-ttl 15)
         (latest
          '(("rest_id" . "200")
            ("legacy" .
             (("full_text" . "latest")
              ("created_at" . "LATEST-TIME")))))
         (stale
          '(("rest_id" . "100")
            ("legacy" .
             (("full_text" . "stale")
              ("created_at" . "STALE-TIME")))))
         (payload
          (chirp-backend-test--edit-history-payload
           (list
            (chirp-backend-test--edit-history-entry
             "latestTweet" latest)
            (chirp-backend-test--edit-history-entry
             "staleTweets"
             `(("__typename" . "TweetWithVisibilityResults")
               ("tweet" . ,stale))))))
         (request-count 0)
         operation variables versions failure)
    (unwind-protect
        (progn
          (chirp-backend-clear-cache)
          (cl-letf
              (((symbol-function 'chirp-x-graphql-request)
                (lambda (request-operation request-variables callback
                                           &rest _options)
                  (setq request-count (1+ request-count)
                        operation request-operation
                        variables request-variables)
                  (funcall callback payload))))
            (chirp-backend-edit-history
             "200"
             (lambda (items _envelope)
               (setq versions items))
             (lambda (message) (setq failure message)))
            (chirp-backend-edit-history "200" #'ignore)
            (should (= request-count 1))
            (chirp-backend-invalidate-edit-history "200")
            (chirp-backend-edit-history "200" #'ignore)
            (should (= request-count 2)))
          (should-not failure)
          (should
           (equal (mapcar (lambda (tweet) (plist-get tweet :id))
                          versions)
                  '("200" "100")))
          (should (equal (plist-get operation :name) "TweetEditHistory"))
          (should
           (equal (alist-get "tweetId" variables nil nil #'string=)
                  "200"))
          (should
           (alist-get "withQuickPromoteEligibilityTweetFields"
                      variables nil nil #'string=))
          (should
           (alist-get "responsive_web_grok_analyze_post_followups_enabled"
                      (plist-get operation :features) nil nil #'string=))
          (should
           (eq
            (alist-get "longform_notetweets_inline_media_enabled"
                       (plist-get operation :features) nil nil #'string=)
            :json-false))
          (should-not (plist-member operation :field-toggles)))
      (chirp-backend-clear-cache))))

(ert-deftest chirp-backend-edit-history-rejects-missing-timeline ()
  "Edit history should report a response without its required timeline."
  (let ((chirp-backend-read-cache-ttl 0)
        failure)
    (cl-letf (((symbol-function 'chirp-x-graphql-request)
               (lambda (_operation _variables callback &rest _options)
                 (funcall callback '(("data" . nil))))))
      (chirp-backend-edit-history
       "200" #'ignore
       (lambda (message) (setq failure message))))
    (should (equal failure "X did not return tweet edit history"))))

(ert-deftest chirp-backend-article-uses-direct-rich-content-operation ()
  "Article enrichment should request and normalize direct X article data."
  (let ((chirp-backend-read-cache-ttl 0)
        operation variables result failure)
    (cl-letf (((symbol-function 'chirp-x-graphql-request)
               (lambda (request-operation request-variables callback &rest _options)
                 (setq operation request-operation
                       variables request-variables)
                 (funcall
                  callback
                  (chirp-backend-test--payload-at-path
                   '("data" "tweetResult" "result")
                   '(("rest_id" . "123")
                     ("legacy" . (("full_text" . "Article preview")))
                     ("article" .
                      (("article_results" .
                        (("result" .
                          (("title" . "Longform")
                           ("plain_text" . "Full article body.")))))))))))))
      (chirp-backend-article
       "123"
       (lambda (tweet _envelope)
         (setq result tweet))
       (lambda (message)
         (setq failure message))))
    (should-not failure)
    (should (equal (plist-get operation :name) "TweetResultByRestId"))
    (should (equal (alist-get "tweetId" variables nil nil #'string=) "123"))
    (should (equal (plist-get result :article-title) "Longform"))
    (should (equal (plist-get result :article-text) "Full article body."))))

(ert-deftest chirp-backend-tweet-weighted-length-handles-wide-text-emoji-and-urls ()
  "Create routing should follow X weighting for common text forms."
  (should (= (chirp-backend-tweet-weighted-length (make-string 280 ?x))
             280))
  (should (= (chirp-backend-tweet-weighted-length (make-string 141 ?你))
             282))
  (should (= (chirp-backend-tweet-weighted-length "👩‍👩‍👧‍👦") 2))
  (should (= (chirp-backend-tweet-weighted-length
              (apply #'concat (make-list 57 "←‍←")))
             285))
  (should (= (chirp-backend-tweet-weighted-length
              (concat "see https://example.com/" (make-string 400 ?x)))
             27)))

(ert-deftest chirp-backend-compose-creates-a-short-direct-post ()
  "Short drafts should use CreateTweet and return the created tweet ID."
  (let (operation variables result envelope failure)
    (cl-letf (((symbol-function 'chirp-x-graphql-request)
               (lambda (request-operation request-variables callback &rest _options)
                 (setq operation request-operation
                       variables request-variables)
                 (funcall
                  callback
                  (chirp-backend-test--payload-at-path
                   '("data" "create_tweet" "tweet_results" "result")
                   '(("rest_id" . "123")))))))
      (chirp-backend-compose
       :kind 'post :text "hello" :attachments nil
       :callback (lambda (created-tweet-id raw-envelope)
                   (setq result created-tweet-id
                         envelope raw-envelope))
       :errback (lambda (message)
                  (setq failure message))))
    (should-not failure)
    (should (equal (plist-get operation :name) "CreateTweet"))
    (should (equal result "123"))
    (should envelope)
    (should (equal (alist-get "tweet_text" variables nil nil #'string=)
                   "hello"))
    (should-not (assoc-string "conversation_control" variables t))
    (should (vectorp
             (chirp-get-in variables '("media" "media_entities"))))
    (should (vectorp
             (chirp-get variables "semantic_annotation_ids")))))

(ert-deftest chirp-backend-compose-uploads-alt-text-after-media ()
  "Image descriptions should be written after upload and before CreateTweet."
  (let ((file (make-temp-file "chirp-backend-alt-" nil ".png"))
        uploaded
        described)
    (unwind-protect
        (progn
          (write-region "png" nil file nil 'silent)
          (cl-letf (((symbol-function 'chirp-x-upload-media)
                     (lambda (path callback &rest _options)
                       (should (equal path file))
                       (setq uploaded t)
                       (funcall callback "media-1")))
                    ((symbol-function 'chirp-x-upload-media-alt-text)
                     (lambda (media-id text callback &rest _options)
                       (should (equal media-id "media-1"))
                       (should (equal text "a cat"))
                       (setq described t)
                       (funcall callback nil)))
                    ((symbol-function 'chirp-x-graphql-request)
                     (lambda (_operation _variables callback &rest _options)
                       (funcall
                        callback
                        (chirp-backend-test--payload-at-path
                         '("data" "create_tweet" "tweet_results" "result")
                         '(("rest_id" . "123")))))))
            (chirp-backend-compose
             :kind 'post :text "hello"
             :attachments (list (list :path file :description "a cat"))
             :callback #'ignore))
          (should uploaded)
          (should described))
      (delete-file file))))

(ert-deftest chirp-backend-compose-includes-reply-audience-for-posts ()
  "Post and quote drafts should send conversation_control for a reply audience."
  (dolist (kind '(post quote))
    (let (variables)
      (cl-letf (((symbol-function 'chirp-x-graphql-request)
                 (lambda (_operation request-variables callback &rest _options)
                   (setq variables request-variables)
                   (funcall
                    callback
                    (chirp-backend-test--payload-at-path
                     '("data" "create_tweet" "tweet_results" "result")
                     '(("rest_id" . "123")))))))
        (chirp-backend-compose
         :kind kind :text "hello"
         :target-id (and (eq kind 'quote) "99")
         :reply-audience 'community
         :attachments nil
         :callback #'ignore))
      (should (equal (chirp-get-in variables '("conversation_control" "mode"))
                     "Community"))
      (when (eq kind 'quote)
        (should (equal (chirp-get variables "attachment_url")
                       "https://x.com/i/status/99"))))))

(ert-deftest chirp-backend-compose-omits-reply-audience-for-replies ()
  "Replies should inherit conversation rules and omit conversation_control."
  (let (variables)
    (cl-letf (((symbol-function 'chirp-x-graphql-request)
               (lambda (_operation request-variables callback &rest _options)
                 (setq variables request-variables)
                 (funcall
                  callback
                  (chirp-backend-test--payload-at-path
                   '("data" "create_tweet" "tweet_results" "result")
                   '(("rest_id" . "456")))))))
      (chirp-backend-compose
       :kind 'reply :text "hello" :target-id "99"
       :reply-audience 'community :attachments nil
       :callback #'ignore))
    (should-not (assoc-string "conversation_control" variables t))
    (should (equal (chirp-get-in variables '("reply" "in_reply_to_tweet_id"))
                   "99"))))

(ert-deftest chirp-backend-compose-rejects-an-invalid-reply-audience ()
  "Unknown reply-audience symbols should fail before CreateTweet."
  (let (success failure)
    (chirp-backend-compose
     :kind 'post :text "hello" :reply-audience 'nobody
     :attachments nil
     :callback (lambda (&rest _args)
                 (setq success t))
     :errback (lambda (message)
                (setq failure message)))
    (should-not success)
    (should (string-match-p "Reply audience is invalid" failure))))

(ert-deftest chirp-backend-compose-builds-reply-quote-and-media-variables ()
  "Replies and quotes should share create routing with structured targets."
  (let ((file (make-temp-file "chirp-backend-compose-" nil ".png")))
    (unwind-protect
        (dolist (kind '(reply quote))
          (let (variables uploaded result)
            (cl-letf (((symbol-function 'chirp-x-upload-media)
                       (lambda (path callback &rest _options)
                         (should (equal path file))
                         (setq uploaded t)
                         (funcall callback "media-1")))
                      ((symbol-function 'chirp-x-graphql-request)
                       (lambda (_operation request-variables callback
                                           &rest _options)
                         (setq variables request-variables)
                         (funcall
                          callback
                          (chirp-backend-test--payload-at-path
                           '("data" "create_tweet" "tweet_results" "result")
                           '(("rest_id" . "456")))))))
              (chirp-backend-compose
               :kind kind :text "hello" :target-id "99"
               :attachments (list file)
               :callback (lambda (created-tweet-id _envelope)
                           (setq result created-tweet-id))))
            (should uploaded)
            (should (equal result "456"))
            (let* ((entities
                    (chirp-get-in variables '("media" "media_entities")))
                   (entity (aref entities 0)))
              (should (= (length entities) 1))
              (should (equal (chirp-get entity "media_id") "media-1"))
              (should (vectorp (chirp-get entity "tagged_users"))))
            (pcase kind
              ('reply
               (should
                (equal
                 (chirp-get-in
                  variables '("reply" "in_reply_to_tweet_id"))
                 "99")))
              ('quote
               (should
                (equal (chirp-get variables "attachment_url")
                       "https://x.com/i/status/99"))))))
      (delete-file file))))

(ert-deftest chirp-backend-compose-routes-wide-text-to-create-note-tweet ()
  "Drafts over 280 weighted units should use CreateNoteTweet."
  (let (operation variables result)
    (cl-letf (((symbol-function 'chirp-x-graphql-request)
               (lambda (request-operation request-variables callback
                                          &rest _options)
                 (setq operation request-operation
                       variables request-variables)
                 (funcall
                  callback
                  (chirp-backend-test--payload-at-path
                   '("data" "notetweet_create" "tweet_results" "result")
                   '(("rest_id" . "789")))))))
      (chirp-backend-compose
       :kind 'post :text (make-string 141 ?你) :attachments nil
       :callback (lambda (created-tweet-id _envelope)
                   (setq result created-tweet-id))))
    (should (equal (plist-get operation :name) "CreateNoteTweet"))
    (should (assoc-string "disallowed_reply_options" variables t))
    (should (equal result "789"))
    (should (assoc-string
             "longform_notetweets_creation_enabled"
             (plist-get operation :features) t))))

(ert-deftest chirp-backend-compose-does-not-retry-an-ambiguous-create-failure ()
  "A failed create request should be surfaced after exactly one attempt."
  (let ((request-count 0)
        failure)
    (cl-letf (((symbol-function 'chirp-x-graphql-request)
               (lambda (_operation _variables _callback &rest options)
                 (setq request-count (1+ request-count))
                 (funcall (plist-get options :errback) "Connection reset"))))
      (chirp-backend-compose
       :kind 'post :text "hello" :attachments nil :callback #'ignore
       :errback (lambda (message)
                  (setq failure message))))
    (should (= request-count 1))
    (should (equal failure "Connection reset"))))

(ert-deftest chirp-backend-compose-rejects-a-success-without-a-tweet-id ()
  "An empty create result should be treated as a failed publication."
  (let (success failure)
    (cl-letf (((symbol-function 'chirp-x-graphql-request)
               (lambda (_operation _variables callback &rest _options)
                 (funcall callback '(("data" . (("create_tweet" . nil))))))))
      (chirp-backend-compose
       :kind 'post :text "hello" :attachments nil
       :callback (lambda (&rest _args)
                   (setq success t))
       :errback (lambda (message)
                  (setq failure message))))
    (should-not success)
    (should (string-match-p "did not return a created tweet ID" failure))))

(defun chirp-backend-test--tweet-id-payload (id)
  "Return a GraphQL payload whose tweet rest_id is ID."
  (chirp-backend-test--payload-at-path
   '("data" "tweet")
   `(("rest_id" . ,id))))

(ert-deftest chirp-backend-save-draft-builds-post-tweet-request ()
  "Draft saves should use CreateDraftTweet and post_tweet_request."
  (let (operation variables result)
    (cl-letf (((symbol-function 'chirp-x-graphql-request)
               (lambda (request-operation request-variables callback
                                          &rest _options)
                 (setq operation request-operation
                       variables request-variables)
                 (funcall callback
                          (chirp-backend-test--tweet-id-payload "7")))))
      (chirp-backend-save-draft
       :kind 'post
       :items (list (list :text "Nice" :attachments nil)
                    (list :text "Two Nices" :attachments nil))
       :callback (lambda (saved-draft-id _envelope)
                   (setq result saved-draft-id))))
    (should (equal (plist-get operation :name) "CreateDraftTweet"))
    (should (equal result "7"))
    (let ((request (chirp-get variables "post_tweet_request")))
      (should (equal (chirp-get request "status") "Nice"))
      (should (equal (chirp-get request "auto_populate_reply_metadata")
                     :json-false))
      (should (vectorp (chirp-get request "media_ids")))
      (should (vectorp (chirp-get request "exclude_reply_user_ids")))
      (should (equal (chirp-get (aref (chirp-get request "thread_tweets") 0)
                                "status")
                     "Two Nices")))))

(ert-deftest chirp-backend-save-draft-edits-an-existing-draft ()
  "A second save should edit the stored draft ID."
  (let (operation variables)
    (cl-letf (((symbol-function 'chirp-x-graphql-request)
               (lambda (request-operation request-variables callback
                                          &rest _options)
                 (setq operation request-operation
                       variables request-variables)
                 (funcall callback '(("data" . nil))))))
      (chirp-backend-save-draft
       :kind 'post
       :items (list (list :text "Nice" :attachments nil))
       :draft-id "2087"
       :callback #'ignore))
    (should (equal (plist-get operation :name) "EditDraftTweet"))
    (should (equal (chirp-get variables "draft_tweet_id") "2087"))))

(ert-deftest chirp-backend-save-draft-includes-reply-and-quote-targets ()
  "Draft replies and quotes should use the captured post_tweet_request fields."
  (dolist (spec '((reply "in_reply_to_status_id" "99")
                  (quote "attachment_url" "https://x.com/i/status/99")))
    (let (variables)
      (cl-letf (((symbol-function 'chirp-x-graphql-request)
                 (lambda (_operation request-variables callback &rest _options)
                   (setq variables request-variables)
                   (funcall callback
                            (chirp-backend-test--tweet-id-payload "1")))))
        (chirp-backend-save-draft
         :kind (car spec)
         :target-id "99"
         :items (list (list :text "hello" :attachments nil))
         :callback #'ignore))
      (should (equal (chirp-get-in variables
                                   (list "post_tweet_request" (cadr spec)))
                     (caddr spec)))
      (when (eq (car spec) 'reply)
        (should (eq (chirp-get-in
                     variables
                     '("post_tweet_request" "auto_populate_reply_metadata"))
                    t))))))

(ert-deftest chirp-backend-save-draft-uploads-media-ids ()
  "Draft media should upload first and become media_ids arrays."
  (let ((file (make-temp-file "chirp-backend-draft-" nil ".png"))
        variables)
    (unwind-protect
        (progn
          (write-region "png" nil file nil 'silent)
          (cl-letf (((symbol-function 'chirp-x-upload-media)
                     (lambda (path callback &rest _options)
                       (should (equal path file))
                       (funcall callback "media-1")))
                    ((symbol-function 'chirp-x-graphql-request)
                     (lambda (_operation request-variables callback
                                         &rest _options)
                       (setq variables request-variables)
                       (funcall callback
                                (chirp-backend-test--tweet-id-payload "1")))))
            (chirp-backend-save-draft
             :kind 'post
             :items (list (list :text "photo"
                                :attachments (list file)))
             :callback #'ignore))
          (should (equal (chirp-get-in variables
                                       '("post_tweet_request" "media_ids"))
                         ["media-1"])))
      (delete-file file))))

(ert-deftest chirp-backend-save-draft-reuses-existing-media-ids ()
  "Attachments that already have a media ID should not be uploaded again."
  (let (variables uploaded)
    (cl-letf (((symbol-function 'chirp-x-upload-media)
               (lambda (&rest _args)
                 (setq uploaded t)
                 (error "Existing media should not be uploaded")))
              ((symbol-function 'chirp-x-graphql-request)
               (lambda (_operation request-variables callback &rest _options)
                 (setq variables request-variables)
                 (funcall callback
                          (chirp-backend-test--tweet-id-payload "1")))))
      (chirp-backend-save-draft
       :kind 'post
       :items (list (list :text "photo"
                          :attachments
                          (list (list :media-id "2087634413287100416"))))
       :callback #'ignore))
    (should-not uploaded)
    (should (equal (chirp-get-in variables
                                 '("post_tweet_request" "media_ids"))
                   ["2087634413287100416"]))))

(ert-deftest chirp-backend-schedule-sends-execute-at ()
  "Scheduling should use CreateScheduledTweet and Unix seconds."
  (let (operation variables result)
    (cl-letf (((symbol-function 'chirp-x-graphql-request)
               (lambda (request-operation request-variables callback
                                          &rest _options)
                 (setq operation request-operation
                       variables request-variables)
                 (funcall callback
                          (chirp-backend-test--tweet-id-payload "8")))))
      (chirp-backend-schedule
       :kind 'post
       :items (list (list :text "later" :attachments nil))
       :execute-at 1786700000
       :callback (lambda (scheduled-tweet-id _envelope)
                   (setq result scheduled-tweet-id))))
    (should (equal (plist-get operation :name) "CreateScheduledTweet"))
    (should (equal result "8"))
    (should (equal (chirp-get variables "execute_at") 1786700000))
    (should (equal (chirp-get-in variables '("post_tweet_request" "status"))
                   "later"))))

(ert-deftest chirp-backend-request-rejects-unknown-actions-without-a-process ()
  "Unknown legacy action names should fail instead of starting a process."
  (let (failure)
    (chirp-backend-request
     '("unknown") #'ignore
     (lambda (message)
       (setq failure message)))
    (should (equal failure "Unknown Chirp backend action: unknown"))))

(provide 'chirp-backend-test)

;;; chirp-backend-test.el ends here
