;;; chirp-timeline-test.el --- Tests for timeline loading -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'chirp-core)
(require 'chirp-timeline)

(ert-deftest chirp-buffer-creates-fresh-view-buffers ()
  "Each `chirp-buffer' call should return a fresh buffer."
  (let ((buffer-a (chirp-buffer))
        (buffer-b (chirp-buffer)))
    (unwind-protect
        (progn
          (should (buffer-live-p buffer-a))
          (should (buffer-live-p buffer-b))
          (should-not (eq buffer-a buffer-b))
          (chirp--apply-buffer-name buffer-a "For You")
          (should (string= (buffer-name buffer-a) "*chirp: For You*")))
      (dolist (buffer (list buffer-a buffer-b))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest chirp-primary-timeline-retains-both-feeds-in-one-view ()
  "Primary commands should reuse one view and both canonical feed states."
  (let ((chirp--app nil)
        buffer
        requests)
    (unwind-protect
        (save-window-excursion
          (cl-letf (((symbol-function 'chirp-backend-feed)
                     (lambda (callback &optional following _errback
                                       _max-results _cursor owner)
                       (push (list :callback callback
                                   :following following
                                   :owner owner)
                             requests)))
                    ((symbol-function 'chirp-media-prefetch-tweets) #'ignore)
                    ((symbol-function 'chirp-enrich-quoted-tweets) #'ignore))
            (let* ((first-buffer (chirp-timeline-open-home))
                   (view
                    (with-current-buffer first-buffer
                      (appkit-current-view))))
              (setq buffer first-buffer)
              (funcall
               (plist-get (car requests) :callback)
               (list '(:kind tweet :id "home" :text "Retained\ndetail"))
               '(("pagination" . (("nextCursor" . "home-cursor")))))
              (appkit-sync-invalidations view)
              (let ((home-state (appkit-view-state view))
                    home-point)
                (with-current-buffer first-buffer
                  (goto-char (point-min))
                  (search-forward "detail")
                  (setq home-point (point)))
                (let ((second-buffer (chirp-timeline-open-home)))
                  (should (eq first-buffer second-buffer))
                  (should (eq view
                              (with-current-buffer second-buffer
                                (appkit-current-view))))
                  (should (= (length requests) 1))
                  (should (equal (appkit-view-id view)
                                 chirp-timeline--primary-view-id)))
                (with-current-buffer first-buffer
                  (should-not chirp--timeline-kind)
                  (chirp-toggle-home-following)
                  (should (eq view (appkit-current-view))))
                (let ((following-state (appkit-view-state view)))
                  (should-not (eq home-state following-state))
                  (should (= (length requests) 2))
                  (should (plist-get (car requests) :following))
                  (should (eq (plist-get (car requests) :owner) view))
                  (funcall
                   (plist-get (car requests) :callback)
                   (list '(:kind tweet :id "following" :text "Following"))
                   '(("pagination" .
                      (("nextCursor" . "following-cursor")))))
                  (appkit-sync-invalidations view)
                  (with-current-buffer first-buffer
                    (chirp-toggle-home-following))
                  (should (eq (appkit-view-state view) home-state))
                  (should (= (length requests) 2))
                  (appkit-sync-invalidations view)
                  (with-current-buffer first-buffer
                    (should (equal (plist-get (chirp-entry-at-point) :id)
                                   "home"))
                    (should (= (point) home-point)))
                  (should
                   (equal
                    (plist-get (plist-get home-state :page) :next-cursor)
                    "home-cursor"))
                  (should
                   (eq (gethash
                        'home
                        (chirp--session-primary-feed-states
                         (chirp--session)))
                       home-state))
                  (should (eq (chirp-timeline-open-following) first-buffer))
                  (should (eq (appkit-view-state view) following-state))
                  (should (= (length requests) 2))
                  (should
                   (equal
                    (plist-get (plist-get following-state :page) :next-cursor)
                    "following-cursor")))))))
      (chirp-stop)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest chirp-primary-feed-state-survives-buffer-recreation ()
  "Killing the primary buffer should not discard session-owned feed state."
  (let ((chirp--app nil)
        buffer
        requests)
    (unwind-protect
        (save-window-excursion
          (cl-letf (((symbol-function 'chirp-backend-feed)
                     (lambda (callback &rest _args)
                       (push callback requests)))
                    ((symbol-function 'chirp-media-prefetch-tweets) #'ignore)
                    ((symbol-function 'chirp-enrich-quoted-tweets) #'ignore))
            (setq buffer (chirp-timeline-open-home))
            (let* ((first-view
                    (with-current-buffer buffer (appkit-current-view)))
                   (state (appkit-view-state first-view)))
              (funcall
               (car requests)
               (list '(:kind tweet :id "1" :text "First\nentry")
                     '(:kind tweet :id "2" :text "Retained\nposition")
                     '(:kind tweet :id "3" :text "Third\nentry"))
               nil)
              (appkit-sync-invalidations first-view)
              (with-current-buffer buffer
                (goto-char (point-min))
                (search-forward "position")
                (set-window-start
                 (selected-window)
                 (appkit-position-find-property-value
                  (point-min) (point-max) 'chirp-entry-id '(tweet "2"))
                 t))
              (kill-buffer buffer)
              (should-not (appkit-view-live-p first-view))
              (setq buffer (chirp-timeline-open-home))
              (let ((second-view
                     (with-current-buffer buffer (appkit-current-view))))
                (should-not (eq first-view second-view))
                (should (eq state (appkit-view-state second-view)))
                (should (= (length requests) 1))
                (with-current-buffer buffer
                  (should (equal (plist-get (chirp-entry-at-point) :id) "2"))
                  (should (search-backward "position" nil t))
                  (should
                   (equal
                    (get-text-property
                     (window-start (selected-window)) 'chirp-entry-id)
                    '(tweet "2"))))))))
      (chirp-stop)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest chirp-primary-recreated-view-rejects-old-callback ()
  "A callback from a killed view must not overwrite its restarted feed."
  (let ((chirp--app nil)
        buffer
        callbacks)
    (unwind-protect
        (save-window-excursion
          (cl-letf (((symbol-function 'chirp-backend-feed)
                     (lambda (callback &rest _args)
                       (setq callbacks (append callbacks (list callback)))))
                    ((symbol-function 'chirp-media-prefetch-tweets) #'ignore)
                    ((symbol-function 'chirp-enrich-quoted-tweets) #'ignore))
            (setq buffer (chirp-timeline-open-home))
            (let* ((first-view
                    (with-current-buffer buffer (appkit-current-view)))
                   (state (appkit-view-state first-view)))
              (kill-buffer buffer)
              (setq buffer (chirp-timeline-open-home))
              (let ((second-generation (plist-get state :generation)))
                (should (= (length callbacks) 2))
                (funcall (nth 0 callbacks)
                         (list '(:kind tweet :id "old" :text "Old")) nil)
                (should-not (plist-get state :items))
                (should (eq (plist-get state :generation)
                            second-generation))
                (funcall (nth 1 callbacks)
                         (list '(:kind tweet :id "new" :text "New")) nil)
                (should
                 (equal (plist-get (car (plist-get state :items)) :id)
                        "new"))))))
      (chirp-stop)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest chirp-primary-callback-updates-state-before-projecting ()
  "Backend completion should update canonical state before scheduled sync."
  (let ((chirp--app nil)
        buffer
        callback
        owner)
    (unwind-protect
        (save-window-excursion
          (cl-letf (((symbol-function 'chirp-backend-feed)
                     (lambda (success &optional _following _errback
                                      _max-results _cursor request-owner)
                       (setq callback success
                             owner request-owner)))
                    ((symbol-function 'chirp-media-prefetch-tweets) #'ignore)
                    ((symbol-function 'chirp-enrich-quoted-tweets) #'ignore))
            (setq buffer (chirp-timeline-open-home))
            (let ((view (with-current-buffer buffer (appkit-current-view))))
              (should (eq owner view))
              (funcall callback
                       (list '(:kind tweet :id "1" :text "Projected later"))
                       nil)
              (should (equal (mapcar
                              (lambda (tweet) (plist-get tweet :id))
                              (plist-get (appkit-view-state view) :items))
                             '("1")))
              (with-current-buffer buffer
                (should-not (string-match-p "Projected later"
                                            (buffer-string))))
              (appkit-sync-invalidations view)
              (with-current-buffer buffer
                (should (string-match-p "Projected later"
                                        (buffer-string)))))))
      (chirp-stop)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest chirp-primary-projection-uses-row-owned-separators ()
  "Primary timeline rows should not receive an EWOC separator."
  (let ((chirp--app nil)
        (chirp-tweet-separator "-----")
        (chirp-tweet-separator-indent 2)
        buffer
        callback)
    (unwind-protect
        (save-window-excursion
          (cl-letf (((symbol-function 'chirp-backend-feed)
                     (lambda (success &rest _args)
                       (setq callback success)))
                    ((symbol-function 'chirp-media-prefetch-tweets) #'ignore)
                    ((symbol-function 'chirp-enrich-quoted-tweets) #'ignore))
            (setq buffer (chirp-timeline-open-home))
            (let ((view (with-current-buffer buffer (appkit-current-view))))
              (funcall
               callback
               (list '(:kind tweet :id "1" :text "First"
                       :author-name "Alice")
                     '(:kind tweet :id "2" :text "Second"
                       :author-name "Bob"))
               nil)
              (appkit-sync-invalidations view)
              (with-current-buffer buffer
                (let ((text (buffer-string)))
                  (should (string-match-p "Views\n\n  -----" text))
                  (should-not (string-match-p "Views\n\n\n  -----" text)))))))
      (chirp-stop)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest chirp-primary-superseding-request-cancels-old-transport ()
  "Starting a replacement generation should cancel its prior transport."
  (let ((chirp--app nil)
        buffer
        (request-count 0)
        canceled)
    (unwind-protect
        (save-window-excursion
          (cl-letf (((symbol-function 'chirp-backend-feed)
                     (lambda (&rest _args)
                       (list 'request (cl-incf request-count))))
                    ((symbol-function 'chirp-x-cancel-request)
                     (lambda (request)
                       (setq canceled request)))
                    ((symbol-function 'chirp-media-prefetch-tweets) #'ignore)
                    ((symbol-function 'chirp-enrich-quoted-tweets) #'ignore))
            (setq buffer (chirp-timeline-open-home))
            (let ((view (with-current-buffer buffer (appkit-current-view))))
              (should
               (equal
                (gethash chirp-timeline--request-key
                         (appkit-view-request-table view))
                '(request 1)))
              (with-current-buffer buffer
                (chirp-refresh))
              (should (equal canceled '(request 1)))
              (should
               (equal
                (gethash chirp-timeline--request-key
                         (appkit-view-request-table view))
                '(request 2))))))
      (chirp-stop)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest chirp-primary-first-position-intent-survives-request-coalescing ()
  "A new request should not erase an unprojected move-to-first intent."
  (let ((chirp--app nil)
        buffer
        callbacks)
    (unwind-protect
        (save-window-excursion
          (cl-letf (((symbol-function 'chirp-backend-feed)
                     (lambda (success &rest _args)
                       (setq callbacks (append callbacks (list success)))
                       (list 'request (length callbacks))))
                    ((symbol-function 'chirp-x-cancel-request) #'ignore)
                    ((symbol-function 'chirp-media-prefetch-tweets) #'ignore)
                    ((symbol-function 'chirp-enrich-quoted-tweets) #'ignore))
            (setq buffer (chirp-timeline-open-home))
            (let ((view (with-current-buffer buffer (appkit-current-view))))
              (funcall (nth 0 callbacks)
                       (list '(:kind tweet :id "1" :text "First")) nil)
              (with-current-buffer buffer
                (goto-char (point-max))
                (chirp-refresh))
              (should
               (memq 'first
                     (mapcar
                      (lambda (event) (plist-get event :position))
                      (appkit-view-pending-events-snapshot view))))
              (appkit-sync-invalidations view)
              (with-current-buffer buffer
                (should (equal (plist-get (chirp-entry-at-point) :id) "1")))
              (should-not (appkit-view-pending-events-snapshot view)))))
      (chirp-stop)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest chirp-primary-stale-generation-cannot-overwrite-newer-state ()
  "A superseded callback should settle without projecting its result."
  (let ((chirp--app nil)
        buffer
        callbacks)
    (unwind-protect
        (save-window-excursion
          (cl-letf (((symbol-function 'chirp-backend-feed)
                     (lambda (success &rest _args)
                       (setq callbacks (append callbacks (list success)))))
                    ((symbol-function 'chirp-media-prefetch-tweets) #'ignore)
                    ((symbol-function 'chirp-enrich-quoted-tweets) #'ignore))
            (setq buffer (chirp-timeline-open-home))
            (let* ((view (with-current-buffer buffer (appkit-current-view)))
                   (state (appkit-view-state view))
                   (first-generation (plist-get state :generation)))
              (with-current-buffer buffer
                (chirp-refresh))
              (let ((second-generation (plist-get state :generation)))
                (should-not (eq first-generation second-generation))
                (funcall (nth 0 callbacks)
                         (list '(:kind tweet :id "old" :text "old")) nil)
                (should
                 (chirp-timeline--generation-settled-p first-generation))
                (should (eq (plist-get state :generation)
                            second-generation))
                (should-not (plist-get state :items))
                (funcall (nth 1 callbacks)
                         (list '(:kind tweet :id "new" :text "new")) nil)
                (should (equal (plist-get (car (plist-get state :items)) :id)
                               "new"))))))
      (chirp-stop)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest chirp-primary-pagination-preserves-existing-ewoc-nodes ()
  "Appending an older page should retain unchanged EWOC node identities."
  (let ((chirp--app nil)
        buffer
        callbacks)
    (unwind-protect
        (save-window-excursion
          (cl-letf (((symbol-function 'chirp-backend-feed)
                     (lambda (success &rest _args)
                       (setq callbacks (append callbacks (list success)))))
                    ((symbol-function 'chirp-media-prefetch-tweets) #'ignore)
                    ((symbol-function 'chirp-enrich-quoted-tweets) #'ignore))
            (setq buffer (chirp-timeline-open-home))
            (let* ((view (with-current-buffer buffer (appkit-current-view)))
                   (first '(:kind tweet :id "1" :text "First"
                            :author-handle "alice"))
                   (reply '(:kind tweet :id "2" :text "Reply"
                            :reply-to-id "1" :reply-to-handle "alice")))
              (funcall
               (nth 0 callbacks) (list first reply)
               '(("pagination" . (("nextCursor" . "older")))))
              (appkit-sync-invalidations view)
              (let ((first-node
                     (appkit-projection-node view '(tweet "1")))
                    (reply-node
                     (appkit-projection-node view '(tweet "2"))))
                (with-current-buffer buffer
                  (chirp-load-more))
                (funcall (nth 1 callbacks)
                         (list '(:kind tweet :id "3" :text "Older")) nil)
                (appkit-sync-invalidations view)
                (should
                 (eq first-node
                     (appkit-projection-node view '(tweet "1"))))
                (should
                 (eq reply-node
                     (appkit-projection-node view '(tweet "2"))))
                (should (appkit-projection-node view '(tweet "3")))
                (with-current-buffer buffer
                  (should (string-match-p "replying to @alice above"
                                          (buffer-string))))))))
      (chirp-stop)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest chirp-primary-tweet-actions-update-canonical-state-by-key ()
  "Tweet actions should not recover primary state from rendered text."
  (let ((chirp--app nil)
        buffer
        callback)
    (unwind-protect
        (save-window-excursion
          (cl-letf (((symbol-function 'chirp-backend-feed)
                     (lambda (success &rest _args)
                       (setq callback success)))
                    ((symbol-function 'chirp-media-prefetch-tweets) #'ignore)
                    ((symbol-function 'chirp-enrich-quoted-tweets) #'ignore))
            (setq buffer (chirp-timeline-open-home))
            (let ((view (with-current-buffer buffer (appkit-current-view))))
              (funcall callback
                       (list '(:kind tweet :id "1" :text "First"
                               :liked-p nil))
                       nil)
              (appkit-sync-invalidations view)
              (cl-letf (((symbol-function 'chirp--map-buffer-tweets)
                         (lambda (&rest _args)
                           (ert-fail "primary state was scanned from text"))))
                (should
                 (chirp-update-tweet-by-id
                  buffer "1"
                  (lambda (tweet)
                    (setf (plist-get tweet :liked-p) t))
                  t)))
              (should (plist-get
                       (car (plist-get (appkit-view-state view) :items))
                       :liked-p))
              (should
               (equal
                (appkit-invalidations-entry-keys
                 (appkit-view-invalidations view))
                '((tweet "1")))))))
      (chirp-stop)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest chirp-primary-tweet-mutations-keep-inactive-feed-coherent ()
  "Tweet updates and deletion should reach both cached primary feeds."
  (let ((chirp--app nil)
        buffer
        callbacks)
    (unwind-protect
        (save-window-excursion
          (cl-letf (((symbol-function 'chirp-backend-feed)
                     (lambda (callback &rest _args)
                       (setq callbacks (append callbacks (list callback)))))
                    ((symbol-function 'chirp-media-prefetch-tweets) #'ignore)
                    ((symbol-function 'chirp-enrich-quoted-tweets) #'ignore))
            (setq buffer (chirp-timeline-open-home))
            (let ((view (with-current-buffer buffer (appkit-current-view))))
              (funcall (nth 0 callbacks)
                       (list '(:kind tweet :id "1" :text "Home"
                               :liked-p nil))
                       nil)
              (appkit-sync-invalidations view)
              (with-current-buffer buffer
                (chirp-toggle-home-following))
              (funcall (nth 1 callbacks)
                       (list '(:kind tweet :id "1" :text "Following"
                               :liked-p nil))
                       nil)
              (appkit-sync-invalidations view)
              (with-current-buffer buffer
                (chirp-toggle-home-following))
              (appkit-sync-invalidations view)
              (should
               (chirp-update-tweet-by-id
                buffer "1"
                (lambda (tweet)
                  (setf (plist-get tweet :liked-p) t))
                t))
              (let ((states
                     (chirp--session-primary-feed-states
                      (chirp--session))))
                (should
                 (plist-get
                  (car (plist-get (gethash 'home states) :items))
                  :liked-p))
                (should
                 (plist-get
                  (car (plist-get (gethash 'following states) :items))
                  :liked-p))
                (should (chirp--remove-tweet-from-primary-feeds buffer "1"))
                (should-not (plist-get (gethash 'home states) :items))
                (should-not (plist-get (gethash 'following states) :items))
                (should
                 (appkit-invalidations-structure-p
                  (appkit-view-invalidations view)))))))
      (chirp-stop)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest chirp-primary-quoted-action-invalidates-owning-row ()
  "Updating a quoted tweet should target its canonical top-level owner row."
  (let ((chirp--app nil)
        buffer
        callback)
    (unwind-protect
        (save-window-excursion
          (cl-letf (((symbol-function 'chirp-backend-feed)
                     (lambda (success &rest _args)
                       (setq callback success)))
                    ((symbol-function 'chirp-media-prefetch-tweets) #'ignore)
                    ((symbol-function 'chirp-enrich-quoted-tweets) #'ignore))
            (setq buffer (chirp-timeline-open-home))
            (let* ((view (with-current-buffer buffer (appkit-current-view)))
                   (quoted '(:kind tweet :id "quoted" :text "Quoted"
                             :liked-p nil))
                   (outer (list :kind 'tweet :id "outer" :text "Outer"
                                :quoted-tweet quoted)))
              (funcall callback (list outer) nil)
              (appkit-sync-invalidations view)
              (should
               (chirp-update-tweet-by-id
                buffer "quoted"
                (lambda (tweet)
                  (setf (plist-get tweet :liked-p) t))
                t))
              (should (plist-get quoted :liked-p))
              (should
               (equal
                (appkit-invalidations-entry-keys
                 (appkit-view-invalidations view))
                '((tweet "outer")))))))
      (chirp-stop)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest chirp-primary-media-completion-invalidates-only-dependent-row ()
  "A media resource completion should reprint only its dependent tweet row."
  (let ((chirp--app nil)
        buffer
        callback
        printed)
    (unwind-protect
        (save-window-excursion
          (cl-letf (((symbol-function 'chirp-backend-feed)
                     (lambda (success &rest _args)
                       (setq callback success)))
                    ((symbol-function 'chirp-media-prefetch-tweets) #'ignore)
                    ((symbol-function 'chirp-enrich-quoted-tweets) #'ignore))
            (setq buffer (chirp-timeline-open-home))
            (let ((view (with-current-buffer buffer (appkit-current-view))))
              (funcall
               callback
               (list '(:kind tweet :id "1" :text "First"
                       :author-avatar-url "https://example.com/one.jpg")
                     '(:kind tweet :id "2" :text "Second"
                       :author-avatar-url "https://example.com/two.jpg"))
               nil)
              (appkit-sync-invalidations view)
              (let ((printer (symbol-function 'chirp-render-print-tweet-row)))
                (cl-letf (((symbol-function 'chirp-render-print-tweet-row)
                           (lambda (row)
                             (push (appkit-projection-row-key row) printed)
                             (funcall printer row))))
                  (chirp-media--invalidate-resource
                   "https://example.com/one.jpg")
                  (should
                   (equal
                    (appkit-invalidations-resource-keys
                     (appkit-view-invalidations view))
                    '("https://example.com/one.jpg")))
                  (appkit-sync-invalidations view)))
              (should (equal printed '((tweet "1")))))))
      (chirp-stop)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest chirp-stop-makes-late-primary-callback-inert ()
  "A callback arriving after stop should not mutate or recreate the session."
  (let ((chirp--app nil)
        buffer
        callback
        view
        state)
    (unwind-protect
        (save-window-excursion
          (cl-letf (((symbol-function 'chirp-backend-feed)
                     (lambda (success &rest _args)
                       (setq callback success)))
                    ((symbol-function 'chirp-media-prefetch-tweets) #'ignore)
                    ((symbol-function 'chirp-enrich-quoted-tweets) #'ignore))
            (setq buffer (chirp-timeline-open-home)
                  view (with-current-buffer buffer (appkit-current-view))
                  state (appkit-view-state view))
            (chirp-stop)
            (should-not (appkit-view-live-p view))
            (funcall callback
                     (list '(:kind tweet :id "late" :text "late")) nil)
            (should-not chirp--app)
            (should-not (plist-get state :items))))
      (chirp-stop)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest chirp-quit-current-buffer-keeps-main-timeline-buffers ()
  "Quitting For You/Following should keep the timeline buffer alive."
  (let ((previous (generate-new-buffer "*chirp-prev-test*"))
        (timeline (generate-new-buffer " *chirp-home*")))
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer previous)
          (switch-to-buffer timeline)
          (with-current-buffer timeline
            (chirp-view-mode)
            (setq-local chirp--timeline-kind 'home))
          (chirp-quit-current-buffer)
          (should (eq (window-buffer (selected-window)) previous))
          (should (buffer-live-p timeline)))
      (dolist (buffer (list previous timeline))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest chirp-quit-current-buffer-kills-secondary-chirp-views ()
  "Quitting secondary Chirp views should still kill the current buffer."
  (let ((previous (generate-new-buffer "*chirp-prev-test*"))
        (detail (generate-new-buffer " *chirp-detail*")))
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer previous)
          (switch-to-buffer detail)
          (with-current-buffer detail
            (chirp-view-mode)
            (setq-local chirp--timeline-kind nil)
            (setq-local chirp--view-title "Thread"))
          (chirp-quit-current-buffer)
          (should (eq (window-buffer (selected-window)) previous))
          (should-not (buffer-live-p detail)))
      (dolist (buffer (list previous detail))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest chirp-normalize-tweet-keeps-session-overrides-explicit ()
  "Payload normalization should stay pure until state overrides are applied."
  (unwind-protect
      (progn
        (chirp-set-tweet-state-override "1" :liked-p t)
        (let ((tweet
               (chirp-normalize-tweet
                '(("id" . "1")
                  ("text" . "hello")
                  ("liked" . chirp-json-false)
                  ("author" . (("screenName" . "alice")))))))
          (should-not (plist-get tweet :liked-p))
          (should (plist-get
                   (chirp-apply-tweet-state-overrides tweet)
                   :liked-p))))
    (chirp-clear-tweet-state-overrides "1")))

(ert-deftest chirp-collect-top-level-tweets-hides-promoted-posts ()
  "Promoted tweets should be dropped when filtering is enabled."
  (let ((chirp-hide-promoted-posts t))
    (should (equal (mapcar (lambda (tweet) (plist-get tweet :id))
                           (chirp-collect-top-level-tweets
                            (list '(("id" . "1")
                                    ("text" . "normal")
                                    ("author" . (("screenName" . "alice")
                                                 ("name" . "Alice"))))
                                  '(("id" . "2")
                                    ("text" . "ad")
                                    ("isPromoted" . t)
                                    ("author" . (("screenName" . "brand")
                                                 ("name" . "Brand")))))))
                   '("1")))))

(ert-deftest chirp-collect-top-level-tweets-keeps-retweet-identity ()
  "Self-retweets and their originals should remain distinct timeline entries."
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
         (retweet
          `(("rest_id" . "100")
            ("core" . (("user_results" . (("result" . ,retweeter)))))
            ("legacy" .
             (("full_text" . "RT @bob: Original post")
              ("retweeted_status_result" . (("result" . ,original)))))))
         (tweets (chirp-collect-top-level-tweets
                  (list retweet original))))
    (should (equal (mapcar (lambda (tweet) (plist-get tweet :id))
                           tweets)
                   '("200" "200")))
    (should (equal (mapcar (lambda (tweet) (plist-get tweet :retweet-id))
                           tweets)
                   '("100" nil)))
    (should (equal (mapcar #'appkit-projection-row-key
                           (chirp-render-project-tweet-rows tweets))
                   '((tweet "100") (tweet "200"))))))

(ert-deftest chirp-collect-top-level-tweets-can-keep-promoted-posts ()
  "Promoted tweets should remain visible when filtering is disabled."
  (let ((chirp-hide-promoted-posts nil))
    (should (equal (mapcar (lambda (tweet) (plist-get tweet :id))
                           (chirp-collect-top-level-tweets
                            (list '(("id" . "1")
                                    ("text" . "normal")
                                    ("author" . (("screenName" . "alice")
                                                 ("name" . "Alice"))))
                                  '(("id" . "2")
                                    ("text" . "ad")
                                    ("isPromoted" . t)
                                    ("author" . (("screenName" . "brand")
                                                 ("name" . "Brand")))))))
                   '("1" "2")))))

(ert-deftest chirp-status-appears-in-mode-line ()
  "Persistent Chirp status should stay visible in the mode line."
  (with-temp-buffer
    (chirp-view-mode)
    (cl-letf (((symbol-function 'run-with-timer)
               (lambda (&rest _args)
                 'chirp-test-timer))
              ((symbol-function 'timerp)
               (lambda (value)
                 (eq value 'chirp-test-timer)))
              ((symbol-function 'cancel-timer) #'ignore)
              ((symbol-function 'float-time)
               (lambda (&optional _time)
                 100.0)))
      (chirp-set-status (current-buffer) "Loading thread...")
      (setq-local chirp--status-start-time 97.5)
      (let ((rendered (chirp--mode-line-status-string)))
        (should (string-match-p "Loading thread\\.\\.\\." rendered))
        (should (string-match-p "2\\.5s" rendered)))
      (chirp-clear-status (current-buffer))
      (should-not (chirp--mode-line-status-string)))))

(ert-deftest chirp-timeline-open-likes-resolves-current-user-before-fetching ()
  "Liked view should resolve the current handle before fetching likes."
  (let (whoami-called likes-handle installed)
    (unwind-protect
        (cl-letf (((symbol-function 'chirp-begin-background-request)
                   (lambda (_buffer _title)
                     'token))
                  ((symbol-function 'chirp-request-current-p)
                   (lambda (_buffer _token)
                     t))
                  ((symbol-function 'chirp-backend-whoami)
                   (lambda (callback &optional _errback)
                     (setq whoami-called t)
                     (funcall callback '(:handle "alice") nil)))
                  ((symbol-function 'chirp-backend-likes)
                   (lambda (handle callback &optional _errback)
                     (setq likes-handle handle)
                     (funcall callback (list (list :id "1")) nil)))
                  ((symbol-function 'chirp-timeline--install-tweets)
                   (lambda (view tweets)
                     (setq installed
                           (list (plist-get (appkit-view-state view) :title)
                                 tweets)))))
          (chirp-timeline-open-likes)
          (should whoami-called)
          (should (equal likes-handle "alice"))
          (should (equal installed '("Liked: @alice" ((:id "1"))))))
      (chirp-stop))))

(ert-deftest chirp-timeline-open-list-prompts-from-accessible-lists ()
  "List selection should prompt with accessible lists and open the chosen id."
  (let (captured-target installed seen-prompt seen-collection)
    (unwind-protect
        (cl-letf (((symbol-function 'chirp-backend-lists)
                   (lambda (callback &optional _errback)
                     (funcall
                      callback
                      '((("id" . "1956792682412345678")
                         ("name" . "Emacs")
                         ("mode" . "private")
                         ("sources" . ("owned"))
                         ("owner" . (("screenName" . "lucius")))))
                      nil)))
                  ((symbol-function 'completing-read)
                   (lambda (prompt collection &rest _args)
                     (setq seen-prompt prompt
                           seen-collection collection)
                     (caar collection)))
                  ((symbol-function 'chirp-begin-background-request)
                   (lambda (_buffer _title)
                     'token))
                  ((symbol-function 'chirp-request-current-p)
                   (lambda (_buffer _token)
                     t))
                  ((symbol-function 'chirp-backend-list)
                   (lambda (list-target callback &optional _errback)
                     (setq captured-target list-target)
                     (funcall callback (list (list :id "1")) nil)))
                  ((symbol-function 'chirp-timeline--install-tweets)
                   (lambda (view tweets)
                     (setq installed
                           (list (plist-get (appkit-view-state view) :title)
                                 tweets)))))
          (chirp-timeline-open-list)
          (should (equal seen-prompt "List (1): "))
          (should (string-match-p "@lucius" (caar seen-collection)))
          (should (string-match-p "owned" (caar seen-collection)))
          (should (equal captured-target "1956792682412345678"))
          (should (equal installed
                         '("List: 1956792682412345678" ((:id "1"))))))
      (chirp-stop))))

(ert-deftest chirp-timeline-list-picker-ignores-a-dead-target-buffer ()
  "Delayed list discovery should not prompt after its target buffer dies."
  (let (buffer callback prompted)
    (cl-letf (((symbol-function 'chirp-backend-lists)
               (lambda (success &optional _errback)
                 (setq callback success)))
              ((symbol-function 'completing-read)
               (lambda (&rest _args)
                 (setq prompted t))))
      (setq buffer (chirp-timeline-open-list))
      (kill-buffer buffer)
      (funcall callback
               '((("id" . "1") ("name" . "One")))
               nil))
    (should-not prompted)))

(ert-deftest chirp-timeline-open-list-uses-list-title-and-renderer ()
  "List view should fetch tweets and render under a list-specific title."
  (let (captured-target installed)
    (unwind-protect
        (cl-letf (((symbol-function 'chirp-begin-background-request)
                   (lambda (_buffer _title)
                     'token))
                  ((symbol-function 'chirp-request-current-p)
                   (lambda (_buffer _token)
                     t))
                  ((symbol-function 'chirp-backend-list)
                   (lambda (list-target callback &optional _errback)
                     (setq captured-target list-target)
                     (funcall callback (list (list :id "1")) nil)))
                  ((symbol-function 'chirp-timeline--install-tweets)
                   (lambda (view tweets)
                     (setq installed
                           (list (plist-get (appkit-view-state view) :title)
                                 tweets)))))
          (chirp-timeline-open-list
           "1956792682412345678")
          (should (equal captured-target
                         "1956792682412345678"))
          (should (equal installed
                         '("List: 1956792682412345678" ((:id "1"))))))
      (chirp-stop))))

(ert-deftest chirp-request-rerender-coalesces-primary-view-invalidations ()
  "Repeated background updates should produce one Appkit view sync."
  (let (buffer callback view)
    (unwind-protect
        (cl-letf (((symbol-function 'chirp-backend-feed)
                   (lambda (success &rest _args)
                     (setq callback success)))
                  ((symbol-function 'chirp-media-prefetch-tweets) #'ignore)
                  ((symbol-function 'chirp-enrich-quoted-tweets) #'ignore))
          (setq buffer (chirp-timeline-open-home))
          (with-current-buffer buffer
            (setq view (appkit-current-view)))
          (funcall callback
                   (list '(:kind tweet :id "1" :text "tweet")) nil)
          (appkit-sync-invalidations view)
          (let ((sync (appkit-view-sync-function view))
                (sync-count 0))
            (setf (appkit-view-sync-function view)
                  (lambda (live-view invalidations)
                    (setq sync-count (1+ sync-count))
                    (funcall sync live-view invalidations)))
            (chirp-request-rerender buffer 60)
            (chirp-request-rerender buffer 60)
            (should (= (length (appkit-view-handles view)) 1))
            (appkit-sync-invalidations view)
            (should (= sync-count 1))
            (should-not (appkit-view-handles view))))
      (chirp-stop)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest chirp-primary-timeline-sync-preserves-point-within-entry ()
  "Background primary timeline syncs should preserve semantic point offsets."
  (let ((tweet-a '(:kind tweet :id "1"
                   :text "Alpha line one\nAlpha line two"))
        (tweet-b '(:kind tweet :id "2"
                   :text "Beta line one\nBeta line two"))
        buffer callback view)
    (unwind-protect
        (cl-letf (((symbol-function 'chirp-backend-feed)
                   (lambda (success &rest _args)
                     (setq callback success)))
                  ((symbol-function 'chirp-media-prefetch-tweets) #'ignore)
                  ((symbol-function 'chirp-enrich-quoted-tweets) #'ignore))
          (setq buffer (chirp-timeline-open-home))
          (with-current-buffer buffer
            (setq view (appkit-current-view)))
          (funcall callback (list tweet-a tweet-b) nil)
          (appkit-sync-invalidations view)
          (with-current-buffer buffer
            (search-forward "Beta line two")
            (let ((before (point)))
              (should (equal (chirp--text-property-at-point 'chirp-entry-id)
                             '(tweet "2")))
              (chirp-request-rerender buffer 60)
              (appkit-sync-invalidations view)
              (should (equal (plist-get (chirp-entry-at-point) :id) "2"))
              (should (> (point) (chirp--current-entry-start)))
              (should (= before (point))))))
      (chirp-stop)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest chirp-timeline-refresh-uses-smaller-head-window ()
  "Refreshing should fetch a smaller head page when configured."
  (let ((state (chirp-timeline--make-state 'home 20))
        (chirp-timeline-refresh-max-results 10))
    (should (= (chirp-timeline--fetch-count state 'refresh) 10))))

(ert-deftest chirp-timeline-refresh-can-use-current-limit ()
  "Refreshing should keep the old fetch size when the head-window override is disabled."
  (let ((state (chirp-timeline--make-state 'home 20))
        (chirp-timeline-refresh-max-results nil))
    (should (= (chirp-timeline--fetch-count state 'refresh) 20))))

(ert-deftest chirp-window-state-restore-preserves-point-and-scroll ()
  "Window-state helpers should preserve point and scroll position."
  (let ((buffer (generate-new-buffer " *chirp-window-state*")))
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer buffer)
          (with-current-buffer buffer
            (chirp-view-mode)
            (let ((inhibit-read-only t))
              (dotimes (index 80)
                (insert (format "line %02d\n" index))))
            (goto-char (point-min))
            (forward-line 40)
            (set-window-start (selected-window) (line-beginning-position))
            (set-window-vscroll (selected-window) 8 t)
            (let ((state (chirp-capture-window-state buffer))
                  (point (point))
                  (window-start (window-start (selected-window)))
                  (vscroll (window-vscroll (selected-window) t)))
              (goto-char (point-min))
              (set-window-start (selected-window) (point-min))
              (set-window-vscroll (selected-window) 0 t)
              (chirp-restore-window-state state)
              (should (= (point) point))
              (should (= (window-start (selected-window)) window-start))
              (should (= (window-vscroll (selected-window) t) vscroll)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest chirp-clean-text-decodes-html-entities ()
  "Tweet text should decode common HTML entities."
  (should (equal (chirp-clean-text "a &gt; b &amp; c &lt; d")
                 "a > b & c < d"))
  (should (equal (chirp-clean-text "say &quot;hi&quot; &#39;now&#39;")
                 "say \"hi\" 'now'"))
  (should (equal (chirp-clean-text "A&#10;B&#x21;")
                 "A\nB!")))

(provide 'chirp-timeline-test)

;;; chirp-timeline-test.el ends here
