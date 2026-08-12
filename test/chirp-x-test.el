;;; chirp-x-test.el --- Tests for Chirp's X web transport -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Commentary:

;; Exercise credential lookup and persisted GraphQL request shaping without
;; contacting X.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'json)
(require 'url-http)
(require 'url-util)
(require 'chirp-x)

(declare-function chirp-login "chirp")
(defvar plz-curl-default-args)

(defun chirp-x-test--response (status payload callback)
  "Run CALLBACK in a disposable HTTP response buffer for STATUS and PAYLOAD."
  (let ((buffer (generate-new-buffer " *chirp-x-test-response*")))
    (with-current-buffer buffer
      (setq-local url-http-response-status status)
      (setq-local url-http-end-of-headers (copy-marker (point-min)))
      (insert payload)
      (funcall callback nil))
    buffer))

(defun chirp-x-test--query-value (name url)
  "Return the decoded query parameter NAME from URL."
  (when (string-match (concat "[?&]" (regexp-quote name) "=\\([^&]+\\)") url)
    (url-unhex-string (match-string 1 url))))

(defun chirp-x-test--query-json (name url)
  "Decode JSON query parameter NAME from URL."
  (when-let* ((value (chirp-x-test--query-value name url)))
    (json-parse-string value
                       :object-type 'alist
                       :array-type 'list
                       :null-object nil
                       :false-object nil)))

(defun chirp-x-test--browser-capture (auth-token ct0 &optional url)
  "Return a browser-session capture containing AUTH-TOKEN and CT0."
  `((schema . 1)
    (source . ((url . ,(or url "https://x.com"))))
    (cookies . (((name . "auth_token")
                 (value . ,auth-token)
                 (domain . ".x.com"))
                ((name . "ct0")
                 (value . ,ct0)
                 (domain . "x.com"))))))

(ert-deftest chirp-x-credentials-read-private-auth-file ()
  "Private browser-imported auth should be Chirp's sole cookie source."
  (let* ((directory (make-temp-file "chirp-x-auth-" t))
         (chirp-x-auth-file (expand-file-name "auth.json" directory))
         (auth-token (make-string 23 ?a))
         (ct0 (make-string 37 ?b))
         (chirp-x-bearer-token "configured-bearer")
         (process-environment (copy-sequence process-environment)))
    (unwind-protect
        (progn
          (setenv "CHIRP_X_AUTH_TOKEN" (make-string 11 ?c))
          (setenv "CHIRP_X_CT0" (make-string 13 ?d))
          (setenv "CHIRP_X_BEARER_TOKEN" nil)
          (chirp-x--write-auth-file auth-token ct0)
          (let ((credentials (chirp-x-credentials)))
            (should (= (length (plist-get credentials :auth-token)) 23))
            (should (= (length (plist-get credentials :ct0)) 37))
            (should (equal (plist-get credentials :bearer-token)
                           "configured-bearer")))
          (when (not (eq system-type 'windows-nt))
            (should (= (logand (file-modes chirp-x-auth-file) #o777) #o600))))
      (delete-directory directory t))))

(ert-deftest chirp-x-credentials-rejects-invalid-auth-file ()
  "Malformed or unsafe private auth data must not reach X headers."
  (let* ((directory (make-temp-file "chirp-x-auth-" t))
         (chirp-x-auth-file (expand-file-name "auth.json" directory))
         (chirp-x-bearer-token "configured-bearer"))
    (unwind-protect
        (progn
          (with-temp-file chirp-x-auth-file
            (insert "{\"schema\":2}"))
          (should-error (chirp-x-credentials) :type 'error)
          (with-temp-file chirp-x-auth-file
            (insert "{\"schema\":1,\"auth_token\":\"invalid;value\",\"ct0\":\"value\"}"))
          (should-error (chirp-x-credentials) :type 'error))
      (delete-directory directory t))))

(ert-deftest chirp-x-capture-browser-session-writes-private-auth-file ()
  "Explicit browser capture should import only a private X auth file."
  (let* ((directory (make-temp-file "chirp-x-login-" t))
         (chirp-x-auth-file (expand-file-name "auth.json" directory))
         (chirp-x--browser-session-process nil)
         (chirp-x-bearer-token "configured-bearer")
         (auth-token (make-string 31 ?a))
         (ct0 (make-string 47 ?b))
         (capture (chirp-x-test--browser-capture auth-token ct0))
         capture-file
         messages
         (stops 0))
    (unwind-protect
        (cl-letf (((symbol-function 'browser-session-capture)
                   (lambda (&rest arguments)
                     (setq capture-file (plist-get arguments :output-file))
                     (with-temp-file capture-file
                       (insert "private capture"))
                     (funcall (plist-get arguments :callback)
                              '((browser . "test")))
                     'synchronous-capture))
                  ((symbol-function 'browser-session-read)
                   (lambda (_file) capture))
                  ((symbol-function 'chirp-stop)
                   (lambda ()
                     (setq stops (1+ stops))))
                  ((symbol-function 'message)
                   (lambda (format-string &rest arguments)
                     (push (apply #'format format-string arguments) messages))))
          (chirp-x-capture-browser-session)
          (should (file-readable-p chirp-x-auth-file))
          (should-not (file-exists-p capture-file))
          (should (= stops 1))
          (let ((credentials (chirp-x-credentials)))
            (should (= (length (plist-get credentials :auth-token)) 31))
            (should (= (length (plist-get credentials :ct0)) 47)))
          (should (member "Chirp imported the X browser session" messages))
          (should-not
           (cl-some (lambda (text)
                      (or (string-match-p auth-token text)
                          (string-match-p ct0 text)))
                    messages)))
      (delete-directory directory t))))

(ert-deftest chirp-x-capture-browser-session-uses-its-profile-root ()
  "X capture should select Chirp's fixed isolated profile root."
  (let* ((directory (make-temp-file "chirp-x-login-" t))
         (chirp-x-browser-session-profile-root
          (expand-file-name "browser-session/" directory))
         (chirp-x--browser-session-process nil)
         arguments
         capture-file)
    (unwind-protect
        (cl-letf (((symbol-function 'browser-session-capture)
                   (lambda (&rest value)
                     (setq arguments value
                           capture-file (plist-get value :output-file))
                     'asynchronous-capture)))
          (chirp-x-capture-browser-session)
          (should (equal (plist-get arguments :profile-root)
                         chirp-x-browser-session-profile-root))
          (should-not (plist-get arguments :profile-directory)))
      (when capture-file
        (ignore-errors (delete-file capture-file)))
      (delete-directory directory t))))

(ert-deftest chirp-x-capture-browser-session-rejects-wrong-origin ()
  "A non-X capture should not replace Chirp's existing auth file."
  (let* ((directory (make-temp-file "chirp-x-login-" t))
         (chirp-x-auth-file (expand-file-name "auth.json" directory))
         (chirp-x--browser-session-process nil)
         (chirp-x-bearer-token "configured-bearer")
         (old-auth-token (make-string 19 ?a))
         (old-ct0 (make-string 29 ?b))
         (capture (chirp-x-test--browser-capture
                   (make-string 31 ?c) (make-string 47 ?d)
                   "https://example.invalid"))
         capture-file
         messages
         (stops 0))
    (unwind-protect
        (progn
          (chirp-x--write-auth-file old-auth-token old-ct0)
          (cl-letf (((symbol-function 'browser-session-capture)
                     (lambda (&rest arguments)
                       (setq capture-file (plist-get arguments :output-file))
                       (with-temp-file capture-file
                         (insert "private capture"))
                       (funcall (plist-get arguments :callback)
                                '((browser . "test")))
                       'synchronous-capture))
                    ((symbol-function 'browser-session-read)
                     (lambda (_file) capture))
                    ((symbol-function 'chirp-stop)
                     (lambda ()
                       (setq stops (1+ stops))))
                    ((symbol-function 'message)
                     (lambda (format-string &rest arguments)
                       (push (apply #'format format-string arguments) messages))))
            (chirp-x-capture-browser-session)
            (should-not (file-exists-p capture-file))
            (should (= stops 0))
            (let ((credentials (chirp-x-credentials)))
              (should (= (length (plist-get credentials :auth-token)) 19))
              (should (= (length (plist-get credentials :ct0)) 29)))
            (should (member "Chirp could not import the X browser session"
                            messages))))
      (delete-directory directory t))))

(ert-deftest chirp-x-capture-browser-session-retries-confirmed-restart ()
  "Browser-session restart requests should be retried only after confirmation."
  (let ((chirp-x--browser-session-process nil)
        (attempts 0)
        capture-files
        second-arguments)
    (unwind-protect
        (cl-letf (((symbol-function 'browser-session-capture)
                   (lambda (&rest arguments)
                     (setq attempts (1+ attempts)
                           capture-files (cons (plist-get arguments :output-file)
                                               capture-files))
                     (if (= attempts 1)
                         (funcall
                          (plist-get arguments :errorback)
                          '((code . "browser-restart-required")
                            (message . "Restart required")))
                       (setq second-arguments arguments))
                     (format "capture-process-%s" attempts)))
                  ((symbol-function 'yes-or-no-p)
                   (lambda (_prompt) t))
                  ((symbol-function 'message)
                   (lambda (&rest _arguments) nil)))
          (chirp-x-capture-browser-session)
          (should (= attempts 2))
          (should (plist-get second-arguments :restart-running)))
      (dolist (file capture-files)
        (chirp-x--delete-browser-session-capture file)))))

(ert-deftest chirp-login-dispatches-to-browser-session-capture ()
  "The public login command should use Chirp's X credential boundary."
  (require 'chirp)
  (let (called)
    (cl-letf (((symbol-function 'chirp-x-capture-browser-session)
               (lambda ()
                 (setq called t))))
      (chirp-login)
      (should called))))

(ert-deftest chirp-x-clear-auth-file-removes-private-session ()
  "Clearing the browser session should delete its auth file and reset Chirp."
  (let* ((directory (make-temp-file "chirp-x-auth-" t))
         (chirp-x-auth-file (expand-file-name "auth.json" directory))
         (chirp-x--browser-session-process nil)
         stopped)
    (unwind-protect
        (progn
          (chirp-x--write-auth-file (make-string 17 ?a) (make-string 23 ?b))
          (cl-letf (((symbol-function 'chirp-stop)
                     (lambda ()
                       (setq stopped t))))
            (chirp-x-clear-auth-file))
          (should-not (file-exists-p chirp-x-auth-file))
          (should stopped))
      (delete-directory directory t))))

(ert-deftest chirp-x-headers-reject-custom-control-character-injection ()
  "Custom headers must not inject a second HTTP header with CR or LF."
  (let ((chirp-x-user-agent "safe\r\nX-Injected: yes")
        requested
        failure)
    (cl-letf (((symbol-function 'chirp-x-credentials)
               (lambda ()
                 '(:auth-token "auth" :ct0 "csrf" :bearer-token "bearer")))
              ((symbol-function 'chirp-x--retrieve)
               (lambda (&rest _args)
                 (setq requested t))))
      (chirp-x-graphql-request
       '(:query-id "query-id" :name "Viewer") nil #'ignore
       :errback (lambda (message)
                  (setq failure message))))
    (should-not requested)
    (should (string-match-p "header is unsafe: User-Agent" failure))))

(ert-deftest chirp-x-query-id-registry-updates-only-read-fallbacks ()
  "Dynamic IDs should affect reads while writes and overrides stay explicit."
  (let* ((table
          (chirp-x--query-id-table
           (concat
            "{\"graphql\":{"
            "\"ReadOp\":{\"method\":\"GET\","
            "\"queryId\":\"AAAAAAAAAAAAAAAAAAAAAA\"},"
            "\"WriteOp\":{\"method\":\"POST\",\"queryId\":\"live-write\"}}}")))
         (chirp-x--query-id-cache table)
         (chirp-x-query-id-overrides nil))
    (should (equal (chirp-x--operation-query-id
                    '(:query-id "fallback" :name "ReadOp") "ReadOp")
                   "AAAAAAAAAAAAAAAAAAAAAA"))
    (should (equal (chirp-x--operation-query-id
                    '(:query-id "fallback" :name "WriteOp" :method post)
                    "WriteOp")
                   "fallback"))
    (let ((chirp-x-query-id-overrides '(("ReadOp" . "override"))))
      (should (equal (chirp-x--operation-query-id
                      '(:query-id "fallback" :name "ReadOp") "ReadOp")
                     "override")))
    (should-error
     (chirp-x--query-id-table
      "{\"graphql\":{\"ReadOp\":{\"method\":\"GET\",\"queryId\":\"short\"}}}"))
    (should-error
     (chirp-x--query-id-table
      (make-string (1+ chirp-x--query-id-source-limit) ?x)))))

(ert-deftest chirp-x-query-id-refresh-is-bounded-public-and-cookie-free ()
  "Registry refresh should avoid account credentials and atomically cache reads."
  (require 'plz)
  (let ((plz-curl-default-args
         '("--cookie" "auth_token=secret" "--config" "/tmp/unsafe"))
        (chirp-x--query-id-cache (make-hash-table :test #'equal))
        (chirp-x--query-id-refresh-process nil)
        (chirp-x--query-id-refresh-listeners nil)
        captured-method captured-url captured-options captured-curl-args
        process result)
    (unwind-protect
        (cl-letf (((symbol-function 'chirp-x-credentials)
                   (lambda ()
                     (ert-fail "registry refresh requested credentials")))
                  ((symbol-function 'plz)
                   (lambda (method url &rest options)
                     (setq captured-method method
                           captured-url url
                           captured-options options
                           captured-curl-args
                           (symbol-value 'plz-curl-default-args)
                           process
                           (make-pipe-process
                            :name "chirp-query-id-test" :noquery t))
                     process)))
          (chirp-x--refresh-query-ids
           (lambda (refresh-result)
             (setq result refresh-result)))
          (funcall
           (plist-get captured-options :then)
           (concat
            "{\"graphql\":{\"ReadOp\":{"
            "\"method\":\"GET\","
            "\"queryId\":\"AAAAAAAAAAAAAAAAAAAAAA\"}}}")))
      (when (process-live-p process)
        (delete-process process)))
    (should (eq captured-method 'get))
    (should (equal captured-url chirp-x--query-id-source-url))
    (should (equal (plist-get captured-options :headers)
                   '(("Accept" . "application/json"))))
    (should (eq (plist-get captured-options :as) 'string))
    (should (eq (plist-get captured-options :noquery) t))
    (should (> (plist-get captured-options :timeout) 0))
    (should (equal (car captured-curl-args) "--disable"))
    (should-not (member "--location" captured-curl-args))
    (should-not (member "--compressed" captured-curl-args))
    (should-not (member "--cookie" captured-curl-args))
    (should-not (member "--config" captured-curl-args))
    (should (member "--max-filesize" captured-curl-args))
    (should (equal (cdr (member "--max-redirs" captured-curl-args))
                   '("0")))
    (should (= (plist-get result :count) 1))
    (should (equal (gethash "ReadOp" chirp-x--query-id-cache)
                   "AAAAAAAAAAAAAAAAAAAAAA"))))

(ert-deftest chirp-x-query-id-refresh-cancellation-blocks-deferred-plz-callback ()
  "App shutdown should suppress a callback queued after curl has exited."
  (require 'plz)
  (chirp-stop)
  (let ((cache (make-hash-table :test #'equal))
        (chirp-x--query-id-refresh-process nil)
        (chirp-x--query-id-refresh-listeners nil)
        options process result)
    (puthash "Existing" "BBBBBBBBBBBBBBBBBBBBBB" cache)
    (let ((chirp-x--query-id-cache cache))
      (unwind-protect
          (cl-letf (((symbol-function 'plz)
                     (lambda (_method _url &rest request-options)
                       (setq options request-options
                             process
                             (make-pipe-process
                              :name "chirp-query-id-dead-test" :noquery t))
                       (delete-process process)
                       process)))
            (chirp-x--refresh-query-ids
             (lambda (refresh-result)
               (setq result refresh-result)))
            (should (= (length (appkit-app-handles (chirp-app))) 1))
            (chirp-stop)
            (funcall
             (plist-get options :then)
             (concat
              "{\"graphql\":{\"ReadOp\":{"
              "\"method\":\"GET\","
              "\"queryId\":\"AAAAAAAAAAAAAAAAAAAAAA\"}}}"))
            (should-not result)
            (should (equal (gethash "Existing" chirp-x--query-id-cache)
                           "BBBBBBBBBBBBBBBBBBBBBB"))
            (should-not (gethash "ReadOp" chirp-x--query-id-cache))
            (should-not chirp-x--query-id-refresh-process))
        (when (process-live-p process)
          (delete-process process))
        (chirp-stop)))))

(ert-deftest chirp-x-stale-read-refreshes-and-retries-once ()
  "A stale read should refresh IDs and retry once; writes must not retry."
  (let ((responses '("X request failed: PersistedQueryNotFound"
                     "X request failed: PersistedQueryNotFound"
                     "X request failed: PersistedQueryNotFound"
                     "X request failed (HTTP 404)"))
        (chirp-x--query-id-cache (make-hash-table :test #'equal))
        (chirp-x-query-id-overrides nil)
        (requests 0)
        (refreshes 0)
        (errors 0)
        succeeded)
    (cl-letf (((symbol-function 'chirp-x--request)
               (lambda (_url _method callback &rest options)
                 (setq requests (1+ requests))
                 (if (= requests 2)
                     (funcall callback '(("ok" . t)))
                   (funcall (plist-get options :errback) (pop responses)))
                 'request))
              ((symbol-function 'chirp-x--refresh-query-ids)
               (lambda (listener)
                 (setq refreshes (1+ refreshes))
                 (puthash "ReadOp" "new-read" chirp-x--query-id-cache)
                 (funcall listener '(:count 1)))))
      (chirp-x-graphql-request
       '(:query-id "read" :name "ReadOp") nil
       (lambda (_payload) (setq succeeded t))
       :errback (lambda (_message) (setq errors (1+ errors))))
      (chirp-x-graphql-request
       '(:query-id "write" :name "WriteOp" :method post) nil #'ignore
       :errback (lambda (_message) (setq errors (1+ errors))))
      (let ((chirp-x-query-id-overrides '(("ReadOp" . "override"))))
        (chirp-x-graphql-request
         '(:query-id "read" :name "ReadOp") nil #'ignore
         :errback (lambda (_message) (setq errors (1+ errors)))))
      (chirp-x-graphql-request
       '(:query-id "read" :name "ReadOp") nil #'ignore
       :errback (lambda (_message) (setq errors (1+ errors)))))
    (should succeeded)
    (should (= requests 5))
    (should (= errors 3))
    (should (= refreshes 1))))

(ert-deftest chirp-x-graphql-get-encodes-a-persisted-operation ()
  "GET operations should carry compact JSON parameters and web auth headers."
  (let (captured-url captured-headers captured-method silent inhibit-cookies
        redirect-limit received)
    (cl-letf (((symbol-function 'chirp-x-credentials)
               (lambda ()
                 '(:auth-token "auth" :ct0 "csrf" :bearer-token "bearer")))
              ((symbol-function 'chirp-x--retrieve)
               (lambda (url callback _callback-args request-silent request-inhibit-cookies)
                 (setq captured-url url
                       captured-headers url-request-extra-headers
                       captured-method url-request-method
                       silent request-silent
                       inhibit-cookies request-inhibit-cookies
                       redirect-limit url-max-redirections)
                 (chirp-x-test--response
                  200 "{\"data\":{\"home\":{}}}" callback))))
      (chirp-x-graphql-request
       '(:query-id "query-id" :name "HomeTimeline" :method get
         :features (("enabled" . t) ("disabled" . :json-false)))
       '(("count" . 20) ("includePromotedContent" . :json-false))
       (lambda (payload)
         (setq received payload))
       :errback (lambda (message)
                  (ert-fail (format "unexpected error: %s" message)))))
    (should (equal captured-method "GET"))
    (should silent)
    (should inhibit-cookies)
    (should (zerop redirect-limit))
    (should (string-prefix-p
             "https://x.com/i/api/graphql/query-id/HomeTimeline?"
             captured-url))
    (should (equal (alist-get "count" (chirp-x-test--query-json
                                         "variables" captured-url)
                              nil nil #'string=)
                   20))
    (let ((features (chirp-x-test--query-json "features" captured-url)))
      (should (eq (alist-get "enabled" features nil nil #'string=) t))
      (should-not (assoc-string "disabled" features t)))
    (should (equal (alist-get "Authorization" captured-headers nil nil #'string=)
                   "Bearer bearer"))
    (should (equal (alist-get "Cookie" captured-headers nil nil #'string=)
                   "auth_token=auth; ct0=csrf"))
    (should (equal (alist-get "X-Csrf-Token" captured-headers nil nil #'string=)
                   "csrf"))
    (should (assoc-string "home"
                          (cdr (assoc-string "data" received t))
                          t))))

(ert-deftest chirp-x-graphql-nil-variables-encode-an-empty-object ()
  "An operation without variables should send an empty JSON object."
  (let (captured-url)
    (cl-letf (((symbol-function 'chirp-x-credentials)
               (lambda ()
                 '(:auth-token "auth" :ct0 "csrf" :bearer-token "bearer")))
              ((symbol-function 'chirp-x--retrieve)
               (lambda (url callback _callback-args _silent _inhibit-cookies)
                 (setq captured-url url)
                 (chirp-x-test--response 200 "{\"data\":{}}" callback))))
      (chirp-x-graphql-request
       '(:query-id "query-id" :name "Viewer") nil #'ignore))
    (should (equal (chirp-x-test--query-value "variables" captured-url)
                   "{}"))))

(ert-deftest chirp-x-api-get-encodes-a-trusted-rest-request ()
  "REST GET requests should stay on an allowlisted root and encode query data."
  (let (captured-url captured-method received)
    (cl-letf (((symbol-function 'chirp-x-credentials)
               (lambda ()
                 '(:auth-token "auth" :ct0 "csrf" :bearer-token "bearer")))
              ((symbol-function 'chirp-x--retrieve)
               (lambda (url callback _callback-args _silent _inhibit-cookies)
                 (setq captured-url url
                       captured-method url-request-method)
                 (chirp-x-test--response
                  200 "{\"users\":[]}" callback))))
      (chirp-x-api-request
       'legacy "followers/list.json"
       (lambda (payload)
         (setq received payload))
       :query '(("user_id" . "42") ("count" . 20))))
    (should (equal captured-method "GET"))
    (should (equal captured-url
                   (concat "https://api.x.com/1.1/followers/list.json?"
                           "user_id=42&count=20")))
    (should (assoc-string "users" received t))))

(ert-deftest chirp-x-api-post-sends-form-data ()
  "REST POST requests should encode form data without using JSON headers."
  (let (captured-url captured-method captured-body captured-headers)
    (cl-letf (((symbol-function 'chirp-x-credentials)
               (lambda ()
                 '(:auth-token "auth" :ct0 "csrf" :bearer-token "bearer")))
              ((symbol-function 'chirp-x--retrieve)
               (lambda (url callback _callback-args _silent _inhibit-cookies)
                 (setq captured-url url
                       captured-method url-request-method
                       captured-body url-request-data
                       captured-headers url-request-extra-headers)
                 (chirp-x-test--response 200 "{\"ok\":true}" callback))))
      (chirp-x-api-request
       'web "1.1/friendships/create.json" #'ignore
       :method 'post
       :form '(("user_id" . "42")
               ("include_profile_interstitial_type" . "1"))))
    (should (equal captured-url
                   "https://x.com/i/api/1.1/friendships/create.json"))
    (should (equal captured-method "POST"))
    (should (equal captured-body
                   "user_id=42&include_profile_interstitial_type=1"))
    (should (equal (alist-get "Content-Type" captured-headers nil nil #'string=)
                   "application/x-www-form-urlencoded"))))

(ert-deftest chirp-x-api-rejects-untrusted-paths-before-authentication ()
  "REST paths must not escape the fixed authenticated API roots."
  (let (authenticated requested failure)
    (cl-letf (((symbol-function 'chirp-x-credentials)
               (lambda ()
                 (setq authenticated t)))
              ((symbol-function 'chirp-x--retrieve)
               (lambda (&rest _args)
                 (setq requested t))))
      (chirp-x-api-request
       'web "../account.example/steal" #'ignore
       :errback (lambda (message)
                  (setq failure message))))
    (should-not authenticated)
    (should-not requested)
    (should (string-match-p "path is invalid" failure))))

(ert-deftest chirp-x-graphql-post-sends-json-body ()
  "POST operations should put persisted GraphQL parameters in a JSON body."
  (let (captured-url captured-headers captured-method captured-body received)
    (cl-letf (((symbol-function 'chirp-x-credentials)
               (lambda ()
                 '(:auth-token "auth" :ct0 "csrf" :bearer-token "bearer")))
              ((symbol-function 'chirp-x--retrieve)
               (lambda (url callback _callback-args _silent _inhibit-cookies)
                 (setq captured-url url
                       captured-headers url-request-extra-headers
                       captured-method url-request-method
                       captured-body url-request-data)
                 (chirp-x-test--response 200 "{\"data\":{}}" callback))))
      (chirp-x-graphql-request
       '(:query-id "query-id" :name "FavoriteTweet" :method post
         :features (("feature" . t))
         :field-toggles (("withArticlePlainText" . :json-false)))
       '(("tweet_id" . "1"))
       (lambda (payload)
         (setq received payload))
       :errback (lambda (message)
                  (ert-fail (format "unexpected error: %s" message)))))
    (should (equal captured-method "POST"))
    (should (equal captured-url
                   "https://x.com/i/api/graphql/query-id/FavoriteTweet"))
    (should (equal (alist-get "Content-Type" captured-headers nil nil #'string=)
                   "application/json"))
    (let ((body (json-parse-string captured-body
                                   :object-type 'alist
                                   :array-type 'list
                                   :null-object nil
                                   :false-object nil)))
      (should (equal (alist-get "queryId" body nil nil #'string=) "query-id"))
      (should (equal (alist-get "tweet_id"
                                (alist-get "variables" body nil nil #'string=)
                                nil nil #'string=)
                     "1"))
      (should (eq (alist-get "feature"
                             (alist-get "features" body nil nil #'string=)
                             nil nil #'string=)
                  t)))
    (should (assoc-string "data" received t))))

(ert-deftest chirp-x-write-disables-url-internal-retry ()
  "A POST retrieval buffer should prohibit url.el transport replay."
  (let* ((chirp--app nil)
         (process (make-pipe-process
                   :name "chirp-x-no-retry"
                   :buffer nil
                   :noquery t))
         request-buffer activated replayed)
    (unwind-protect
        (cl-letf (((symbol-function 'chirp-x-credentials)
                   (lambda ()
                     '(:auth-token "auth" :ct0 "csrf"
                       :bearer-token "bearer")))
                  ((symbol-function 'url-find-proxy-for-url)
                   (lambda (&rest _args) nil))
                  ((symbol-function 'url-http-find-free-connection)
                   (lambda (&rest _args) process)))
          (setq request-buffer
                (chirp-x-graphql-request
                 '(:query-id "query-id" :name "CreateTweet" :method post)
                 '(("tweet_text" . "hello")) #'ignore
                 :errback #'ignore))
          (should (buffer-live-p request-buffer))
          (with-current-buffer request-buffer
            (should (buffer-local-value
                     'url-http-no-retry request-buffer))
            (should-not url-http-attempt-keepalives))
          (cl-letf (((symbol-function 'url-http-idle-sentinel) #'ignore)
                    ((symbol-function 'url-http-activate-callback)
                     (lambda () (setq activated t)))
                    ((symbol-function 'url-http)
                     (lambda (&rest _args) (setq replayed t))))
            (url-http-end-of-document-sentinel process "closed"))
          (should activated)
          (should-not replayed))
      (chirp-stop)
      (when (process-live-p process)
        (delete-process process))
      (when (buffer-live-p request-buffer)
        (kill-buffer request-buffer)))))

(ert-deftest chirp-x-synchronous-callback-error-does-not-double-settle ()
  "A signaling success callback should not subsequently invoke ERRBACK."
  (let (successes failures)
    (cl-letf (((symbol-function 'chirp-x-credentials)
               (lambda ()
                 '(:auth-token "auth" :ct0 "csrf" :bearer-token "bearer")))
              ((symbol-function 'chirp-x--retrieve)
               (lambda (_url callback _callback-args _silent
                        _inhibit-cookies)
                 (chirp-x-test--response 200 "{\"data\":{}}" callback))))
      (let ((condition
             (should-error
              (chirp-x-graphql-request
               '(:query-id "query-id" :name "CreateTweet" :method post)
               nil
               (lambda (_payload)
                 (setq successes (1+ (or successes 0)))
                 (error "consumer callback failed"))
               :errback
               (lambda (_message)
                 (setq failures (1+ (or failures 0))))))))
        (should (equal (error-message-string condition)
                       "consumer callback failed"))))
    (should (= successes 1))
    (should-not failures)))

(ert-deftest chirp-x-synchronous-errback-error-does-not-double-settle ()
  "A signaling ERRBACK should be called only once after dispatch failure."
  (let ((failures 0))
    (cl-letf (((symbol-function 'chirp-x-credentials)
               (lambda ()
                 '(:auth-token "auth" :ct0 "csrf" :bearer-token "bearer")))
              ((symbol-function 'chirp-x--retrieve)
               (lambda (&rest _args) (error "dispatch failed"))))
      (let ((condition
             (should-error
              (chirp-x-graphql-request
               '(:query-id "query-id" :name "CreateTweet" :method post)
               nil #'ignore
               :errback
               (lambda (_message)
                 (setq failures (1+ failures))
                 (error "consumer errback failed"))))))
        (should (equal (error-message-string condition)
                       "consumer errback failed"))))
    (should (= failures 1))))

(ert-deftest chirp-x-graphql-request-reports-remote-errors ()
  "HTTP and GraphQL failures should reach ERRBACK with no success callback."
  (let (success failure)
    (cl-letf (((symbol-function 'chirp-x-credentials)
               (lambda ()
                 '(:auth-token "auth" :ct0 "csrf" :bearer-token "bearer")))
              ((symbol-function 'chirp-x--retrieve)
               (lambda (_url callback _callback-args _silent _inhibit-cookies)
                 (chirp-x-test--response
                  429 "{\"errors\":[{\"message\":\"Rate limited\",\"code\":88}]}"
                  callback))))
      (chirp-x-graphql-request
       '(:query-id "query-id" :name "HomeTimeline")
       '(("count" . 1))
       (lambda (_payload)
         (setq success t))
       :errback (lambda (message)
                  (setq failure message))))
    (should-not success)
    (should (equal failure "X request failed (HTTP 429): Rate limited (88)"))))

(ert-deftest chirp-x-write-treats-408-and-429-as-ambiguous ()
  "Timeout and rate-limit write responses should have unknown outcomes."
  (dolist (status '(408 429))
    (let (failure)
      (cl-letf (((symbol-function 'chirp-x-credentials)
                 (lambda ()
                   '(:auth-token "auth" :ct0 "csrf" :bearer-token "bearer")))
                ((symbol-function 'chirp-x--retrieve)
                 (lambda (_url callback _callback-args _silent
                          _inhibit-cookies)
                   (chirp-x-test--response status "{}" callback))))
        (chirp-x-graphql-request
         '(:query-id "query-id" :name "WriteMutation" :method post)
         nil #'ignore :errback (lambda (message) (setq failure message))))
      (should (string-prefix-p "X write outcome is unknown" failure)))))

(ert-deftest chirp-x-graphql-request-marks-partial-mutation-errors-unknown ()
  "Mutation errors alongside partial data should have an unknown outcome."
  (let (success failure)
    (cl-letf (((symbol-function 'chirp-x-credentials)
               (lambda ()
                 '(:auth-token "auth" :ct0 "csrf" :bearer-token "bearer")))
              ((symbol-function 'chirp-x--retrieve)
               (lambda (_url callback _callback-args _silent _inhibit-cookies)
                 (chirp-x-test--response
                  200
                  (concat
                   "{\"data\":{\"unrelated\":{},\"favorite_tweet\":{\"errors\":["
                   "{\"message\":\"Already liked\",\"code\":139}]}}}")
                  callback))))
      (chirp-x-graphql-request
       '(:query-id "query-id" :name "FavoriteTweet" :method post)
       '(("tweet_id" . "1"))
       (lambda (_payload)
         (setq success t))
       :errback (lambda (message)
                  (setq failure message))))
    (should-not success)
    (should (string-prefix-p "X write outcome is unknown" failure))
    (should (string-match-p "Already liked (139)" failure))))

(ert-deftest chirp-x-graphql-errors-with-write-data-have-unknown-outcomes ()
  "GraphQL errors must not discard a possible write acknowledgement."
  (dolist (status '(200 400))
    (let (success failure)
      (cl-letf (((symbol-function 'chirp-x-credentials)
                 (lambda ()
                   '(:auth-token "auth" :ct0 "csrf"
                     :bearer-token "bearer")))
                ((symbol-function 'chirp-x--retrieve)
                 (lambda (_url callback _callback-args _silent
                          _inhibit-cookies)
                   (chirp-x-test--response
                    status
                    (concat
                     "{\"data\":{\"create_tweet\":{\"tweet_id\":\"1\"}},"
                     "\"errors\":[{\"message\":\"resolver failed\","
                     "\"code\":5000}]}")
                    callback))))
        (chirp-x-graphql-request
         '(:query-id "query-id" :name "CreateTweet" :method post)
         '(("tweet_text" . "hello"))
         (lambda (_payload) (setq success t))
         :errback (lambda (message) (setq failure message))))
      (should-not success)
      (should (string-prefix-p "X write outcome is unknown" failure))
      (should (string-match-p "resolver failed (5000)" failure)))))

(ert-deftest chirp-x-message-less-graphql-write-error-is-not-success ()
  "A GraphQL error without readable text should still fail the write safely."
  (let (success failure)
    (cl-letf (((symbol-function 'chirp-x-credentials)
               (lambda ()
                 '(:auth-token "auth" :ct0 "csrf" :bearer-token "bearer")))
              ((symbol-function 'chirp-x--retrieve)
               (lambda (_url callback _callback-args _silent
                        _inhibit-cookies)
                 (chirp-x-test--response
                  200 "{\"data\":{},\"errors\":[{\"code\":5001}]}"
                  callback))))
      (chirp-x-graphql-request
       '(:query-id "query-id" :name "WriteMutation" :method post)
       nil (lambda (_payload) (setq success t))
       :errback (lambda (message) (setq failure message))))
    (should-not success)
    (should (string-prefix-p "X write outcome is unknown" failure))
    (should (string-match-p "GraphQL returned an error (5001)" failure))))

(ert-deftest chirp-x-write-rejects-oversized-acknowledgements-before-parsing ()
  "A write acknowledgement over the bound should fail safely before JSON."
  (let ((chirp-x--write-response-limit 32)
        parsed success failure)
    (cl-letf (((symbol-function 'chirp-x-credentials)
               (lambda ()
                 '(:auth-token "auth" :ct0 "csrf" :bearer-token "bearer")))
              ((symbol-function 'json-parse-string)
               (lambda (&rest _args)
                 (setq parsed t)
                 (error "oversized acknowledgement was parsed")))
              ((symbol-function 'chirp-x--retrieve)
               (lambda (_url callback _callback-args _silent
                        _inhibit-cookies)
                 (chirp-x-test--response
                  200 (concat "{\"data\":\"" (make-string 64 ?x) "\"}")
                  callback))))
      (chirp-x-graphql-request
       '(:query-id "query-id" :name "WriteMutation" :method post)
       nil (lambda (_payload) (setq success t))
       :errback (lambda (message) (setq failure message))))
    (should-not parsed)
    (should-not success)
    (should (string-prefix-p "X write outcome is unknown" failure))
    (should (string-match-p "exceeds 32 bytes" failure))))

(ert-deftest chirp-x-graphql-request-disables-redirects-in-the-request-buffer ()
  "The request buffer should retain the no-redirect policy asynchronously."
  (let ((request-buffer (generate-new-buffer " *chirp-x-test-request*")))
    (unwind-protect
        (cl-letf (((symbol-function 'chirp-x-credentials)
                   (lambda ()
                     '(:auth-token "auth" :ct0 "csrf" :bearer-token "bearer")))
                  ((symbol-function 'chirp-x--retrieve)
                   (lambda (&rest _args)
                     request-buffer)))
          (should
           (eq (chirp-x-graphql-request
                '(:query-id "query-id" :name "HomeTimeline")
                '(("count" . 1))
                #'ignore)
               request-buffer))
          (with-current-buffer request-buffer
            (should (local-variable-p 'url-max-redirections))
            (should (zerop url-max-redirections))))
      (when (buffer-live-p request-buffer)
        (kill-buffer request-buffer)))))

(ert-deftest chirp-x-request-lifecycle-can-belong-to-an-exact-view ()
  "Killing a view should cancel its retrieval and suppress late callbacks."
  (let ((chirp--app nil)
        (view-buffer (generate-new-buffer " *chirp-x-owner-view*"))
        (request-buffer (generate-new-buffer " *chirp-x-owner-request*"))
        retrieval-callback
        success
        failure
        view)
    (unwind-protect
        (progn
          (with-current-buffer view-buffer
            (chirp-view-mode)
            (setq view
                  (appkit-attach-view
                   :app (chirp-app)
                   :id '(test transport-owner)
                   :state '(:type test)
                   :mode 'chirp-view-mode
                   :sync-function #'ignore
                   :parts nil)))
          (cl-letf (((symbol-function 'chirp-x-credentials)
                     (lambda ()
                       '(:auth-token "auth" :ct0 "csrf"
                         :bearer-token "bearer")))
                    ((symbol-function 'chirp-x--retrieve)
                     (lambda (_url callback &rest _args)
                       (setq retrieval-callback callback)
                       request-buffer)))
            (chirp-x-graphql-request
             '(:query-id "query-id" :name "HomeTimeline")
             '(("count" . 1))
             (lambda (_payload)
               (setq success t))
             :errback (lambda (message)
                        (setq failure message))
             :owner view))
          (should (= (length (appkit-view-handles view)) 1))
          (should-not (appkit-app-handles (chirp-app)))
          (appkit-kill-view view)
          (should-not (buffer-live-p request-buffer))
          (should-not (appkit-view-handles view))
          (let ((late-buffer
                 (generate-new-buffer " *chirp-x-late-response*")))
            (with-current-buffer late-buffer
              (funcall retrieval-callback nil))
            (should-not (buffer-live-p late-buffer)))
          (should-not success)
          (should (equal failure "X request was canceled")))
      (chirp-stop)
      (dolist (buffer (list view-buffer request-buffer))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest chirp-x-app-read-can-settle-its-continuation-on-cancel ()
  "An app-owned continuation GET should opt into cancellation settlement."
  (let ((chirp--app nil)
        (request-buffer (generate-new-buffer " *chirp-x-status-request*"))
        failure)
    (unwind-protect
        (cl-letf (((symbol-function 'chirp-x-credentials)
                   (lambda ()
                     '(:auth-token "auth" :ct0 "csrf"
                       :bearer-token "bearer")))
                  ((symbol-function 'chirp-x--retrieve)
                   (lambda (&rest _args)
                     request-buffer)))
          (chirp-x--request
           "https://upload.twitter.com/i/media/upload.json?command=STATUS"
           'get #'ignore
           :owner (chirp-app)
           :settle-on-cancel t
           :cancel-message "upload continuation canceled"
           :errback (lambda (message)
                      (setq failure message)))
          (chirp-stop)
          (should-not (buffer-live-p request-buffer))
          (should (equal failure "upload continuation canceled")))
      (chirp-stop)
      (when (buffer-live-p request-buffer)
        (kill-buffer request-buffer)))))

(ert-deftest chirp-x-canceled-write-reports-an-unknown-outcome ()
  "Stopping an in-flight POST should visibly settle its ambiguous outcome."
  (let ((chirp--app nil)
        (request-buffer (generate-new-buffer " *chirp-x-write-request*"))
        failure)
    (unwind-protect
        (cl-letf (((symbol-function 'chirp-x-credentials)
                   (lambda ()
                     '(:auth-token "auth" :ct0 "csrf"
                       :bearer-token "bearer")))
                  ((symbol-function 'chirp-x--retrieve)
                   (lambda (&rest _args)
                     request-buffer)))
          (chirp-x-graphql-request
           '(:query-id "query-id" :name "CreateTweet" :method post)
           '(("tweet_text" . "hello")) #'ignore
           :errback (lambda (message)
                      (setq failure message)))
          (chirp-stop)
          (should-not (buffer-live-p request-buffer))
          (should (string-prefix-p "X write outcome is unknown" failure)))
      (chirp-stop)
      (when (buffer-live-p request-buffer)
        (kill-buffer request-buffer)))))

(ert-deftest chirp-x-graphql-request-rejects-redirects ()
  "A redirect result should be terminal for an authenticated request."
  (let ((request-count 0)
        success
        failure)
    (cl-letf (((symbol-function 'chirp-x-credentials)
               (lambda ()
                 '(:auth-token "auth" :ct0 "csrf" :bearer-token "bearer")))
              ((symbol-function 'chirp-x--retrieve)
               (lambda (_url callback _callback-args _silent _inhibit-cookies)
                 (setq request-count (1+ request-count))
                 (chirp-x-test--response
                  302 ""
                  (lambda (_status)
                    (funcall callback
                             '(:error
                               (error http-redirect-limit
                                      "https://example.invalid/"))))))))
      (chirp-x-graphql-request
       '(:query-id "query-id" :name "HomeTimeline")
       '(("count" . 1))
       (lambda (_payload)
         (setq success t))
       :errback (lambda (message)
                  (setq failure message))))
    (should (= request-count 1))
    (should-not success)
    (should (equal failure "X request failed (HTTP 302)"))))

(ert-deftest chirp-x-request-encodes-unicode-post-data-as-utf-8 ()
  "POST data should reach url.el as an encoded unibyte string."
  (let (captured-data)
    (cl-letf (((symbol-function 'chirp-x-credentials)
               (lambda ()
                 '(:auth-token "auth" :ct0 "csrf" :bearer-token "bearer")))
              ((symbol-function 'chirp-x--retrieve)
               (lambda (_url callback _callback-args _silent _inhibit-cookies)
                 (setq captured-data url-request-data)
                 (chirp-x-test--response 200 "{\"data\":{}}" callback))))
      (chirp-x-graphql-request
       '(:query-id "query-id" :name "CreateTweet" :method post)
       '(("tweet_text" . "你好")) #'ignore))
    (should-not (multibyte-string-p captured-data))
    (should (string-match-p
             (regexp-quote (encode-coding-string "你好" 'utf-8))
             captured-data))))

(ert-deftest chirp-x-request-marks-dispatch-attempt-errors-unknown-and-redacted ()
  "A POST dispatch attempt failure should be unknown and redact credentials."
  (let (failure)
    (cl-letf (((symbol-function 'chirp-x-credentials)
               (lambda ()
                 '(:auth-token "secret" :ct0 "secret-suffix"
                   :bearer-token "secret-bearer")))
              ((symbol-function 'chirp-x--retrieve)
               (lambda (&rest _args)
                 (error "headers secret secret-suffix secret-bearer"))))
      (chirp-x-graphql-request
       '(:query-id "query-id" :name "CreateTweet" :method post)
       '(("tweet_text" . "hello")) #'ignore
       :errback (lambda (message)
                  (setq failure message))))
    (should (string-prefix-p "X write outcome is unknown" failure))
    (should (string-match-p
             "headers \\[REDACTED\\] \\[REDACTED\\] \\[REDACTED\\]"
             failure))
    (should-not (string-match-p "secret\\|suffix\\|bearer" failure))))

(ert-deftest chirp-x-dispatch-signal-discards-preallocated-write-buffer ()
  "A signal inside POST startup should discard its known buffer and process."
  (let* ((chirp--app nil)
         (request-buffer (generate-new-buffer " *chirp-x-dispatch-write*"))
         (process (make-pipe-process
                   :name "chirp-x-dispatch-write"
                   :buffer request-buffer
                   :noquery t))
         failure)
    (unwind-protect
        (cl-letf (((symbol-function 'chirp-x-credentials)
                   (lambda ()
                     '(:auth-token "auth" :ct0 "csrf"
                       :bearer-token "bearer")))
                  ((symbol-function 'chirp-x--retrieve)
                   (lambda (&rest _args)
                     (setq chirp-x--dispatch-buffer request-buffer)
                     (error "write startup failed"))))
          (should-not
           (chirp-x-graphql-request
            '(:query-id "query-id" :name "CreateTweet" :method post)
            '(("tweet_text" . "hello")) #'ignore
            :errback (lambda (message) (setq failure message))))
          (should-not (buffer-live-p request-buffer))
          (should-not (process-live-p process))
          (should (string-prefix-p "X write outcome is unknown" failure)))
      (chirp-stop)
      (when (process-live-p process)
        (delete-process process))
      (when (buffer-live-p request-buffer)
        (kill-buffer request-buffer)))))

(ert-deftest chirp-x-post-dispatch-setup-error-discards-unowned-request ()
  "A write left unowned after dispatch should be canceled and marked unknown."
  (let* ((chirp--app nil)
         (request-buffer (generate-new-buffer " *chirp-x-unowned-write*"))
         (process (make-pipe-process
                   :name "chirp-x-unowned-write"
                   :buffer request-buffer
                   :noquery t))
         failure)
    (unwind-protect
        (cl-letf (((symbol-function 'chirp-x-credentials)
                   (lambda ()
                     '(:auth-token "auth" :ct0 "csrf"
                       :bearer-token "bearer")))
                  ((symbol-function 'chirp-x--retrieve)
                   (lambda (&rest _args) request-buffer))
                  ((symbol-function 'appkit-register-handle)
                   (lambda (&rest _args)
                     (error "owner rejected request"))))
          (should-not
           (chirp-x-graphql-request
            '(:query-id "query-id" :name "CreateTweet" :method post)
            '(("tweet_text" . "hello")) #'ignore
            :errback (lambda (message) (setq failure message))))
          (should-not (buffer-live-p request-buffer))
          (should-not (process-live-p process))
          (should (string-prefix-p "X write outcome is unknown" failure))
          (should (string-match-p "owner rejected request" failure)))
      (chirp-stop)
      (when (process-live-p process)
        (delete-process process))
      (when (buffer-live-p request-buffer)
        (kill-buffer request-buffer)))))

(ert-deftest chirp-x-post-dispatch-quit-discards-unowned-request ()
  "A quit after write dispatch should clean up, warn, and keep quitting."
  (let* ((chirp--app nil)
         (request-buffer (generate-new-buffer " *chirp-x-quit-write*"))
         (process (make-pipe-process
                   :name "chirp-x-quit-write"
                   :buffer request-buffer
                   :noquery t))
         escaped failure)
    (unwind-protect
        (cl-letf (((symbol-function 'chirp-x-credentials)
                   (lambda ()
                     '(:auth-token "auth" :ct0 "csrf"
                       :bearer-token "bearer")))
                  ((symbol-function 'chirp-x--retrieve)
                   (lambda (&rest _args) request-buffer))
                  ((symbol-function 'appkit-register-handle)
                   (lambda (&rest _args)
                     (signal 'quit nil))))
          (condition-case nil
              (chirp-x-graphql-request
               '(:query-id "query-id" :name "CreateTweet" :method post)
               '(("tweet_text" . "hello")) #'ignore
               :errback (lambda (message) (setq failure message)))
            (quit (setq escaped t)))
          (should escaped)
          (should-not (buffer-live-p request-buffer))
          (should-not (process-live-p process))
          (should (string-prefix-p "X write outcome is unknown" failure)))
      (chirp-stop)
      (when (process-live-p process)
        (delete-process process))
      (when (buffer-live-p request-buffer)
        (kill-buffer request-buffer)))))

(ert-deftest chirp-x-graphql-request-reports-missing-credentials ()
  "Credential setup failures should not start an HTTP request."
  (let (called failure)
    (cl-letf (((symbol-function 'chirp-x-credentials)
               (lambda ()
                 (error "credentials unavailable")))
              ((symbol-function 'chirp-x--retrieve)
               (lambda (&rest _args)
                 (setq called t))))
      (chirp-x-graphql-request
       '(:query-id "query-id" :name "HomeTimeline")
       '(("count" . 1))
       #'ignore
       :errback (lambda (message)
                  (setq failure message))))
    (should-not called)
    (should (equal failure "credentials unavailable"))))

(ert-deftest chirp-x-upload-media-runs-init-append-finalize-and-status ()
  "Media upload should use the trusted chunked protocol and await processing."
  (let ((file (make-temp-file "chirp-x-upload-" nil ".png"))
        requests
        received
        failure)
    (unwind-protect
        (progn
          (with-temp-buffer
            (set-buffer-multibyte nil)
            (insert (encode-coding-string "\x89PNG\r\n\0payload" 'binary))
            (write-region (point-min) (point-max) file nil 'silent))
          (cl-letf (((symbol-function 'chirp-x-credentials)
                     (lambda ()
                       '(:auth-token "auth" :ct0 "csrf"
                         :bearer-token "bearer")))
                    ((symbol-function 'run-at-time)
                     (lambda (_delay _repeat function &rest args)
                       (apply function args)
                       'timer))
                    ((symbol-function 'chirp-x--retrieve)
                     (lambda (url callback _callback-args _silent _inhibit-cookies)
                       (let* ((request-url
                               (if (url-p url) (url-recreate-url url) url))
                              (request
                               (list :url request-url
                                     :method url-request-method
                                     :data url-request-data
                                     :headers url-request-extra-headers)))
                         (push request requests)
                         (cond
                          ((string-match-p "command=STATUS" request-url)
                           (chirp-x-test--response
                            200
                            "{\"processing_info\":{\"state\":\"succeeded\"}}"
                            callback))
                          ((string-prefix-p "command=INIT" url-request-data)
                           (chirp-x-test--response
                            200 "{\"media_id_string\":\"987\"}" callback))
                          ((string-prefix-p "command=FINALIZE" url-request-data)
                           (chirp-x-test--response
                            200
                            (concat
                             "{\"processing_info\":{\"state\":\"pending\"," 
                             "\"check_after_secs\":0}}")
                            callback))
                          (t (chirp-x-test--response 204 "" callback)))))))
            (chirp-x-upload-media
             file
             (lambda (media-id)
               (setq received media-id))
             :errback (lambda (message)
                        (setq failure message))))
          (setq requests (nreverse requests))
          (should-not failure)
          (should (equal received "987"))
          (should (= (length requests) 4))
          (dolist (request requests)
            (should (string-prefix-p
                     "https://upload.twitter.com/i/media/upload.json"
                     (plist-get request :url))))
          (let* ((append-request (nth 1 requests))
                 (content-type
                  (alist-get "Content-Type"
                             (plist-get append-request :headers)
                             nil nil #'string=)))
            (should (equal (plist-get append-request :method) "POST"))
            (should-not
             (multibyte-string-p (plist-get append-request :method)))
            (dolist (header (plist-get append-request :headers))
              (should-not (multibyte-string-p (car header)))
              (should-not (multibyte-string-p (cdr header))))
            (should (string-prefix-p "multipart/form-data; boundary="
                                     content-type))
            (should (string-match-p "name=\"segment_index\""
                                    (plist-get append-request :data)))
            (should (string-match-p "987"
                                    (plist-get append-request :data))))
          (should (equal (plist-get (car (last requests)) :method) "GET")))
      (when (file-exists-p file)
        (delete-file file)))))

(ert-deftest chirp-x-upload-media-does-not-retry-a-failed-init ()
  "An INIT failure should be surfaced after exactly one mutation request."
  (let ((file (make-temp-file "chirp-x-upload-" nil ".png"))
        (request-count 0)
        failure)
    (unwind-protect
        (progn
          (write-region "png" nil file nil 'silent)
          (cl-letf (((symbol-function 'chirp-x-credentials)
                     (lambda ()
                       '(:auth-token "auth" :ct0 "csrf"
                         :bearer-token "bearer")))
                    ((symbol-function 'chirp-x--retrieve)
                     (lambda (_url callback _callback-args _silent
                              _inhibit-cookies)
                       (setq request-count (1+ request-count))
                       (chirp-x-test--response
                        503 "{\"errors\":[{\"message\":\"Unavailable\"}]}"
                        callback))))
            (chirp-x-upload-media
             file #'ignore
             :errback (lambda (message)
                        (setq failure message)))))
      (delete-file file))
    (should (= request-count 1))
    (should (string-match-p "HTTP 503" failure))))

(ert-deftest chirp-x-upload-media-surfaces-processing-failure ()
  "A failed FINALIZE processing state should reach the error callback."
  (let ((file (make-temp-file "chirp-x-upload-" nil ".png"))
        (request-count 0)
        success
        failure)
    (unwind-protect
        (progn
          (write-region "png" nil file nil 'silent)
          (cl-letf (((symbol-function 'chirp-x--request)
                     (lambda (_url _method callback &rest _options)
                       (setq request-count (1+ request-count))
                       (pcase request-count
                         (1 (funcall callback '(("media_id_string" . "7"))))
                         (2 (funcall callback
                                     (make-hash-table :test #'equal)))
                         (3 (funcall
                             callback
                             '(("processing_info" .
                                (("state" . "failed")
                                 ("error" . (("code" . 3)
                                             ("message" .
                                              "Invalid media"))))))))))))
            (chirp-x-upload-media
             file (lambda (_media-id)
                    (setq success t))
             :errback (lambda (message)
                        (setq failure message)))))
      (delete-file file))
    (should (= request-count 3))
    (should-not success)
    (should (equal failure "Invalid media (3)"))))

(ert-deftest chirp-x-upload-poll-timer-is-owned-and-settled-on-stop ()
  "Stopping Chirp should cancel a pending STATUS poll without restarting."
  (let ((chirp--app nil)
        (file (make-temp-file "chirp-x-upload-" nil ".png"))
        (request-count 0)
        failure
        app
        timer)
    (unwind-protect
        (progn
          (write-region "png" nil file nil 'silent)
          (cl-letf (((symbol-function 'chirp-x--request)
                     (lambda (_url _method callback &rest _options)
                       (setq request-count (1+ request-count))
                       (pcase request-count
                         (1 (funcall callback '(("media_id_string" . "7"))))
                         (2 (funcall callback
                                     (make-hash-table :test #'equal)))
                         (3 (funcall
                             callback
                             '(("processing_info" .
                                (("state" . "pending")
                                 ("check_after_secs" . 30))))))))))
            (chirp-x-upload-media
             file #'ignore
             :errback (lambda (message)
                        (setq failure message))))
          (setq app chirp--app)
          (should (appkit-app-live-p app))
          (should (= (length (appkit-app-handles app)) 1))
          (setq timer
                (plist-get
                 (appkit-handle-object (car (appkit-app-handles app)))
                 :timer))
          (should (timerp timer))
          (chirp-stop)
          (should-not chirp--app)
          (should-not (appkit-app-live-p app))
          (should (= request-count 3))
          (should (string-prefix-p "X write outcome is unknown" failure))
          (should-not (memq timer timer-list)))
      (chirp-stop)
      (when (file-exists-p file)
        (delete-file file)))))

(ert-deftest chirp-x-upload-media-bounds-pending-status-polls ()
  "Repeated pending media processing should stop at the configured limit."
  (let ((file (make-temp-file "chirp-x-upload-" nil ".png"))
        (chirp-x--upload-status-limit 2)
        (request-count 0)
        failure)
    (unwind-protect
        (progn
          (write-region "png" nil file nil 'silent)
          (cl-letf (((symbol-function 'run-at-time)
                     (lambda (_delay _repeat function &rest args)
                       (apply function args)
                       'timer))
                    ((symbol-function 'chirp-x--request)
                     (lambda (_url _method callback &rest _options)
                       (setq request-count (1+ request-count))
                       (pcase request-count
                         (1 (funcall callback '(("media_id_string" . "7"))))
                         (2 (funcall callback
                                     (make-hash-table :test #'equal)))
                         (_ (funcall
                             callback
                             '(("processing_info" .
                                (("state" . "pending")
                                 ("check_after_secs" . 0))))))))))
            (chirp-x-upload-media
             file #'ignore
             :errback (lambda (message)
                        (setq failure message)))))
      (delete-file file))
    (should (= request-count 5))
    (should (equal failure
                   "X media processing did not finish in time"))))

(ert-deftest chirp-x-upload-media-rejects-unsupported-files-before-authentication ()
  "Unsupported upload files should fail before credentials leave their source."
  (let ((file (make-temp-file "chirp-x-upload-" nil ".txt"))
        authenticated
        failure)
    (unwind-protect
        (cl-letf (((symbol-function 'chirp-x-credentials)
                   (lambda ()
                     (setq authenticated t))))
          (chirp-x-upload-media
           file #'ignore
           :errback (lambda (message)
                      (setq failure message))))
      (delete-file file))
    (should-not authenticated)
    (should (string-match-p "Unsupported image format" failure))))

(ert-deftest chirp-x-upload-gif-splits-large-media-into-one-mebibyte-segments ()
  "Large GIF payloads should be divided into consecutive APPEND segments."
  (let* ((bytes (make-string (1+ chirp-x--upload-chunk-size) ?x))
         (segments (chirp-x--upload-segments bytes "image/gif")))
    (should (= (length segments) 2))
    (should (= (length (car segments)) chirp-x--upload-chunk-size))
    (should (= (length (cadr segments)) 1))))

(provide 'chirp-x-test)

;;; chirp-x-test.el ends here
