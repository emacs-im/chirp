;;; chirp-x.el --- X web API transport for Chirp -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Authenticate and issue persisted GraphQL and allowlisted REST requests to
;; X's web API.  This module owns credentials, HTTP details, and remote error
;; decoding; callers own operation selection and response adaptation.

;;; Code:

(require 'browser-session)
(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'url)
(require 'url-http)
(require 'url-util)
(require 'chirp-core)

(declare-function plz "plz" (method url &rest options))
(declare-function plz-error-curl-error "plz" (failure))
(declare-function plz-error-message "plz" (failure))
(declare-function plz-error-response "plz" (failure))
(declare-function plz-response-status "plz" (response))
(defvar plz-curl-default-args)

(defgroup chirp-x nil
  "X web API transport for Chirp."
  :group 'chirp)

(define-error 'chirp-x--callback-error "X callback failed")

(defconst chirp-x--api-base-url "https://x.com/i/api/graphql"
  "Trusted X web GraphQL endpoint that receives session credentials.")

(defconst chirp-x--rest-base-urls
  '((web . "https://x.com/i/api/")
    (legacy . "https://api.x.com/1.1/")
    (upload . "https://upload.twitter.com/i/media/")
    (upload-legacy . "https://upload.twitter.com/1.1/"))
  "Trusted X web API roots that may receive session credentials.")

(defconst chirp-x--media-alt-text-limit 1000
  "Maximum number of characters accepted in uploaded image alt text.")

(defconst chirp-x--upload-chunk-size (* 1024 1024)
  "Number of bytes in one chunked media APPEND request.")

(defconst chirp-x--upload-image-limit (* 5 1024 1024)
  "Maximum accepted JPEG, PNG, or WebP upload size in bytes.")

(defconst chirp-x--upload-gif-limit (* 15 1024 1024)
  "Maximum accepted GIF upload size in bytes.")

(defconst chirp-x--upload-video-limit (* 512 1024 1024)
  "Maximum accepted MP4 upload size in bytes.")

(defconst chirp-x--upload-status-limit 20
  "Maximum number of media processing status checks.")

(defconst chirp-x--upload-video-status-limit 40
  "Maximum number of video processing status checks.")

(defconst chirp-x--auth-schema-version 1
  "Schema version written to Chirp's private X auth file.")

(defconst chirp-x--browser-session-url "https://x.com"
  "X origin used for browser-session capture.")

(defconst chirp-x--browser-session-cookie-names '("auth_token" "ct0")
  "X cookies required for Chirp's authenticated web requests.")

(defconst chirp-x--query-id-source-url
  (concat "https://raw.githubusercontent.com/fa0311/"
          "TwitterInternalAPIDocument/master/docs/json/API.json")
  "Public operation registry used to refresh stale read query IDs.")

(defconst chirp-x--query-id-source-limit (* 512 1024)
  "Maximum accepted body size of the public query ID registry.")

(defconst chirp-x--read-response-limit (* 64 1024 1024)
  "Maximum accepted response body size for one authenticated X read.")

(defconst chirp-x--write-response-limit (* 4 1024 1024)
  "Maximum accepted response body size for one authenticated X write.")

(defvar chirp-x--browser-session-process nil
  "Current asynchronous browser-session capture process, or nil.")

(defvar chirp-x--query-id-cache (make-hash-table :test #'equal)
  "Process cache of dynamically discovered read-operation query IDs.")

(defvar chirp-x--query-id-refresh-process nil
  "Current public query ID registry retrieval process, or nil.")

(defvar chirp-x--query-id-refresh-listeners nil
  "Callbacks awaiting the current query ID registry refresh.")

(defcustom chirp-x-auth-file
  (locate-user-emacs-file "chirp/auth.json")
  "Private file holding Chirp's browser-imported X session.

`chirp-login' creates this file from an explicit browser-session capture."
  :type 'file
  :group 'chirp-x)

(defcustom chirp-x-browser-session-profile-root
  (locate-user-emacs-file "chirp/browser-session/")
  "Root for Chirp's persistent isolated X login profiles.

`browser-session' creates a browser-specific profile below this root.  This
must not be an ordinary browser profile directory."
  :type 'directory
  :group 'chirp-x)

(defcustom chirp-x-bearer-token
  (concat "AAAAAAAAAAAAAAAAAAAAANRILgAAAAAAnNwIzUejRCOuH5E6I8xnZz4puTs"
          "%3D1Zv7ttfk8LF81IUq16cHjhLTvJu4FA33AGWWjCpTnA")
  "Public X web bearer token used for authenticated web requests.

Set `CHIRP_X_BEARER_TOKEN' to override this value when X rotates its web
client token.  This is not an account credential."
  :type 'string
  :group 'chirp-x)

(defcustom chirp-x-query-id-overrides nil
  "Persisted GraphQL query IDs that override Chirp's built-in operation IDs.

Each entry has the form (OPERATION-NAME . QUERY-ID).  Overrides take precedence
over dynamically refreshed read IDs and built-in fallbacks."
  :type '(repeat (cons (string :tag "Operation")
                       (string :tag "Query ID")))
  :group 'chirp-x)

(defcustom chirp-x-user-agent
  (concat "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 "
          "(KHTML, like Gecko) Chrome/133.0.0.0 Safari/537.36")
  "User agent sent with X web API requests."
  :type 'string
  :group 'chirp-x)

(defun chirp-x--nonblank-string (value)
  "Return VALUE when it is a nonblank string, otherwise nil."
  (and (stringp value)
       (not (string-blank-p value))
       value))

(defun chirp-x--environment-value (name)
  "Return nonblank environment variable NAME, or nil."
  (chirp-x--nonblank-string (getenv name)))

(defun chirp-x--session-cookie-values-valid-p (auth-token ct0)
  "Return non-nil when AUTH-TOKEN and CT0 are safe X cookie values."
  (and (chirp-x--nonblank-string auth-token)
       (chirp-x--nonblank-string ct0)
       (not (string-match-p "[;\r\n]" auth-token))
       (not (string-match-p "[;\r\n]" ct0))))

(defun chirp-x--read-auth-file ()
  "Return the validated X cookie pair from `chirp-x-auth-file'."
  (unless (file-readable-p chirp-x-auth-file)
    (error "X authentication is not configured; run M-x chirp-login"))
  (let ((payload
         (condition-case nil
             (with-temp-buffer
               (insert-file-contents-literally chirp-x-auth-file)
               (json-parse-string (buffer-string)
                                  :object-type 'alist
                                  :array-type 'list
                                  :null-object nil
                                  :false-object nil))
           (error
            (error "X authentication file is invalid; run M-x chirp-login again")))))
    (unless (and (listp payload)
                 (equal (alist-get 'schema payload) chirp-x--auth-schema-version))
      (error "X authentication file has an unsupported schema"))
    (let ((auth-token (alist-get 'auth_token payload))
          (ct0 (alist-get 'ct0 payload)))
      (unless (chirp-x--session-cookie-values-valid-p auth-token ct0)
        (error "X authentication file contains invalid session cookies"))
      (cons auth-token ct0))))

(defun chirp-x--set-private-file-modes (file)
  "Set FILE to private Unix permissions when the platform supports them."
  (unless (eq system-type 'windows-nt)
    (set-file-modes file #o600)))

(defun chirp-x--write-auth-file (auth-token ct0)
  "Atomically write AUTH-TOKEN and CT0 to `chirp-x-auth-file'."
  (unless (chirp-x--session-cookie-values-valid-p auth-token ct0)
    (error "X browser session contains invalid cookies"))
  (let* ((file (expand-file-name chirp-x-auth-file))
         (directory (file-name-directory file))
         temporary)
    (make-directory directory t)
    (unwind-protect
        (progn
          (setq temporary
                (make-temp-file (expand-file-name ".chirp-auth-" directory)
                                nil ".json"))
          (chirp-x--set-private-file-modes temporary)
          (let ((coding-system-for-write 'utf-8-unix))
            (with-temp-file temporary
              (insert
               (json-encode `((schema . ,chirp-x--auth-schema-version)
                              (auth_token . ,auth-token)
                              (ct0 . ,ct0))))
              (insert "\n")))
          (rename-file temporary file t)
          (setq temporary nil))
      (when temporary
        (ignore-errors (delete-file temporary))))))

(defun chirp-x-credentials ()
  "Return the current authenticated X web-session credentials.

The private provider auth file is created only by `chirp-login'.  The optional
`CHIRP_X_BEARER_TOKEN' override remains available because it is a public web
client token, not an account credential."
  (pcase-let* ((`(,auth-token . ,ct0) (chirp-x--read-auth-file))
               (bearer-token
                (or (chirp-x--environment-value "CHIRP_X_BEARER_TOKEN")
                    (chirp-x--nonblank-string chirp-x-bearer-token))))
    (unless bearer-token
      (error "X public bearer token is not configured"))
    (when (string-match-p "[\r\n]" bearer-token)
      (error "X public bearer token contains an invalid header character"))
    (list :auth-token auth-token
          :ct0 ct0
          :bearer-token bearer-token)))

(defun chirp-x--browser-session-capture-file ()
  "Create and return a private temporary browser-session capture file."
  (make-temp-file "chirp-x-browser-session-" nil ".json"))

(defun chirp-x--delete-browser-session-capture (file)
  "Delete private browser-session capture FILE when it still exists."
  (when (and (stringp file) (file-exists-p file))
    (ignore-errors (delete-file file))))

(defun chirp-x--x-cookie-domain-p (domain)
  "Return non-nil when DOMAIN is an X cookie domain Chirp accepts."
  (and (stringp domain)
       (member (downcase domain) '("x.com" ".x.com"))))

(defun chirp-x--captured-cookie-value (capture name)
  "Return the unique accepted NAME cookie value from browser-session CAPTURE."
  (let ((matches
         (cl-loop for cookie in (browser-session-cookies capture)
                  when (and (equal (alist-get 'name cookie) name)
                            (chirp-x--x-cookie-domain-p
                             (alist-get 'domain cookie)))
                  collect cookie)))
    (unless (= (length matches) 1)
      (error "Captured X session must contain exactly one %s cookie" name))
    (let ((value (alist-get 'value (car matches))))
      (unless (chirp-x--nonblank-string value)
        (error "Captured X session contains an empty %s cookie" name))
      value)))

(defun chirp-x--browser-session-credentials (capture-file)
  "Return the validated X cookie pair from private CAPTURE-FILE."
  (let* ((capture (browser-session-read capture-file))
         (source (alist-get 'source capture)))
    (unless (and (listp source)
                 (equal (alist-get 'url source) chirp-x--browser-session-url))
      (error "Captured browser session is not scoped to x.com"))
    (let ((auth-token
           (chirp-x--captured-cookie-value capture "auth_token"))
          (ct0 (chirp-x--captured-cookie-value capture "ct0")))
      (unless (chirp-x--session-cookie-values-valid-p auth-token ct0)
        (error "Captured X session contains invalid cookies"))
      (cons auth-token ct0))))

(defun chirp-x--browser-session-capture-running-p ()
  "Return non-nil while Chirp is capturing a browser session."
  (and (processp chirp-x--browser-session-process)
       (process-live-p chirp-x--browser-session-process)))

(defun chirp-x--finish-browser-session-capture (capture-file)
  "Import private browser-session CAPTURE-FILE into Chirp's auth file."
  (unwind-protect
      (condition-case nil
          (pcase-let ((`(,auth-token . ,ct0)
                       (chirp-x--browser-session-credentials capture-file)))
            (chirp-x--write-auth-file auth-token ct0)
            (chirp-stop)
            (message "Chirp imported the X browser session"))
        (error
         (message "Chirp could not import the X browser session")))
    (setq chirp-x--browser-session-process nil)
    (chirp-x--delete-browser-session-capture capture-file)))

(defun chirp-x--browser-session-restart-needed-p (error)
  "Return non-nil when browser-session ERROR requests a restart."
  (equal (browser-session-error-code error) "browser-restart-required"))

(defun chirp-x--finish-browser-session-error (capture-file restart-running error)
  "Handle browser-session ERROR while capturing private CAPTURE-FILE.

When RESTART-RUNNING is nil, offer one explicit browser restart before giving
up."
  (chirp-x--delete-browser-session-capture capture-file)
  (setq chirp-x--browser-session-process nil)
  (if (and (not restart-running)
           (chirp-x--browser-session-restart-needed-p error)
           (yes-or-no-p "Restart the X login browser once to enable capture? "))
      (chirp-x--start-browser-session-capture t)
    (message "X browser session capture failed: %s"
             (browser-session-error-message error))))

(defun chirp-x--start-browser-session-capture (&optional restart-running)
  "Start an asynchronous X browser-session capture.

When RESTART-RUNNING is non-nil, permit one supported browser restart."
  (when (chirp-x--browser-session-capture-running-p)
    (user-error "An X browser-session capture is already running"))
  (let ((capture-file (chirp-x--browser-session-capture-file))
        settled)
    (message "Opening X login window...")
    (condition-case error
        (let ((process
               (browser-session-capture
                :url chirp-x--browser-session-url
                :cookies chirp-x--browser-session-cookie-names
                :output-file capture-file
                :profile-root chirp-x-browser-session-profile-root
                :restart-running restart-running
                :callback (lambda (_metadata)
                            (setq settled t)
                            (chirp-x--finish-browser-session-capture capture-file))
                :errorback (lambda (browser-error)
                             (setq settled t)
                             (chirp-x--finish-browser-session-error
                              capture-file restart-running browser-error)))))
          (unless settled
            (setq chirp-x--browser-session-process process))
          process)
      (error
       (chirp-x--delete-browser-session-capture capture-file)
       (setq chirp-x--browser-session-process nil)
       (signal (car error) (cdr error))))))

(defun chirp-x-capture-browser-session ()
  "Capture an X browser session and write Chirp's private auth file."
  (interactive)
  (chirp-x--start-browser-session-capture))

(defun chirp-x-clear-auth-file ()
  "Delete Chirp's private browser-imported auth file.

This does not sign out of X in the browser."
  (interactive)
  (when (chirp-x--browser-session-capture-running-p)
    (user-error "An X browser-session capture is already running"))
  (let ((file (expand-file-name chirp-x-auth-file)))
    (when (file-exists-p file)
      (delete-file file)))
  (chirp-stop)
  (message "Chirp browser session removed"))

(defun chirp-x--operation-string (operation property label)
  "Return nonblank string PROPERTY from OPERATION, or signal for LABEL."
  (let ((value (chirp-x--nonblank-string (plist-get operation property))))
    (unless value
      (error "X GraphQL %s is invalid" label))
    value))

(defun chirp-x--operation-query-id (operation operation-name)
  "Return OPERATION's query ID for OPERATION-NAME with configured precedence."
  (let ((query-id
         (or (cdr (assoc-string operation-name chirp-x-query-id-overrides t))
             (and (not (eq (plist-get operation :method) 'post))
                  (gethash operation-name chirp-x--query-id-cache))
             (plist-get operation :query-id))))
    (unless (chirp-x--nonblank-string query-id)
      (error "X GraphQL query ID is invalid"))
    query-id))

(defun chirp-x--operation-method (operation)
  "Return the validated HTTP method selected by OPERATION."
  (pcase (or (plist-get operation :method) 'get)
    ('get 'get)
    ('post 'post)
    (method (error "X GraphQL method is invalid: %S" method))))

(defun chirp-x--operation-features (operation)
  "Return OPERATION's feature alist after omitting false defaults."
  (let ((features (plist-get operation :features)))
    (unless (or (null features) (listp features))
      (error "X GraphQL features are invalid: %S" features))
    (delq nil
          (mapcar (lambda (entry)
                    (unless (and (consp entry)
                                 (stringp (car entry)))
                      (error "X GraphQL feature is invalid: %S" entry))
                    (unless (memq (cdr entry) '(nil :json-false))
                      entry))
                  features))))

(defun chirp-x--operation-field-toggles (operation)
  "Return the optional field-toggle alist selected by OPERATION."
  (let ((field-toggles (plist-get operation :field-toggles)))
    (unless (or (null field-toggles) (listp field-toggles))
      (error "X GraphQL field toggles are invalid: %S" field-toggles))
    field-toggles))

(defun chirp-x--json-encode (value)
  "Encode VALUE as compact JSON."
  (let ((json-encoding-pretty-print nil))
    (json-encode value)))

(defun chirp-x--query-parameter (name value)
  "Return encoded URL query parameter NAME with JSON VALUE."
  (format "%s=%s"
          (url-hexify-string name)
          (url-hexify-string (chirp-x--json-encode value))))

(defun chirp-x--urlencode (parameters)
  "Encode string-keyed alist PARAMETERS for a query or form body."
  (unless (listp parameters)
    (error "X request parameters are invalid: %S" parameters))
  (mapconcat
   (lambda (entry)
     (unless (and (consp entry) (stringp (car entry)))
       (error "X request parameter is invalid: %S" entry))
     (format "%s=%s"
             (url-hexify-string (car entry))
             (url-hexify-string (format "%s" (or (cdr entry) "")))))
   parameters
   "&"))

(defun chirp-x--rest-url (service path query)
  "Return a trusted X REST URL for SERVICE, PATH, and QUERY parameters."
  (let ((base-url (cdr (assq service chirp-x--rest-base-urls))))
    (unless base-url
      (error "Unknown X API service: %S" service))
    (unless (and (chirp-x--nonblank-string path)
                 (not (string-prefix-p "/" path))
                 (not (string-match-p "[?#\r\n\\\\]" path))
                 (not (string-match-p
                       "\\(?:\\`\\|/\\)\\.\\.?\\(?:/\\|\\'\\)" path)))
      (error "X API path is invalid: %S" path))
    (concat base-url path
            (when query
              (concat "?" (chirp-x--urlencode query))))))

(defun chirp-x--trusted-url-p (url)
  "Return non-nil when URL belongs to an authenticated X API root."
  (or (string-prefix-p (concat chirp-x--api-base-url "/") url)
      (cl-some (lambda (entry)
                 (string-prefix-p (cdr entry) url))
               chirp-x--rest-base-urls)))

(defun chirp-x--graphql-url (query-id operation-name method variables features field-toggles)
  "Build the persisted GraphQL URL for QUERY-ID and OPERATION-NAME.

METHOD selects whether VARIABLES, FEATURES, and FIELD-TOGGLES belong in the
GET query string or in the POST body."
  (unless (string-match-p "\\`[[:alnum:]_-]+\\'" query-id)
    (error "X GraphQL query ID is invalid"))
  (unless (string-match-p "\\`[[:alnum:]_]+\\'" operation-name)
    (error "X GraphQL operation name is invalid"))
  (let ((url (format "%s/%s/%s"
                     chirp-x--api-base-url query-id operation-name)))
    (if (eq method 'get)
        (concat url "?"
                (mapconcat
                 #'identity
                 (append
                  (list (chirp-x--query-parameter "variables" variables))
                  (when features
                    (list (chirp-x--query-parameter "features" features)))
                  (when field-toggles
                    (list (chirp-x--query-parameter
                           "fieldToggles" field-toggles))))
                 "&"))
      url)))

(defun chirp-x--graphql-post-body (query-id variables features field-toggles)
  "Return a persisted GraphQL POST body for QUERY-ID and VARIABLES.

FEATURES and FIELD-TOGGLES are included when non-nil."
  (chirp-x--json-encode
   (append `(("variables" . ,variables)
             ("queryId" . ,query-id))
           (when features
             `(("features" . ,features)))
           (when field-toggles
             `(("fieldToggles" . ,field-toggles))))))

(defun chirp-x--ascii-header (name value)
  "Return an unibyte HTTP header cons from NAME and VALUE."
  (unless (and (stringp name) (stringp value)
               (not (string-match-p "[^[:ascii:]]\\|[[:cntrl:]]" name))
               (not (string-match-p "[^[:ascii:]]\\|[[:cntrl:]]" value)))
    (error "X request header is unsafe: %s" name))
  (cons (encode-coding-string name 'us-ascii)
        (encode-coding-string value 'us-ascii)))

(defun chirp-x--headers (credentials &optional content-type)
  "Return authenticated X headers from CREDENTIALS and CONTENT-TYPE."
  (mapcar
   (lambda (header)
     (chirp-x--ascii-header (car header) (cdr header)))
   (append
    `(("Authorization" . ,(concat "Bearer "
                                  (plist-get credentials :bearer-token)))
      ("Cookie" . ,(format "auth_token=%s; ct0=%s"
                           (plist-get credentials :auth-token)
                           (plist-get credentials :ct0)))
      ("X-Csrf-Token" . ,(plist-get credentials :ct0))
      ("X-Twitter-Active-User" . "yes")
      ("X-Twitter-Auth-Type" . "OAuth2Session")
      ("X-Twitter-Client-Language" . "en")
      ("Origin" . "https://x.com")
      ("Referer" . "https://x.com/")
      ("User-Agent" . ,chirp-x-user-agent)
      ("Accept" . "*/*"))
    (when content-type
      `(("Content-Type" . ,content-type))))))

(defun chirp-x--response-body (limit)
  "Return the current HTTP response body when no larger than LIMIT bytes."
  (let* ((header-end (and (boundp 'url-http-end-of-headers)
                          url-http-end-of-headers))
         (start
          (cond
           ((markerp header-end) (marker-position header-end))
           ((integerp header-end) header-end)
           (t (point-min))))
         (end (point-max))
         (bytes (- (position-bytes end) (position-bytes start))))
    (when (> bytes limit)
      (error "X response body exceeds %d bytes" limit))
    (buffer-substring-no-properties start end)))

(defun chirp-x--response-status (request-status)
  "Return HTTP status from REQUEST-STATUS in the current retrieval buffer."
  (or (and (boundp 'url-http-response-status)
           (integerp url-http-response-status)
           url-http-response-status)
      (let ((error-data (plist-get request-status :error)))
        (and (listp error-data)
             (eq (nth 1 error-data) 'http)
             (integerp (nth 2 error-data))
             (nth 2 error-data)))))

(defun chirp-x--query-id-table (body)
  "Parse bounded registry BODY into a validated read-operation ID table."
  (unless (and (stringp body)
               (<= (string-bytes body) chirp-x--query-id-source-limit))
    (error "X query ID registry is too large"))
  (let* ((payload
          (json-parse-string body
                             :object-type 'alist
                             :array-type 'list
                             :null-object nil
                             :false-object nil))
         (operations (chirp-get payload "graphql"))
         (table (make-hash-table :test #'equal))
         (count 0))
    (unless (and (chirp-object-p operations)
                 (<= (length operations) 1000))
      (error "X query ID registry has an invalid shape"))
    (dolist (entry operations)
      (let* ((raw-name (car entry))
             (operation-name
              (if (symbolp raw-name) (symbol-name raw-name) raw-name))
             (operation (cdr entry))
             (method (chirp-get operation "method"))
             (query-id (chirp-get operation "queryId")))
        (when (equal method "GET")
          (unless (and (stringp operation-name)
                       (string-match-p "\\`[[:alnum:]_]+\\'" operation-name)
                       (chirp-x--nonblank-string query-id)
                       (string-match-p
                        "\\`[[:alnum:]_-]\\{22\\}\\'" query-id))
            (error "X query ID registry contains an invalid read operation"))
          (puthash operation-name query-id table)
          (setq count (1+ count)))))
    (when (zerop count)
      (error "X query ID registry contains no read operations"))
    table))

(defun chirp-x--finish-query-id-refresh (result)
  "Deliver query ID refresh RESULT to all waiting listeners."
  (let ((listeners (prog1 (nreverse chirp-x--query-id-refresh-listeners)
                     (setq chirp-x--query-id-refresh-listeners nil)))
        first-error)
    (dolist (listener listeners)
      (condition-case err
          (funcall listener result)
        (error
         (unless first-error
           (setq first-error err)))))
    (when first-error
      (signal (car first-error) (cdr first-error)))))

(defun chirp-x--query-id-refresh-error (failure)
  "Return a readable registry refresh message for plz FAILURE."
  (let ((response (plz-error-response failure))
        (curl-error (plz-error-curl-error failure)))
    (format "X query ID registry refresh failed: %s"
            (or (plz-error-message failure)
                (and response
                     (format "HTTP %s" (plz-response-status response)))
                (cdr curl-error)
                "unknown error"))))

(defun chirp-x--cancel-query-id-refresh (process)
  "Cancel public query ID registry retrieval PROCESS."
  (when (eq chirp-x--query-id-refresh-process process)
    (setq chirp-x--query-id-refresh-process nil
          chirp-x--query-id-refresh-listeners nil))
  (when (process-live-p process)
    (delete-process process)))

(defun chirp-x--refresh-query-ids (listener)
  "Refresh public read query IDs and notify LISTENER unless canceled."
  (require 'plz)
  (when listener
    (push listener chirp-x--query-id-refresh-listeners))
  (if (processp chirp-x--query-id-refresh-process)
      chirp-x--query-id-refresh-process
    (condition-case err
        (let ((plz-curl-default-args
               (list "--disable" "--silent"
                     "--proto" "=https"
                     "--max-filesize"
                     (number-to-string chirp-x--query-id-source-limit)
                     "--max-redirs" "0"))
              handle
              process)
          (cl-labels
              ((active-p ()
                 (or (null handle)
                     (appkit-handle-alive-p handle)))
               (finish
                 (result)
                 (when (active-p)
                   (when (appkit-handle-p handle)
                     (appkit-retire-handle handle))
                   (when (eq chirp-x--query-id-refresh-process process)
                     (setq chirp-x--query-id-refresh-process nil))
                   (chirp-x--finish-query-id-refresh result))))
            (setq process
                  (plz 'get chirp-x--query-id-source-url
                    :headers '(("Accept" . "application/json"))
                    :as 'string
                    :then
                    (lambda (body)
                      (when (active-p)
                        (let ((result
                               (condition-case response-error
                                   (let ((table
                                          (chirp-x--query-id-table body)))
                                     (setq chirp-x--query-id-cache table)
                                     (list :count (hash-table-count table)))
                                 (error
                                  (list :error
                                        (error-message-string
                                         response-error))))))
                          (finish result))))
                    :else
                    (lambda (failure)
                      (when (active-p)
                        (finish
                         (list :error
                               (chirp-x--query-id-refresh-error failure)))))
                    :timeout 60
                    :noquery t))
            (unless (processp process)
              (error "Plz did not start the X query ID registry request"))
            (setq chirp-x--query-id-refresh-process process
                  handle
                  (appkit-register-handle
                   (chirp-app) 'function process
                   #'chirp-x--cancel-query-id-refresh))
            process))
      (error
       (chirp-x--finish-query-id-refresh
        (list :error (error-message-string err)))
       nil))))

;;;###autoload
(defun chirp-refresh-query-ids ()
  "Refresh cached query IDs for X GraphQL read operations."
  (interactive)
  (message "Refreshing X query IDs...")
  (chirp-x--refresh-query-ids
   (lambda (result)
     (if-let* ((error-message (plist-get result :error)))
         (display-warning 'chirp error-message :error)
       (message "Refreshed %d X query IDs" (plist-get result :count))))))

(defun chirp-x--stale-query-error-p (message)
  "Return non-nil when MESSAGE definitively reports a stale query ID."
  (and (stringp message)
       (string-match-p
        (concat "\\(?:query: unspecified"
                "\\|persistedquerynotfound"
                "\\|persisted query not found"
                "\\|query not found\\)")
        (downcase message))))

(defun chirp-x--graphql-errors (payload)
  "Return top-level or mutation-nested GraphQL errors from PAYLOAD."
  (or (chirp-get payload "errors")
      (when-let* ((data (chirp-get payload "data")))
        (cl-loop for (_key . value) in data
                 for errors = (chirp-get value "errors")
                 when (consp errors)
                 return errors))))

(defun chirp-x--graphql-error-message (payload)
  "Return a human-readable GraphQL error from PAYLOAD, or nil."
  (when-let* ((errors (chirp-x--graphql-errors payload)))
    (let* ((first-error
            (or (cl-find-if
                 (lambda (item)
                   (chirp-x--nonblank-string (chirp-get item "message")))
                 errors)
                (car errors)))
           (message
            (or (chirp-x--nonblank-string
                 (chirp-get first-error "message"))
                "GraphQL returned an error")))
      (if-let* ((code (chirp-get first-error "code")))
          (if (string-match-p
               (format "(%s)\\'" (regexp-quote (format "%s" code)))
               message)
              message
            (format "%s (%s)" message code))
        message))))

(defun chirp-x--failure-message (http-status payload request-status)
  "Return a readable error from HTTP-STATUS, PAYLOAD, and REQUEST-STATUS."
  (let ((graphql-error (chirp-x--graphql-error-message payload)))
    (cond
     (http-status
      (format "X request failed (HTTP %d)%s"
              http-status
              (if graphql-error
                  (format ": %s" graphql-error)
                "")))
     ((plist-get request-status :error)
      (format "X request failed: %s"
              (error-message-string (plist-get request-status :error))))
     (graphql-error
      (format "X request failed: %s" graphql-error))
     (t "X request failed without an HTTP response"))))

(defun chirp-x-unknown-write-outcome (message)
  "Mark write failure MESSAGE as having an unknown remote outcome."
  (concat
   "X write outcome is unknown; the request may have succeeded. "
   "Check X before trying again. " message))

(defun chirp-x--decode-response (request-status &optional allow-empty method)
  "Decode the current X response buffer for REQUEST-STATUS.

When ALLOW-EMPTY is non-nil, accept an empty successful response as an empty
object.  METHOD identifies writes whose transport outcome can be uncertain.
Return either `(:success PAYLOAD)' or `(:error MESSAGE)'."
  (let* ((http-status (chirp-x--response-status request-status))
         (body (chirp-x--response-body
                (if (eq method 'post)
                    chirp-x--write-response-limit
                  chirp-x--read-response-limit)))
         (empty-p (string-empty-p (string-trim body)))
         (payload
          (unless empty-p
            (condition-case nil
                (json-parse-string body
                                   :object-type 'alist
                                   :array-type 'list
                                   :null-object nil
                                   :false-object nil)
              (error nil)))))
    (cond
     ((or (not http-status)
          (< http-status 200)
          (>= http-status 300))
      (let ((message
             (chirp-x--failure-message
              http-status payload request-status)))
        (list :error
              (if (eq method 'post)
                  (chirp-x-unknown-write-outcome message)
                message))))
     ((and empty-p allow-empty)
      (list :success (make-hash-table :test #'equal)))
     ((not payload)
      (list :error
            (if (eq method 'post)
                (chirp-x-unknown-write-outcome "X returned invalid JSON")
              "X returned invalid JSON")))
     ((chirp-x--graphql-error-message payload)
      (let ((message (chirp-x--failure-message nil payload request-status)))
        (list :error
              (if (eq method 'post)
                  (chirp-x-unknown-write-outcome message)
                message))))
     (t (list :success payload)))))

(defvar-local chirp-x--request-handle nil
  "Appkit lifecycle handle for the current X retrieval buffer.")

(defvar chirp-x--dispatch-buffer nil
  "Dynamically bound retrieval buffer allocated before X write dispatch.")

(defvar chirp-x--dispatch-attempted-p nil
  "Dynamically bound non-nil once the current X request may be dispatched.")

(defun chirp-x--retire-request-handle ()
  "Retire the current X retrieval buffer's lifecycle handle."
  (when (appkit-handle-p chirp-x--request-handle)
    (appkit-retire-handle chirp-x--request-handle))
  (setq-local chirp-x--request-handle nil))

(defun chirp-x--discard-request-buffer (buffer)
  "Stop and kill X retrieval BUFFER without delivering a callback."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq-local chirp-x--request-handle nil)
      (when-let* ((process (get-buffer-process buffer))
                  ((eq (process-buffer process) buffer)))
        (set-process-sentinel process nil)
        (when (process-live-p process)
          (delete-process process)))
      (kill-buffer buffer))))

(defun chirp-x--cancel-request (request)
  "Cancel the X retrieval described by REQUEST and settle its error callback."
  (let ((buffer (plist-get request :buffer))
        (method (plist-get request :method))
        (owner (plist-get request :owner))
        (settle-on-cancel (plist-get request :settle-on-cancel))
        (cancel-message (plist-get request :cancel-message))
        (error-fn (plist-get request :errback)))
    (chirp-x--discard-request-buffer buffer)
    (when (and (functionp error-fn)
               (or settle-on-cancel
                   (eq method 'post)
                   (appkit-view-p owner)))
      (funcall error-fn
               (or cancel-message
                   (if (eq method 'post)
                       (chirp-x-unknown-write-outcome
                        "X request was canceled before a response was received")
                     "X request was canceled"))))))

(defun chirp-x-cancel-request (request)
  "Cancel active X retrieval buffer REQUEST exactly once."
  (when (buffer-live-p request)
    (with-current-buffer request
      (if (and (appkit-handle-p chirp-x--request-handle)
               (appkit-handle-alive-p chirp-x--request-handle))
          (appkit-cancel-handle chirp-x--request-handle)
        (kill-buffer request)))
    t))

(defun chirp-x--safe-error-message (error-data credentials)
  "Return ERROR-DATA's message with CREDENTIALS redacted."
  (let ((message (error-message-string error-data))
        (values
         (and credentials
              (list (plist-get credentials :auth-token)
                    (plist-get credentials :ct0)
                    (plist-get credentials :bearer-token)))))
    (dolist (value
             (sort (delete-dups values)
                   (lambda (left right)
                     (> (length left) (length right)))))
      (when (and (stringp value) (not (string-empty-p value)))
        (setq message (string-replace value "[REDACTED]" message))))
    message))

(defun chirp-x--resignal-callback-error (wrapped-error)
  "Resignal the original callback error inside WRAPPED-ERROR."
  (let ((cause (cadr wrapped-error)))
    (unless (and (consp cause) (symbolp (car cause)))
      (error "X callback error lost its original condition"))
    (signal (car cause) (cdr cause))))

(defun chirp-x--url-post-once
    (request-url callback callback-args silent inhibit-cookies)
  "Start one non-retrying POST to REQUEST-URL in a preallocated buffer.

CALLBACK, CALLBACK-ARGS, SILENT, and INHIBIT-COOKIES follow `url-retrieve'."
  (url-do-setup)
  (let* ((url (url-generic-parse-url
               (url-encode-url (copy-sequence request-url))))
         (proxy
          (and (url-host url)
               (url-find-proxy-for-url url (url-host url))))
         (proxy-url (and proxy (url-generic-parse-url proxy))))
    (when (and proxy-url (not (equal (url-type proxy-url) "http")))
      (error "X request proxy scheme is unsupported"))
    (setf (url-silent url) silent
          (url-asynchronous url) url-asynchronous
          (url-use-cookies url) (not inhibit-cookies))
    (setq chirp-x--dispatch-buffer
          (generate-new-buffer " *chirp X write*")
          chirp-x--dispatch-attempted-p t)
    (let* ((url-current-object url)
           (url-using-proxy proxy-url)
           (started
            (url-http url callback (cons nil callback-args)
                      chirp-x--dispatch-buffer
                      (and (null proxy-url) 'tls))))
      (unless (eq started chirp-x--dispatch-buffer)
        (chirp-x--discard-request-buffer started)
        (error "URL transport did not retain the preallocated X write buffer"))
      started)))

(defun chirp-x--retrieve
    (request-url callback callback-args silent inhibit-cookies)
  "Retrieve REQUEST-URL once and invoke CALLBACK with CALLBACK-ARGS.

SILENT and INHIBIT-COOKIES follow `url-retrieve'.  POST requests use a
preallocated `url-http' buffer so url.el cannot replay or orphan the write."
  (if (equal url-request-method "POST")
      (chirp-x--url-post-once
       request-url callback callback-args silent inhibit-cookies)
    (setq chirp-x--dispatch-attempted-p t)
    (url-retrieve request-url callback callback-args
                  silent inhibit-cookies)))

(cl-defun chirp-x--request
    (request-url method callback
                 &key data content-type errback allow-empty owner
                 settle-on-cancel cancel-message)
  "Request trusted REQUEST-URL with METHOD and call CALLBACK.

DATA is encoded as UTF-8 when needed.  CONTENT-TYPE adds its corresponding
header.  When ALLOW-EMPTY is non-nil, a successful empty body reaches CALLBACK
as an empty object.  ERRBACK receives setup, transport, HTTP, or response
errors.  OWNER is the Appkit app or view whose lifecycle owns the retrieval.
SETTLE-ON-CANCEL asks cancellation to call ERRBACK even for an app-owned read;
CANCEL-MESSAGE overrides that cancellation error."
  (unless (functionp callback)
    (error "X request callback is not callable"))
  (let ((error-fn (or errback (lambda (message) (message "%s" message))))
        (chirp-x--dispatch-buffer nil)
        (chirp-x--dispatch-attempted-p nil)
        credentials request-buffer handle settled-p)
    (unless (functionp error-fn)
      (error "X request error callback is not callable"))
    (condition-case err
        (progn
          (unless (and (stringp request-url)
                       (chirp-x--trusted-url-p request-url))
            (error "X request URL is not trusted: %S" request-url))
          (unless (memq method '(get post))
            (error "X request method is invalid: %S" method))
          (setq credentials (chirp-x-credentials))
          (let* ((encoded-url (encode-coding-string request-url 'us-ascii))
                 (encoded-data
                  (and data
                       (if (multibyte-string-p data)
                           (encode-coding-string data 'utf-8)
                         data)))
                 ;; The explicit Cookie header must never be replayed to a
                 ;; redirect target.  Keep the redirect limit buffer-local
                 ;; because redirect handling is asynchronous.
                 (url-max-redirections 0)
                 ;; A POST asks the server to close its connection and uses a
                 ;; preallocated no-retry buffer in `chirp-x--retrieve'.
                 (url-http-attempt-keepalives
                  (and url-http-attempt-keepalives
                       (not (eq method 'post))))
                 (url-request-method
                  (encode-coding-string
                   (upcase (symbol-name method)) 'us-ascii))
                 (url-request-data encoded-data)
                 (url-request-extra-headers
                  (chirp-x--headers credentials content-type)))
            (let ((inhibit-quit t))
              (setq chirp-x--dispatch-attempted-p t
                    request-buffer
                    (chirp-x--retrieve
                     encoded-url
                     (lambda (request-status)
                       (let ((buffer (current-buffer))
                             (deliver-p
                              (or (null handle)
                                  (appkit-handle-alive-p handle))))
                         (unwind-protect
                             (progn
                               (when (appkit-handle-p handle)
                                 (appkit-retire-handle handle)
                                 (setq-local chirp-x--request-handle nil))
                               (when deliver-p
                                 (let ((result
                                        (condition-case response-error
                                            (chirp-x--decode-response
                                             request-status allow-empty method)
                                          (error
                                           (let ((message
                                                  (format
                                                   (concat
                                                    "X response processing "
                                                    "failed: %s")
                                                   (error-message-string
                                                    response-error))))
                                             (list
                                              :error
                                              (if (eq method 'post)
                                                  (chirp-x-unknown-write-outcome
                                                   message)
                                                message)))))))
                                   (setq settled-p t)
                                   (if-let* ((payload
                                              (plist-get result :success)))
                                       (funcall callback payload)
                                     (funcall error-fn
                                              (plist-get result :error))))))
                           (when (buffer-live-p buffer)
                             (kill-buffer buffer)))))
                     nil t t))
              (cond
               ((not request-buffer)
                (setq settled-p t)
                (funcall error-fn "X did not start the HTTP request")
                nil)
               ((not (buffer-live-p request-buffer))
                request-buffer)
               (t
                (let ((request-owner (or owner (chirp-app))))
                  (setq handle
                        (appkit-register-handle
                         request-owner
                         'function
                         (list :buffer request-buffer
                               :method method
                               :owner request-owner
                               :settle-on-cancel settle-on-cancel
                               :cancel-message cancel-message
                               :errback error-fn)
                         #'chirp-x--cancel-request)))
                (with-current-buffer request-buffer
                  (setq-local url-max-redirections 0)
                  (setq-local chirp-x--request-handle handle)
                  (add-hook 'kill-buffer-hook
                            #'chirp-x--retire-request-handle nil t))
                request-buffer)))))
      ((error quit)
       (let ((quit-p (eq (car err) 'quit))
             (inhibit-quit t))
         (when (appkit-handle-p handle)
           (appkit-retire-handle handle))
         (chirp-x--discard-request-buffer
          (or request-buffer chirp-x--dispatch-buffer))
         (if settled-p
             (signal 'chirp-x--callback-error (list err))
           (let* ((message (chirp-x--safe-error-message err credentials))
                  (failure
                   (if (and chirp-x--dispatch-attempted-p
                            (eq method 'post))
                       (chirp-x-unknown-write-outcome message)
                     message)))
             (setq settled-p t)
             (if quit-p
                 (unwind-protect
                     (funcall error-fn failure)
                   (signal 'quit nil))
               (condition-case callback-error
                   (funcall error-fn failure)
                 ((error quit)
                  (signal 'chirp-x--callback-error
                          (list callback-error)))))))
         nil)))))

(cl-defun chirp-x-api-request
    (service path callback &key (method 'get) query form errback owner)
  "Request an authenticated X API PATH from SERVICE asynchronously.

SERVICE is `web' for x.com/i/api or `legacy' for api.x.com/1.1.  METHOD may be
`get' or `post'.  QUERY and FORM are string-keyed alists; FORM is valid only
for POST requests.  CALLBACK receives decoded JSON, and ERRBACK receives one
readable error string.  OWNER optionally owns the transport lifecycle."
  (unless (functionp callback)
    (error "X API callback is not callable"))
  (let ((error-fn (or errback (lambda (message) (message "%s" message)))))
    (unless (functionp error-fn)
      (error "X API error callback is not callable"))
    (condition-case err
        (progn
          (unless (memq method '(get post))
            (error "X API method is invalid: %S" method))
          (when (and form (not (eq method 'post)))
            (error "X API form data requires POST"))
          (let ((request-url (chirp-x--rest-url service path query)))
            (chirp-x--request
             request-url method callback
             :data (and form (chirp-x--urlencode form))
             :content-type (and form "application/x-www-form-urlencoded")
             :errback error-fn
             :owner owner)))
      (chirp-x--callback-error
       (chirp-x--resignal-callback-error err))
      (error
       (funcall error-fn (error-message-string err))
       nil))))

(cl-defun chirp-x--graphql-request-attempt
    (operation variables callback &key errback owner retried-p)
  "Issue one persisted GraphQL attempt for OPERATION.

VARIABLES are sent to X.  CALLBACK receives a decoded response.  ERRBACK
receives failures.  OWNER optionally owns the transport lifecycle.  When
RETRIED-P is non-nil, do not refresh or retry again after another stale-query
failure."
  (let ((error-fn (or errback (lambda (message) (message "%s" message)))))
    (condition-case err
        (let* ((variables (or variables (make-hash-table :test #'equal)))
               (operation-name
                (chirp-x--operation-string operation :name "operation name"))
               (method (chirp-x--operation-method operation))
               (override-p
                (assoc-string operation-name chirp-x-query-id-overrides t))
               (query-id
                (chirp-x--operation-query-id operation operation-name))
               (features (chirp-x--operation-features operation))
               (field-toggles
                (chirp-x--operation-field-toggles operation))
               (request-url
                (chirp-x--graphql-url query-id operation-name method variables
                                      features field-toggles))
               (request-error
                (lambda (message)
                  (let ((stale-p
                         (and (eq method 'get)
                              (not override-p)
                              (chirp-x--stale-query-error-p message))))
                    (cond
                     ((and stale-p (not retried-p))
                      (chirp-x--refresh-query-ids
                       (lambda (result)
                         (if-let* ((refresh-error (plist-get result :error)))
                             (funcall
                              error-fn
                              (format "%s. Query ID refresh failed: %s"
                                      message refresh-error))
                           (let ((new-query-id
                                  (chirp-x--operation-query-id
                                   operation operation-name)))
                             (if (equal query-id new-query-id)
                                 (funcall
                                  error-fn
                                  (format
                                   "%s. Refreshed query IDs did not change %s"
                                   message operation-name))
                               (chirp-x--graphql-request-attempt
                                operation variables callback
                                :errback error-fn
                                :owner owner
                                :retried-p t)))))))
                     (stale-p
                      (funcall
                       error-fn
                       (concat message
                               ". Query ID remained invalid after one "
                               "refresh retry")))
                     (t
                      (funcall error-fn message)))))))
          (chirp-x--request
           request-url method callback
           :data (and (eq method 'post)
                      (chirp-x--graphql-post-body
                       query-id variables features field-toggles))
           :content-type (and (eq method 'post) "application/json")
           :errback request-error
           :owner owner))
      (chirp-x--callback-error
       (chirp-x--resignal-callback-error err))
      (error
       (funcall error-fn (error-message-string err))
       nil))))

(cl-defun chirp-x-graphql-request
    (operation variables callback &key errback owner)
  "Request persisted X GraphQL OPERATION with VARIABLES asynchronously.

OPERATION is a plist containing `:query-id', `:name', and optional `:method',
`:features', and `:field-toggles'.  Nil VARIABLES represents an empty object.
CALLBACK receives decoded JSON as an alist.  ERRBACK receives one readable
error string.  A definitive stale read refreshes the public query-ID registry
and retries the read once with the refreshed ID; writes and explicit overrides
are never retried.  OWNER optionally owns the transport lifecycle.  Return the
URL retrieval buffer when the request starts, or nil when setup fails."
  (unless (functionp callback)
    (error "X GraphQL callback is not callable"))
  (let ((error-fn (or errback (lambda (message) (message "%s" message)))))
    (unless (functionp error-fn)
      (error "X GraphQL error callback is not callable"))
    (chirp-x--graphql-request-attempt
     operation variables callback :errback error-fn :owner owner)))

(defun chirp-x--upload-media-type (file)
  "Return the supported X media MIME type for FILE."
  (pcase (downcase (or (file-name-extension file) ""))
    ((or "jpg" "jpeg") "image/jpeg")
    ("png" "image/png")
    ("gif" "image/gif")
    ("webp" "image/webp")
    ("mp4" "video/mp4")
    (_ (error "Unsupported media format: %s" file))))

(defun chirp-x--upload-media-category (media-type)
  "Return the X media_category for MEDIA-TYPE, or nil."
  (pcase media-type
    ("image/gif" "tweet_gif")
    ("video/mp4" "tweet_video")))

(defun chirp-x--upload-size-limit (media-type)
  "Return the maximum accepted byte size for MEDIA-TYPE."
  (pcase media-type
    ("image/gif" chirp-x--upload-gif-limit)
    ("video/mp4" chirp-x--upload-video-limit)
    (_ chirp-x--upload-image-limit)))

(defun chirp-x--read-file-range (file start length)
  "Return LENGTH bytes of FILE beginning at START."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file nil start (+ start length))
    (buffer-string)))

(defun chirp-x--upload-segment-plan (file-size media-type)
  "Return (START . LENGTH) segments for FILE-SIZE and MEDIA-TYPE."
  (let ((chunked (or (equal media-type "image/gif")
                     (equal media-type "video/mp4"))))
    (if (and chunked (> file-size chirp-x--upload-chunk-size))
        (cl-loop for start from 0 below file-size
                 by chirp-x--upload-chunk-size
                 collect (cons start
                               (min chirp-x--upload-chunk-size
                                    (- file-size start))))
      (list (cons 0 file-size)))))

(defun chirp-x--multipart-data (media-id segment-index bytes)
  "Return (BODY . CONTENT-TYPE) for an APPEND of BYTES.

MEDIA-ID identifies the upload and SEGMENT-INDEX is its zero-based part."
  (let ((boundary
         (format "----------------chirp-%x-%x"
                 (random most-positive-fixnum)
                 (random most-positive-fixnum))))
    (while (string-match-p (regexp-quote boundary) bytes)
      (setq boundary
            (format "----------------chirp-%x-%x"
                    (random most-positive-fixnum)
                    (random most-positive-fixnum))))
    (let ((body
           (concat
            (encode-coding-string
             (format
              (concat "--%s\r\n"
                      "Content-Disposition: form-data; name=\"command\"\r\n\r\n"
                      "APPEND\r\n"
                      "--%s\r\n"
                      "Content-Disposition: form-data; name=\"media_id\"\r\n\r\n"
                      "%s\r\n"
                      "--%s\r\n"
                      "Content-Disposition: form-data; name=\"segment_index\"\r\n\r\n"
                      "%d\r\n"
                      "--%s\r\n"
                      "Content-Disposition: form-data; name=\"media\"; "
                      "filename=\"media\"\r\n"
                      "Content-Type: application/octet-stream\r\n\r\n")
              boundary boundary media-id boundary segment-index boundary)
             'us-ascii)
            bytes
            (encode-coding-string
             (format "\r\n--%s--\r\n" boundary) 'us-ascii))))
      (cons body (format "multipart/form-data; boundary=%s" boundary)))))

(defun chirp-x--upload-media-id (payload)
  "Return the media identifier from INIT PAYLOAD, or nil."
  (let ((identifier (or (chirp-get payload "media_id_string")
                        (chirp-get payload "media_id"))))
    (and identifier (format "%s" identifier))))

(defun chirp-x--upload-processing-error (processing-info)
  "Return a readable media processing error from PROCESSING-INFO."
  (let* ((remote-error (chirp-get processing-info "error"))
         (message (or (chirp-get remote-error "message")
                      (chirp-get remote-error "name")
                      "X could not process the uploaded media"))
         (code (chirp-get remote-error "code")))
    (if code
        (format "%s (%s)" message code)
      message)))

(defun chirp-x--owner-live-p (owner)
  "Return non-nil when Appkit OWNER is live."
  (cond
   ((appkit-app-p owner) (appkit-app-live-p owner))
   ((appkit-view-p owner) (appkit-view-live-p owner))))

(defun chirp-x--cancel-upload-poll (poll)
  "Cancel the upload timer described by POLL and settle its workflow."
  (when-let* ((timer (plist-get poll :timer))
              ((timerp timer)))
    (cancel-timer timer))
  (when-let* ((cancel (plist-get poll :cancel))
              ((functionp cancel)))
    (funcall cancel)))

(defun chirp-x--notify-upload-progress (progress event)
  "Call PROGRESS with EVENT when PROGRESS is callable."
  (when (functionp progress)
    (funcall progress event)))

(cl-defun chirp-x-upload-media (file callback &key errback owner progress)
  "Upload media FILE to X and call CALLBACK with its media ID.

JPEG, PNG, and WebP files may be at most 5 MiB; GIF files may be at most 15
MiB; MP4 files may be at most 512 MiB.  Videos use `tweet_video' and are
uploaded in 1 MiB chunks without loading the whole file.  ERRBACK receives
setup, upload, cancellation, or asynchronous processing failures.  OWNER
optionally owns the complete upload lifecycle.  PROGRESS, when callable,
receives plists with `:phase', optional `:media-type', optional `:index'
and `:count' for APPEND, and optional `:progress' as a 0-1 float.  No
upload mutation is retried automatically."
  (unless (functionp callback)
    (error "X media upload callback is not callable"))
  (when (and progress (not (functionp progress)))
    (error "X media upload progress callback is not callable"))
  (let ((error-fn (or errback (lambda (message) (message "%s" message)))))
    (unless (functionp error-fn)
      (error "X media upload error callback is not callable"))
    (condition-case err
        (progn
          (unless (and (stringp file) (file-regular-p file)
                       (file-readable-p file))
            (error "Media file is not readable: %s" file))
          (let* ((media-type (chirp-x--upload-media-type file))
                 (file-size (file-attribute-size (file-attributes file)))
                 (size-limit (chirp-x--upload-size-limit media-type))
                 (segments (chirp-x--upload-segment-plan file-size media-type))
                 (segment-count (length segments))
                 (status-limit (if (equal media-type "video/mp4")
                                   chirp-x--upload-video-status-limit
                                 chirp-x--upload-status-limit))
                 (upload-url
                  (chirp-x--rest-url 'upload "upload.json" nil)))
            (unless (> file-size 0)
              (error "Media file is empty: %s" file))
            (when (> file-size size-limit)
              (error "Media file exceeds the %d MiB limit: %s"
                     (/ size-limit 1024 1024) file))
            (let* ((upload-owner (or owner (chirp-app)))
                   (cancel-message
                    (chirp-x-unknown-write-outcome
                     "X media upload was canceled before completion"))
                   settled-p
                   workflow-handle)
              (cl-labels
                  ((notify
                     (phase &optional extra)
                     (chirp-x--notify-upload-progress
                      progress
                      (append (list :phase phase :media-type media-type)
                              extra)))
                   (retire-workflow
                     ()
                     (when (and (appkit-handle-p workflow-handle)
                                (appkit-handle-alive-p workflow-handle))
                       (appkit-retire-handle workflow-handle)
                       (setq workflow-handle nil)))
                   (fail
                     (message)
                     (unless settled-p
                       (setq settled-p t)
                       (retire-workflow)
                       (funcall error-fn message)))
                   (succeed
                     (media-id)
                     (unless settled-p
                       (setq settled-p t)
                       (retire-workflow)
                       (funcall callback media-id)))
                   (ensure-active
                     ()
                     (cond
                      (settled-p nil)
                      ((chirp-x--owner-live-p upload-owner) t)
                      (t
                       (fail cancel-message)
                       nil)))
                   (check-status
                     (media-id attempt)
                     (when (ensure-active)
                       (if (>= attempt status-limit)
                           (fail "X media processing did not finish in time")
                         (notify 'status (list :progress 1.0))
                         (chirp-x--request
                          (chirp-x--rest-url
                           'upload "upload.json"
                           `(("command" . "STATUS")
                             ("media_id" . ,media-id)))
                          'get
                          (lambda (payload)
                            (handle-processing media-id payload (1+ attempt)))
                          :errback #'fail
                          :owner upload-owner
                          :settle-on-cancel t
                          :cancel-message cancel-message))))
                   (schedule-status
                     (media-id attempt delay)
                     (when (ensure-active)
                       (let (fired-p handle timer)
                         (setq timer
                               (run-at-time
                                delay nil
                                (lambda ()
                                  (setq fired-p t)
                                  (when (appkit-handle-p handle)
                                    (appkit-retire-handle handle))
                                  (unless settled-p
                                    (if (chirp-x--owner-live-p upload-owner)
                                        (check-status media-id attempt)
                                      (fail cancel-message))))))
                         (unless fired-p
                           (setq handle
                                 (appkit-register-handle
                                  upload-owner
                                  'function
                                  (list :timer timer
                                        :cancel (lambda ()
                                                  (fail cancel-message)))
                                  #'chirp-x--cancel-upload-poll))))))
                   (handle-processing
                     (media-id payload attempt)
                     (when (ensure-active)
                       (if-let* ((processing-info
                                  (chirp-get payload "processing_info")))
                           (pcase (chirp-get processing-info "state")
                             ("succeeded" (succeed media-id))
                             ("failed"
                              (fail
                               (chirp-x--upload-processing-error
                                processing-info)))
                             ((or "pending" "in_progress")
                              (let ((delay
                                     (chirp-get processing-info
                                                "check_after_secs")))
                                (schedule-status
                                 media-id attempt
                                 (if (numberp delay)
                                     (min 30 (max 0.1 delay))
                                   1))))
                             (_ (fail
                                 "X returned an invalid media processing state")))
                         (succeed media-id))))
                   (finalize
                     (media-id)
                     (when (ensure-active)
                       (notify 'finalize (list :progress 1.0))
                       (chirp-x--request
                        upload-url 'post
                        (lambda (payload)
                          (handle-processing media-id payload 0))
                        :data (chirp-x--urlencode
                               `(("command" . "FINALIZE")
                                 ("media_id" . ,media-id)))
                        :content-type "application/x-www-form-urlencoded"
                        :errback #'fail
                        :owner upload-owner
                        :settle-on-cancel t
                        :cancel-message cancel-message)))
                   (append-segment
                     (media-id remaining segment-index)
                     (when (ensure-active)
                       (if (null remaining)
                           (finalize media-id)
                         (notify
                          'append
                          (list :index (1+ segment-index)
                                :count segment-count
                                :progress
                                (/ (float segment-index)
                                   (max 1 segment-count))))
                         (pcase-let* ((`(,start . ,length) (car remaining))
                                      (`(,body . ,content-type)
                                       (chirp-x--multipart-data
                                        media-id segment-index
                                        (chirp-x--read-file-range
                                         file start length))))
                           (chirp-x--request
                            upload-url 'post
                            (lambda (_payload)
                              (append-segment media-id (cdr remaining)
                                              (1+ segment-index)))
                            :data body
                            :content-type content-type
                            :allow-empty t
                            :errback #'fail
                            :owner upload-owner
                            :settle-on-cancel t
                            :cancel-message cancel-message))))))
                (setq workflow-handle
                      (appkit-register-handle
                       upload-owner
                       'function
                       (lambda () (fail cancel-message))))
                (notify 'init (list :progress 0.0))
                (chirp-x--request
                 upload-url 'post
                 (lambda (payload)
                   (if-let* ((media-id (chirp-x--upload-media-id payload)))
                       (append-segment media-id segments 0)
                     (fail "X media INIT did not return a media ID")))
                 :data (chirp-x--urlencode
                        (append
                         `(("command" . "INIT")
                           ("total_bytes" . ,(number-to-string file-size))
                           ("media_type" . ,media-type))
                         (when-let* ((category
                                      (chirp-x--upload-media-category
                                       media-type)))
                           `(("media_category" . ,category)))))
                 :content-type "application/x-www-form-urlencoded"
                 :errback #'fail
                 :owner upload-owner
                 :settle-on-cancel t
                 :cancel-message cancel-message)))))
      (chirp-x--callback-error
       (chirp-x--resignal-callback-error err))
      (error
       (funcall error-fn (error-message-string err))
       nil))))

(cl-defun chirp-x-upload-media-alt-text
    (media-id text callback &key errback owner)
  "Attach alt TEXT to uploaded MEDIA-ID and call CALLBACK.

TEXT may contain at most `chirp-x--media-alt-text-limit' characters.
ERRBACK receives setup or write failures.  OWNER optionally owns the
request.  The metadata write is never retried automatically."
  (unless (functionp callback)
    (error "X media alt-text callback is not callable"))
  (let ((error-fn (or errback (lambda (message) (message "%s" message)))))
    (unless (functionp error-fn)
      (error "X media alt-text error callback is not callable"))
    (condition-case err
        (progn
          (unless (and (stringp media-id)
                       (not (string-empty-p media-id)))
            (error "Media ID is invalid"))
          (unless (and (stringp text)
                       (not (string-empty-p (string-trim text))))
            (error "Alt text cannot be empty"))
          (when (> (length text) chirp-x--media-alt-text-limit)
            (error "Alt text cannot exceed %d characters"
                   chirp-x--media-alt-text-limit))
          (chirp-x--request
           (chirp-x--rest-url 'upload-legacy "media/metadata/create.json" nil)
           'post
           callback
           :data (chirp-x--json-encode
                  `(("media_id" . ,media-id)
                    ("alt_text" . (("text" . ,text)))))
           :content-type "application/json"
           :allow-empty t
           :errback error-fn
           :owner owner
           :settle-on-cancel t
           :cancel-message
           (chirp-x-unknown-write-outcome
            "X media alt text was canceled before completion")))
      (chirp-x--callback-error
       (chirp-x--resignal-callback-error err))
      (error
       (funcall error-fn (error-message-string err))
       nil))))

(provide 'chirp-x)

;;; chirp-x.el ends here
