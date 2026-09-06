;;; chirp-url.el --- X URL targets for Chirp -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Parse trusted X and legacy Twitter URLs into Chirp domain targets.

;;; Code:

(require 'subr-x)
(require 'url-parse)
(require 'url-util)

;;; URL Grammar

(defconst chirp-url--trusted-hosts
  '("x.com" "www.x.com" "twitter.com" "www.twitter.com")
  "HTTPS hosts accepted as X URL targets.")

(defconst chirp-url-browse-regexp
  (concat "\\`https://" (regexp-opt chirp-url--trusted-hosts)
          "\\(?:[/:?#]\\|\\'\\)")
  "Regexp suitable for routing X URLs through `browse-url-handlers'.")

;;; Parsing

(defun chirp-url--query-value (query name)
  "Return NAME's first decoded value from QUERY, or nil."
  (cadr (assoc-string name (url-parse-query-string (or query "") t) t)))

(defun chirp-url--parse-path (segments query)
  "Return an X URL target from SEGMENTS and QUERY, or nil."
  (pcase segments
    ('("home") '(:kind home))
    ('("i" "bookmarks") '(:kind bookmarks))
    ((or '("messages") '("messages" "compose")) '(:kind direct-messages))
    ('("search")
     (when-let* ((text (chirp-url--query-value query "q"))
                 (text (string-trim text))
                 ((not (string-empty-p text))))
       (list :kind 'search :query text)))
    (`("i" "web" "status" ,id . ,_)
     (when (string-match-p "\\`[0-9]+\\'" id)
       (list :kind 'tweet :id id)))
    (`(,_handle "status" ,id "history")
     (when (string-match-p "\\`[0-9]+\\'" id)
       (list :kind 'edit-history :id id)))
    (`(,_handle "status" ,id . ,_)
     (when (string-match-p "\\`[0-9]+\\'" id)
       (list :kind 'tweet :id id)))
    (`(,_owner ,list-word ,id)
     (when (and (member list-word '("list" "lists"))
                (string-match-p "\\`[0-9]+\\'" id))
       (list :kind 'list :id id)))
    (`(,handle "followers")
     (list :kind 'followers :handle handle))
    (`(,handle "following")
     (list :kind 'following-users :handle handle))
    (`(,handle "likes")
     (list :kind 'likes :handle handle))
    (`(,handle)
     (unless (member handle '("compose" "explore" "home" "i" "login"
                              "messages" "notifications" "search" "settings"))
       (list :kind 'profile :handle handle)))))

(defun chirp-url-parse (value)
  "Parse VALUE as a trusted X URL target, or return nil.

The returned plist contains `:kind' and destination-specific identity.  Only
HTTPS URLs on X or legacy Twitter hosts are accepted."
  (when (stringp value)
    (condition-case nil
        (let* ((parsed (url-generic-parse-url (string-trim value)))
               (host (downcase (or (url-host parsed) "")))
               (filename (or (url-filename parsed) ""))
               (path-query (split-string filename "\\?"))
               (path (car path-query))
               (query (mapconcat #'identity (cdr path-query) "?")))
          (when (and (equal (url-type parsed) "https")
                     (member host chirp-url--trusted-hosts)
                     (null (url-user parsed))
                     (or (null (url-port parsed))
                         (= (url-port parsed) 443)))
            (chirp-url--parse-path
             (mapcar #'url-unhex-string (split-string path "/" t))
             query)))
      (error nil))))

;;; Tweet Targets

(defun chirp-url-tweet-id (value)
  "Return the tweet ID represented by VALUE, or nil."
  (cond
   ((null value) nil)
   ((and (stringp value)
         (string-match-p "\\`[0-9]+\\'" value))
    value)
   ((stringp value)
    (when-let* ((target (chirp-url-parse value))
                ((memq (plist-get target :kind) '(tweet edit-history))))
      (plist-get target :id)))
   ((listp value)
    (or (plist-get value :id)
        (chirp-url-tweet-id (plist-get value :url))))))

(provide 'chirp-url)

;;; chirp-url.el ends here
