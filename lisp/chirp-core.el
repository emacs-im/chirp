;;; chirp-core.el --- Shared state and utilities for chirp -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Shared customization, normalized tweet data, buffer state, and navigation
;; utilities used by Chirp's view modules.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'browse-url)
(require 'json)
(require 'xml)
(require 'warnings)
(require 'appkit-core)
(require 'appkit-evil)
(require 'appkit-invalidation)
(require 'appkit-projection)
(require 'appkit-ui)
(require 'appkit-view)
(require 'chirp-url)

(declare-function appkit-compose-cancel-operation "appkit-compose" ())
(declare-function appkit-compose-operation-active-p "appkit-compose" ())

(declare-function chirp-backend-tweet "chirp-backend"
                  (tweet-id callback &optional errback))
(declare-function chirp-profile-open "chirp-profile" (handle &optional mode))
(declare-function chirp-thread-open "chirp-thread" (tweet-id))
(declare-function chirp-thread-open-tweet "chirp-thread" (tweet))
(declare-function chirp-thread-add-spam-rule "chirp-thread" (&optional authorp))
(declare-function chirp-dispatch "chirp-actions" ())
(declare-function chirp-toggle-follow-user-at-point "chirp-actions" ())
(declare-function chirp-load-more "chirp-timeline" (&optional anchor-id))
(declare-function chirp-toggle-home-following "chirp-timeline" ())
(declare-function chirp-media-at-point "chirp-media" ())
(declare-function chirp-media-open "chirp-media-view" (media-list index &optional title buffer))
(declare-function chirp-media-open-at-point "chirp-media-view" ())
(declare-function chirp-media-download-at-point "chirp-media" ())
(declare-function chirp-media-prefetch-tweet "chirp-media" (tweet buffer))
(declare-function chirp-media-image-mode "chirp-media-view" ())
(declare-function chirp-media-view-mode "chirp-media-view" ())
(declare-function chirp-xchat-native-session-destroy
                  "chirp-xchat-native-module" (session))

;;; Options

(defgroup chirp nil
  "Browse X/Twitter from Emacs."
  :group 'applications)

(defcustom chirp-buffer-name "*chirp*"
  "Base buffer name used for newly created Chirp views."
  :type 'string
  :group 'chirp)

(defcustom chirp-language "zh-CN"
  "BCP 47 language tag used for localized timestamps and X requests.

Chirp currently localizes timestamps for Chinese and English; other language
tags use the English timestamp forms."
  :type 'string
  :group 'chirp)

(defun chirp--language-tag-p (value)
  "Return non-nil when VALUE is a conservative BCP 47 language tag."
  (and (stringp value)
       (string-match-p
        "\\`[[:alpha:]]\\{2,3\\}\\(?:-[[:alnum:]]\\{2,8\\}\\)*\\'"
        value)))

(defcustom chirp-default-max-results 20
  "Default number of posts requested for list views."
  :type 'integer
  :group 'chirp)

(defun chirp--view-width ()
  "Return the current Chirp view's responsive width in columns."
  (or (appkit-view-responsive-width) fill-column))

(defcustom chirp-timeline-refresh-max-results 10
  "Number of head posts requested when refreshing a timeline with `g'.

Refreshing only needs a recent head window to detect and merge newer posts, so
this can stay smaller than `chirp-default-max-results' for better latency.
Set it to nil to refresh using the current timeline size instead."
  :type '(choice (const :tag "Use current timeline size" nil)
                 integer)
  :group 'chirp)

(defcustom chirp-rerender-idle-delay 0.2
  "Seconds Chirp coalesces background projection updates.

This applies to updates triggered by async media, link-card, and quoted-tweet
enrichment.  Appkit views coalesce invalidations; legacy views retain one idle
redraw timer while their projections are migrated."
  :type 'number
  :group 'chirp)

(defcustom chirp-timeline-load-more-step 20
  "Number of additional posts fetched when loading more timeline items."
  :type 'integer
  :group 'chirp)

(defcustom chirp-profile-post-limit 15
  "Number of posts fetched per profile page."
  :type 'integer
  :group 'chirp)

(defcustom chirp-thread-max-results 20
  "Number of tweets to request when opening a thread.

This caps the initial thread fetch so Chirp does not load an unbounded reply
window."
  :type 'integer
  :group 'chirp)

(defcustom chirp-hide-promoted-posts t
  "When non-nil, hide tweets explicitly marked as promoted by the backend."
  :type 'boolean
  :group 'chirp)

(defcustom chirp-show-avatars t
  "When non-nil, show avatar images in Chirp views."
  :type 'boolean
  :group 'chirp)

(defcustom chirp-show-tweet-media t
  "When non-nil, show tweet media thumbnails in list and thread views.

When nil, Chirp keeps compact text entries for media so RET and download
commands still work, and displays alt text when the backend provides it."
  :type 'boolean
  :group 'chirp)

;;; Session

(appkit-define-app-kind chirp :shutdown #'chirp--shutdown-app)

(cl-defstruct (chirp--session (:constructor chirp--session-create))
  "State owned by one Chirp application session."
  tweet-state-overrides
  quoted-tweet-cache
  quoted-tweet-pending
  backend-read-cache
  backend-pending-reads
  primary-feed-states
  dm-conversations
  dm-live
  media-runtime
  xchat-native-session
  xchat-native-epoch
  xchat-recovery
  xchat-user
  xchat-user-id)

(defun chirp--shutdown-app (app)
  "Destroy native state owned by stopped Chirp APP."
  (let* ((state (appkit-app-state app))
         (native-session
          (and (chirp--session-p state)
               (chirp--session-xchat-native-session state))))
    (when native-session
      (setf (chirp--session-xchat-native-session state) nil
            (chirp--session-xchat-native-epoch state) nil
            (chirp--session-xchat-recovery state) nil)
      (unless (fboundp 'chirp-xchat-native-session-destroy)
        (error "Chirp lost the loaded XChat native module"))
      (chirp-xchat-native-session-destroy native-session))))

(defvar chirp--app nil
  "Lazy Appkit application session owned by Chirp.")

(defun chirp--make-session ()
  "Return initialized state for a new Chirp application session."
  (chirp--session-create
   :tweet-state-overrides (make-hash-table :test #'equal)
   :quoted-tweet-cache (make-hash-table :test #'equal)
   :quoted-tweet-pending (make-hash-table :test #'equal)
   :backend-read-cache (make-hash-table :test #'equal)
   :backend-pending-reads (make-hash-table :test #'equal)
   :dm-conversations (make-hash-table :test #'equal)
   :primary-feed-states (make-hash-table :test #'eq)))

(defun chirp-app ()
  "Return Chirp's live Appkit application session, creating it when needed."
  (unless (appkit-app-live-p chirp--app)
    (setq chirp--app
          (appkit-start-app
           'chirp :id 'default :state (chirp--make-session))))
  chirp--app)

(defun chirp--session ()
  "Return state for Chirp's current application session."
  (appkit-app-state (chirp-app)))

(defun chirp-stop ()
  "Stop Chirp's runtime and cancel its owned asynchronous work."
  (interactive)
  (unwind-protect
      (progn
        (dolist (buffer (buffer-list))
          (when (buffer-live-p buffer)
            (with-current-buffer buffer
              (when (and (derived-mode-p 'appkit-chat-compose-mode)
                         (fboundp 'appkit-compose-operation-active-p)
                         (appkit-compose-operation-active-p))
                (ignore-errors (appkit-compose-cancel-operation))))))
        (when (appkit-app-live-p chirp--app)
          (appkit-stop-app chirp--app)))
    (setq chirp--app nil)))

;;; Variables

(defvar-local chirp--refresh-function nil
  "Function used to refresh the current Chirp buffer.")

(defvar-local chirp--view-title nil
  "Human-readable title for the current Chirp buffer.")

(defvar-local chirp--request-token nil
  "Latest async request token for the current Chirp buffer.")

(defvar-local chirp--timeline-kind nil
  "Timeline kind shown in the current Chirp buffer.")

(defvar-local chirp--timeline-limit nil
  "Current max post count for the active timeline buffer.")

(defvar-local chirp--timeline-count nil
  "Current number of posts shown in the active timeline buffer.")

(defvar-local chirp--timeline-next-cursor nil
  "Pagination cursor used to fetch older posts for the active timeline buffer.")

(defvar-local chirp--timeline-load-more-function nil
  "Function used to fetch older posts for the current timeline.")

(defvar-local chirp--timeline-exhausted-p nil
  "Non-nil when the active timeline has no more older posts to fetch.")

(defvar-local chirp--timeline-loading-more nil
  "Non-nil while Chirp is fetching older timeline posts.")

(defvar-local chirp--rerender-function nil
  "Function used to redraw the current Chirp view without refetching data.")

(defvar-local chirp--rerender-timer nil
  "Idle timer used to coalesce legacy Chirp redraws.")

(defvar-local chirp--expanded-tweet-ids nil
  "Hash table of tweet ids expanded inline in the current Chirp buffer.")

(defvar-local chirp--entry-wrap-navigation t
  "When non-nil, entry navigation wraps around at buffer boundaries.")

(defvar-local chirp--profile-handle nil
  "Profile handle represented by the current profile buffer, or nil.")

(defvar-local chirp--profile-view-mode nil
  "Current profile subview mode for the active profile buffer.")

(defvar-local chirp--profile-switch-mode-function nil
  "Function used to switch the current profile buffer to another subview.")

(defvar-local chirp--status-text nil
  "Persistent status text shown for the current Chirp buffer.")

(defvar-local chirp--status-kind nil
  "Kind of status currently shown for the current Chirp buffer.")

(defvar-local chirp--status-start-time nil
  "Timestamp when the current Chirp status started.")

(defvar-local chirp--status-timer nil
  "Timer used to refresh Chirp's persistent mode-line status.")

(put 'chirp--request-token 'permanent-local t)
(put 'chirp--timeline-kind 'permanent-local t)
(put 'chirp--timeline-limit 'permanent-local t)
(put 'chirp--timeline-count 'permanent-local t)
(put 'chirp--timeline-next-cursor 'permanent-local t)
(put 'chirp--timeline-load-more-function 'permanent-local t)
(put 'chirp--timeline-exhausted-p 'permanent-local t)
(put 'chirp--rerender-function 'permanent-local t)
(put 'chirp--rerender-timer 'permanent-local t)
(put 'chirp--expanded-tweet-ids 'permanent-local t)
(put 'chirp--entry-wrap-navigation 'permanent-local t)
(put 'chirp--profile-handle 'permanent-local t)
(put 'chirp--profile-view-mode 'permanent-local t)
(put 'chirp--profile-switch-mode-function 'permanent-local t)
(put 'chirp--status-text 'permanent-local t)
(put 'chirp--status-kind 'permanent-local t)
(put 'chirp--status-start-time 'permanent-local t)
(put 'chirp--status-timer 'permanent-local t)

;;; View Mode

(defvar-keymap chirp-view-mode-map
  :doc "Keymap for `chirp-view-mode'."
  "g" #'chirp-refresh
  "TAB" #'chirp-toggle-home-following
  "n" #'chirp-next-entry
  "p" #'chirp-previous-entry
  "N" #'chirp-load-more
  "RET" #'chirp-open-at-point
  "t" #'chirp-open-at-point
  "m" #'chirp-open-primary-media
  "D" #'chirp-media-download-at-point
  "A" #'chirp-open-author-at-point
  "S" #'chirp-thread-add-spam-rule
  "x" #'chirp-dispatch
  "o" #'chirp-browse-at-point
  "q" #'chirp-quit-current-buffer)

(define-derived-mode chirp-view-mode special-mode "Chirp"
  "Major mode for Chirp buffers."
  (setq-local truncate-lines nil)
  (setq-local word-wrap t)
  ;; Positive line spacing opens visible seams between thumbnail slices.
  (setq-local line-spacing 0)
  (setq-local mode-line-process
              '((:eval (chirp--mode-line-status-string))))
  (add-hook 'text-scale-mode-hook #'chirp--on-text-scale-change nil t)
  (appkit-evil-normalize-keymaps)
  (visual-line-mode 1))

(defun chirp--setup-evil ()
  "Install optional Evil bindings for Chirp browsing views."
  (when appkit-evil-enable-integration
    (appkit-evil-set-initial-states '(chirp-view-mode) 'normal)
    (appkit-evil-define-readonly-keys 'chirp-view-mode-map)
    (appkit-evil-map
      (:map chirp-view-mode-map
       :nm
       "RET" #'chirp-open-at-point
       "<return>" #'chirp-open-at-point
       "TAB" #'chirp-toggle-home-following
       "g r" #'chirp-refresh
       "g j" #'chirp-next-entry
       "g k" #'chirp-previous-entry
       "g n" #'chirp-load-more
       "g m" #'chirp-open-primary-media
       "g d" #'chirp-media-download-at-point
       "g a" #'chirp-open-author-at-point
       "g S" #'chirp-thread-add-spam-rule
       "?" #'chirp-dispatch
       "g o" #'chirp-browse-at-point))
    (appkit-evil-normalize-buffers '(chirp-view-mode))))

(chirp--setup-evil)

(with-eval-after-load 'evil
  (chirp--setup-evil))

;;; Status

(defun chirp--status-face (kind)
  "Return a mode-line face for status KIND."
  (pcase kind
    ('error 'error)
    (_ 'mode-line-emphasis)))

(defun chirp--mode-line-status-string ()
  "Return the mode-line string for the current Chirp status."
  (when (and chirp--status-text
             (not (string-empty-p chirp--status-text)))
    (let* ((elapsed (if chirp--status-start-time
                        (max 0.0 (- (float-time) chirp--status-start-time))
                      0.0))
           (text (format " · %s %.1fs" chirp--status-text elapsed)))
      (propertize text 'face (chirp--status-face chirp--status-kind)))))

(defun chirp--ensure-status-timer (buffer)
  "Ensure BUFFER has a status refresh timer."
  (with-current-buffer buffer
    (unless (timerp chirp--status-timer)
      (let (timer)
        (setq timer
              (run-with-timer
               0.0 0.5
               (lambda ()
                 (if (not (buffer-live-p buffer))
                     (cancel-timer timer)
                   (with-current-buffer buffer
                     (if chirp--status-text
                         (force-mode-line-update t)
                       (cancel-timer timer)
                       (setq-local chirp--status-timer nil)))))))
        (setq-local chirp--status-timer timer)))))

(defun chirp-set-status (buffer text &optional kind)
  "Set BUFFER's persistent status TEXT and KIND."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq-local chirp--status-text text)
      (setq-local chirp--status-kind (or kind 'loading))
      (setq-local chirp--status-start-time (float-time)))
    (chirp--ensure-status-timer buffer)
    (with-current-buffer buffer
      (force-mode-line-update t))))

(defun chirp-clear-status (&optional buffer)
  "Clear persistent status in BUFFER."
  (let ((target (or buffer (current-buffer))))
    (when (buffer-live-p target)
      (with-current-buffer target
        (when (timerp chirp--status-timer)
          (cancel-timer chirp--status-timer))
        (setq-local chirp--status-text nil)
        (setq-local chirp--status-kind nil)
        (setq-local chirp--status-start-time nil)
        (setq-local chirp--status-timer nil)
        (force-mode-line-update t)))))

;;; Buffers

(defun chirp--base-buffer-stem ()
  "Return the display stem used for Chirp buffer names."
  (let ((name chirp-buffer-name))
    (if (string-match "\\`\\*\\(.*?\\)\\*\\'" name)
        (match-string 1 name)
      name)))

(defun chirp--format-buffer-name (&optional title)
  "Return a display buffer name for TITLE."
  (if (and (stringp title)
           (not (string-empty-p title)))
      (format "*%s: %s*" (chirp--base-buffer-stem) title)
    chirp-buffer-name))

(defun chirp--apply-buffer-name (buffer &optional title)
  "Rename BUFFER to match TITLE and return BUFFER."
  (with-current-buffer buffer
    (let ((new-name (chirp--format-buffer-name title)))
      (unless (string= (buffer-name buffer) new-name)
        (rename-buffer new-name t))))
  buffer)

(defun chirp-buffer ()
  "Create and return a fresh legacy Chirp buffer."
  (generate-new-buffer chirp-buffer-name))

(defun chirp-display-buffer (buffer)
  "Display BUFFER in the selected window."
  (unless (eq (window-buffer (selected-window)) buffer)
    (switch-to-buffer buffer)))

(defun chirp--appkit-timeline-state (&optional buffer)
  "Return canonical Appkit timeline state for BUFFER, or nil."
  (let ((target (or buffer (current-buffer))))
    (when (buffer-live-p target)
      (with-current-buffer target
        (when-let* ((view (appkit-current-view))
                    (state (appkit-view-state view))
                    ((eq (plist-get state :type) 'timeline)))
          state)))))

(defun chirp--persistent-timeline-buffer-p (&optional buffer)
  "Return non-nil when BUFFER is a main timeline Chirp buffer.

For You and Following stay alive when the user quits the window so they can be
revisited later."
  (when (buffer-live-p (or buffer (current-buffer)))
    (with-current-buffer (or buffer (current-buffer))
      (or (memq chirp--timeline-kind '(home following))
          (when-let* ((state (chirp--appkit-timeline-state)))
            (memq (plist-get (plist-get state :query) :kind)
                  '(home following)))))))

(defun chirp-quit-current-buffer ()
  "Close the current Chirp buffer."
  (interactive)
  (quit-window (not (chirp--persistent-timeline-buffer-p))))

;;; Requests

(defun chirp-begin-background-request (buffer title)
  "Start an async request for BUFFER titled TITLE without displaying it yet."
  (chirp-set-status buffer (format "Loading %s..." title))
  (chirp-begin-request buffer))

(defun chirp-begin-request (buffer)
  "Return a new request token for BUFFER."
  (let ((token (gensym "chirp-request-")))
    (with-current-buffer buffer
      (setq-local chirp--request-token token))
    token))

(defun chirp-request-current-p (buffer token)
  "Return non-nil when TOKEN still matches BUFFER's active request."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (eq chirp--request-token token))))

(defun chirp-view-state-token-current-p (view state token &optional property)
  "Return non-nil when TOKEN still owns PROPERTY in VIEW and STATE.

PROPERTY defaults to `:generation'."
  (and (appkit-view-live-p view)
       (eq state (appkit-view-state view))
       (eq token (plist-get state (or property :generation)))))

(defun chirp-cancel-view-request (view request-key cancel-function)
  "Remove VIEW's REQUEST-KEY transport and call CANCEL-FUNCTION on it."
  (let* ((table (appkit-view-request-table view))
         (request (gethash request-key table)))
    (remhash request-key table)
    (when request
      (funcall cancel-function request))))

;;; Projection Views

(defun chirp--live-projection-view (&optional buffer)
  "Return BUFFER's live Appkit projection view, or nil."
  (let ((target (or buffer (current-buffer))))
    (when (buffer-live-p target)
      (with-current-buffer target
        (when-let* ((view (appkit-current-view))
                    ((appkit-view-live-p view))
                    ((appkit-projection-view-p view)))
          view)))))

(defun chirp--projection-state (&optional buffer)
  "Return BUFFER's Appkit projection state, or nil."
  (when-let* ((view (chirp--live-projection-view buffer)))
    (appkit-view-state view)))

(defun chirp-projection-position-intent (events)
  "Return the effective semantic position intent from EVENTS."
  (or (cl-loop for event in events
               when (eq (plist-get event :position) 'first)
               return 'first)
      (cl-loop for event in (reverse events)
               for position = (plist-get event :position)
               when position return position)
      'preserve))

(cl-defun chirp--setup-projection-view
    (view title printer anchor-property &optional (no-separator-p t))
  "Initialize VIEW's read-only projection titled TITLE.

PRINTER renders one row.  ANCHOR-PROPERTY carries stable row identity.
NO-SEPARATOR-P suppresses EWOC's automatic newlines between rows."
  (let* ((buffer (appkit-view-buffer view))
         (state (appkit-view-state view)))
    (setq-local chirp--view-title title)
    (setq-local chirp--refresh-function (plist-get state :refresh))
    (setq-local chirp--rerender-function nil)
    (setq-local header-line-format nil)
    (setq-local chirp--entry-wrap-navigation
                (if (plist-member state :wrap-navigation)
                    (plist-get state :wrap-navigation)
                  t))
    (chirp--apply-buffer-name buffer title)
    (appkit-projection-ensure
     view
     :printer printer
     :anchor-property anchor-property
     :no-separator-p no-separator-p)
    (appkit-view-enqueue-event
     view (list :position (or (plist-get state :position) 'first)))
    (appkit-invalidate view :structure t :part 'frame :position t)
    (appkit-sync-invalidations view)))

(cl-defun chirp-open-projection-view
    (&key id title state sync-function printer
          (mode 'chirp-view-mode)
          (anchor-property 'chirp-entry-id)
          (parts '(frame entries geometry))
          setup select)
  "Open or reuse a read-only Chirp projection view.

ID identifies the view.  TITLE names the buffer.  STATE is the canonical
view plist.  SYNC-FUNCTION applies invalidations.  PRINTER renders one
projected row.  MODE, ANCHOR-PROPERTY, PARTS, and SELECT are forwarded to
`appkit-open-view'.  SETUP runs after responsive geometry is enabled."
  (let ((view
         (appkit-open-view
          :app (chirp-app)
          :id id
          :mode mode
          :buffer-name (chirp--format-buffer-name title)
          :state state
          :sync-function sync-function
          :parts parts
          :position-policy anchor-property
          :setup
          (lambda (live)
            (appkit-view-enable-responsive-geometry live)
            (if setup
                (funcall setup live)
              (chirp--setup-projection-view
               live title printer anchor-property)))
          :select select)))
    (with-current-buffer (appkit-view-buffer view)
      (setq-local chirp--view-title title)
      (setq-local chirp--refresh-function (plist-get state :refresh))
      (chirp--apply-buffer-name (current-buffer) title))
    view))

(defun chirp-sync-projection (view invalidations rows &optional header)
  "Apply INVALIDATIONS to VIEW by reconciling ROWS.

HEADER updates the generated frame when supplied."
  (let* ((events (appkit-view-pending-events-snapshot view))
         (event-count (length events))
         (position-intent (chirp-projection-position-intent events))
         (resources (appkit-invalidations-resource-keys invalidations))
         (parts (appkit-invalidations-parts invalidations))
         (all-resources-p (memq 'all resources))
         (force-all-rows-p
          (or all-resources-p (memq 'geometry parts)))
         (reconcile-p
          (or (appkit-invalidations-structure-p invalidations)
              (appkit-invalidations-entry-keys invalidations)
              resources
              parts))
         (force-keys
          (append
           (appkit-invalidations-entry-keys invalidations)
           (and force-all-rows-p
                (mapcar #'appkit-projection-row-key rows)))))
    (appkit-projection-sync
     view (and reconcile-p rows)
     :header (or header "")
     :force-keys force-keys
     :changed-dependencies (and (not all-resources-p) resources)
     :position position-intent
     :reconcile-p reconcile-p)
    (appkit-view-acknowledge-events view event-count)))

(defun chirp--on-text-scale-change ()
  "Rebuild pixel-aligned chrome after `text-scale-mode' changes.

The hook only requests a redraw.  Appkit projection sync owns the
buffer mutation, so card prefixes and avatars are recreated at the
current line metrics."
  (when (derived-mode-p 'chirp-view-mode)
    (chirp-request-rerender nil 0)))

(defun chirp-request-rerender (&optional buffer delay)
  "Schedule one coalesced projection update for BUFFER after DELAY."
  (let ((target (or buffer (current-buffer)))
        (wait (or delay chirp-rerender-idle-delay)))
    (when (buffer-live-p target)
      (if-let* ((view (chirp--live-projection-view target)))
          (appkit-request-sync
           view :resources '(all) :position t :delay wait)
        (with-current-buffer target
          (when (timerp chirp--rerender-timer)
            (cancel-timer chirp--rerender-timer))
          (setq-local
           chirp--rerender-timer
           (run-with-idle-timer
            wait nil
            (lambda (buf)
              (when (buffer-live-p buf)
                (with-current-buffer buf
                  (setq-local chirp--rerender-timer nil)
                  (when chirp--rerender-function
                    (let ((window-state (chirp-capture-window-state buf)))
                      (funcall chirp--rerender-function)
                      (chirp-restore-window-state window-state))))))
            target)))))))

(defun chirp-request-tweet-rerender (tweet-id &optional buffer delay)
  "Schedule a projection update for TWEET-ID in BUFFER after DELAY."
  (let ((target (or buffer (current-buffer))))
    (if (and tweet-id (buffer-live-p target))
        (if-let* ((view (chirp--live-projection-view target)))
            (appkit-request-sync
             view
             :entry (list 'tweet tweet-id)
             :resource (list 'tweet tweet-id)
             :position t
             :delay (or delay chirp-rerender-idle-delay))
          (chirp-request-rerender target delay))
      (chirp-request-rerender target delay))))

;;; Navigation and Commands

(defun chirp--text-property-at-point (property)
  "Return PROPERTY at point or immediately before point."
  (or (get-text-property (point) property)
      (and (> (point) (point-min))
           (get-text-property (1- (point)) property))))

(defun chirp-author-handle-at-point ()
  "Return the author handle stored at point, or nil."
  (chirp--text-property-at-point 'chirp-author-handle))

(defun chirp-open-profile-handle (handle)
  "Open HANDLE's profile."
  (if (and (stringp handle)
           (not (string-empty-p (string-remove-prefix "@" handle))))
      (chirp-profile-open handle)
    (user-error "No profile available at point")))

(defun chirp-reply-parent-id-at-point ()
  "Return the inline reply parent id stored at point, or nil."
  (chirp--text-property-at-point 'chirp-reply-parent-id))

(defun chirp-open-reply-parent-at-point ()
  "Jump to the visible parent tweet referenced at point."
  (interactive)
  (if-let* ((parent-id (chirp-reply-parent-id-at-point)))
      (or (chirp-goto-entry-id parent-id)
          (chirp-thread-open parent-id))
    (user-error "No reply parent available at point")))

(defun chirp-show-error (buffer title refresh message)
  "Display MESSAGE in BUFFER for TITLE using REFRESH for the view."
  (chirp-set-status buffer "Load failed" 'error)
  (with-current-buffer buffer
    (setq-local chirp--view-title title)
    (setq-local chirp--refresh-function refresh)
    (if-let* ((view (chirp--live-projection-view buffer))
              (state (appkit-view-state view)))
        (progn
          (when (plist-member state :status)
            (setf (plist-get state :status)
                  (list :phase 'error :message message)))
          (when (plist-member state :title)
            (setf (plist-get state :title) title))
          (when (plist-member state :refresh)
            (setf (plist-get state :refresh) refresh))
          (appkit-request-sync view :part 'frame :position t))
      (unless (derived-mode-p 'chirp-view-mode)
        (chirp-view-mode))
      (chirp--apply-buffer-name buffer title)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Unable to load data.\n\n")
        (insert message)
        (insert "\n")
        (goto-char (point-min)))))
  (chirp-display-buffer buffer))

(defun chirp-refresh ()
  "Refresh the current Chirp buffer."
  (interactive)
  (if chirp--refresh-function
      (funcall chirp--refresh-function)
    (user-error "No refresh function for this buffer")))

;;;; Entry Navigation

(defun chirp--entry-position-forward (start)
  "Return the next entry start at or after START."
  (or (and (< start (point-max))
           (eq (get-text-property start 'chirp-entry-start) t)
           start)
      (text-property-any start (point-max) 'chirp-entry-start t)))

(defun chirp--entry-position-backward (start)
  "Return the previous entry start before START."
  (let ((search-end (max (point-min) start))
        (probe (text-property-any (point-min) (max (point-min) start)
                                  'chirp-entry-start t))
        last)
    (while probe
      (setq last probe
            probe (text-property-any (min (point-max) (1+ probe))
                                     search-end
                                     'chirp-entry-start t)))
    last))

(defun chirp--current-entry-start ()
  "Return the top-level entry start that contains point, or nil."
  (let ((search-end (min (point-max) (1+ (point)))))
    (when (> search-end (point-min))
      (chirp--entry-position-backward search-end))))

(defun chirp-capture-point-anchor ()
  "Return a stable anchor describing the current point location."
  (if-let* ((entry-start (chirp--current-entry-start))
            (entry (get-text-property entry-start 'chirp-entry-item)))
      (list :entry-id (plist-get entry :id)
            :offset (- (point) entry-start))
    (list :position (point))))

(defun chirp-point-position-from-anchor (anchor)
  "Return a buffer position for ANCHOR, or nil when it cannot be restored."
  (cond
   ((null anchor) nil)
   ((and (listp anchor)
         (plist-member anchor :position))
    (max (point-min)
         (min (point-max)
              (plist-get anchor :position))))
   ((listp anchor)
    (when-let* ((entry-id (plist-get anchor :entry-id))
                (entry-pos (chirp-entry-position-by-id entry-id)))
      (let* ((offset (max 0 (or (plist-get anchor :offset) 0)))
             (next-start (chirp--entry-position-forward (min (point-max) (1+ entry-pos))))
             (entry-end (or next-start (point-max))))
        (max entry-pos
             (min (+ entry-pos offset)
                  (max entry-pos (1- entry-end)))))))
   ((stringp anchor)
    (chirp-entry-position-by-id anchor))
   (t nil)))

(defun chirp-restore-point-anchor (anchor)
  "Restore point from ANCHOR, returning non-nil on success."
  (when-let* ((pos (chirp-point-position-from-anchor anchor)))
    (goto-char pos)
    t))

(defun chirp-buffer-window (&optional buffer)
  "Return the interactive window showing BUFFER, or nil."
  (let ((target (or buffer (current-buffer))))
    (or (and (window-live-p (selected-window))
             (eq (window-buffer (selected-window)) target)
             (selected-window))
        (get-buffer-window target t))))

(defun chirp-capture-window-state (&optional buffer)
  "Return the current window state for BUFFER, or nil when not visible."
  (when-let* ((window (chirp-buffer-window buffer))
              ((window-live-p window)))
    (with-current-buffer (window-buffer window)
      (list :window window
            :point-anchor (save-excursion
                            (goto-char (window-point window))
                            (chirp-capture-point-anchor))
            :start-anchor (save-excursion
                            (goto-char (window-start window))
                            (chirp-capture-point-anchor))
            :hscroll (window-hscroll window)
            :vscroll (window-vscroll window t)))))

(defun chirp-restore-window-state (state)
  "Restore window STATE captured by `chirp-capture-window-state'."
  (when-let* ((window (plist-get state :window))
              ((window-live-p window)))
    (with-current-buffer (window-buffer window)
      (when-let* ((start (chirp-point-position-from-anchor
                          (plist-get state :start-anchor))))
        (set-window-start window start t))
      (set-window-hscroll window (or (plist-get state :hscroll) 0))
      (set-window-vscroll window (or (plist-get state :vscroll) 0) t)
      (when-let* ((pos (chirp-point-position-from-anchor
                        (plist-get state :point-anchor))))
        (set-window-point window pos)))))

(defun chirp-next-entry ()
  "Move to the next entry."
  (interactive)
  (let* ((current-start (chirp--current-entry-start))
         (pos (if current-start
                  (chirp--entry-position-forward
                   (min (point-max) (1+ current-start)))
                (chirp--entry-position-forward (point-min)))))
    (cond
     (pos
      (goto-char pos))
     ((and (chirp--appkit-timeline-state)
           (chirp-entry-at-point))
      (chirp-load-more (chirp-entry-id-at-point)))
     ((and chirp--timeline-load-more-function
           (chirp-entry-at-point))
      (funcall chirp--timeline-load-more-function (chirp-entry-id-at-point)))
     ((not chirp--entry-wrap-navigation)
      (user-error "Already at last entry"))
     ((setq pos (chirp--entry-position-forward (point-min)))
      (goto-char pos))
     (t
      (user-error "No entries in this buffer")))))

(defun chirp-previous-entry ()
  "Move to the previous entry."
  (interactive)
  (let* ((current-start (chirp--current-entry-start))
         (pos (and current-start
                   (chirp--entry-position-backward current-start))))
    (when (and (not pos)
               (not chirp--entry-wrap-navigation))
      (user-error "Already at first entry"))
    (unless pos
      (setq pos (chirp--entry-position-backward (point-max))))
    (if pos
        (goto-char pos)
      (user-error "No entries in this buffer"))))

(defun chirp-entry-at-point ()
  "Return the Chirp entry stored at point, or nil."
  (or (get-text-property (point) 'chirp-subentry-item)
      (and (> (point) (point-min))
           (get-text-property (1- (point)) 'chirp-subentry-item))
      (get-text-property (point) 'chirp-entry-item)
      (and (> (point) (point-min))
           (get-text-property (1- (point)) 'chirp-entry-item))))

(defun chirp-entry-id-at-point ()
  "Return the current Chirp entry id, or nil."
  (plist-get (chirp-entry-at-point) :id))

(defun chirp-entry-position-by-id (id)
  "Return the start position of the entry whose id equals ID."
  (when id
    (let ((pos (chirp--entry-position-forward (point-min)))
          found)
      (while (and pos (not found))
        (when (equal (plist-get (get-text-property pos 'chirp-entry-item) :id) id)
          (setq found pos))
        (unless found
          (setq pos (chirp--entry-position-forward (min (point-max) (1+ pos))))))
      found)))

(defun chirp-goto-entry-id (id)
  "Move point to the entry whose id equals ID."
  (when-let* ((pos (chirp-entry-position-by-id id)))
    (goto-char pos)
    t))

(defun chirp-entry-url-at-point ()
  "Return the hidden URL stored on the current Chirp entry, or nil."
  (or (get-text-property (point) 'chirp-subentry-url)
      (and (> (point) (point-min))
           (get-text-property (1- (point)) 'chirp-subentry-url))
      (get-text-property (point) 'chirp-entry-url)
      (and (> (point) (point-min))
           (get-text-property (1- (point)) 'chirp-entry-url))))

;;;; Entry Actions

(defun chirp--expanded-tweet-table (&optional buffer)
  "Return the expanded-tweet table for BUFFER, or nil."
  (or (plist-get (chirp--projection-state buffer) :expanded-tweet-ids)
      (and (buffer-live-p (or buffer (current-buffer)))
           (with-current-buffer (or buffer (current-buffer))
             chirp--expanded-tweet-ids))))

(defun chirp--tweet-expanded-p (tweet)
  "Return non-nil when TWEET is expanded in the current buffer."
  (let ((table (chirp--expanded-tweet-table)))
    (and (hash-table-p table)
         (gethash (plist-get tweet :id) table))))

(defun chirp--expand-tweet (tweet-id)
  "Expand TWEET-ID inline and update the current view."
  (if-let* ((view (chirp--live-projection-view))
            (state (appkit-view-state view)))
      (let ((table
             (or (plist-get state :expanded-tweet-ids)
                 (setf (plist-get state :expanded-tweet-ids)
                       (make-hash-table :test #'equal)))))
        (puthash tweet-id t table)
        (appkit-request-sync
         view :entry (list 'tweet tweet-id) :position t))
    (unless (functionp chirp--rerender-function)
      (user-error "This Chirp view cannot expand tweet content"))
    (unless (hash-table-p chirp--expanded-tweet-ids)
      (setq-local chirp--expanded-tweet-ids (make-hash-table :test #'equal)))
    (puthash tweet-id t chirp--expanded-tweet-ids)
    (funcall chirp--rerender-function)))

(defun chirp-open-at-point ()
  "Activate the Appkit action at point, or open the current entry."
  (interactive)
  (unless (appkit-ui-activate-at)
    (let ((entry (chirp-entry-at-point)))
      (cond
       ((eq (plist-get entry :kind) 'tweet)
        (chirp-thread-open-tweet entry))
       ((eq (plist-get entry :kind) 'user)
        (chirp-profile-open (plist-get entry :handle)))
       (t
        (user-error "No entry at point"))))))

(defun chirp-open-primary-media ()
  "Open the media at point or the first media of the current entry."
  (interactive)
  (if (chirp-media-at-point)
      (chirp-media-open-at-point)
    (let* ((entry (chirp-entry-at-point))
           (media-list (or (plist-get entry :media)
                           (chirp-tweet-article-images entry))))
      (if media-list
          (chirp-media-open media-list
                            0
                            (or chirp--view-title "Chirp Media"))
        (user-error "No media available at point")))))

(defun chirp-open-author-at-point ()
  "Open the author profile for the current entry."
  (interactive)
  (let* ((entry (chirp-entry-at-point))
         (handle (or (plist-get entry :author-handle)
                     (plist-get entry :handle))))
    (if handle
        (chirp-profile-open handle)
      (user-error "No profile available at point"))))

(defun chirp-browse-at-point ()
  "Open the current entry in a browser."
  (interactive)
  (let* ((media (chirp-media-at-point))
         (entry (chirp-entry-at-point))
         (url (or (plist-get media :url)
                  (chirp--text-property-at-point 'chirp-author-profile-url)
                  (chirp-entry-url-at-point)
                  (plist-get entry :url)
                  (plist-get entry :profile-url))))
    (if url
        (browse-url url)
      (user-error "No URL available at point"))))

;;; Data Access

(defun chirp-object-p (value)
  "Return non-nil when VALUE looks like an alist-style JSON object."
  (and (listp value)
       (or (null value)
           (let ((head (car value)))
             (and (consp head)
                  (or (stringp (car head))
                      (symbolp (car head))))))))

(defun chirp-get (object &rest keys)
  "Return the first matching value in OBJECT for KEYS."
  (when (chirp-object-p object)
    (cl-loop for key in keys
             for cell = (assoc-string key object t)
             when cell
             return (cdr cell))))

(defun chirp-get-in (object path)
  "Return the value at PATH inside OBJECT."
  (let ((value object))
    (catch 'missing
      (dolist (key path value)
        (setq value
              (cond
               ((chirp-object-p value)
                (let ((cell (assoc-string key value t)))
                  (if cell
                      (cdr cell)
                    (throw 'missing nil))))
               (t
                (throw 'missing nil))))))))


(defun chirp-boolean-value (value)
  "Normalize VALUE into a Lisp boolean."
  (cond
   ((null value) nil)
   ((eq value t) t)
   ((numberp value) (not (zerop value)))
   ((stringp value)
    (not (null (member (downcase value) '("1" "true" "yes" "on")))))
   ((symbolp value)
    (not (string= (symbol-name value) "chirp-json-false")))
   (t t)))

(defun chirp-first-nonblank (&rest values)
  "Return the first non-blank string in VALUES."
  (cl-loop for value in values
           when (and (stringp value)
                     (not (string-blank-p value)))
           return value))

;;; Text Normalization

(defun chirp-decode-html-entities (text)
  "Decode XML/HTML entities in TEXT using `xml-substitute-special'."
  (if (and (stringp text) (not (string-empty-p text)))
      (xml-substitute-special text)
    (or text "")))

(defun chirp-clean-text (value)
  "Normalize VALUE into a human-readable string."
  (cond
   ((stringp value)
    (string-trim
     (chirp-decode-html-entities
      (replace-regexp-in-string "\r" "" value))))
   ((null value) "")
   (t
    (string-trim (format "%s" value)))))

(defun chirp--display-text-range (value)
  "Return VALUE as a (START . END) code-point range, or nil."
  (cond
   ((and (vectorp value) (>= (length value) 2)
         (integerp (aref value 0)) (integerp (aref value 1)))
    (cons (aref value 0) (aref value 1)))
   ((and (consp value) (integerp (car value)) (integerp (cadr value)))
    (cons (car value) (cadr value)))
   (t nil)))

(defun chirp--tweet-display-text-range (object legacy)
  "Return OBJECT or LEGACY's display text range, or nil."
  (chirp--display-text-range
   (or (chirp-get object "display_text_range" "displayTextRange")
       (chirp-get legacy "display_text_range" "displayTextRange"))))

;;;; URLs

(defconst chirp--short-url-regexp "https?://t\\.co/[[:alnum:]]+"
  "Regexp that matches short X/Twitter URLs in tweet text.")

(defconst chirp--markdown-image-regexp "!\\[\\([^]\n]*\\)\\](\\([^)\n]+\\))"
  "Regexp that matches one Markdown image.")

(defun chirp--url-from-x (value)
  "Return one expanded URL decoded from X VALUE, or nil."
  (cond
   ((stringp value)
    (let ((text (string-trim value)))
      (unless (string-empty-p text)
        text)))
   ((chirp-object-p value)
    (chirp-first-nonblank
     (chirp-get value
                "expanded_url"
                "expandedUrl"
                "expanded"
                "url"
                "shortUrl")))
   (t nil)))

(defun chirp--urls-from-x (&rest values)
  "Return distinct expanded URLs decoded from X VALUES."
  (let ((seen (make-hash-table :test #'equal))
        items)
    (dolist (value values (nreverse items))
      (when (listp value)
        (dolist (item value)
          (when-let* ((url (chirp--url-from-x item)))
            (unless (gethash url seen)
              (puthash url t seen)
              (push url items))))))))

(defun chirp-extract-tweet-urls (object &optional legacy)
  "Extract expanded URLs for tweet OBJECT and optional LEGACY payload."
  (chirp--urls-from-x
   (chirp-get object "urls")
   (chirp-get-in object '("note_tweet" "note_tweet_results" "result" "entity_set" "urls"))
   (chirp-get-in object '("note_tweet" "entity_set" "urls"))
   (chirp-get-in object '("entities" "urls"))
   (and legacy
        (chirp-get-in legacy '("entities" "urls")))))

;;;; Text Entities

(defun chirp--entity-indices (value)
  "Return VALUE's code-point (START . END) indices, or nil."
  (chirp--display-text-range
   (or (chirp-get value "indices")
       (and (integerp (chirp-get value "from_index"))
            (integerp (chirp-get value "to_index"))
            (vector (chirp-get value "from_index")
                    (chirp-get value "to_index"))))))

(defun chirp--mention-from-x (value)
  "Return one Chirp mention decoded from X VALUE, or nil."
  (cond
   ((stringp value)
    (let ((handle (string-remove-prefix "@" (string-trim value))))
      (unless (string-empty-p handle)
        (list :handle handle :name handle))))
   ((chirp-object-p value)
    (when-let* ((handle (chirp-first-nonblank
                         (chirp-get value "screen_name" "screenName"
                                    "username" "handle")
                         (chirp-get-in value '("core" "screen_name")))))
      (setq handle (string-remove-prefix "@" handle))
      (list :handle handle
            :name (or (chirp-first-nonblank
                       (chirp-get value "name" "display_name" "displayName")
                       (chirp-get-in value '("core" "name")))
                      handle))))
   (t nil)))

(defun chirp--hashtag-from-x (value)
  "Return one hashtag decoded from X VALUE, or nil."
  (let ((tag
         (cond
          ((stringp value)
           (string-remove-prefix "#" (string-trim value)))
          ((chirp-object-p value)
           (chirp-first-nonblank
            (chirp-get value "text" "tag" "hashtag" "cashtag")))
          (t nil))))
    (and tag
         (not (string-empty-p tag))
         (string-remove-prefix "#" (string-remove-prefix "$" tag)))))

(defun chirp--text-entity-from-x (kind value)
  "Return one Chirp text entity decoded from X VALUE of KIND.

KIND is `mention', `hashtag', `cashtag', `url', `media', or `timestamp'.
Indices are Unicode code-point offsets into the tweet text."
  (when-let* ((indices (chirp--entity-indices value)))
    (pcase kind
      ('mention
       (when-let* ((mention (chirp--mention-from-x value)))
         (append mention
                 (list :kind 'mention
                       :start (car indices)
                       :end (cdr indices)))))
      ((or 'hashtag 'cashtag)
       (when-let* ((tag (chirp--hashtag-from-x value)))
         (list :kind kind
               :tag tag
               :start (car indices)
               :end (cdr indices))))
      ((or 'url 'media)
       (list :kind kind
             :url (chirp--url-from-x value)
             :display (chirp-first-nonblank
                       (chirp-get value "display_url" "displayUrl")
                       (chirp-get value "display"))
             :start (car indices)
             :end (cdr indices)))
      ('timestamp
       (list :kind 'timestamp
             :tag (chirp-first-nonblank (chirp-get value "text"))
             :seconds (chirp-get value "seconds")
             :start (car indices)
             :end (cdr indices))))))

(defun chirp--entities-of (entities field kind)
  "Return normalized KIND entities from ENTITIES field FIELD."
  (let (items)
    (dolist (item (chirp-get entities field) (nreverse items))
      (when-let* ((entity (chirp--text-entity-from-x kind item)))
        (push entity items)))))

(defun chirp-extract-text-entities (entities)
  "Extract X tweet-text entities from one ENTITIES object.

This is the same set `tweetTextParts` walks: mentions, hashtags, cashtags,
URLs, media, and timestamps.  Each item keeps its code-point `indices'."
  (when (chirp-object-p entities)
    (append
     (chirp--entities-of entities "user_mentions" 'mention)
     (chirp--entities-of entities "userMentions" 'mention)
     (chirp--entities-of entities "hashtags" 'hashtag)
     (chirp--entities-of entities "symbols" 'cashtag)
     (chirp--entities-of entities "urls" 'url)
     (chirp--entities-of entities "media" 'media)
     (chirp--entities-of entities "timestamps" 'timestamp))))

;;;; Visible Text

(defun chirp--note-tweet-result (object)
  "Return OBJECT's note-tweet result, or nil."
  (or (chirp-get-in object '("note_tweet" "note_tweet_results" "result"))
      (chirp-get object "note_tweet")))

(defun chirp--tweet-source-text-and-entities (object legacy)
  "Return a plist describing OBJECT's visible text source.

LEGACY supplies fallback tweet text and entities.  The plist contains
`:text', `:entities', and `:note-p'.  Note-tweet text is paired with its
`entity_set'.  Ordinary tweets use `full_text' and the matching `entities'
object.  Mixing those sources would apply the wrong code-point indices."
  (let* ((note (chirp--note-tweet-result object))
         (note-text (and note (chirp-first-nonblank (chirp-get note "text"))))
         (legacy-text (chirp-first-nonblank
                       (chirp-get object "full_text" "text")
                       (chirp-get legacy "full_text" "text"))))
    (if (and (stringp note-text)
             (not (string-empty-p note-text)))
        (list :text note-text
              :entities (chirp-extract-text-entities
                         (or (chirp-get note "entity_set")
                             (chirp-get note "entitySet")))
              :note-p t)
      (list :text legacy-text
            :entities (chirp-extract-text-entities
                       (or (chirp-get object "entities")
                           (chirp-get legacy "entities")))))))

(defun chirp--leading-reply-mention-length (text)
  "Return the character length of a leading reply-mention run in TEXT."
  (if (and (stringp text)
           (string-match "\\`\\(?:@[A-Za-z0-9_]+[ \t]+\\)+" text))
      (match-end 0)
    0))

(defun chirp--visible-text-range (text range replyp)
  "Return the code-point (START . END) visible range for TEXT.

RANGE is an optional display-text range.  REPLYP enables the leading
@handle fallback used when X omits `display_text_range'."
  (or range
      (let ((end (length (or text ""))))
        (cons (if replyp (chirp--leading-reply-mention-length text) 0)
              end))))

(defun chirp--rebase-text-entities (entities from to)
  "Shift ENTITIES so indices are relative to code-point slice FROM..TO."
  (let (visible)
    (dolist (entity entities (nreverse visible))
      (let ((start (plist-get entity :start))
            (end (plist-get entity :end)))
        (when (and (integerp start)
                   (integerp end)
                   (< start to)
                   (> end from))
          (let ((copy (copy-sequence entity)))
            (setq copy (plist-put copy :start (max 0 (- start from))))
            (setq copy (plist-put copy :end (min (- to from) (- end from))))
            (when (< (plist-get copy :start) (plist-get copy :end))
              (push copy visible))))))))

(defun chirp--url-entity-label (entity)
  "Return the visible label X would show for URL ENTITY."
  (or (let ((display (plist-get entity :display)))
        (and (stringp display)
             (not (string-empty-p display))
             display))
      (plist-get entity :url)))

(defun chirp--omit-text-entity-p (entity context)
  "Return non-nil when ENTITY should be omitted in CONTEXT.

Media and quoted-tweet permalinks stay out of the body, as on the web.
Ordinary URL entities stay in place as their `display_url'."
  (pcase (plist-get entity :kind)
    ('media t)
    ('url
     (let ((url (plist-get entity :url)))
       (or (null (chirp--url-entity-label entity))
           (chirp--media-url-p url (plist-get context :media))
           (cl-some (lambda (tweet)
                      (chirp--tweet-permalink-p url tweet))
                    (delq nil (list (plist-get context :quoted-tweet)
                                    (plist-get context :tweet)))))))
    (_ nil)))

(defun chirp--substring (string start &optional end)
  "Return the substring of STRING between code-point indices START and END."
  (let* ((len (length (or string "")))
         (from (min (max 0 (or start 0)) len))
         (to (min (max from (or end len)) len)))
    (substring (or string "") from to)))

(defun chirp--emit-visible-text (text entities &optional context)
  "Return (DISPLAY . SPANS) by walking TEXT with code-point ENTITIES.

CONTEXT decides whether a URL entity is inlined as `display_url' or omitted.
SPAN offsets are Emacs character positions in DISPLAY."
  (let* ((sorted (cl-sort (copy-sequence entities) #'<
                          :key (lambda (entity)
                                 (or (plist-get entity :start) 0))))
         (limit (length text))
         (cursor 0)
         (omit-next-space nil)
         (parts nil)
         (spans nil)
         (char-pos 0)
         (trim-last-spaces
          (lambda ()
            (when parts
              (let* ((last (car parts))
                     (trimmed (replace-regexp-in-string "[ \t]+\\'" "" last)))
                (setcar parts trimmed)
                (setq char-pos (- char-pos
                                  (- (length last) (length trimmed))))))))
         (take-chunk
          (lambda (chunk)
            (setq chunk (chirp-decode-html-entities (or chunk "")))
            (when omit-next-space
              (cond
               ((string-prefix-p "\n" chunk)
                (funcall trim-last-spaces))
               ((string-match "\\`[ \t]" chunk)
                (setq chunk (substring chunk 1)))))
            (push chunk parts)
            (setq char-pos (+ char-pos (length chunk)))
            (setq omit-next-space nil))))
    (dolist (entity sorted)
      (let ((start (max 0 (or (plist-get entity :start) 0)))
            (end (min limit (or (plist-get entity :end) 0))))
        (when (and (< start end)
                   (>= start cursor))
          (when (< cursor start)
            (funcall take-chunk (chirp--substring text cursor start)))
          (if (chirp--omit-text-entity-p entity context)
              (setq omit-next-space t)
            (let* ((chunk
                    (pcase (plist-get entity :kind)
                      ('url
                       (or (chirp--url-entity-label entity)
                           (chirp--substring text start end)))
                      ('timestamp
                       (or (plist-get entity :tag)
                           (chirp--substring text start end)))
                      (_
                       (chirp--substring text start end))))
                   (from char-pos)
                   (url
                    (or (plist-get entity :url)
                        (chirp--timestamp-entity-url entity context))))
              (funcall take-chunk chunk)
              (push (list :kind (plist-get entity :kind)
                          :handle (plist-get entity :handle)
                          :name (plist-get entity :name)
                          :tag (plist-get entity :tag)
                          :url url
                          :start from
                          :end char-pos)
                    spans)))
          (setq cursor end))))
    (when (< cursor limit)
      (funcall take-chunk (chirp--substring text cursor)))
    (chirp--trim-visible-text
     (apply #'concat (nreverse parts))
     (nreverse spans))))

(defun chirp--timestamp-entity-url (entity context)
  "Return a tweet permalink with timestamp for ENTITY when CONTEXT has one."
  (when-let* ((seconds (plist-get entity :seconds))
              ((integerp seconds))
              (base (plist-get (plist-get context :tweet) :url)))
    (format "%s%st=%s"
            base
            (if (string-match-p "\\?" base) "&" "?")
            seconds)))

(defun chirp--trim-visible-text (text spans)
  "Trim TEXT and shift SPANS by the removed prefix."
  (let* ((trimmed (string-trim (or text "")))
         (prefix (or (and (not (string-empty-p trimmed))
                          (cl-search trimmed text))
                     0)))
    (cons
     trimmed
     (delq nil
           (mapcar
            (lambda (span)
              (let ((start (max 0 (- (plist-get span :start) prefix)))
                    (end (- (plist-get span :end) prefix)))
                (when (< start end)
                  (let ((copy (copy-sequence span)))
                    (setq copy (plist-put copy :start start))
                    (setq copy (plist-put copy :end (min (length trimmed) end)))
                    copy))))
            spans)))))

;;;; URL Presentation

(defun chirp-tweet-candidate-urls (tweet)
  "Return likely canonical URLs for TWEET."
  (let ((id (plist-get tweet :id))
        (handle (plist-get tweet :author-handle))
        urls)
    (when-let* ((url (plist-get tweet :url)))
      (push url urls))
    (when id
      (push (format "https://x.com/i/status/%s" id) urls)
      (when handle
        (push (format "https://x.com/%s/status/%s" handle id) urls)))
    (delete-dups (delq nil urls))))

(defun chirp-tweet-fixupx-url (tweet)
  "Return a fixupx.com URL for TWEET, or nil when unavailable."
  (when-let* ((url (car (chirp-tweet-candidate-urls tweet))))
    (replace-regexp-in-string
     "\\`https?://\\(?:www\\.\\)?\\(?:x\\.com\\|twitter\\.com\\)"
     "https://fixupx.com"
     url)))


(defun chirp--tweet-permalink-p (url tweet)
  "Return non-nil when URL is a permalink for TWEET."
  (let ((id (plist-get tweet :id)))
    (or (member url (chirp-tweet-candidate-urls tweet))
        (and id
             (equal (format "%s" id)
                    (chirp-url-tweet-id url))))))

(defun chirp--media-url-p (url media)
  "Return non-nil when URL represents one of the MEDIA items."
  (and (stringp url)
       (or (string-match-p
            "\\`https?://\\(?:pic\\.\\(?:x\\.com\\|twitter\\.com\\)\\|\\(?:pbs\\|video\\)\\.twimg\\.com\\)/"
            url)
           (and media
                (cl-loop
                 for item in media
                 thereis
                 (or (member url (list (plist-get item :url)
                                       (plist-get item :preview-url)))
                     (cl-loop for variant in (plist-get item :variants)
                              thereis (equal url
                                             (plist-get variant :url)))))))))

(defun chirp--filter-display-urls (urls context)
  "Return genuine external URLS described by CONTEXT.
CONTEXT is a plist containing the current tweet, quoted tweet, and media."
  (let* ((tweet (plist-get context :tweet))
         (quoted-tweet (plist-get context :quoted-tweet))
         (media (plist-get context :media))
         (tweets (delq nil (list quoted-tweet tweet))))
    (cl-remove-if
     (lambda (url)
       (or (chirp--media-url-p url media)
           (cl-some (lambda (candidate)
                      (chirp--tweet-permalink-p url candidate))
                    tweets)))
     urls)))

(defun chirp-short-url-count (text)
  "Return how many `t.co` placeholders appear in TEXT."
  (let ((start 0)
        (count 0)
        (value (or text "")))
    (while (string-match chirp--short-url-regexp value start)
      (setq count (1+ count)
            start (match-end 0)))
    count))


(defun chirp--entity-overlaps-p (left right)
  "Return non-nil when LEFT and RIGHT code-point ranges overlap."
  (and (integerp (plist-get left :start))
       (integerp (plist-get left :end))
       (integerp (plist-get right :start))
       (integerp (plist-get right :end))
       (< (plist-get left :start) (plist-get right :end))
       (> (plist-get left :end) (plist-get right :start))))

(defun chirp--uncovered-short-url-entities (text entities)
  "Return synthetic omit-entities for uncovered `t.co` URLs in TEXT.
ENTITIES identifies ranges that already have server-provided metadata."
  (cl-remove-if
   (lambda (synthetic)
     (cl-some (lambda (entity)
                (chirp--entity-overlaps-p synthetic entity))
              entities))
   (chirp--synthetic-url-entities text)))

(defun chirp--synthetic-url-entities (text)
  "Return URL entities for each `t.co` placeholder in TEXT.

Indices are code-point offsets into TEXT.  Used only when expanded URL coverage
says leftover placeholders should be omitted from the visible body."
  (let ((start 0)
        (value (or text ""))
        items)
    (while (string-match chirp--short-url-regexp value start)
      (let ((beg (match-beginning 0))
            (end (match-end 0)))
        (push (list :kind 'url
                    :start beg
                    :end end)
              items)
        (setq start end)))
    (nreverse items)))

;;;; Article Preview

(defun chirp--normalize-markdown-summary (text)
  "Flatten markdown-ish TEXT into a readable single paragraph."
  (let ((summary (or text "")))
    (setq summary (replace-regexp-in-string "!\\[[^]]*\\](\\([^)]*\\))" "" summary t))
    (setq summary (replace-regexp-in-string "\\[\\([^]]+\\)\\](\\([^)]*\\))" "\\1" summary t))
    (setq summary (replace-regexp-in-string "`\\([^`]+\\)`" "\\1" summary t))
    (setq summary (replace-regexp-in-string "^[#>*-]+[ \t]*" "" summary t))
    (setq summary (replace-regexp-in-string "[ \t\n\r]+" " " summary t))
    (string-trim summary)))

(defun chirp-tweet-article-preview (tweet &optional max-length)
  "Return a short readable article preview for TWEET.

When MAX-LENGTH is non-nil, truncate the preview to that many characters."
  (let* ((limit (or max-length 240))
         (paragraphs (split-string (or (plist-get tweet :article-text) "")
                                   "\n[ \t]*\n+"
                                   t))
         (summary
          (cl-loop for paragraph in paragraphs
                   for cleaned = (chirp--normalize-markdown-summary paragraph)
                   unless (or (string-empty-p cleaned)
                              (string-prefix-p "```" cleaned))
                   return cleaned)))
    (when summary
      (if (<= (length summary) limit)
          summary
        (concat (string-trim-right (substring summary 0 (max 0 (- limit 3))))
                "...")))))

(defun chirp--markdown-image-media (text)
  "Return a photo plist when TEXT is exactly one Markdown image paragraph."
  (let ((paragraph (string-trim (or text ""))))
    (when (string-match (format "\\`%s\\'" chirp--markdown-image-regexp) paragraph)
      (let ((url (chirp-clean-text (match-string 2 paragraph)))
            (alt (chirp-clean-text (match-string 1 paragraph))))
        (when (and (not (string-empty-p url))
                   (string-match-p "\\`https?://" url))
          (list :type "photo"
                :url url
                :alt alt
                :article-image-p t))))))

(defun chirp-article-segments (text)
  "Split article TEXT into renderable text and image segments."
  (let (segments)
    (dolist (paragraph (split-string (or text "")
                                     "\n[ \t]*\n+"
                                     t))
      (if-let* ((media (chirp--markdown-image-media paragraph)))
          (push (list :type 'image :media media) segments)
        (let ((cleaned (chirp-clean-text paragraph)))
          (unless (string-empty-p cleaned)
            (push (list :type 'text :text cleaned) segments)))))
    (nreverse segments)))

(defun chirp-tweet-article-images (tweet &optional max-count)
  "Return article images parsed from TWEET.

When MAX-COUNT is non-nil, return at most that many images."
  (let ((images
         (cl-loop for segment in (chirp-article-segments
                                  (plist-get tweet :article-text))
                  when (eq (plist-get segment :type) 'image)
                  collect (plist-get segment :media))))
    (if (and (integerp max-count)
             (>= max-count 0))
        (cl-subseq images 0 (min max-count (length images)))
      images)))

;;; Quoted Tweets

(defconst chirp--quoted-tweet-fetch-failed
  (make-symbol "chirp-quoted-tweet-fetch-failed")
  "Sentinel value used when quoted tweet enrichment fails.")

(defun chirp--quoted-tweet-from-x (value)
  "Return a quoted tweet decoded from X VALUE, or nil."
  (when-let* ((quoted
               (and (chirp-object-p value)
                    (or (chirp-get value "quotedTweet" "quoted_tweet")
                        (chirp-get-in
                         value '("quoted_status_result" "result"))))))
    (chirp--tweet-from-x quoted)))

(defun chirp-quoted-tweet-enriched-p (tweet)
  "Return non-nil when quoted TWEET already carries full fetched detail."
  (plist-get tweet :chirp-enriched-p))

(defun chirp--dispatch-quoted-tweet-callbacks (tweet-id payload)
  "Run pending callbacks for TWEET-ID with PAYLOAD."
  (let* ((pending (chirp--session-quoted-tweet-pending (chirp--session)))
         (callbacks (prog1 (gethash tweet-id pending)
                      (remhash tweet-id pending))))
    (dolist (callback callbacks)
      (when callback
        (condition-case err
            (funcall callback payload)
          (error
           (display-warning
            'chirp-core
            (format "Quoted-tweet callback failed for %s: %s"
                    tweet-id (error-message-string err))
            :warning)))))))

(defun chirp--request-quoted-tweet (tweet-id callback)
  "Fetch quoted tweet TWEET-ID and run CALLBACK with the result."
  (let* ((session (chirp--session))
         (cache (chirp--session-quoted-tweet-cache session))
         (pending (chirp--session-quoted-tweet-pending session))
         (cached (gethash tweet-id cache)))
    (cond
     ((eq cached chirp--quoted-tweet-fetch-failed)
      nil)
     (cached
      (funcall callback cached))
     ((gethash tweet-id pending)
      (puthash tweet-id
               (cons callback (gethash tweet-id pending))
               pending))
     (t
      (puthash tweet-id (list callback) pending)
      (chirp-backend-tweet
       tweet-id
       (lambda (tweet _envelope)
         (puthash tweet-id tweet cache)
         (chirp--dispatch-quoted-tweet-callbacks tweet-id tweet))
       (lambda (_message)
         (puthash tweet-id chirp--quoted-tweet-fetch-failed cache)
         (chirp--dispatch-quoted-tweet-callbacks tweet-id nil)))))))

(defun chirp-enrich-quoted-tweets (tweets buffer)
  "Asynchronously enrich quoted tweets inside TWEETS and rerender BUFFER."
  (dolist (tweet tweets)
    (when-let* ((quoted (plist-get tweet :quoted-tweet))
                (quoted-id (plist-get quoted :id))
                ((not (chirp-quoted-tweet-enriched-p quoted))))
      (chirp--request-quoted-tweet
       quoted-id
       (lambda (full-quoted)
         (when (and full-quoted
                    (buffer-live-p buffer))
           (plist-put full-quoted :chirp-enriched-p t)
           (plist-put tweet :quoted-tweet full-quoted)
           (chirp-media-prefetch-tweet full-quoted buffer)
           (chirp-request-tweet-rerender
            (plist-get tweet :id) buffer)))))))

;;; Tweet State

;;;; Counts

(defun chirp--count-value (&rest values)
  "Return the first scalar count in VALUES or their `count' members."
  (cl-loop for value in values
           for count = (if (chirp-object-p value)
                           (chirp-get value "count")
                         value)
           when (or (numberp count)
                    (and (stringp count)
                         (not (string-blank-p count))))
           return count))

(defun chirp-format-count (value)
  "Return a display string for VALUE."
  (cond
   ((null value) "-")
   ((numberp value) (number-to-string value))
   ((stringp value) value)
   (t (format "%s" value))))

(defun chirp-adjust-count (value delta)
  "Return VALUE adjusted by DELTA when it looks numeric."
  (let ((number
         (cond
          ((numberp value) value)
          ((and (stringp value)
                (string-match-p "\\`[0-9]+\\'" value))
           (string-to-number value))
          (t nil))))
    (if number
        (max 0 (+ number delta))
      value)))

;;;; Identity and Merging

(defun chirp-tweet-key (tweet)
  "Return a stable merge key for normalized TWEET.

Retweets use their wrapper ID so the wrapper and original remain distinct."
  (or (plist-get tweet :retweet-id)
      (plist-get tweet :id)
      (plist-get tweet :url)
      (plist-get tweet :text)))

(defun chirp-append-unique-tweets (current fetched)
  "Append unique FETCHED tweets to CURRENT and return the merged list."
  (let ((seen (make-hash-table :test #'equal))
        additions)
    (dolist (tweet current)
      (puthash (chirp-tweet-key tweet) t seen))
    (dolist (tweet fetched)
      (let ((key (chirp-tweet-key tweet)))
        (unless (gethash key seen)
          (puthash key t seen)
          (push tweet additions))))
    (append current (nreverse additions))))

;;;; State Overrides


(defun chirp-set-tweet-state-override (tweet-id prop value)
  "Store VALUE as local PROP override for TWEET-ID."
  (when tweet-id
    (let* ((table (chirp--session-tweet-state-overrides (chirp--session)))
           (state (copy-sequence (gethash tweet-id table))))
      (setq state (plist-put state prop value))
      (puthash tweet-id state table))))

(defun chirp-clear-tweet-state-overrides (tweet-id)
  "Clear local state overrides for TWEET-ID."
  (when tweet-id
    (remhash tweet-id (chirp--session-tweet-state-overrides (chirp--session)))))

(defun chirp-apply-tweet-state-overrides (tweet)
  "Apply current session state overrides to normalized TWEET and return it."
  (when tweet
    (let ((overrides
           (and-let* ((tweet-id (plist-get tweet :id)))
             (copy-sequence
              (gethash tweet-id
                       (chirp--session-tweet-state-overrides
                        (chirp--session)))))))
      (while overrides
        (setq tweet
              (plist-put tweet (pop overrides) (pop overrides)))))
    (when-let* ((quoted (plist-get tweet :quoted-tweet)))
      (plist-put tweet :quoted-tweet
                 (chirp-apply-tweet-state-overrides quoted)))
    tweet))

;;;; Feed Updates

(defun chirp--map-buffer-tweets (buffer fn)
  "Call FN for each distinct tweet entry visible in BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((pos (chirp--entry-position-forward (point-min)))
            (seen (make-hash-table :test #'eq)))
        (while pos
          (let ((entry (get-text-property pos 'chirp-entry-item)))
            (when (and (eq (plist-get entry :kind) 'tweet)
                       (not (gethash entry seen)))
              (puthash entry t seen)
              (funcall fn entry)))
          (setq pos (chirp--entry-position-forward
                     (min (point-max) (1+ pos)))))))))

(defun chirp--update-tweet-tree (tweet tweet-id fn)
  "Apply FN to TWEET-ID within TWEET or its quoted descendants."
  (if (equal (plist-get tweet :id) tweet-id)
      (progn
        (funcall fn tweet)
        t)
    (when-let* ((quoted (plist-get tweet :quoted-tweet)))
      (chirp--update-tweet-tree quoted tweet-id fn))))

(defun chirp--primary-feed-state-values (&optional extra-state)
  "Return retained primary feed states, including EXTRA-STATE once."
  (let* ((session
          (and (appkit-app-live-p chirp--app)
               (appkit-app-state chirp--app)))
         (table
          (and (chirp--session-p session)
               (chirp--session-primary-feed-states session)))
         states)
    (when table
      (maphash (lambda (_kind state)
                 (push state states))
               table))
    (when extra-state
      (cl-pushnew extra-state states :test #'eq))
    states))

(defun chirp-update-tweet-by-id (buffer tweet-id fn &optional rerender)
  "Apply FN to every cached tweet matching TWEET-ID for BUFFER.

Quoted descendants are included.  When RERENDER is non-nil, request targeted
updates for the owning top-level rows visible in BUFFER."
  (let ((active-state (chirp--appkit-timeline-state buffer))
        changed-p
        dirty-ids)
    (dolist (state (chirp--primary-feed-state-values active-state))
      (dolist (slot '(:items :pending-new-items))
        (dolist (tweet (plist-get state slot))
          (when (chirp--update-tweet-tree tweet tweet-id fn)
            (setq changed-p t)
            (when (and (eq state active-state) (eq slot :items))
              (cl-pushnew
               (plist-get tweet :id) dirty-ids :test #'equal))))))
    (unless active-state
      (chirp--map-buffer-tweets
       buffer
       (lambda (tweet)
         (when (chirp--update-tweet-tree tweet tweet-id fn)
           (setq changed-p t)
           (cl-pushnew (plist-get tweet :id) dirty-ids :test #'equal)))))
    (when rerender
      (dolist (dirty-id dirty-ids)
        (chirp-request-tweet-rerender dirty-id buffer)))
    changed-p))

(defun chirp--remove-tweet-from-primary-feeds (buffer tweet-id)
  "Remove TWEET-ID from retained primary feeds represented by BUFFER.

Return non-nil when BUFFER currently projects a primary feed."
  (let ((active-state (chirp--appkit-timeline-state buffer))
        active-changed-p)
    (dolist (state (chirp--primary-feed-state-values active-state))
      (dolist (slot '(:items :pending-new-items))
        (let* ((items (plist-get state slot))
               (remaining
                (cl-remove-if
                 (lambda (tweet)
                   (equal (plist-get tweet :id) tweet-id))
                 items)))
          (unless (= (length items) (length remaining))
            (setf (plist-get state slot) remaining)
            (when (eq state active-state)
              (setq active-changed-p t))))))
    (when (and active-changed-p (buffer-live-p buffer))
      (with-current-buffer buffer
        (when-let* ((view (appkit-current-view)))
          (appkit-request-sync
           view :structure t :part 'frame :position t))))
    (and active-state t)))

;;; Tweet Fields

(defun chirp-user-like-p (object)
  "Return non-nil when OBJECT resembles a user payload."
  (and (chirp-object-p object)
       (or (chirp-first-nonblank
            (chirp-get object "screen_name")
            (chirp-get object "screenName")
            (chirp-get object "username")
            (chirp-get object "handle")
            (chirp-get-in object '("core" "screen_name"))
            (chirp-get-in object '("legacy" "screen_name")))
           (chirp-get object "rest_id" "id"))
       (or (chirp-first-nonblank
            (chirp-get object "name")
            (chirp-get object "display_name")
            (chirp-get object "description")
            (chirp-get object "bio")
            (chirp-get-in object '("core" "name"))
            (chirp-get-in object '("legacy" "name"))
            (chirp-get-in object '("legacy" "description")))
           (chirp-get object "followers_count" "friends_count" "statuses_count")
           (chirp-get object "followers" "following" "tweets")
           (chirp-get-in object '("legacy" "followers_count"))
           (chirp-get-in object '("legacy" "friends_count")))))

(defun chirp--tweet-result (object)
  "Return the inner tweet from visibility wrapper OBJECT."
  (or (chirp-get object "tweet") object))

(defun chirp--tweet-reply-control-mode (object)
  "Return the reply-control mode carried by tweet OBJECT, or nil."
  (let* ((tweet (chirp--tweet-result object))
         (legacy (chirp-get tweet "legacy"))
         (control (or (chirp-get legacy "conversation_control"
                                 "conversationControl")
                      (chirp-get tweet "conversation_control"
                                 "conversationControl")))
         (mode (and (chirp-object-p control)
                    (chirp-first-nonblank
                     (chirp-get control "mode" "type")))))
    (and mode (format "%s" mode))))

(defun chirp--tweet-reply-limited-p (object)
  "Return non-nil when OBJECT explicitly limits the viewer's reply action."
  (let ((actions (or (chirp-get-in object
                                   '("limitedActionResults" "limited_actions"))
                     (chirp-get-in object
                                   '("limited_action_results" "limited_actions")))))
    (or (and (listp actions)
             (cl-some (lambda (action)
                        (or (equal action "Reply")
                            (equal (chirp-get action "action") "Reply")))
                      actions))
        (equal (chirp-get (chirp-get object "legacy") "limited_actions")
               "limited_replies"))))

(defun chirp--tweet-edit-metadata (object)
  "Return normalized edit-history metadata carried by tweet OBJECT."
  (let* ((tweet (chirp--tweet-result object))
         (control (chirp-get tweet "edit_control" "editControl"))
         (initial-control
          (and (chirp-object-p control)
               (chirp-get control
                          "edit_control_initial"
                          "editControlInitial")))
         (raw-ids
          (or (and (chirp-object-p initial-control)
                   (chirp-get initial-control
                              "edit_tweet_ids"
                              "editTweetIds"))
              (and (chirp-object-p control)
                   (chirp-get control
                              "edit_tweet_ids"
                              "editTweetIds"))))
         (ids
          (delete-dups
           (cl-loop for id in raw-ids
                    when (and (stringp id)
                              (not (string-blank-p id)))
                    collect id)))
         (initial-id
          (or (and (chirp-object-p control)
                   (chirp-first-nonblank
                    (chirp-get control
                               "initial_tweet_id"
                               "initialTweetId")))
              (car ids))))
    (list :edit-history-ids ids
          :edit-history-initial-id initial-id
          :edited-p (> (length ids) 1))))

(defun chirp-tweet-like-p (object)
  "Return non-nil when OBJECT resembles a tweet payload."
  (let* ((tweet (chirp--tweet-result object))
         (legacy (chirp-get tweet "legacy"))
         (metrics (chirp-get tweet "metrics"))
         (id (chirp-first-nonblank
              (chirp-get tweet "rest_id" "id_str" "id")
              (chirp-get legacy "id_str")))
         (text (chirp-first-nonblank
                (chirp-get tweet "full_text" "text")
                (chirp-get legacy "full_text" "text")
                (chirp-get-in tweet '("note_tweet" "note_tweet_results"
                                      "result" "text"))
                (chirp-get-in tweet '("note_tweet" "text"))))
         (stats (or (chirp-get tweet "favorite_count" "retweet_count"
                               "reply_count" "quote_count" "bookmark_count"
                               "view_count")
                    (and metrics
                         (chirp-get metrics "likes" "retweets" "replies"
                                    "quotes" "bookmarks" "views"))
                    (and legacy
                         (chirp-get legacy "favorite_count" "retweet_count"
                                    "reply_count" "quote_count")))))
    (and (chirp-object-p tweet)
         id
         (or text stats (chirp-get tweet "conversationId" "conversation_id"))
         (not (chirp-user-like-p tweet)))))

(defun chirp-find-first-object (value predicate)
  "Return the first object inside VALUE that satisfies PREDICATE."
  (let (result)
    (cl-labels ((walk (node)
                  (cond
                   (result nil)
                   ((chirp-object-p node)
                    (when (funcall predicate node)
                      (setq result node))
                    (unless result
                      (dolist (cell node)
                        (walk (cdr cell)))))
                   ((vectorp node)
                    (mapc #'walk (append node nil)))
                   ((listp node)
                    (mapc #'walk node)))))
      (walk value))
    result))

;;; Normalized Objects

;;;; Users

(defun chirp--extract-user-object (object)
  "Extract the most relevant user object from OBJECT."
  (let ((direct (or (chirp-get object "user" "author")
                    (chirp-get-in object '("core" "user_results" "result"))
                    (chirp-get-in object '("author_results" "result"))
                    (chirp-get-in object '("user_results" "result"))
                    (chirp-get-in object '("result")))))
    (cond
     ((chirp-user-like-p object) object)
     ((chirp-user-like-p direct) direct)
     (t (chirp-find-first-object object #'chirp-user-like-p)))))

(defun chirp--user-from-x (object)
  "Return a Chirp user decoded from X OBJECT, or nil."
  (let* ((user (chirp--extract-user-object object))
         (legacy (chirp-get user "legacy"))
         (handle (chirp-first-nonblank
                  (chirp-get user "screen_name")
                  (chirp-get user "screenName")
                  (chirp-get user "username")
                  (chirp-get user "handle")
                  (chirp-get-in user '("core" "screen_name"))
                  (chirp-get legacy "screen_name")))
         (name (chirp-first-nonblank
                (chirp-get user "name")
                (chirp-get user "display_name")
                (chirp-get-in user '("core" "name"))
                (chirp-get legacy "name")
                handle))
         (id (chirp-first-nonblank
              (chirp-get user "rest_id" "id_str" "id")
              (chirp-get legacy "id_str")))
         (bio (chirp-clean-text
               (chirp-first-nonblank
                (chirp-get user "description")
                (chirp-get user "bio")
                (chirp-get-in user '("profile_bio" "description"))
                (chirp-get legacy "description"))))
         (followers (or (chirp-get user "followers_count")
                        (chirp-get user "followers")
                        (chirp-get-in user '("relationship_counts" "followers"))
                        (chirp-get legacy "followers_count")))
         (following (or (chirp-get user "friends_count")
                        (chirp-get user "following_count")
                        (chirp-get user "following")
                        (chirp-get-in user '("relationship_counts" "following"))
                        (chirp-get legacy "friends_count")))
         (posts (or (chirp-get user "statuses_count")
                    (chirp-get user "tweets_count")
                    (chirp-get user "tweets")
                    (chirp-get-in user '("tweet_counts" "tweets"))
                    (chirp-get legacy "statuses_count")))
         (joined (chirp-first-nonblank
                  (chirp-get user "createdAtLocal")
                  (chirp-get user "createdAtISO")
                  (chirp-get user "createdAt")
                  (chirp-get user "created_at")
                  (chirp-get-in user '("core" "created_at"))
                  (chirp-get legacy "created_at")))
         (avatar-url (chirp-first-nonblank
                      (chirp-get user "profileImageUrl")
                      (chirp-get user "profile_image_url_https")
                      (chirp-get user "profile_image_url")
                      (chirp-get-in user '("avatar" "image_url"))
                      (chirp-get legacy "profile_image_url_https" "profile_image_url")))
         (viewer-following-p (chirp-boolean-value
                              (or (chirp-get user "viewerFollowing" "viewer_following")
                                  (chirp-get-in user '("relationship_perspectives" "following")))))
         (viewer-followed-by-p (chirp-boolean-value
                                (or (chirp-get user "viewerFollowedBy" "viewer_followed_by")
                                    (chirp-get-in user '("relationship_perspectives" "followed_by")))))
         (viewer-blocking-p (chirp-boolean-value
                             (or (chirp-get user "viewerBlocking" "viewer_blocking")
                                 (chirp-get-in user '("relationship_perspectives" "blocking")))))
         (viewer-muting-p (chirp-boolean-value
                           (or (chirp-get user "viewerMuting" "viewer_muting")
                               (chirp-get-in user '("relationship_perspectives" "muting"))))))
    (when (or handle id name)
      (list :kind 'user
            :id id
            :name name
            :handle (and handle (string-remove-prefix "@" handle))
            :bio bio
            :avatar-url avatar-url
            :followers followers
            :following following
            :viewer-following-p viewer-following-p
            :viewer-followed-by-p viewer-followed-by-p
            :viewer-blocking-p viewer-blocking-p
            :viewer-muting-p viewer-muting-p
            :posts posts
            :joined joined
            :profile-url (and handle
                              (format "https://x.com/%s"
                                      (string-remove-prefix "@" handle)))
            :raw object))))

;;;; Media

(defun chirp--media-variant-from-x (object)
  "Return one media variant decoded from X OBJECT."
  (let ((url (chirp-first-nonblank (chirp-get object "url")))
        (bitrate (chirp-get object "bitrate")))
    (when url
      (list :url url
            :bitrate bitrate))))


;;;; Link Cards

(defun chirp--tweet-card (object)
  "Return OBJECT's card object, or nil."
  (or (chirp-get object "card")
      (chirp-get object "tweet_card")))

(defun chirp--tweet-card-legacy (object)
  "Return OBJECT's card legacy payload, or nil."
  (when-let* ((card (chirp--tweet-card object)))
    (or (chirp-get card "legacy") card)))

(defun chirp--card-binding-entries (bindings)
  "Normalize card BINDINGS into an alist of (KEY . VALUE)."
  (cond
   ((null bindings) nil)
   ((vectorp bindings)
    (chirp--card-binding-entries (append bindings nil)))
   ((not (listp bindings)) nil)
   ((and (consp (car bindings))
         (stringp (chirp-get (car bindings) "key")))
    (cl-loop for binding in bindings
             for key = (chirp-get binding "key")
             for value = (or (chirp-get binding "value") binding)
             when (stringp key)
             collect (cons key value)))
   ((chirp-object-p bindings)
    (cl-loop for (key . value) in bindings
             when (stringp key)
             collect (cons key value)))))

(defun chirp--tweet-card-bindings (object)
  "Return OBJECT's card bindings as an alist of (KEY . VALUE)."
  (chirp--card-binding-entries
   (chirp-get (chirp--tweet-card-legacy object) "binding_values")))

(defun chirp--card-binding (bindings key)
  "Return the VALUE object for KEY in BINDINGS, or nil."
  (cdr (assoc-string key bindings t)))

(defun chirp--card-binding-raw-string (bindings key)
  "Return the raw string_value for KEY in BINDINGS, or nil."
  (let ((value (chirp--card-binding bindings key)))
    (chirp-first-nonblank
     (chirp-get value "string_value" "stringValue")
     (and (stringp value) value))))

(defun chirp--card-binding-string (bindings &rest keys)
  "Return the first cleaned string in BINDINGS for KEYS, or nil."
  (cl-loop for key in keys
           for raw = (chirp--card-binding-raw-string bindings key)
           for text = (and raw (chirp-clean-text raw))
           when (and text (not (string-empty-p text)))
           return text))

(defun chirp--card-binding-image-url (bindings)
  "Return the best website-card image URL from BINDINGS, or nil."
  (cl-loop for key in '("thumbnail_image_large"
                        "thumbnail_image"
                        "thumbnail_image_x_large"
                        "thumbnail_image_original"
                        "summary_photo_image_original"
                        "summary_photo_image"
                        "photo_image_full_size_original"
                        "photo_image_full_size"
                        "player_image_original"
                        "player_image")
           for value = (chirp--card-binding bindings key)
           for url = (chirp-first-nonblank
                      (chirp-get-in value '("image_value" "url"))
                      (chirp-get-in value '("imageValue" "url"))
                      (chirp-get value "url"))
           when (and (stringp url)
                     (string-match-p "\\`https?://" url))
           return url))

(defun chirp--link-card-name-p (name)
  "Return non-nil when card NAME is a website preview card."
  (when (stringp name)
    (let ((base (downcase (car (last (split-string name ":"))))))
      (or (string-prefix-p "summary" base)
          (string= base "player")))))

(defun chirp--external-http-url-p (url)
  "Return non-nil when URL is an http(s) link outside X itself."
  (and (stringp url)
       (string-match-p "\\`https?://" url)
       (not (string-match-p
             "\\`https?://\\(?:x\\.com\\|twitter\\.com\\|t\\.co\\)/"
             url))))

(defun chirp--link-card-from-x (object &optional urls)
  "Return OBJECT's X website card as a Chirp plist, or nil.

URLS are already-expanded tweet URLs used to prefer a real destination
over the card's `t.co` permalink."
  (when-let* ((legacy (chirp--tweet-card-legacy object))
              (name (chirp-first-nonblank
                     (chirp-get legacy "name")
                     (chirp-get (chirp--tweet-card object) "name")))
              ((chirp--link-card-name-p name))
              (bindings (chirp--tweet-card-bindings object)))
    (let* ((title (chirp--card-binding-string bindings "title" "vanity_title"))
           (description (chirp--card-binding-string bindings "description"))
           (domain (chirp--card-binding-string bindings "vanity_url" "domain"))
           (website (chirp--card-binding-string
                     bindings "website_url" "card_url"))
           (image-url (chirp--card-binding-image-url bindings))
           (url (or (car (cl-remove-if-not #'chirp--external-http-url-p urls))
                    (and (chirp--external-http-url-p website) website)
                    (and domain
                         (not (string-match-p "://" domain))
                         (concat "https://" domain))
                    website
                    (chirp-get legacy "url"))))
      (when (or title description image-url)
        (list :url url
              :title title
              :description description
              :image-url image-url
              :domain domain)))))

(defun chirp--unified-card-media-from-x (object)
  "Return video media decoded from OBJECT's bounded X unified card."
  (when-let* ((encoded (chirp--card-binding-raw-string
                        (chirp--tweet-card-bindings object)
                        "unified_card"))
              ((stringp encoded))
              ((<= (string-bytes encoded) (* 256 1024))))
    (condition-case nil
        (let* ((parsed
                (json-parse-string
                 encoded :object-type 'alist :array-type 'list
                 :null-object nil :false-object nil))
               (entities (chirp-get parsed "media_entities")))
          (and (chirp-object-p entities)
               (cl-remove-if-not
                (lambda (media)
                  (member (plist-get media :type)
                          '("video" "animated_gif")))
                (chirp--media-list-from-x (mapcar #'cdr entities)))))
      (json-parse-error nil))))

(defun chirp--media-item-from-x (object)
  "Return one Chirp media item decoded from X OBJECT."
  (let* ((type (chirp-first-nonblank (chirp-get object "type")))
         (url (chirp-first-nonblank
               (chirp-get object "media_url_https" "media_url" "url")))
         (preview-url (chirp-first-nonblank
                       (chirp-get object
                                  "preview_url" "previewUrl"
                                  "preview_image_url" "previewImageUrl"
                                  "thumbnail_url" "thumbnailUrl"
                                  "poster_url" "posterUrl"
                                  "media_url_https" "media_url")
                       (chirp-get-in object '("preview" "url"))
                       (chirp-get-in object '("thumbnail" "url"))
                       (chirp-get-in object '("poster" "url"))))
         (raw-variants
          (or (chirp-get object "variants")
              (chirp-get-in object '("video_info" "variants"))))
         (variants
          (and (listp raw-variants)
               (delq nil
                     (mapcar #'chirp--media-variant-from-x raw-variants))))
         (width (or (chirp-get object "width")
                    (chirp-get-in object '("original_info" "width"))
                    (chirp-get-in object '("sizes" "large" "w"))))
         (height (or (chirp-get object "height")
                     (chirp-get-in object '("original_info" "height"))
                     (chirp-get-in object '("sizes" "large" "h"))))
         (alt (chirp-first-nonblank
               (chirp-get object "altText" "alt_text" "ext_alt_text" "description"))))
    (when (and type url)
      (list :type type
            :url url
            :preview-url preview-url
            :variants variants
            :width width
            :height height
            :alt alt))))

(defun chirp--media-list-from-x (value)
  "Return Chirp media items decoded from X VALUE."
  (if (listp value)
      (delq nil (mapcar #'chirp--media-item-from-x value))
    nil))

;;;; Articles

(defun chirp--article-find-string (value keys &optional predicate)
  "Find a string below VALUE under KEYS that satisfies PREDICATE."
  (cond
   ((chirp-object-p value)
    (or (let ((candidate (apply #'chirp-get value keys)))
          (and (stringp candidate)
               (not (string-blank-p candidate))
               (or (null predicate) (funcall predicate candidate))
               candidate))
        (cl-loop for (_key . nested) in value
                 thereis (chirp--article-find-string nested keys predicate))))
   ((vectorp value)
    (cl-loop for nested across value
             thereis (chirp--article-find-string nested keys predicate)))
   ((listp value)
    (cl-loop for nested in value
             thereis (chirp--article-find-string nested keys predicate)))))

(defun chirp--article-image-url (value)
  "Return the first image URL found below VALUE."
  (chirp--article-find-string
   value
   '("original_img_url" "originalImgUrl" "original_url" "originalUrl"
     "media_url_https" "mediaUrlHttps" "media_url" "mediaUrl" "url"
     "src" "uri")
   (lambda (candidate)
     (let ((url (downcase candidate)))
       (or (string-prefix-p "https://pbs.twimg.com/" url)
           (string-match-p
            "\\.\\(?:jpe?g\\|png\\|gif\\|webp\\)\\(?:[?#].*\\)?\\'"
            url))))))

(defun chirp--article-entity-map (content-state)
  "Return CONTENT-STATE's Draft.js entity map as a string-keyed alist."
  (let ((value (chirp-get content-state "entityMap")))
    (cond
     ((chirp-object-p value)
      (mapcar (lambda (cell)
                (cons (format "%s" (car cell)) (cdr cell)))
              value))
     ((listp value)
      (cl-loop for item in value
               for key = (chirp-get item "key")
               for entity = (chirp-get item "value")
               when (and key entity)
               collect (cons (format "%s" key) entity))))))

(defun chirp--article-media-map (article)
  "Return media identifiers mapped to image URLs from ARTICLE."
  (cl-loop for media in (append
                         (when-let* ((cover (chirp-get article "cover_media")))
                           (list cover))
                         (chirp-get article "media_entities"))
           for url = (chirp--article-image-url media)
           when url
           append (cl-loop for key in '("media_id" "media_key" "id")
                           for identifier = (chirp-get media key)
                           when identifier
                           collect (cons (format "%s" identifier) url))))

(defun chirp--article-block-entity (range entity-map)
  "Return RANGE's Draft.js entity from ENTITY-MAP."
  (when-let* ((key (chirp-get range "key")))
    (cdr (assoc-string (format "%s" key) entity-map t))))

(defun chirp--article-render-text-block (block entity-map)
  "Render Draft.js text BLOCK using links from ENTITY-MAP."
  (let ((text (chirp-get block "text"))
        ranges)
    (when (stringp text)
      (dolist (range (chirp-get block "entityRanges"))
        (let* ((entity (chirp--article-block-entity range entity-map))
               (type (upcase (or (chirp-get entity "type") "")))
               (offset (chirp-get range "offset"))
               (length (chirp-get range "length"))
               (url (chirp-get-in entity '("data" "url"))))
          (when (and (equal type "LINK")
                     (integerp offset) (>= offset 0)
                     (integerp length) (> length 0)
                     (stringp url) (not (string-blank-p url)))
            (push (list offset length url) ranges))))
      (dolist (range (sort ranges (lambda (a b) (> (car a) (car b)))))
        (pcase-let ((`(,offset ,length ,url) range))
          (when (<= (+ offset length) (length text))
            (let ((label (substring text offset (+ offset length))))
              (setq text
                    (concat
                     (substring text 0 offset)
                     "[" (string-replace "]" "\\]"
                                         (string-replace "[" "\\[" label))
                     "](" (string-replace ")" "%29" url) ")"
                     (substring text (+ offset length))))))))
      text)))

(defun chirp--article-entity-images (entity media-map)
  "Return distinct (URL . CAPTION) images from ENTITY using MEDIA-MAP."
  (let ((default-caption
         (or (chirp--article-find-string
              entity '("caption" "alt" "alt_text" "altText" "title" "name"))
             ""))
        (seen (make-hash-table :test #'equal))
        images)
    (cl-labels ((add-image (url caption)
                  (when (and url (not (gethash url seen)))
                    (puthash url t seen)
                    (push (cons url (or caption default-caption)) images))))
      (add-image (chirp--article-image-url entity) default-caption)
      (dolist (media (chirp-get-in entity '("data" "mediaItems")))
        (let* ((media-id (chirp-get media "mediaId"))
               (url (or (chirp--article-image-url media)
                        (and media-id
                             (cdr (assoc-string (format "%s" media-id)
                                                media-map t)))))
               (caption (chirp--article-find-string
                         media '("caption" "alt" "alt_text" "altText"
                                 "title" "name"))))
          (add-image url caption))))
    (nreverse images)))

(defun chirp--article-atomic-parts (block entity-map media-map)
  "Render atomic Draft.js BLOCK using ENTITY-MAP and MEDIA-MAP."
  (cl-loop for range in (chirp-get block "entityRanges")
           for entity = (chirp--article-block-entity range entity-map)
           for type = (upcase (or (chirp-get entity "type") ""))
           for markdown = (chirp-get-in entity '("data" "markdown"))
           append
           (cond
            ((and (equal type "MARKDOWN")
                  (stringp markdown) (not (string-blank-p markdown)))
             (list (string-trim markdown)))
            (t
             (mapcar
              (lambda (image)
                (format "![%s](%s)"
                        (cdr image)
                        (string-replace ")" "%29" (car image))))
              (chirp--article-entity-images entity media-map))))))

(defun chirp--article-content-text (article)
  "Render ARTICLE's Draft.js content as lightweight Markdown."
  (let* ((content-state (chirp-get article "content_state"))
         (entity-map (chirp--article-entity-map content-state))
         (media-map (chirp--article-media-map article))
         (ordered-counter 0)
         parts)
    (dolist (block (chirp-get content-state "blocks"))
      (let ((type (or (chirp-get block "type") "unstyled")))
        (if (equal type "atomic")
            (progn
              (setq ordered-counter 0)
              (dolist (part (chirp--article-atomic-parts
                             block entity-map media-map))
                (push part parts)))
          (unless (equal type "ordered-list-item")
            (setq ordered-counter 0))
          (when-let* ((text (chirp-first-nonblank
                             (chirp--article-render-text-block
                              block entity-map))))
            (push (pcase type
                    ("header-one" (concat "# " text))
                    ("header-two" (concat "## " text))
                    ("header-three" (concat "### " text))
                    ("blockquote" (concat "> " text))
                    ("unordered-list-item" (concat "- " text))
                    ("ordered-list-item"
                     (setq ordered-counter (1+ ordered-counter))
                     (format "%d. %s" ordered-counter text))
                    ("code-block" (format "```\n%s\n```" text))
                    (_ text))
                  parts)))))
    (and parts (string-join (nreverse parts) "\n\n"))))

;;;; Tweets

(defun chirp--tweet-from-x (object)
  "Return a Chirp tweet decoded from X OBJECT, or nil."
  (let* ((result object)
         (wrapper (chirp--tweet-result result))
         (wrapper-legacy (chirp-get wrapper "legacy"))
         (raw-retweet (chirp-get-in
                       wrapper-legacy '("retweeted_status_result" "result")))
         (retweet (and raw-retweet
                       (or (chirp-get raw-retweet "tweet") raw-retweet)))
         (retweet-p (and retweet (chirp-tweet-like-p retweet)))
         (retweet-id
          (and retweet-p
               (chirp-first-nonblank
                (chirp-get wrapper "rest_id" "id_str" "id")
                (chirp-get wrapper-legacy "id_str"))))
         (retweeter (and retweet
                         (chirp--user-from-x
                          (chirp--extract-user-object wrapper))))
         (object (if retweet-p retweet wrapper))
         (legacy (chirp-get object "legacy"))
         (metrics (chirp-get object "metrics"))
         (author (chirp--extract-user-object object))
         (author-user (chirp--user-from-x author))
         (author-handle (plist-get author-user :handle))
         (id (chirp-first-nonblank
              (chirp-get object "rest_id" "id_str" "id")
              (chirp-get legacy "id_str")))
         (url (chirp-first-nonblank
               (chirp-get object "url")
               (chirp-get legacy "url")
               (and id author-handle
                    (format "https://x.com/%s/status/%s" author-handle id))
               (and id (format "https://x.com/i/status/%s" id))))
         (tweet-identity (list :id id :url url :author-handle author-handle))
         (source (chirp--tweet-source-text-and-entities object legacy))
         (raw-source (plist-get source :text))
         (source-entities (plist-get source :entities))
         (display-range
          (and (not (plist-get source :note-p))
               (chirp--tweet-display-text-range object legacy)))
         (reply-to-handle (let ((handle (chirp-first-nonblank
                                         (chirp-get object "inReplyToScreenName"
                                                    "in_reply_to_screen_name")
                                         (chirp-get legacy "in_reply_to_screen_name"))))
                            (and handle
                                 (string-remove-prefix "@" handle))))
         (reply-to-id (chirp-first-nonblank
                       (chirp-get object "inReplyToStatusId"
                                  "in_reply_to_status_id_str"
                                  "in_reply_to_status_id")
                       (chirp-get legacy "in_reply_to_status_id_str"
                                  "in_reply_to_status_id")))
         (visible-range
          (chirp--visible-text-range
           raw-source display-range
           (or reply-to-id reply-to-handle)))
         (visible-raw
          (chirp--substring
           (or raw-source "")
           (car visible-range)
           (cdr visible-range)))
         (visible-entities
          (chirp--rebase-text-entities
           source-entities
           (car visible-range)
           (cdr visible-range)))
         (full-text (chirp-clean-text raw-source))
         (quoted-tweet (chirp--quoted-tweet-from-x object))
         (timeline-context
          (pcase (or (chirp-get wrapper "timelineContext" "timeline_context")
                     (chirp-get object "timelineContext" "timeline_context"))
            ("related" 'related)
            (_ nil)))
         (all-urls (chirp-extract-tweet-urls object legacy))
         (media
          (or (chirp--media-list-from-x
               (or (chirp-get object "media")
                   (chirp-get-in object '("extended_entities" "media"))
                   (chirp-get-in legacy '("extended_entities" "media"))
                   (chirp-get-in legacy '("entities" "media"))))
              (chirp--unified-card-media-from-x object)))
         (url-context (list :tweet tweet-identity
                            :quoted-tweet quoted-tweet
                            :media media))
         (media-url-covered-p
          (and media
               (cl-some (lambda (candidate)
                          (or (chirp--media-url-p candidate media)
                              (chirp--tweet-permalink-p candidate tweet-identity)))
                        all-urls)))
         (covered-short-url-count
          (+ (length all-urls)
             (if (and media (not media-url-covered-p)) 1 0)))
         (walk-entities
          (if (>= covered-short-url-count
                  (chirp-short-url-count visible-raw))
              (append visible-entities
                      (chirp--uncovered-short-url-entities
                       visible-raw visible-entities))
            visible-entities))
         (emitted
          (chirp--emit-visible-text visible-raw walk-entities url-context))
         (display-text (car emitted))
         (text-entities (cdr emitted))
         (mentions
          (cl-remove-if-not
           (lambda (entity)
             (eq (plist-get entity :kind) 'mention))
           text-entities))
         (hashtags
          (delq nil
                (mapcar
                 (lambda (entity)
                   (and (eq (plist-get entity :kind) 'hashtag)
                        (plist-get entity :tag)))
                 text-entities)))
         (urls (chirp--filter-display-urls all-urls url-context))
         (link-card (chirp--link-card-from-x object urls))
         (article-result
          (chirp-get-in object '("article" "article_results" "result")))
         (article-title (chirp-first-nonblank
                         (chirp-get object "articleTitle" "article_title")
                         (chirp-get article-result "title")))
         (article-text-raw
          (chirp-first-nonblank
           (chirp-get object "articleText" "article_text")
           (chirp--article-content-text article-result)
           (chirp-get article-result "plain_text")))
         (article-text (and article-text-raw
                            (chirp-clean-text article-text-raw)))
         (retweeted-by
          (let ((handle (chirp-first-nonblank
                         (chirp-get wrapper "retweetedBy" "retweeted_by")
                         (plist-get retweeter :handle))))
            (and handle (string-remove-prefix "@" handle))))
         (retweeted-by-name
          (chirp-first-nonblank
           (chirp-get wrapper "retweetedByName" "retweeted_by_name")
           (plist-get retweeter :name)
           retweeted-by))
         (retweeted-p (chirp-boolean-value
                       (or (chirp-get object "retweeted" "isRetweeted")
                           (chirp-get-in object '("viewer" "retweeted"))
                           (chirp-get legacy "retweeted")
                           (chirp-get wrapper-legacy "retweeted"))))
         (liked-p (chirp-boolean-value
                   (or (chirp-get object "liked" "favorited" "isLiked" "isFavorited")
                       (chirp-get-in object '("viewer" "liked"))
                       (chirp-get-in object '("viewer" "favorited"))
                       (chirp-get legacy "liked" "favorited"))))
         (bookmarked-p (chirp-boolean-value
                        (or (chirp-get object "bookmarked" "isBookmarked")
                            (chirp-get-in object '("viewer" "bookmarked"))
                            (chirp-get legacy "bookmarked"))))
         (promoted-p (chirp-boolean-value
                      (or (chirp-get wrapper "isPromoted" "is_promoted" "promoted")
                          (chirp-get object "isPromoted" "is_promoted" "promoted")
                          (chirp-get-in wrapper '("itemContent" "promotedMetadata"))
                          (chirp-get wrapper "promotedMetadata"))))
         (reply-control-mode
          (chirp--tweet-reply-control-mode object))
         (reply-limited-p
          (or (chirp--tweet-reply-limited-p result)
              (chirp--tweet-reply-limited-p object)
              (equal (chirp-get legacy "limited_actions")
                     "limited_replies")))
         (edit-metadata (chirp--tweet-edit-metadata object)))
    (when (or id (not (string-empty-p display-text)))
      (list :kind 'tweet
            :id id
            :retweet-id retweet-id
            :text display-text
            :raw-text full-text
            :created-at (chirp-first-nonblank
                         (chirp-get object "createdAtLocal" "createdAtISO" "createdAt" "created_at")
                         (chirp-get legacy "created_at"))
            :url url
            :urls urls
            :link-card link-card
            :mentions mentions
            :hashtags hashtags
            :text-entities text-entities
            :conversation-id (chirp-first-nonblank
                              (chirp-get object "conversationId" "conversation_id")
                              (chirp-get legacy "conversation_id_str"))
            :timeline-context timeline-context
            :reply-to-id reply-to-id
            :reply-to-handle reply-to-handle
            :reply-control-mode reply-control-mode
            :reply-limited-p reply-limited-p
            :edit-history-ids
            (plist-get edit-metadata :edit-history-ids)
            :edit-history-initial-id
            (plist-get edit-metadata :edit-history-initial-id)
            :edited-p (plist-get edit-metadata :edited-p)
            :retweeted-by retweeted-by
            :retweeted-by-name retweeted-by-name
            :author-name (plist-get author-user :name)
            :author-handle author-handle
            :author-avatar-url (plist-get author-user :avatar-url)
            :quoted-tweet quoted-tweet
            :article-title article-title
            :article-text article-text
            :promoted-p promoted-p
            :media media
            :retweeted-p retweeted-p
            :liked-p liked-p
            :bookmarked-p bookmarked-p
            :translation nil
            :translation-language nil
            :reply-count (chirp--count-value
                          (chirp-get object "reply_count")
                          (chirp-get metrics "replies")
                          (chirp-get legacy "reply_count"))
            :retweet-count (chirp--count-value
                            (chirp-get object "retweet_count")
                            (chirp-get metrics "retweets")
                            (chirp-get legacy "retweet_count"))
            :like-count (chirp--count-value
                         (chirp-get object "favorite_count" "like_count")
                         (chirp-get metrics "likes")
                         (chirp-get legacy "favorite_count"))
            :quote-count (chirp--count-value
                          (chirp-get object "quote_count")
                          (chirp-get metrics "quotes")
                          (chirp-get legacy "quote_count"))
            :bookmark-count (chirp--count-value
                             (chirp-get object "bookmark_count")
                             (chirp-get metrics "bookmarks")
                             (chirp-get legacy "bookmark_count"))
            :view-count (chirp--count-value
                         (chirp-get object "view_count" "views")
                         (chirp-get metrics "views"))
            :raw wrapper))))

;;; Collections

;;;; Visibility

(defun chirp-tweet-visible-p (tweet)
  "Return non-nil when TWEET should be shown in Chirp."
  (or (not chirp-hide-promoted-posts)
      (not (plist-get tweet :promoted-p))))

(defun chirp--top-level-tweets-from-x (value)
  "Return visible Chirp tweets decoded from top-level X VALUE."
  (cond
   ((chirp-tweet-like-p value)
    (let ((tweet
           (chirp-apply-tweet-state-overrides
            (chirp--tweet-from-x value))))
      (if (and tweet
               (chirp-tweet-visible-p tweet))
          (list tweet)
        nil)))
   ((listp value)
    (delq nil
          (mapcar (lambda (item)
                    (when (chirp-tweet-like-p item)
                      (let ((tweet
                             (chirp-apply-tweet-state-overrides
                              (chirp--tweet-from-x item))))
                        (when (and tweet
                                   (chirp-tweet-visible-p tweet))
                          tweet))))
                  value)))
   (t nil)))



(provide 'chirp-core)

;;; chirp-core.el ends here
