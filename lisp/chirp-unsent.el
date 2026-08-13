;;; chirp-unsent.el --- Unsent draft and scheduled post lists -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Present X-server drafts and scheduled posts in a tabulated list.  Marks
;; follow Buffer Menu conventions: `d' flags deletion and `x' executes.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'tabulated-list)
(require 'appkit-evil)
(require 'chirp-core)
(require 'chirp-backend)
(require 'chirp-actions)

(defconst chirp-unsent--mark-char ?>
  "Character used to mark an unsent row.")

(defconst chirp-unsent--delete-char ?D
  "Character used to flag an unsent row for deletion.")

(defvar-local chirp-unsent-kind 'draft
  "Unsent collection shown in the current buffer.

Either `draft' or `scheduled'.")

(defvar-local chirp-unsent-entries nil
  "Normalized unsent entries last rendered in the current buffer.")

(defvar chirp-unsent-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") #'chirp-unsent-open)
    (define-key map (kbd "g") #'chirp-unsent-refresh)
    (define-key map (kbd "m") #'chirp-unsent-mark)
    (define-key map (kbd "u") #'chirp-unsent-unmark)
    (define-key map (kbd "U") #'chirp-unsent-unmark-all)
    (define-key map (kbd "d") #'chirp-unsent-flag-delete)
    (define-key map (kbd "x") #'chirp-unsent-execute)
    (define-key map (kbd "TAB") #'chirp-unsent-toggle-kind)
    (define-key map (kbd "q") #'chirp-quit-current-buffer)
    map)
  "Keymap for `chirp-unsent-mode'.")

(defun chirp-unsent--when-label (entry)
  "Return the When column label for ENTRY."
  (if-let* ((execute-at (plist-get entry :execute-at)))
      (format-time-string "%Y-%m-%d %H:%M" execute-at)
    (if (eq (plist-get entry :kind) 'scheduled)
        "Scheduled"
      "Draft")))

(defun chirp-unsent--preview (entry)
  "Return the Text column value for ENTRY."
  (let* ((text (string-replace "\n" " " (or (car (plist-get entry :texts)) "")))
         (media (or (plist-get entry :media-count) 0))
         (prefix (if (> media 0)
                     (format "[%d img] " media)
                   "")))
    (truncate-string-to-width (concat prefix text) 80 nil nil "…")))

(defun chirp-unsent--row (entry)
  "Return a tabulated-list row for ENTRY."
  (list (plist-get entry :id)
        (vector " "
                (format "%d" (length (plist-get entry :texts)))
                (chirp-unsent--preview entry)
                (chirp-unsent--when-label entry))))

(defun chirp-unsent--title ()
  "Return the buffer title for the current unsent collection."
  (if (eq chirp-unsent-kind 'scheduled)
      "Scheduled"
    "Drafts"))

(define-derived-mode chirp-unsent-mode tabulated-list-mode "Chirp-Unsent"
  "Major mode for X drafts and scheduled posts."
  (setq-local truncate-lines t)
  (setq-local tabulated-list-format
              [("C" 1 nil :pad-right 1)
               ("Posts" 5 t :right-align t)
               ("Text" 80 t)
               ("When" 16 t :right-align t)])
  (setq-local tabulated-list-padding 0)
  (setq-local tabulated-list-sort-key nil)
  (setq-local mode-line-process
              '((:eval (chirp--mode-line-status-string))))
  (tabulated-list-init-header)
  (appkit-evil-normalize-keymaps))

(defun chirp-unsent--setup-evil ()
  "Install optional Evil bindings for unsent draft lists."
  (when appkit-evil-enable-integration
    (when (and (featurep 'evil)
               (fboundp 'evil-set-initial-state))
      (evil-set-initial-state 'chirp-unsent-mode 'normal))
    (appkit-evil-define-readonly-keys 'chirp-unsent-mode-map)
    (appkit-evil-define-keys '(normal motion) 'chirp-unsent-mode-map
      (kbd "RET") #'chirp-unsent-open
      (kbd "g r") #'chirp-unsent-refresh
      (kbd "m") #'chirp-unsent-mark
      (kbd "u") #'chirp-unsent-unmark
      (kbd "U") #'chirp-unsent-unmark-all
      (kbd "d") #'chirp-unsent-flag-delete
      (kbd "x") #'chirp-unsent-execute
      (kbd "TAB") #'chirp-unsent-toggle-kind)))

(chirp-unsent--setup-evil)

(defun chirp-unsent--buffer ()
  "Return the reusable unsent list buffer."
  (or (cl-find-if (lambda (buffer)
                    (with-current-buffer buffer
                      (derived-mode-p 'chirp-unsent-mode)))
                  (buffer-list))
      (generate-new-buffer (chirp--format-buffer-name "Drafts"))))

(defun chirp-unsent--apply-entries (entries)
  "Render ENTRIES in the current unsent buffer."
  (setq-local chirp-unsent-entries entries)
  (setq-local tabulated-list-entries
              (mapcar #'chirp-unsent--row entries))
  (tabulated-list-print t)
  (chirp--apply-buffer-name (current-buffer) (chirp-unsent--title)))

(defun chirp-unsent-refresh ()
  "Reload the current unsent collection from X."
  (interactive)
  (unless (derived-mode-p 'chirp-unsent-mode)
    (user-error "Not in a Chirp unsent buffer"))
  (let ((buffer (current-buffer))
        (kind chirp-unsent-kind)
        (token (chirp-begin-request (current-buffer))))
    (chirp-set-status buffer (format "Loading %s..." (chirp-unsent--title)))
    (chirp-backend-fetch-unsent
     kind
     (lambda (entries _envelope)
       (when (chirp-request-current-p buffer token)
         (with-current-buffer buffer
           (chirp-clear-status buffer)
           (chirp-unsent--apply-entries entries)
           (message "%s: %d" (chirp-unsent--title) (length entries)))))
     (lambda (message)
       (when (chirp-request-current-p buffer token)
         (chirp-clear-status buffer)
         (chirp-actions--show-error message))))))

(defun chirp-unsent-open-kind (kind)
  "Open the unsent list for KIND.

KIND is `draft' or `scheduled'."
  (unless (memq kind '(draft scheduled))
    (error "Unsent kind is invalid: %S" kind))
  (let ((buffer (chirp-unsent--buffer)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'chirp-unsent-mode)
        (chirp-unsent-mode))
      (setq-local chirp-unsent-kind kind)
      (chirp--apply-buffer-name buffer (chirp-unsent--title))
      (chirp-unsent-refresh))
    (pop-to-buffer buffer)
    buffer))

;;;###autoload
(defun chirp-unsent-drafts ()
  "Open the authenticated account's X drafts."
  (interactive)
  (chirp-unsent-open-kind 'draft))

;;;###autoload
(defun chirp-unsent-scheduled ()
  "Open the authenticated account's scheduled posts."
  (interactive)
  (chirp-unsent-open-kind 'scheduled))

(defun chirp-unsent-toggle-kind ()
  "Switch the current unsent buffer between drafts and scheduled posts."
  (interactive)
  (unless (derived-mode-p 'chirp-unsent-mode)
    (user-error "Not in a Chirp unsent buffer"))
  (chirp-unsent-open-kind
   (if (eq chirp-unsent-kind 'scheduled) 'draft 'scheduled)))

(defun chirp-unsent--entry-at-point ()
  "Return the normalized unsent entry at point."
  (let ((id (tabulated-list-get-id)))
    (or (cl-find-if (lambda (entry)
                      (equal (plist-get entry :id) id))
                    chirp-unsent-entries)
        (user-error "No unsent post at point"))))

(defun chirp-unsent-open ()
  "Open the unsent post at point in a compose buffer."
  (interactive)
  (chirp-compose-open-unsent (chirp-unsent--entry-at-point)))

(defun chirp-unsent--set-mark (char)
  "Put CHAR on the current unsent row and move down."
  (unless (tabulated-list-get-id)
    (user-error "No unsent post at point"))
  (tabulated-list-set-col 0 (char-to-string char) t)
  (forward-line 1))

(defun chirp-unsent-mark ()
  "Mark the unsent post at point."
  (interactive)
  (chirp-unsent--set-mark chirp-unsent--mark-char))

(defun chirp-unsent-flag-delete ()
  "Flag the unsent post at point for deletion."
  (interactive)
  (chirp-unsent--set-mark chirp-unsent--delete-char))

(defun chirp-unsent-unmark ()
  "Remove the mark from the unsent post at point."
  (interactive)
  (unless (tabulated-list-get-id)
    (user-error "No unsent post at point"))
  (tabulated-list-set-col 0 " " t)
  (forward-line 1))

(defun chirp-unsent-unmark-all ()
  "Remove every mark in the current unsent list."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (when (tabulated-list-get-id)
        (tabulated-list-set-col 0 " " t))
      (forward-line 1))))

(defun chirp-unsent--flagged-ids (char)
  "Return IDs whose first column is CHAR."
  (let (ids)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when-let* ((entry (tabulated-list-get-entry))
                    (mark (aref entry 0))
                    ((and (stringp mark)
                          (not (string-empty-p mark))
                          (eq (aref mark 0) char))))
          (push (tabulated-list-get-id) ids))
        (forward-line 1)))
    (nreverse ids)))

(defun chirp-unsent--delete-ids (kind ids buffer)
  "Delete IDS of KIND sequentially, then refresh BUFFER."
  (if (null ids)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (chirp-unsent-refresh)))
    (chirp-backend-delete-unsent
     kind (car ids)
     (lambda (_payload _envelope)
       (chirp-unsent--delete-ids kind (cdr ids) buffer))
     (lambda (message)
       (when (buffer-live-p buffer)
         (chirp-clear-status buffer))
       (chirp-actions--show-error message)))))

(defun chirp-unsent-execute ()
  "Delete every unsent post flagged with `chirp-unsent-flag-delete'."
  (interactive)
  (unless (derived-mode-p 'chirp-unsent-mode)
    (user-error "Not in a Chirp unsent buffer"))
  (let* ((ids (chirp-unsent--flagged-ids chirp-unsent--delete-char))
         (kind chirp-unsent-kind)
         (label (if (eq kind 'scheduled) "scheduled post" "draft"))
         (buffer (current-buffer)))
    (when (null ids)
      (user-error "No unsent posts flagged for deletion"))
    (unless (yes-or-no-p
             (format "Delete %d %s? "
                     (length ids)
                     (if (= (length ids) 1)
                         label
                       (if (eq kind 'scheduled)
                           "scheduled posts"
                         "drafts"))))
      (user-error "Delete canceled"))
    (chirp-set-status buffer "Deleting...")
    (chirp-unsent--delete-ids kind ids buffer)))

(provide 'chirp-unsent)

;;; chirp-unsent.el ends here
