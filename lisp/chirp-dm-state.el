;;; chirp-dm-state.el --- Canonical XChat conversation state -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Session-owned normalized XChat conversations and cross-view publication.
;; View-local history windows, requests, and composers remain outside this
;; module.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'appkit-core)
(require 'appkit-view)
(require 'chirp-core)
(require 'chirp-xchat)

(defun chirp-dm-state--table ()
  "Return the current Chirp session's canonical conversation table."
  (or (chirp--session-dm-conversations (chirp--session))
      (error "Chirp session has no direct-message conversation store")))

(defun chirp-dm-state--event-id (event)
  "Return normalized EVENT's stable identity."
  (or (plist-get event :sequence-id)
      (plist-get event :id)
      (error "XChat event has no stable identity")))

(defun chirp-dm-state-merge-events (current fetched)
  "Merge ordered XChat event lists CURRENT and FETCHED by stable identity.

When both lists contain one event, preserve CURRENT so verified plaintext is
not replaced by a later encrypted snapshot."
  (let (merged)
    (while (and current fetched)
      (let* ((left (car current))
             (right (car fetched))
             (left-id (chirp-dm-state--event-id left))
             (right-id (chirp-dm-state--event-id right)))
        (cond
         ((equal left-id right-id)
          (push left merged)
          (setq current (cdr current)
                fetched (cdr fetched)))
         ((chirp-xchat-event-before-p left right)
          (push left merged)
          (setq current (cdr current)))
         (t
          (push right merged)
          (setq fetched (cdr fetched))))))
    (nconc (nreverse merged) current fetched)))

(defun chirp-dm-state--refresh-derived-fields (conversation)
  "Refresh event-derived fields on canonical CONVERSATION."
  (let ((latest (car (last (plist-get conversation :events)))))
    (setf (plist-get conversation :latest-event) latest
          (plist-get conversation :preview)
          (and latest (chirp-xchat-event-label latest))
          (plist-get conversation :updated-at-msec)
          (and latest (plist-get latest :created-at-msec))))
  conversation)

(defun chirp-dm-state-set-events (conversation events)
  "Replace canonical CONVERSATION's ordered EVENTS and derived fields."
  (setf (plist-get conversation :events) events)
  (chirp-dm-state--refresh-derived-fields conversation))

(defun chirp-dm-state-accept-live-event (event)
  "Merge normalized websocket EVENT into its canonical conversation.

Return the canonical conversation, or nil when its metadata has not been
loaded.  Existing inboxes promote the changed conversation to the recent edge."
  (let* ((conversation-id (and (listp event)
                               (plist-get event :conversation-id)))
         (conversation
          (and (stringp conversation-id)
               (gethash conversation-id (chirp-dm-state--table)))))
    (when conversation
      (chirp-dm-state--event-id event)
      (chirp-dm-state-set-events
       conversation
       (chirp-dm-state-merge-events
        (plist-get conversation :events) (list event)))
      (when (appkit-app-live-p chirp--app)
        (maphash
         (lambda (_id view)
           (when (appkit-view-live-p view)
             (let ((state (appkit-view-state view)))
               (when (eq (plist-get state :type) 'dm-inbox)
                 (setf (plist-get state :items)
                       (cons conversation
                             (delq conversation
                                   (copy-sequence
                                    (plist-get state :items)))))))))
         (appkit-app-view-registry chirp--app)))
      (chirp-dm-state-publish conversation)
      conversation)))

(defun chirp-dm-state--copy-field (target source property)
  "Copy PROPERTY from SOURCE to TARGET when SOURCE carries it."
  (when (plist-member source property)
    (setf (plist-get target property)
          (copy-tree (plist-get source property)))))

(cl-defun chirp-dm-state-merge-snapshot
    (conversation snapshot &key events)
  "Merge normalized SNAPSHOT into canonical CONVERSATION.

EVENTS, when non-nil, is the already continuity-checked ordered event list.
Otherwise preserve canonical events while adding events from SNAPSHOT."
  (dolist (property '(:type :title :participants :muted-p
                      :message-request-p :has-more :older-cursor))
    (chirp-dm-state--copy-field conversation snapshot property))
  (chirp-dm-state-set-events
   conversation
   (or events
       (chirp-dm-state-merge-events
        (plist-get conversation :events)
        (plist-get snapshot :events))))
  conversation)

(defun chirp-dm-state-acquire (snapshot)
  "Return the session-owned canonical conversation for normalized SNAPSHOT."
  (let ((id (and (listp snapshot) (plist-get snapshot :id))))
    (unless (and (stringp id) (not (string-empty-p id)))
      (error "XChat conversation has no canonical identity"))
    (let* ((table (chirp-dm-state--table))
           (conversation (gethash id table)))
      (cond
       ((eq conversation snapshot))
       (conversation
        (chirp-dm-state-merge-snapshot conversation snapshot))
       (t
        (setq conversation (copy-tree snapshot))
        (chirp-dm-state--refresh-derived-fields conversation)
        (puthash id conversation table)))
      conversation)))

(defun chirp-dm-state-publish (conversation)
  "Synchronize every live DM view that references canonical CONVERSATION."
  (when (appkit-app-live-p chirp--app)
    (maphash
     (lambda (_id view)
       (when (appkit-view-live-p view)
         (let ((state (appkit-view-state view)))
           (pcase (plist-get state :type)
             ('dm-conversation
              (when (eq (plist-get state :conversation) conversation)
                (appkit-request-sync
                 view :structure t :parts '(frame timeline) :position t)))
             ('dm-inbox
              (when (memq conversation (plist-get state :items))
                (appkit-request-sync
                 view :structure t :part 'entries :position t)))))))
     (appkit-app-view-registry chirp--app))))

(provide 'chirp-dm-state)

;;; chirp-dm-state.el ends here
