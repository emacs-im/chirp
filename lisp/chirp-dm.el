;;; chirp-dm.el --- XChat direct messages for Chirp -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Public unlock and inbox entry point for the Appkit-owned XChat DM subsystem.

;;; Code:

(require 'subr-x)
(require 'appkit-core)
(require 'chirp-backend)
(require 'chirp-core)
(require 'chirp-dm-inbox)
(require 'chirp-dm-live)

(declare-function chirp-xchat-native-load "chirp-xchat-native" ())
(declare-function chirp-xchat-native-recovery-active-p
                  "chirp-xchat-native" ())
(declare-function chirp-xchat-native-unlocked-p
                  "chirp-xchat-native" ())
(declare-function chirp-xchat-native-recover
                  "chirp-xchat-native" (pin input callback &rest keys))
(declare-function chirp-xchat-native-discard-recovery-input
                  "chirp-xchat-native" (input))
(declare-function chirp-xchat-native-cancel-recovery
                  "chirp-xchat-native" ())

(defun chirp-dm--unlock-result (result)
  "Report XChat unlock RESULT and open the inbox on success."
  (pcase (plist-get result :status)
    ('unlocked
     (message "XChat keys unlocked for this Chirp session")
     (chirp-dm--open-unlocked-inbox))
    ('incorrect-pin
     (if-let* ((remaining (plist-get result :guesses-remaining)))
         (display-warning
          'chirp
          (format "Incorrect XChat PIN; %d attempt%s reported remaining"
                  remaining (if (= remaining 1) "" "s"))
          :warning)
       (display-warning 'chirp "Incorrect XChat PIN" :warning)))
    ('not-registered
     (display-warning 'chirp "No XChat keys are registered for this PIN" :warning))
    ('invalid-auth
     (display-warning 'chirp "XChat Juicebox authorization was rejected" :warning))
    ('upgrade-required
     (display-warning 'chirp "The XChat cryptography module must be upgraded" :error))
    ('rate-limited
     (display-warning 'chirp "XChat key recovery is rate-limited" :warning))
    ('no-tokens
     (display-warning 'chirp "XChat recovery configuration has no realm tokens" :error))
    ('key-reconstruction-failed
     (display-warning 'chirp "XChat returned invalid recovered key material" :error))
    ('registered-key-mismatch
     (display-warning 'chirp "Recovered XChat keys do not match this account" :error))
    ('ambiguous-registered-key
     (display-warning 'chirp "Recovered XChat keys match multiple versions" :error))
    ('assertion-failed
     (display-warning 'chirp "The Juicebox recovery protocol rejected its state" :error))
    ((or 'uncertain 'cancelled)
     (display-warning
      'chirp
      "XChat recovery outcome is uncertain; do not retry automatically"
      :warning))
    (_
     (display-warning 'chirp "XChat key recovery failed" :error))))

(defun chirp-dm--unlock-error (message)
  "Report XChat unlock failure MESSAGE without placing it in a DM buffer."
  (display-warning 'chirp message :error))

(defun chirp-dm--remember-user (app user)
  "Retain authenticated XChat USER in live APP and return its ID."
  (when-let* ((user-id (plist-get user :id))
              ((stringp user-id))
              ((string-match-p "\\`[0-9]+\\'" user-id))
              ((appkit-app-live-p app)))
    (let ((state (appkit-app-state app)))
      (setf (chirp--session-xchat-user state) (copy-tree user)
            (chirp--session-xchat-user-id state) user-id))
    user-id))

(defun chirp-dm--prompt-and-unlock (app input)
  "Prompt using INPUT and recover XChat keys in APP."
  (require 'chirp-xchat-native)
  (let (pin)
    (unwind-protect
        (when (appkit-app-live-p app)
          (setq pin (read-passwd "XChat 4-digit PIN: "))
          (if (not (and (= (length pin) 4)
                        (string-match-p "\\`[0-9]+\\'" pin)))
              (display-warning
               'chirp "XChat PIN must contain exactly four digits" :warning)
            (when (chirp-xchat-native-recover
                   pin input
                   (lambda (result)
                     (chirp-dm--unlock-result result))
                   :errback #'chirp-dm--unlock-error)
              (message "Recovering XChat keys..."))))
      (when (stringp pin)
        (clear-string pin))
      (chirp-xchat-native-discard-recovery-input input))))

(defun chirp-dm--start-unlock ()
  "Start one explicit XChat unlock before opening its inbox."
  (let ((app (chirp-app)))
    (message "Fetching XChat key configuration...")
    (chirp-backend-whoami
     (lambda (user _envelope)
       (if-let* ((user-id (chirp-dm--remember-user app user)))
           (progn
             (chirp-backend-dm-recovery-input
              user-id
              (lambda (input _response-envelope)
                (chirp-dm--prompt-and-unlock app input))
              :errback #'chirp-dm--unlock-error
              :owner app))
         (chirp-dm--unlock-error
          "Authenticated X profile has no user identity")))
     #'chirp-dm--unlock-error)))

;;;###autoload
(defun chirp-dm-cancel-unlock ()
  "Cancel the active XChat recovery without automatically retrying it."
  (interactive)
  (require 'chirp-xchat-native)
  (if (chirp-xchat-native-cancel-recovery)
      (message "Canceling XChat recovery; its remote outcome may be uncertain")
    (user-error "No XChat key recovery is active")))

(defun chirp-dm--open-unlocked-inbox ()
  "Open an unlocked inbox after ensuring the current XChat user identity."
  (let* ((app (chirp-app))
         (state (appkit-app-state app))
         (user-id (chirp--session-xchat-user-id state))
         (user (chirp--session-xchat-user state)))
    (if (and (stringp user-id)
             (string-match-p "\\`[0-9]+\\'" user-id)
             (equal (plist-get user :id) user-id))
        (progn
          (chirp-dm-live-ensure)
          (chirp-dm-inbox-open))
      (message "Resolving the authenticated XChat identity...")
      (chirp-backend-whoami
       (lambda (resolved _envelope)
         (if (chirp-dm--remember-user app resolved)
             (progn
               (chirp-dm-live-ensure)
               (chirp-dm-inbox-open))
           (chirp-dm--unlock-error
            "Authenticated X profile has no user identity")))
       #'chirp-dm--unlock-error))))

(defun chirp-dm-open-inbox ()
  "Unlock XChat, then open a fresh direct-message inbox."
  (require 'chirp-xchat-native)
  (chirp-xchat-native-load)
  (cond
   ((chirp-xchat-native-unlocked-p)
    (chirp-dm--open-unlocked-inbox))
   ((chirp-xchat-native-recovery-active-p)
    (user-error "XChat key recovery is already active"))
   (t
    (chirp-dm--start-unlock)
    nil)))

(provide 'chirp-dm)

;;; chirp-dm.el ends here
