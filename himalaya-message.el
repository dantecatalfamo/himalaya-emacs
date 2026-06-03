;;; himalaya-message.el --- Message management of email client Himalaya CLI  -*- lexical-binding: t -*-

;; Copyright (C) 2021 Dante Catalfamo
;; Copyright (C) 2022-2026 soywod <clement.douin@posteo.net>

;; Author: Dante Catalfamo
;;      soywod <clement.douin@posteo.net>
;; Maintainer: soywod <clement.douin@posteo.net>
;;      Dante Catalfamo
;; Version: 2.0
;; Package-Requires: ((emacs "27.1"))
;; URL: https://github.com/dantecatalfamo/himalaya-emacs
;; Keywords: mail comm

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; Interface for the email client Himalaya CLI
;; <https://github.com/pimalaya/himalaya>

;;; Code:

(require 'subr-x)
(require 'mailheader)
(require 'message)
(require 'mml)
(require 'rfc2047)
(require 'himalaya-process)
(require 'himalaya-account)
(require 'himalaya-mailbox)
(require 'himalaya-envelope-mark)
(require 'himalaya-flag)
(require 'himalaya-attachment)

(defcustom himalaya-from nil
  "Default value used to pre-fill the `From:' header when composing
a new message. When nil, the header is inserted empty and left for
the user to fill in."
  :type '(choice (const :tag "Unset" nil) string)
  :group 'himalaya)

(defun himalaya--read-message-raw (id callback)
  "Fetch the raw RFC 5322 bytes of the message matching envelope
ID in the current mailbox of the current account, then pass them
verbatim to CALLBACK."
  (message "Reading raw message %s…" id)
  (himalaya--run-raw
   callback
   nil
   "message"
   "read"
   "--raw"
   (when himalaya-account (list "--account" himalaya-account))
   (when himalaya-mailbox (list "--mailbox" himalaya-mailbox))
   (format "%s" id)))

(defun himalaya--extract-headers (msg)
  "Extract headers from the RFC 5322 string MSG."
  (with-temp-buffer
    (insert msg)
    (goto-char (point-min))
    (mail-header-extract-no-properties)))

(defun himalaya--decode-header-value (value)
  "Decode VALUE, an RFC 2047 encoded header string."
  (when value
    (rfc2047-decode-string value)))

(defun himalaya--render-message (raw)
  "Render RAW (an RFC 5322 message) in the current buffer with
headers at the top followed by a blank line and the decoded
text/plain body. Falls back to the raw bytes when no text body
can be decoded."
  (let* ((headers (himalaya--extract-headers raw))
         (body (himalaya--extract-text-body raw)))
    (dolist (key '(from to cc subject date))
      (when-let ((value (alist-get key headers)))
        (insert (capitalize (symbol-name key))
                ": "
                (himalaya--decode-header-value value)
                "\n")))
    (insert "\n")
    (insert (or body raw))))

(defun himalaya--extract-text-body (raw)
  "Return the decoded text/plain body of RAW, or nil when it
cannot be located. Uses Gnus' mm-decode when available."
  (require 'mm-decode nil t)
  (require 'mm-bodies nil t)
  (condition-case _err
      (with-temp-buffer
        (insert raw)
        (goto-char (point-min))
        (let* ((handle (mm-dissect-buffer t)))
          (cond
           ((null handle) nil)
           ((stringp (car-safe handle))
            (himalaya--collect-text-part handle))
           (t (himalaya--collect-text-part handle)))))
    (error nil)))

(defun himalaya--collect-text-part (handle)
  "Return the text/plain payload reachable from HANDLE, or nil."
  (cond
   ((null handle) nil)
   ((and (consp handle) (bufferp (car handle)))
    (let ((type (mm-handle-media-type handle)))
      (when (and type (string-prefix-p "text/" type))
        (with-current-buffer (mm-handle-buffer handle)
          (mm-decode-string (buffer-string)
                            (mm-handle-encoding handle))))))
   ((consp handle)
    (let (text)
      (dolist (part (if (stringp (car handle)) (cdr handle) handle))
        (unless text (setq text (himalaya--collect-text-part part))))
      text))))

(defun himalaya--copy-messages (ids target callback)
  "Copy message(s) matching envelope IDS from the current mailbox
of the current account to TARGET mailbox."
  (message "Copying message(s) to %s…" target)
  (himalaya--run
   callback
   nil
   "message"
   "copy"
   (when himalaya-account (list "--account" himalaya-account))
   (when himalaya-mailbox (list "--from" himalaya-mailbox))
   "--to" target
   ids))

(defun himalaya--move-messages (ids target callback)
  "Move message(s) matching envelope IDS from the current mailbox
of the current account to TARGET mailbox."
  (message "Moving message(s) to %s…" target)
  (himalaya--run
   callback
   nil
   "message"
   "move"
   (when himalaya-account (list "--account" himalaya-account))
   (when himalaya-mailbox (list "--from" himalaya-mailbox))
   "--to" target
   ids))

(defun himalaya--send-message (raw callback)
  "Send RAW (an RFC 5322 message string) through the current
account and call CALLBACK with the response."
  (message "Sending message…")
  (himalaya--run
   callback
   raw
   "message"
   "send"
   (when himalaya-account (list "--account" himalaya-account))))

(defun himalaya--buffer-to-mime ()
  "Compile MML directives in the current buffer into RFC 5322
bytes. Operates on a temporary copy so the source buffer is left
untouched."
  (let ((source (buffer-string)))
    (with-temp-buffer
      (insert source)
      (mml-to-mime)
      (buffer-string))))

(defun himalaya--read-current-message (&optional pre-hook)
  "Fetch the raw RFC 5322 message for the current envelope id,
render it in a new buffer in `himalaya-read-message-mode' and
focus that buffer. When PRE-HOOK is non-nil, call it before
switching."
  (himalaya--read-message-raw
   himalaya-id
   (lambda (raw)
     (when pre-hook (funcall pre-hook))
     (let* ((headers (himalaya--extract-headers raw))
            (subject (himalaya--decode-header-value (alist-get 'subject headers))))
       (switch-to-buffer (format "*%s*" (or subject "Himalaya Message")))
       (let ((inhibit-read-only t))
         (erase-buffer)
         (himalaya--render-message raw)
         (set-buffer-modified-p nil)
         (himalaya-read-message-mode)
         (goto-char (point-min))
         (setq buffer-read-only t)
         (setq himalaya-subject subject))))))

(defun himalaya--read-current-message-raw (&optional pre-hook)
  "Display the raw RFC 5322 bytes of the current envelope in a
buffer in `himalaya-read-message-raw-mode'. When PRE-HOOK is
non-nil, call it before switching."
  (himalaya--read-message-raw
   himalaya-id
   (lambda (raw)
     (when pre-hook (funcall pre-hook))
     (let* ((headers (himalaya--extract-headers raw))
            (subject (himalaya--decode-header-value (alist-get 'subject headers))))
       (switch-to-buffer (format "*Raw: %s*" (or subject "Himalaya Message")))
       (let ((inhibit-read-only t))
         (erase-buffer)
         (insert raw)
         (set-buffer-modified-p nil)
         (himalaya-read-message-raw-mode)
         (goto-char (point-min))
         (setq himalaya-subject subject))))))

(defun himalaya-read-current-message-plain ()
  "Read the message at the current envelope id in the current
mailbox of the current account."
  (interactive)
  (himalaya--read-current-message #'kill-current-buffer))

(defun himalaya-read-current-message-raw ()
  "Read the raw RFC 5322 bytes of the message at the current
envelope id."
  (interactive)
  (himalaya--read-current-message-raw #'kill-current-buffer))

(defun himalaya-read-message-at-point ()
  "Pick the envelope at point and read its associated message."
  (interactive)
  (setq himalaya-id (tabulated-list-get-id))
  (himalaya--read-current-message))

(defun himalaya--quote-body (text)
  "Return TEXT with each line prefixed by `> '. Falls back to an
empty string when TEXT is nil."
  (if (or (null text) (string-empty-p text))
      ""
    (mapconcat (lambda (line) (concat "> " line))
               (split-string text "\n")
               "\n")))

(defun himalaya--insert-skeleton (from to subject &optional in-reply-to references)
  "Insert a minimal RFC 5322 / MML compose skeleton in the current
buffer. The MML body delimiter (an empty line) follows the
headers."
  (insert "From: " (or from "") "\n")
  (insert "To: " (or to "") "\n")
  (insert "Subject: " (or subject "") "\n")
  (when in-reply-to
    (insert "In-Reply-To: " in-reply-to "\n"))
  (when references
    (insert "References: " references "\n"))
  (insert "\n"))

(defun himalaya--start-compose (buffer-name &optional from to subject body in-reply-to references)
  "Open a `message-mode' compose buffer named BUFFER-NAME and
pre-populate it with the given header values and BODY."
  (switch-to-buffer (generate-new-buffer buffer-name))
  (himalaya--insert-skeleton from to subject in-reply-to references)
  (when body (insert body))
  (himalaya-message-write-mode)
  (goto-char (point-min))
  (when (search-forward "To: " nil t)
    (end-of-line))
  (set-buffer-modified-p nil))

(defun himalaya-write-new-message ()
  "Compose a new message in a `message-mode' buffer."
  (interactive)
  (setq himalaya-reply nil)
  (himalaya--start-compose "*Himalaya New Message*" himalaya-from "" ""))

(defun himalaya--reply-buffer (raw &optional reply-all)
  "Open a reply buffer for the source message in RAW. When
REPLY-ALL is non-nil, address every recipient (Cc included)."
  (let* ((headers (himalaya--extract-headers raw))
         (subject (himalaya--decode-header-value (alist-get 'subject headers)))
         (re-subject (if (and subject (string-match-p "^[Rr][Ee]:" subject))
                         subject
                       (concat "Re: " (or subject ""))))
         (reply-to (or (himalaya--decode-header-value (alist-get 'reply-to headers))
                       (himalaya--decode-header-value (alist-get 'from headers))))
         (extra-to (when reply-all
                     (himalaya--decode-header-value (alist-get 'to headers))))
         (cc (when reply-all (himalaya--decode-header-value (alist-get 'cc headers))))
         (to (string-join (delq nil (list reply-to extra-to)) ", "))
         (message-id (alist-get 'message-id headers))
         (refs (alist-get 'references headers))
         (references (string-join (delq nil (list refs message-id)) " "))
         (body (concat "\n\n"
                       (himalaya--quote-body (himalaya--extract-text-body raw))
                       "\n")))
    (himalaya--start-compose
     (format "*Reply: %s*" (or subject "Himalaya Message"))
     himalaya-from
     to
     re-subject
     body
     message-id
     (and (not (string-empty-p references)) references))
    (when cc
      (save-excursion
        (goto-char (point-min))
        (when (search-forward "To: " nil t)
          (end-of-line)
          (insert "\nCc: " cc))))))

(defun himalaya--forward-buffer (raw)
  "Open a forward buffer attaching the source message in RAW."
  (let* ((headers (himalaya--extract-headers raw))
         (subject (himalaya--decode-header-value (alist-get 'subject headers)))
         (fwd-subject (if (and subject (string-match-p "^[Ff][Ww][Dd]?:" subject))
                          subject
                        (concat "Fwd: " (or subject "")))))
    (himalaya--start-compose
     (format "*Forward: %s*" (or subject "Himalaya Message"))
     himalaya-from
     ""
     fwd-subject
     "\n\n")
    (save-excursion
      (goto-char (point-max))
      (insert "<#part type=\"message/rfc822\" disposition=\"inline\">\n")
      (insert raw)
      (unless (bolp) (insert "\n"))
      (insert "<#/part>\n"))))

(defun himalaya-reply-to-current-message (&optional reply-all)
  "Open a reply buffer to the current message. When called with
\\[universal-argument], reply to every recipient."
  (interactive "P")
  (setq himalaya-reply t)
  (himalaya--read-message-raw
   himalaya-id
   (lambda (raw) (himalaya--reply-buffer raw reply-all))))

(defun himalaya-forward-current-message ()
  "Open a forward buffer for the current message."
  (interactive)
  (setq himalaya-reply nil)
  (himalaya--read-message-raw
   himalaya-id
   (lambda (raw) (himalaya--forward-buffer raw))))

(defun himalaya-reply-to-message-at-point (&optional reply-all)
  "Pick the envelope at point then reply to its associated message.
When called with \\[universal-argument], reply-all."
  (interactive "P")
  (let* ((id (tabulated-list-get-id))
         (subject (substring-no-properties (elt (tabulated-list-get-entry) 2))))
    (setq himalaya-id id)
    (setq himalaya-subject subject)
    (himalaya-reply-to-current-message reply-all)))

(defun himalaya-forward-message-at-point ()
  "Pick the envelope at point then forward its associated message."
  (interactive)
  (let* ((id (tabulated-list-get-id))
         (subject (substring-no-properties (elt (tabulated-list-get-entry) 2))))
    (setq himalaya-id id)
    (setq himalaya-subject subject)
    (himalaya-forward-current-message)))

(defun himalaya-copy-marked-messages ()
  "Copy message(s) matching marked envelope(s) (or envelope at point)
from the current mailbox of the current account to the selected
mailbox."
  (interactive)
  (himalaya--pick-mailbox
   "Copy to mailbox: "
   (lambda (target)
     (himalaya--copy-messages
      (or himalaya-marked-ids (list (tabulated-list-get-id)))
      target
      (lambda (status)
	(message "%s" (or (plist-get status :message) status))
	(himalaya-unmark-all-envelopes t))))))

(defun himalaya-move-marked-messages ()
  "Move message(s) matching marked envelope(s) (or envelope at point)
from the current mailbox of the current account to the selected
mailbox."
  (interactive)
  (himalaya--pick-mailbox
   "Move to mailbox: "
   (lambda (target)
     (let ((prev-point (point))
	   (ids (or himalaya-marked-ids (list (tabulated-list-get-id)))))
       (himalaya--move-messages
	ids
	target
	(lambda (status)
	  (message "%s" (or (plist-get status :message) status))
	  (himalaya-unmark-all-envelopes t)
	  (revert-buffer)
	  (goto-char prev-point)))))))

(defun himalaya-next-message ()
  "Go to the next message."
  (interactive)
  (setq himalaya-id (prin1-to-string (1+ (string-to-number himalaya-id))))
  (condition-case nil
      (himalaya--read-current-message)
    (t (user-error "At end of mailbox"))))

(defun himalaya-prev-message ()
  "Go to the previous message."
  (interactive)
  (when (string= himalaya-id "1")
    (user-error "At beginning of mailbox"))
  (setq himalaya-id (prin1-to-string (max 1 (1- (string-to-number himalaya-id)))))
  (himalaya--read-current-message))

(defun himalaya-send-buffer ()
  "Compile the current buffer (MML directives included) into
RFC 5322 bytes, then send through the current account."
  (interactive)
  (let ((raw (himalaya--buffer-to-mime)))
    (himalaya--send-message
     raw
     (lambda (status)
       (if himalaya-reply
	   (himalaya--add-flag
	    (list himalaya-id)
	    "answered"
	    (lambda (_)
	      (message "%s" (or (plist-get status :message) "Message sent"))
	      (set-buffer-modified-p nil)
	      (kill-current-buffer)
	      (himalaya-list-envelopes)))
	 (message "%s" (or (plist-get status :message) "Message sent"))
	 (set-buffer-modified-p nil)
	 (kill-current-buffer))))))

(defvar himalaya-read-message-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a") #'himalaya-download-current-attachments)
    (define-key map (kbd "R") #'himalaya-read-current-message-raw)
    (define-key map (kbd "r") #'himalaya-reply-to-current-message)
    (define-key map (kbd "f") #'himalaya-forward-current-message)
    (define-key map (kbd "q") #'kill-current-buffer)
    (define-key map (kbd "n") #'himalaya-next-message)
    (define-key map (kbd "p") #'himalaya-prev-message)
    map))

(define-derived-mode himalaya-read-message-mode message-mode "Himalaya-Read"
  "Message reading mode."
  (setq mail-header-separator ""))

(defvar himalaya-read-message-raw-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a") #'himalaya-download-current-attachments)
    (define-key map (kbd "R") #'himalaya-read-current-message-plain)
    (define-key map (kbd "r") #'himalaya-reply-to-current-message)
    (define-key map (kbd "f") #'himalaya-forward-current-message)
    (define-key map (kbd "q") #'kill-current-buffer)
    (define-key map (kbd "n") #'himalaya-next-message)
    (define-key map (kbd "p") #'himalaya-prev-message)
    map))

(define-derived-mode himalaya-read-message-raw-mode message-mode "Himalaya-Read-Raw"
  "Himalaya raw message reading mode."
  (setq mail-header-separator ""))

(defvar himalaya-message-write-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'himalaya-send-buffer)
    map))

(define-derived-mode himalaya-message-write-mode message-mode "Himalaya-Write"
  "Himalaya message writing mode."
  (setq mail-header-separator ""))

(provide 'himalaya-message)
;;; himalaya-message.el ends here
