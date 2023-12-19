;;; himalaya-message.el --- Message management of email client Himalaya CLI  -*- lexical-binding: t -*-

;; Copyright (C) 2021 Dante Catalfamo
;; Copyright (C) 2022-2023 soywod <clement.douin@posteo.net>

;; Author: Dante Catalfamo
;;      soywod <clement.douin@posteo.net>
;; Maintainer: soywod <clement.douin@posteo.net>
;;      Dante Catalfamo
;; Version: 1.0
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
;; <https://github.com/soywod/himalaya>

;;; Code:

(require 'subr-x)
(require 'mailheader)
(require 'message)
(require 'himalaya-process)
(require 'himalaya-account)
(require 'himalaya-folder)
(require 'himalaya-envelope-mark)
(require 'himalaya-attachment)
(require 'himalaya-template)

(defun himalaya--extract-headers (msg)
  "Extract MESSAGE headers."
  (with-temp-buffer
    (insert msg)
    (goto-char (point-min))
    (mail-header-extract-no-properties)))

(defun himalaya--generate-write-buffer (buffer-name content)
  "Setup to be used to write a message."
  (switch-to-buffer (generate-new-buffer buffer-name))
  (insert content)
  (goto-char (point-min))
  (search-forward "\n\n")
  (himalaya-message-write-mode)
  (set-buffer-modified-p nil))

(defun himalaya--read-message (id callback &optional raw html)
  "Return the contents of message matching the envelope ID from
current folder on current account. If RAW is non-nil, return the raw
contents of the message including headers. If HTML is non-nil, return
the HTML version of the message, otherwise return the plain text
version."
  (message "Reading message %s…" id)
  (himalaya--run
   (lambda (msg) (funcall callback (replace-regexp-in-string "" "" msg)))
   nil
   "message"
   "read"
   (when himalaya-account (list "--account" himalaya-account))
   (when himalaya-folder (list "--folder" himalaya-folder))
   (when raw "--raw")
   (when html "--html")
   (format "%s" id))) ; force id as a string

(defun himalaya--copy-messages (ids folder callback)
  "Copy message(s) matching envelope IDS from current folder of
current account to target FOLDER."
  (message "Copying message(s) to %s…" folder)
  (himalaya--run
   callback
   nil
   "message"
   "copy"
   (when himalaya-account (list "--account" himalaya-account))
   (when himalaya-folder (list "--folder" himalaya-folder))
   folder
   ids))

(defun himalaya--move-messages (ids folder callback)
  "Move message(s) matching envelope IDS from current folder of
current account to target FOLDER."
  (message "Moving message(s) to %s…" folder)
  (himalaya--run
   callback
   nil
   "message"
   "move"
   (when himalaya-account (list "--account" himalaya-account))
   (when himalaya-folder (list "--folder" himalaya-folder))
   folder
   ids))

(defun himalaya--delete-messages (ids callback)
  "Delete message(s) matching envelope IDS from current folder of
current account."
  (message "Deleting message(s)…")
  (himalaya--run
   callback
   nil
   "message"
   "delete"
   (when himalaya-account (list "--account" himalaya-account))
   (when himalaya-folder (list "--folder" himalaya-folder))
   ids))

(defun himalaya--read-current-message (&optional pre-hook)
  "Read message matching current envelope id in current folder from
current account."
  (himalaya--read-message
   himalaya-id
   (lambda (msg)
     (when pre-hook (funcall pre-hook))
     (let* ((headers (himalaya--extract-headers msg))
	    (subject (alist-get 'subject headers)))
       (switch-to-buffer (format "*%s*" subject))
       (erase-buffer)
       (insert msg)
       (set-buffer-modified-p nil)
       (himalaya-read-message-mode)
       (goto-char (point-min))
       (setq buffer-read-only t)
       (setq himalaya-subject subject)))))

(defun himalaya--read-current-message-raw (&optional pre-hook)
  "Read raw message matching current envelope id in current folder
from current account."
  (himalaya--read-message
   himalaya-id
   (lambda (msg)
     (when pre-hook (funcall pre-hook))
     (let* ((headers (himalaya--extract-headers msg))
	    (subject (alist-get 'subject headers)))
       (switch-to-buffer (format "*Raw: %s*" subject))
       (let ((inhibit-read-only t))
	 (erase-buffer)
	 (insert msg)
	 (set-buffer-modified-p nil)
	 (himalaya-read-message-raw-mode)
	 (goto-char (point-min))
	 (setq himalaya-subject subject))))
   t))

(defun himalaya-read-current-message-plain ()
  "Read message matching current envelope id in current folder from
current account."
  (interactive)
  (himalaya--read-current-message #'kill-current-buffer))

(defun himalaya-read-current-message-raw ()
  "Read raw message matching current envelope id in current folder
from current account."
  (interactive)
  (himalaya--read-current-message-raw #'kill-current-buffer))

(defun himalaya-reply-to-current-message (&optional reply-all)
  "Open a new buffer with a reply template to the current email.
If called with \\[universal-argument], email will be REPLY-ALL."
  (interactive "P")
  (himalaya--reply-template
   himalaya-id
   (lambda (tpl)
     (himalaya--generate-write-buffer (format "*Reply: %s*" himalaya-subject) tpl))
   reply-all))

(defun himalaya-forward-current-message ()
  "Open a new buffer with a forward template to the current email."
  (interactive)
  (himalaya--forward-template
   himalaya-id
   (lambda (tpl)
     (himalaya--generate-write-buffer (format "*Forward: %s*" himalaya-subject) tpl))))

(defun himalaya-next-message ()
  "Go to the next email."
  (interactive)
  (setq himalaya-id (prin1-to-string (1+ (string-to-number himalaya-id))))
  (condition-case
      nil (himalaya--read-current-message)
    (t (user-error "At end of folder"))))

(defun himalaya-prev-message ()
  "Go to the previous message."
  (interactive)
  (when (string= himalaya-id "1")
    (user-error "At beginning of folder"))
  (setq himalaya-id (prin1-to-string (max 1 (1- (string-to-number himalaya-id)))))
  (himalaya--read-current-message))

(defun himalaya-read-message-at-point ()
  "Pick the envelope at point and read its associated message."
  (interactive)
  (setq himalaya-id (tabulated-list-get-id))
  (himalaya--read-current-message))

(defun himalaya-write-new-message ()
  "Compose a new message in a buffer."
  (interactive)
  (himalaya--write-template
   (lambda (tpl)
     (himalaya--generate-write-buffer "*Himalaya New Message*" tpl))))

(defun himalaya-reply-to-message-at-point (&optional reply-all)
  "Pick the envelope at point then reply to its associated message.
If called with \\[universal-argument], message will be REPLY-ALL."
  (interactive "P")
  (let* ((id (tabulated-list-get-id))
         (subject (substring-no-properties (elt (tabulated-list-get-entry) 2))))
    (setq himalaya-id id)
    (setq himalaya-subject subject)
    (himalaya-read-message-reply reply-all)))

(defun himalaya-forward-message-at-point ()
  "Pick the envelope at point then forward its associated message."
  (interactive)
  (let* ((id (tabulated-list-get-id))
         (subject (substring-no-properties (elt (tabulated-list-get-entry) 2))))
    (setq himalaya-id id)
    (setq himalaya-subject subject)
    (himalaya-read-message-forward)))

(defun himalaya-copy-marked-messages ()
  "Copy message(s) matching marked envelope(s) (or envelope at point)
from current folder of current account to selected folder."
  (interactive)
  (himalaya--pick-folder
   "Copy to folder: "
   (lambda (folder)
     (himalaya--copy-messages
      (or himalaya-marked-ids (list (tabulated-list-get-id)))
      folder
      (lambda (status)
	(message "%s" status)
	(himalaya-unmark-all-envelopes t))))))

(defun himalaya-move-marked-messages ()
  "Move message(s) matching marked envelope(s) (or envelope at point)
from current folder of current account to selected folder."
  (interactive)
  (himalaya--pick-folder
   "Move to folder: "
   (lambda (folder)
     (let ((prev-point (point))
	   (ids (or himalaya-marked-ids (list (tabulated-list-get-id)))))
       (himalaya--move-messages
	ids
	folder
	(lambda (status)
	  (message "%s" status)
	  (himalaya-unmark-all-envelopes t)
	  (revert-buffer)
	  (goto-char prev-point)))))))

(defun himalaya-delete-marked-messages ()
  "Delete message(s) matching marked envelope(s) (or envelope at
point) from current folder of current account."
  (interactive)
  (let* ((prev-point (point))
	 (envelope (tabulated-list-get-entry))
         (subject (substring-no-properties (elt envelope 2)))
	 (subject-or-ids (if himalaya-marked-ids (string-join himalaya-marked-ids ", ") subject)))
    (when (y-or-n-p (format "Delete message(s) %s? " subject-or-ids))
      (himalaya--delete-messages
       (or himalaya-marked-ids (tabulated-list-get-id))
       (lambda (status)
	 (message "%s" status)
	 (himalaya-unmark-all-envelopes t)
	 (revert-buffer)
	 (goto-char prev-point))))))

(defun himalaya-send-buffer ()
  "Send the current buffer."
  (interactive)
  (himalaya--send-template
   (buffer-string)
   (lambda (status)
     (message "%s" status)
     (set-buffer-modified-p nil)
     (kill-current-buffer))))

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
  "Message reading mode.")

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
  "Himalaya raw message reading mode.")

(defvar himalaya-message-write-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'himalaya-send-buffer)
    map))

(define-derived-mode himalaya-message-write-mode message-mode "Himalaya-Write"
  "Himalaya message writing mode.")

(provide 'himalaya-message)
;;; himalaya-message.el ends here
