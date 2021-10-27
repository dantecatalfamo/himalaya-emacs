;;; himalaya.el --- Interface for the himalaya email client  -*- lexical-binding: t -*-

;; Copyright (C) 2021 Dante Catalfamo

;; Author: Dante Catalfamo

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

;; Interface for the himalaya email client

;;; Code:

(require 'subr-x)
(require 'mailheader)

(defgroup himalaya nil
  "Options related to the himalaya mail client."
  :group 'mail)

(defcustom himalaya-executable "himalaya"
  "Name or location of the himalaya executable."
  :type 'text
  :group 'himalaya)

(defcustom himalaya-page-size 100
  "The number of emails to return per mailbox page."
  :type 'number
  :group 'himalaya)

(defcustom himalaya-id-face font-lock-variable-name-face
  "Font face for himalaya email IDs."
  :type 'face
  :group 'himalaya)

(defcustom himalaya-sender-face font-lock-function-name-face
  "Font face for himalaya sender names."
  :type 'face
  :group 'himalaya)

(defcustom himalaya-date-face font-lock-constant-face
  "Font face for himalaya dates."
  :type 'face
  :group 'himalaya)

(defcustom himalaya-unseen-face font-lock-string-face
  "Font face for unseen message symbol."
  :type 'face
  :group 'himalaya)

(defvar-local himalaya-mailbox nil
  "The current mailbox.")

(defvar-local himalaya-account nil
  "The current account.")

(defvar-local himalaya-uid nil
  "The current message uid.")

(defun himalaya--run (&rest args)
  "Run himalaya with ARGS.
Results are returned as a string. Signals a Lisp error and
displaus the output on non-zero exit."
  (with-temp-buffer
    (let* ((args (flatten-list args))
           (ret (apply #'call-process himalaya-executable nil t nil args))
           (output (buffer-string)))
      (unless (eq ret 0)
        (with-current-buffer-window "*himalaya error*" nil nil
          (insert output))
        (error "Himalaya exited with a non-zero status"))
      output)))

(defun himalaya--run-json (&rest args)
  "Run himalaya with ARGS arguments.
The result is parsed as JSON and returned."
  (let ((args (append '("-o" "json") args)))
    ;; Remove { "response": [...] } wrapper
    (cadr (json-parse-string (himalaya--run args)
                             :object-type 'plist
                             :array-type 'list))))

(defun himalaya--extract-headers (message)
  "Extract email headers from MESSAGE."
  (with-temp-buffer
    (insert message)
    (goto-char (point-min))
    (mail-header-extract-no-properties)))

(defun himalaya--mailbox-list (&optional account)
  "Return a list of mailboxes for ACCOUNT.
If ACCOUNT is nil, the default account is used."
  (himalaya--run-json (when account (list "-a" account)) "mailboxes"))

(defun himalaya--mailbox-list-names (&optional account)
  "Return a list of mailbox names for ACCOUNT.
If ACCOUNT is nil, the default account is used."
  (mapcar (lambda (mbox) (plist-get mbox :name))
          (himalaya--mailbox-list account)))

(defun himalaya--message-list (&optional account mailbox page)
  "Return a list of emails from ACCOUNT in MAILBOX.
Paginate using PAGE of PAGE-SIZE.
If ACCOUNT, MAILBOX, or PAGE are nil, the default values are used."
  (himalaya--run-json (when account (list "-a" account))
                      (when mailbox (list "-m" mailbox))
                      "list"
                      (when page (list "-p" (format "%s" page)))
                      (when himalaya-page-size (list "-s" (prin1-to-string himalaya-page-size)))))

(defun himalaya--message-read (uid &optional account mailbox raw html)
  "Return the contents of message with UID from MAILBOX on ACCOUNT.
If ACCOUNT or MAILBOX are nil, use the defaults. If RAW is
non-nil, return the raw contents of the email including headers.
If HTML is non-nil, return the HTML version of the email,
otherwise return the plain text version."
  (himalaya--run-json (when account (list "-a" account))
                      (when mailbox (list "-m" mailbox))
                      "read"
                      (format "%s" uid) ; Ensure uid is a string
                      (when raw "-r")
                      (when html (list "-t" "html"))))

(defun himalaya--message-copy (uid target &optional account mailbox)
  "Copy message with UID from MAILBOX to TARGET mailbox on ACCOUNT.
If ACCOUNT or MAILBOX are nil, use the defaults."
  (himalaya--run-json (when account (list "-a" account))
                      (when mailbox (list "-m" mailbox))
                      "copy"
                      (format "%s" uid)
                      target))

(defun himalaya--message-move (uid target &optional account mailbox)
  "Move message with UID from MAILBOX to TARGET mailbox on ACCOUNT.
If ACCOUNT or MAILBOX are nil, use the defaults."
  (himalaya--run-json (when account (list "-a" account))
                      (when mailbox (list "-m" mailbox))
                      "move"
                      (format "%s" uid)
                      target))

(defun himalaya--message-flag-symbols (flags)
  "Generate a display string for FLAGS."
  (concat
   (if (member "Seen" flags) " " (propertize "●" 'face himalaya-unseen-face))
   (if (member "Answered" flags) "↵" " ")))

(defun himalaya--message-list-build-table ()
  "Construct the message list table."
  (let ((messages (himalaya--message-list himalaya-account himalaya-mailbox))
        entries)
    (dolist (message messages entries)
      (push (list (plist-get message :id)
                  (vector
                   (propertize (prin1-to-string (plist-get message :id)) 'face himalaya-id-face)
                   (himalaya--message-flag-symbols (plist-get message :flags))
                   (plist-get message :subject)
                   (propertize (plist-get message :sender) 'face himalaya-sender-face)
                   (propertize (plist-get message :date) 'face himalaya-date-face)))
            entries))))

(defun himalaya-message-list (&optional account mailbox)
  "List messages in MAILBOX on ACCOUNT."
  (interactive)
  (switch-to-buffer (concat "*Himalaya Mailbox"
                            (when (or account mailbox) ": ")
                            account
                            (and account mailbox "/")
                            mailbox
                            "*"))

  (himalaya-message-list-mode)
  (setq-local himalaya-mailbox mailbox)
  (setq-local himalaya-account account)
  (revert-buffer))

(defun himalaya-switch-mailbox (mailbox)
  "Switch to MAILBOX on the current email account."
  (interactive (list (completing-read "Mailbox: " (himalaya--mailbox-list-names himalaya-account))))
  (himalaya-message-list himalaya-account mailbox))

(defun himalaya-message-read (uid &optional account mailbox)
  "Display message UID from MAILBOX on ACCOUNT.
If ACCOUNT or MAILBOX are nil, use the defaults."
  (let* ((message (replace-regexp-in-string "" "" (himalaya--message-read uid account mailbox)))
         (message-raw (replace-regexp-in-string "" "" (himalaya--message-read uid account mailbox 'raw)))
         (headers (himalaya--extract-headers message-raw)))
    (switch-to-buffer (format "*%s*" (alist-get 'subject headers)))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert message))
    (himalaya-message-read-mode)
    (setq himalaya-account account)
    (setq himalaya-mailbox mailbox)
    (setq himalaya-uid uid)))

(defun himalaya-message-select ()
  "Read the message at point."
  (interactive)
  (let* ((message (tabulated-list-get-entry))
         (uid (substring-no-properties (elt message 0))))
    (himalaya-message-read uid himalaya-account himalaya-mailbox)))

(defvar himalaya-message-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "m") #'himalaya-switch-mailbox)
    (define-key map (kbd "RET") #'himalaya-message-select)
    map))

(define-derived-mode himalaya-message-list-mode tabulated-list-mode "Himylaya-Messages"
  "Himylaya email client message list mode."
  (setq tabulated-list-format [("ID" 5 nil :right-align t)
                               ("Flags" 6 nil)
                               ("Subject" 70 nil)
                               ("Sender" 30 nil)
                               ("Date" 19 nil)])
  (setq tabulated-list-sort-key nil)
  (setq tabulated-list-entries #'himalaya--message-list-build-table)
  (tabulated-list-init-header))

(define-derived-mode himalaya-message-read-mode special-mode "Himalaya-Read"
  "Himalaya email client message reading mode.")

(provide 'himalaya)
;;; himalaya.el ends here
