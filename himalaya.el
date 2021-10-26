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

(defgroup himalaya nil
  "Options related to the himalaya mail client."
  :group 'mail)

(defcustom himalaya-executable "himalaya"
  "Name or location of the himalaya executable."
  :type 'text)

(defun himalaya--run (&rest args)
  "Run himalaya with ARGS.
Results are returned as a string. Signals a lisp error and
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

(defun himalaya--mailbox-list (&optional account)
  "Return a list of mailboxes for ACCOUNT.
If ACCOUNT is nil, the default account is used."
  (himalaya--run-json (when account (list "-a" account)) "mailboxes"))

(defun himalaya--message-list (&optional account mailbox page page-size)
  "Return a list of emails from ACCOUNT in MAILBOX.
Paginate using PAGE of PAGE-SIZE.
If ACCOUNT, MAILBOX, PAGE, or PAGE-SIZE are nil, the default values are used."
  (himalaya--run-json (when account (list "-a" account))
                      (when mailbox (list "-m" mailbox))
                      "list"
                      (when page (list "-p" (format "%s" page)))
                      (when page-size (list "-s" (format "%s" page-size)))))

(defun himalaya--message-read (uid &optional account raw html)
  "Return the contents of message with UID from ACCOUNT.
If ACCOUNT is nil, use the default. If RAW is non-nil, return the
raw contents of the email including headers. If HTML is non-nil,
return the HTML version of the email, otherwise return the plain
text version."
  (himalaya--run-json (when account (concat "-a " account))
                      "read"
                      (format "%s" uid) ; Ensure uid is a string
                      (when raw "-r")
                      (when html (list "-t" "html"))))

(defun himalaya--message-copy (uid target &optional account)
  "Copy message with UID to TARGET mailbox on ACCOUNT.
If ACCOUNT is nil, use the default."
  (himalaya--run-json (when account (list "-a" account))
                      "copy"
                      (format "%s" uid)
                      target))

(defun himalaya--message-move (uid target &optional account)
  "Move message with UID to TARGET mailbox on ACCOUNT.
If ACCOUNT is nil, use the default."
  (himalaya--run-json (when account (list "-a" account))
                      "move"
                      (format "%s" uid)
                      target))

(provide 'himalaya)
;;; himalaya.el ends here
