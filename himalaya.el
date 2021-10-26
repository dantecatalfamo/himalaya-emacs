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

(defgroup himalaya nil
  "Options related to the himalaya mail client."
  :group 'mail)

(defcustom himalaya-executable "himalaya"
  "Name or location of the himalaya executable."
  :type 'text)

(defun himalaya--run-json (&rest args)
  "Run himalaya with ARGS arguments.
The result is parsed as JSON and returned."
  (let* ((args (string-join args " "))
         (cmd (concat himalaya-executable " -o json " args))
         (output (shell-command-to-string cmd)))
    (when (string-prefix-p "Error:" output)
      (error "%s" (car (split-string output "\n"))))
    ;; Remove { "response": [...] } wrapper
    (cadr (json-parse-string output :object-type 'plist :array-type 'list))))

(defun himalaya--mailbox-list (&optional account)
  "Return a list of mailboxes for ACCOUNT.
If ACCOUNT is nil, the default account is used."
  (himalaya--run-json (when account (concat "-a " account)) "mailboxes"))

(defun himalaya--message-list (&optional account mailbox page page-size)
  "Return a list of emails from ACCOUNT in MAILBOX.
Paginate using PAGE of PAGE-SIZE.
If ACCOUNT, MAILBOX, PAGE, or PAGE-SIZE are nil, the default values are used."
  (himalaya--run-json (when account (concat "-a " account))
                      (when mailbox (concat "-m " mailbox))
                      "list"
                      (when page (concat "-p " page))
                      (when page-size "-s " page-size)))

(defun himalaya--message-read (uid &optional account raw html)
  "Return the contents of message with UID (string) from ACCOUNT.
If ACCOUNT is nil, use the default. If RAW is non-nil, return the
raw contents of the email including headers. If HTML is non-nil,
return the HTML version of the email, otherwise return the plain
text version."

  (himalaya--run-json (when account (concat "-a " account))
                      "read"
                      uid
                      (when raw "-r")
                      (when html "-t html")))

(defun himalaya--message-copy (uid target &optional account)
  "Move message wit UID (string) to TARGET mailbox on ACCOUNT.
If ACCOUNT is nil, use the default."
  (himalaya--run-json (when account (concat "-a " account))
                      "copy"
                      uid
                      target))

(provide 'himalaya)
;;; himalaya.el ends here
