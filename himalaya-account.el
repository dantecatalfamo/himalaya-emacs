;;; himalaya-account.el --- Account management of email client Himalaya CLI  -*- lexical-binding: t -*-

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

(require 'himalaya-process)

(defvar himalaya-account nil
  "The current account.")

(defun himalaya--list-accounts (callback)
  "Fetch all accounts defined in the configuration file."
  (message "Listing accounts…")
  (himalaya--run
   callback
   nil
   "account"
   "list"))

(defun himalaya--with-account-names (callback)
  "Fetch all accounts then call CALLBACK with their names."
  (himalaya--list-accounts
   (lambda (accounts)
     (funcall callback (mapcar (lambda (account) (plist-get account :name)) accounts)))))

(defun himalaya--pick-account (prompt callback)
  "Ask user to pick an account using PROMPT then call CALLBACK with
the selected account."
  (interactive)
  (himalaya--with-account-names
   (lambda (accounts)
     (funcall callback (completing-read prompt accounts)))))

(defun himalaya--sync-account (callback &optional folder)
  "Synchronize FOLDER of the current account.
If FOLDER is nil, synchronize all the folders of the current
account."
  (message "Synchronizing account…")
  (himalaya--run
   callback
   nil
   "account"
   "sync"
   (when himalaya-account (list "--account" himalaya-account))
   (when folder (list "--include-folder" folder))))

(defun himalaya-switch-account ()
  "Ask user to pick an account, set it as current account then
reload envelopes."
  (interactive)
  (himalaya--pick-account
   "Account: "
   (lambda (account)
     (setq himalaya-account account)
     (setq himalaya-folder nil)
     (setq himalaya-page 1)
     (himalaya--update-mode-line)
     (revert-buffer))))

(defun himalaya-sync-account (&optional sync-all)
  "Synchronize the current folder of the current account then reload
the current buffer. If called with \\[universal-argument],
SYNC-ALL the folders of the current account."
  (interactive "P")
  (himalaya--sync-account
   (lambda (output)
     (message "%s" output)
     (revert-buffer))
   (unless sync-all himalaya-folder)))

(provide 'himalaya-account)
;;; himalaya-account.el ends here
