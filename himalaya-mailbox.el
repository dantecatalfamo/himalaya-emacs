;;; himalaya-mailbox.el --- Mailbox management for email client Himalaya CLI  -*- lexical-binding: t -*-

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

(require 'himalaya-process)
(require 'himalaya-account)

(defvar himalaya-mailbox nil
  "The current mailbox.")

(defun himalaya--list-mailboxes (callback)
  "Fetch all mailboxes of the current account."
  (message "Listing mailboxes…")
  (himalaya--run
   callback
   nil
   "mailbox"
   "list"
   (when himalaya-account (list "--account" himalaya-account))))

(defun himalaya--with-mailbox-names (callback)
  "Fetch all mailboxes then call CALLBACK with their names."
  (himalaya--list-mailboxes
   (lambda (result)
     (funcall callback (mapcar (lambda (mailbox) (plist-get mailbox :name))
                               (plist-get result :mailboxes))))))

(defun himalaya--pick-mailbox (prompt callback)
  "Ask user to pick a mailbox using PROMPT then call CALLBACK with
the selected mailbox."
  (interactive)
  (himalaya--with-mailbox-names
   (lambda (mailboxes)
     (funcall callback (completing-read prompt mailboxes)))))

(defun himalaya-switch-mailbox ()
  "Ask user to pick a mailbox, set it as the current mailbox then
list envelopes."
  (interactive)
  (himalaya--pick-mailbox
   "Mailbox: "
   (lambda (mailbox)
     (setq himalaya-mailbox mailbox)
     (setq himalaya-page 1)
     (himalaya--update-mode-line)
     (revert-buffer))))

(provide 'himalaya-mailbox)
;;; himalaya-mailbox.el ends here
