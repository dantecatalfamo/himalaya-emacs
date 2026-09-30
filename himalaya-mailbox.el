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

(require 'seq)
(require 'himalaya-process)
(require 'himalaya-account)

(defvar himalaya-mailbox nil
  "The current mailbox id, as given to the CLI.")

(defvar himalaya-mailbox-name nil
  "The current mailbox name, as shown to the user.")

(defvar himalaya-mailbox-role nil
  "The current mailbox role (inbox, sent, drafts…), as reported by
the CLI.")

(defun himalaya--list-mailboxes (callback)
  "Fetch all mailboxes of the current account."
  (message "Listing mailboxes…")
  (himalaya--run
   callback
   nil
   "mailbox"
   "list"
   (when himalaya-account (list "--account" himalaya-account))))

(defun himalaya--mailbox-candidates (mailboxes)
  "Map MAILBOXES (a list of `{id, name}' plists) to an alist of
completion labels and mailboxes. A name shared by several
mailboxes is suffixed with the mailbox id."
  (let ((names (mapcar (lambda (mailbox) (plist-get mailbox :name)) mailboxes)))
    (mapcar
     (lambda (mailbox)
       (let ((name (plist-get mailbox :name)))
         (cons (if (> (seq-count (lambda (n) (equal n name)) names) 1)
                   (format "%s <%s>" name (plist-get mailbox :id))
                 name)
               mailbox)))
     mailboxes)))

(defun himalaya--pick-mailbox (prompt callback)
  "Ask user to pick a mailbox using PROMPT then call CALLBACK with
the selected mailbox, a `{id, name, role}' plist. A value matching
no listed mailbox is passed as both id and name, for the CLI to
resolve (alias, role)."
  (himalaya--list-mailboxes
   (lambda (result)
     (let* ((candidates (himalaya--mailbox-candidates (plist-get result :mailboxes)))
            (label (completing-read prompt candidates)))
       (funcall callback (or (cdr (assoc label candidates))
                             (list :id label :name label)))))))

(defun himalaya-switch-mailbox ()
  "Ask user to pick a mailbox, set it as the current mailbox then
list envelopes."
  (interactive)
  (himalaya--pick-mailbox
   "Mailbox: "
   (lambda (mailbox)
     (setq himalaya-mailbox (plist-get mailbox :id))
     (setq himalaya-mailbox-name (plist-get mailbox :name))
     (setq himalaya-mailbox-role (plist-get mailbox :role))
     (setq himalaya-page 1)
     (himalaya--update-mode-line)
     (revert-buffer))))

(provide 'himalaya-mailbox)
;;; himalaya-mailbox.el ends here
