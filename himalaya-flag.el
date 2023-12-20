;;; himalaya-flag.el --- Flag management of email client Himalaya CLI  -*- lexical-binding: t -*-

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
(require 'himalaya-account)
(require 'himalaya-folder)

(defcustom himalaya-unseen-face font-lock-string-face
  "Font face for unseen envelope symbol."
  :type 'face
  :group 'himalaya)

(defcustom himalaya-unseen-symbol "●"
  "Symbol to display in the flags column when a message hasn't
been read yet."
  :type 'text
  :group 'himalaya)

(defcustom himalaya-answered-symbol "↵"
  "Symbol to display in the flags column when a message has been
replied to."
  :type 'text
  :group 'himalaya)

(defcustom himalaya-flagged-face font-lock-warning-face
  "Font face for flagged envelope symbol."
  :type 'face
  :group 'himalaya)

(defcustom himalaya-flagged-symbol "⚑"
  "Symbol to display in the flags column when a message has been
flagged."
  :type 'text
  :group 'himalaya)

(defcustom himalaya-deleted-face font-lock-keyword-face
  "Font face for deleted envelope symbol."
  :type 'face
  :group 'himalaya)

(defcustom himalaya-deleted-symbol "✘"
  "Symbol to display in the flags column when a message has been
marked for deletion."
  :type 'text
  :group 'himalaya)

(defun himalaya--add-flag (ids flag callback)
  "Add FLAG to envelopes IDS from the current folder of the current
account."
  (message "Adding flag %s…" flag)
  (himalaya--run
   callback
   nil
   "flag"
   "add"
   (when himalaya-account (list "--account" himalaya-account))
   (when himalaya-folder (list "--folder" himalaya-folder))
   ids
   flag))

(defun himalaya--remove-flag (ids flag callback)
  "Remove FLAG to envelopes IDS from the current folder of the current
account."
  (message "Adding flag %s…" flag)
  (himalaya--run
   callback
   nil
   "flag"
   "remove"
   (when himalaya-account (list "--account" himalaya-account))
   (when himalaya-folder (list "--folder" himalaya-folder))
   ids
   flag))

(defun himalaya--flag-symbols (flags)
  "Generate a display string for FLAGS."
  (concat
   (if (member "Seen" flags) " " (propertize himalaya-unseen-symbol 'face himalaya-unseen-face))
   (if (member "Answered" flags) himalaya-answered-symbol " ")
   (if (member "Flagged" flags) (propertize himalaya-flagged-symbol 'face himalaya-flagged-face) " ")
   (if (member "Deleted" flags) (propertize himalaya-deleted-symbol 'face himalaya-deleted-face) " ")))

(defun himalaya-add-flag-marked-envelopes ()
  "Ask user to pick a flag then add it to marked envelopes, or to
 envelope at point if mark not set."
  (interactive)
  (let ((prev-point (point))
	(ids (or himalaya-marked-ids (list (tabulated-list-get-id))))
	(flag (completing-read "Add flag: " (list "Seen" "Answered" "Flagged" "Deleted" "Drafts"))))
    (himalaya--add-flag
     ids
     flag
     (lambda (status)
       (message "%s" status)
       (himalaya-unmark-all-envelopes t)
       (revert-buffer)
       (goto-char prev-point)))))

(defun himalaya-remove-flag-marked-envelopes ()
  "Ask user to pick a flag then remove it from marked envelopes, or
 from envelope at point if mark not set."
  (interactive)
  (let ((prev-point (point))
	(ids (or himalaya-marked-ids (list (tabulated-list-get-id))))
	(flag (completing-read "Remove flag: " (list "Seen" "Answered" "Flagged" "Deleted" "Drafts"))))
    (himalaya--remove-flag
     ids
     flag
     (lambda (status)
       (message "%s" status)
       (himalaya-unmark-all-envelopes t)
       (revert-buffer)
       (goto-char prev-point)))))

(provide 'himalaya-flag)
;;; himalaya-flag.el ends here
