;;; himalaya-envelope.el --- Envelope management of email client Himalaya CLI  -*- lexical-binding: t -*-

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
(require 'himalaya-envelope-mark)
(require 'himalaya-flag)
(require 'himalaya-message)
(require 'himalaya-attachment)

(defcustom himalaya-list-envelopes-order nil
  "Order of how envelopes are displayed on each listing page."
  :type '(radio (const :tag "Ascending (oldest first)" t)
                (const :tag "Descending (newest first)" nil))
  :group 'himalaya)

(defcustom himalaya-list-envelopes-page-size 25
  "The number of envelopes to return per envelope list page."
  :type 'number
  :group 'himalaya)

(defcustom himalaya-id-face font-lock-variable-name-face
  "Font face for envelope ids."
  :type 'face
  :group 'himalaya)

(defcustom himalaya-sender-face font-lock-function-name-face
  "Font face for sender names."
  :type 'face
  :group 'himalaya)

(defcustom himalaya-date-face font-lock-constant-face
  "Font face for dates."
  :type 'face
  :group 'himalaya)

(defcustom himalaya-headers-face font-lock-constant-face
  "Font face for headers when reading a message."
  :type 'face
  :group 'himalaya)

(defcustom himalaya-subject-width 70
  "Width of the subject column in the envelope list."
  :type 'number
  :group 'himalaya)

(defcustom himalaya-from-width 30
  "Width of the from column in the envelope list."
  :type 'number
  :group 'himalaya)

(defvar himalaya-id nil
  "The current envelope id.")

(defvar himalaya-subject nil
  "The current envelope subject.")

(defvar himalaya-page 1
  "The current envelope list page.")

(defun himalaya--list-envelopes ()
  "Fetch envelopes from the current account in the current
folder. Paginate using the current page of global page size. This
function is blocking because it is used by 'tabulated-list-entries'
which cannot work with callbacks."
  (himalaya--run-blocking
   "envelope"
   "list"
   (when himalaya-account (list "--account" himalaya-account))
   (when himalaya-page (list "--page" (format "%s" himalaya-page)))
   (when himalaya-list-envelopes-page-size (list "--page-size" (prin1-to-string himalaya-list-envelopes-page-size)))
   (when himalaya-folder himalaya-folder)))

(defun himalaya--build-envelopes-table ()
  "Build the envelopes table."
  (when (consp current-prefix-arg)
    (message "CONSP")
    (setq himalaya-page 1)
    (himalaya--update-mode-line)
    (goto-char (point-min)))
  (let ((emails (himalaya--list-envelopes)) entries)
    (dolist (email emails entries)
      (push
       (list
	(plist-get email :id)
        (vector
         (propertize (plist-get email :id) 'face himalaya-id-face)
         (himalaya--flag-symbols (plist-get email :flags))
         (plist-get email :subject)
         (himalaya--build-envelopes-table-sender-column email)
         (propertize (plist-get email :date) 'face himalaya-date-face)))
       entries))
    (if himalaya-list-envelopes-order entries (nreverse entries))))

(defun himalaya--build-envelopes-table-sender-column (email)
  "Build the sender column of the envelopes table."
  (let* ((from (plist-get email :from))
         (name (plist-get from :name))
         (addr (plist-get from :addr)))
    (propertize (if (eq name :null) addr name) 'face himalaya-sender-face)))

(defun himalaya-list-envelopes-next-page ()
  "Go to the next envelope listing page of the current folder."
  (interactive)
  (setq himalaya-page (1+ himalaya-page))
  (himalaya--update-mode-line)
  (revert-buffer))

(defun himalaya-list-envelopes-prev-page ()
  "Go to the previous envelopes listing page of the current folder."
  (interactive)
  (setq himalaya-page (max 1 (1- himalaya-page)))
  (himalaya--update-mode-line)
  (revert-buffer))

(defun himalaya-list-envelopes-at-page (page)
  "Jump to envelopes listing PAGE of the current folder."
  (interactive "nJump to page: ")
  (setq himalaya-page (max 1 page))
  (himalaya--update-mode-line)
  (revert-buffer))

(defvar himalaya-list-envelopes-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c a") #'himalaya-switch-account)
    (define-key map (kbd "s"    ) #'himalaya-sync-account)
    (define-key map (kbd "C-c f") #'himalaya-switch-folder)
    (define-key map (kbd "e"    ) #'himalaya-expunge-folder)
    (define-key map (kbd "f"    ) #'himalaya-list-envelopes-next-page)
    (define-key map (kbd "b"    ) #'himalaya-list-envelopes-prev-page)
    (define-key map (kbd "j"    ) #'himalaya-list-envelopes-at-page)
    (define-key map (kbd "m"    ) #'himalaya-mark-envelope-forward)
    (define-key map (kbd "DEL"  ) #'himalaya-unmark-envelope-backward)
    (define-key map (kbd "u"    ) #'himalaya-unmark-envelope-forward)
    (define-key map (kbd "U"    ) #'himalaya-unmark-all-envelopes)
    (define-key map (kbd "RET"  ) #'himalaya-read-message-at-point)
    (define-key map (kbd "w"    ) #'himalaya-write-new-message)
    (define-key map (kbd "R"    ) #'himalaya-reply-to-message-at-point)
    (define-key map (kbd "F"    ) #'himalaya-forward-message-at-point)
    (define-key map (kbd "C"    ) #'himalaya-copy-marked-messages)
    (define-key map (kbd "M"    ) #'himalaya-move-marked-messages)
    (define-key map (kbd "D"    ) #'himalaya-delete-marked-messages)
    (define-key map (kbd "a"    ) #'himalaya-download-marked-attachments)
    map))

(define-derived-mode himalaya-list-envelopes-mode tabulated-list-mode "Himalaya-Envelopes"
  "Himalaya envelope listing mode."
  (setq tabulated-list-format
	(vector
         (list "ID" 5 nil :right-align t)
         (list "Flags" 6 nil)
         (list "Subject" himalaya-subject-width nil)
         (list "From" himalaya-from-width nil)
         (list "Date" 19 nil)))
  (setq tabulated-list-sort-key nil)
  (setq tabulated-list-entries #'himalaya--build-envelopes-table)
  (setq tabulated-list-padding 1)
  (tabulated-list-init-header)
  (hl-line-mode))

;;;###autoload
(defun himalaya-list-envelopes ()
  "Display envelopes from the current folder of the current account
in a table."
  (interactive)
  (switch-to-buffer "*Himalaya Envelopes*")
  (himalaya-list-envelopes-mode)
  (himalaya--update-mode-line)
  (revert-buffer))

(provide 'himalaya-envelope)
;;; himalaya-envelope.el ends here
