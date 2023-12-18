;;; himalaya-template.el --- Template management of email client Himalaya CLI  -*- lexical-binding: t -*-

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

(defun himalaya--write-template (callback)
  "Return a template for writing a new message using current account."
  (message "Generating new template…")
  (himalaya--run
   callback
   nil
   "template"
   "write"
   (when himalaya-account (list "--account" himalaya-account))))

(defun himalaya--reply-template (id callback &optional reply-all)
  "Return a reply template for email with ID from FOLDER on ACCOUNT.
If ACCOUNT or FOLDER are nil, use the defaults.
If REPLY-ALL is non-nil, the template will be generated as a reply all email."
  (message "Generating reply template…")
  (himalaya--run
   callback
   nil
   "template"
   "reply"
   (when himalaya-account (list "--account" himalaya-account))
   (when himalaya-folder (list "--folder" himalaya-folder))
   (when reply-all "--all")
   (format "%s" id)))

(defun himalaya--forward-template (id callback)
  "Return a forward template for email with ID from FOLDER on ACCOUNT.
If ACCOUNT or FOLDER are nil, use the defaults."
  (message "Generating forward template…")
  (himalaya--run
   callback
   nil
   "template"
   "forward"
   (when himalaya-account (list "--account" himalaya-account))
   (when himalaya-folder (list "--folder" himalaya-folder))
   (format "%s" id)))

(defun himalaya--send-template (template callback)
  "Send TEMPLATE using ACCOUNT."
  (message "Sending template…")
  (himalaya--run
   callback
   template
   "template"
   "send"
   (when himalaya-account (list "--account" himalaya-account))))

(provide 'himalaya-template)
;;; himalaya-template.el ends here
