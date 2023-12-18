;;; himalaya-folder.el --- Himalaya folder-related code  -*- lexical-binding: t -*-

;; Copyright (C) 2021 Dante Catalfamo
;; Copyright (C) 2022-2023 soywod <clement.douin@posteo.net>

;; Author: Dante Catalfamo
;;      soywod <clement.douin@posteo.net>
;; Maintainer: Dante Catalfamo
;;      soywod <clement.douin@posteo.net>
;; Version: 0.3
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
;; Interface for the himalaya email client
;; https://github.com/soywod/himalaya

;;; Code:

(require 'himalaya-process)

(defcustom himalaya-default-folder nil
  "Default folder for himalaya, overrides the himalaya config."
  :type '(choice (const :tag "None" nil)
                 (text  :tag "String"))
  :group 'himalaya)

(defvar himalaya-folder nil
  "The current folder.")

(defun himalaya--list-folder (callback)
  "Fetch all folders of the current account."
  (message "Listing folders…")
  (himalaya--run
   callback
   nil
   "folder"
   "list"
   (when himalaya-account (list "--account" himalaya-account))))

(defun himalaya--with-folder-names (callback)
  "Fetch all folders then call CALLBACK with their names."
  (himalaya--list-folder
   (lambda (folders)
     (funcall callback (mapcar (lambda (folder) (plist-get folder :name)) folders)))))

(defun himalaya--pick-folder (prompt callback)
  "Ask user to pick a folder using PROMPT then call CALLBACK with
the selected folder."
  (interactive)
  (himalaya--with-folder-names
   (lambda (folders)
     (funcall callback (completing-read prompt folders)))))

(defun himalaya-switch-folder-then-reload ()
  "Ask user to pick a folder, set it as the current folder then
reload envelopes."
  (interactive)
  (himalaya--pick-folder
   "Folder: "
   (lambda (folder)
     (setq himalaya-folder folder)
     (himalaya-list-envelopes))))

(defun himalaya--expunge-folder (callback)
  "Expunge the current folder of the current account."
  (when himalaya-folder
    (message "Expunging folder %s…" himalaya-folder)
    (himalaya--run
     callback
     nil
     "folder"
     "expunge"
     (when himalaya-account (list "--account" himalaya-account))
     himalaya-folder)))

(defun himalaya-expunge-folder-then-reload ()
  "Expunge the current folder then reload envelopes."
  (interactive)
  (when ((and himalaya-folder (y-or-n-p (format "Expunge folder %s? " himalaya-folder))))
    (himalaya--expunge-folder
     (lambda (output)
       (message "%s" output)
       (revert-buffer t t t)))))

(provide 'himalaya-folder)
;;; himalaya-folder.el ends here
