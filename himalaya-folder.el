;;; himalaya-folder.el --- Folder management for email client Himalaya CLI  -*- lexical-binding: t -*-

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

(defun himalaya-switch-folder ()
  "Ask user to pick a folder, set it as the current folder then
list envelopes."
  (interactive)
  (himalaya--pick-folder
   "Folder: "
   (lambda (folder)
     (setq himalaya-folder folder)
     (setq himalaya-page 1)
     (himalaya--update-mode-line)
     (revert-buffer))))

(defun himalaya-expunge-folder ()
  "Expunge the current folder then list envelopes."
  (interactive)
  (when ((and himalaya-folder (y-or-n-p (format "Expunge folder %s? " himalaya-folder))))
    (himalaya--expunge-folder
     (lambda (output)
       (message "%s" output)
       (revert-buffer)))))

(provide 'himalaya-folder)
;;; himalaya-folder.el ends here
