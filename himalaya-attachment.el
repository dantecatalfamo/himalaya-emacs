;;; himalaya-attachment.el --- Attachment management of email client Himalaya CLI  -*- lexical-binding: t -*-

;; Copyright (C) 2021 Dante Catalfamo
;; Copyright (C) 2022-2024 soywod <clement.douin@posteo.net>

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

(defun himalaya--download-attachments (ids callback)
  "Download attachment(s) of message(s) matching envelope IDS."
  (message "Downloading attachments…")
  (himalaya--run
   callback
   nil
   "attachment"
   "download"
   (when himalaya-account (list "--account" himalaya-account))
   (when himalaya-folder (list "--folder" himalaya-folder))
   ids))

(defun himalaya-download-marked-attachments ()
  "Download attachment(s) of message(s) matching marked envelope(s),
or matching the envelope at point if mark is not set."
  (interactive)
  (himalaya--download-attachments
   (or himalaya-marked-ids (list (tabulated-list-get-id)))
   (lambda (status)
     (message "%s" (string-trim status))
     (himalaya-unmark-all-envelopes t))))

(defun himalaya-download-current-attachments ()
  "Download attachment(s) of message matching the current envelope."
  (interactive)
  (himalaya--download-attachments
   himalaya-id
   (lambda (status)
     (message "%s" (string-trim status)))))

(provide 'himalaya-attachment)
;;; himalaya-attachment.el ends here
