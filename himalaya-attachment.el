;;; himalaya-attachment.el --- Attachment management of email client Himalaya CLI  -*- lexical-binding: t -*-

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

(require 'cl-lib)

(defun himalaya--download-attachments (message-id callback)
  "Download every attachment of MESSAGE-ID from the current mailbox
of the current account."
  (message "Downloading attachments of %s…" message-id)
  (himalaya--run
   callback
   nil
   "attachment"
   "download"
   (when himalaya-account (list "--account" himalaya-account))
   (when himalaya-mailbox (list "--mailbox" himalaya-mailbox))
   message-id))

(defun himalaya--download-attachments-many (ids final-callback)
  "Sequentially download attachments for each id in IDS, then call
FINAL-CALLBACK with a list of per-id results."
  (let ((results nil))
    (cl-labels
        ((step (remaining)
           (if (null remaining)
               (funcall final-callback (nreverse results))
             (himalaya--download-attachments
              (car remaining)
              (lambda (result)
                (push result results)
                (step (cdr remaining)))))))
      (step ids))))

(defun himalaya-download-marked-attachments ()
  "Download attachment(s) of message(s) matching marked envelope(s),
or matching the envelope at point if mark is not set."
  (interactive)
  (let ((ids (or himalaya-marked-ids (list (tabulated-list-get-id)))))
    (himalaya--download-attachments-many
     ids
     (lambda (results)
       (let ((count (apply #'+ (mapcar (lambda (r) (length (plist-get r :attachments))) results))))
         (message "Downloaded %d attachment(s) across %d message(s)" count (length ids)))
       (himalaya-unmark-all-envelopes t)))))

(defun himalaya-download-current-attachments ()
  "Download attachment(s) of the current message."
  (interactive)
  (himalaya--download-attachments
   himalaya-id
   (lambda (result)
     (message "Downloaded %d attachment(s)" (length (plist-get result :attachments))))))

(provide 'himalaya-attachment)
;;; himalaya-attachment.el ends here
