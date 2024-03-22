;;; himalaya-envelope-mark.el --- Envelope marks management of email client Himalaya CLI  -*- lexical-binding: t -*-

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

(defvar himalaya-marked-ids nil
  "The current marked envelope ids.")

(defun himalaya--save-face-property (beg end)
  ;; we need to distinguish ,,not set'' from ''no face''
  (unless (and (text-property-not-all beg end 'face nil) (< beg end))
    (put-text-property beg (1+ beg) 'face 'default))
  (unless (text-property-not-all beg end 'himalaya-saved-face nil)
    (himalaya--copy-text-property beg end 'face 'himalaya-saved-face)))

(defun himalaya--restore-face-property (beg end)
  (when (text-property-not-all beg end 'himalaya-saved-face nil)
    (himalaya--copy-text-property beg end 'himalaya-saved-face 'face)))

(defun himalaya--copy-text-property (beg end from to)
  "Copy text property FROM to TO in region BEG to END."
  (let ((inhibit-read-only t))
    (save-excursion
      (while (< beg end)
        (goto-char beg)
        (put-text-property
         (point)
         (setq beg (next-single-property-change (point) from nil end))
         to
         (get-text-property (point) from))))))

(defun himalaya-mark-envelope-forward ()
  "Mark the email at point and move the cursor to the next line."
  (interactive)
  (save-excursion
    (let ((inhibit-read-only t))
      (add-to-list 'himalaya-marked-ids (tabulated-list-get-id))
      (tabulated-list-put-tag (string dired-marker-char))
      (himalaya--save-face-property (point-at-bol) (point-at-eol))
      (put-text-property (point-at-bol) (point-at-eol) 'face 'dired-marked)))
  (next-line)
  (goto-char (point-at-bol)))

(defun himalaya-unmark-envelope-backward ()
  "Unmark the envelope at point and move the cursor to the previous
line."
  (interactive)
  (previous-line)
  (goto-char (pos-bol))
  (let ((inhibit-read-only t))
    (setq himalaya-marked-ids (remove (tabulated-list-get-id) himalaya-marked-ids))
    (tabulated-list-put-tag "")
    (himalaya--restore-face-property (point-at-bol) (point-at-eol))))

(defun himalaya-unmark-envelope-forward ()
  "Unmark the envelope at point and move the cursor to the next
line."
  (interactive)
  (let ((inhibit-read-only t))
    (setq himalaya-marked-ids (remove (tabulated-list-get-id) himalaya-marked-ids))
    (tabulated-list-put-tag "")
    (himalaya--restore-face-property (point-at-bol) (point-at-eol)))
  (next-line)
  (goto-char (point-at-bol)))

(defun himalaya-unmark-all-envelopes (&optional quiet)
  "Unmark all marked envelopes."
  (interactive)
  (when himalaya-marked-ids
    (let ((inhibit-read-only t))
      (goto-char (point-min))
      (while (re-search-forward (format "^[%s]" (string dired-marker-char)) nil t)
        (himalaya--restore-face-property (point-at-bol) (point-at-eol)))
      (tabulated-list-clear-all-tags)))
  (unless quiet (message "%d marks removed" (length himalaya-marked-ids)))
  (setq himalaya-marked-ids nil))

(provide 'himalaya-envelope-mark)
;;; himalaya-envelope-mark.el ends here
