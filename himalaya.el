;;; himalaya.el --- Interface for the email client Himalaya CLI  -*- lexical-binding: t -*-

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

(require 'himalaya-envelope)

(defgroup himalaya nil
  "Options related to the email client Himalaya CLI."
  :group 'mail)

(defcustom himalaya-executable "himalaya"
  "Name or location of the email client Himalaya CLI executable."
  :type 'text
  :group 'himalaya)

(defcustom himalaya-config-path nil
  "Path to the email client Himalaya CLI configuration file."
  :type '(file :must-match t)
  :group 'himalaya)

(defun himalaya--update-mode-line ()
  "Update the mode line with the current account, folder and
envelope listing page."
  (let* ((account (or himalaya-account "-"))
	 (folder (or himalaya-folder "-"))
	 (mode-line (format " Account[%s] Folder[%s] Page[%s]" account folder himalaya-page)))
    (setq mode-line-process mode-line)))

;;;###autoload
(defalias 'himalaya #'himalaya-list-envelopes)

(provide 'himalaya)
;;; himalaya.el ends here
