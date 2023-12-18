;;; himalaya.el --- Interface for the himalaya email client  -*- lexical-binding: t -*-

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

(require 'subr-x)
(require 'mailheader)
(require 'message)
(require 'himalaya-process)
(require 'himalaya-account)
(require 'himalaya-folder)

(defgroup himalaya nil
  "Options related to the himalaya email client."
  :group 'mail)

(defcustom himalaya-executable "himalaya"
  "Name or location of the himalaya executable."
  :type 'text
  :group 'himalaya)

(defcustom himalaya-config-path nil
  "Custom path to the himalaya configuration file."
  :type '(file :must-match t)
  :group 'himalaya)

(defcustom himalaya-list-envelopes-order nil
  "Order of how emails are displayed on each page of the folder."
  :type '(radio (const :tag "Ascending (oldest first)" t)
                (const :tag "Descending (newest first)" nil))
  :group 'himalaya)

(defcustom himalaya-page-size 50
  "The number of emails to return per folder page."
  :type 'number
  :group 'himalaya)

(defcustom himalaya-id-face font-lock-variable-name-face
  "Font face for himalaya email IDs."
  :type 'face
  :group 'himalaya)

(defcustom himalaya-sender-face font-lock-function-name-face
  "Font face for himalaya sender names."
  :type 'face
  :group 'himalaya)

(defcustom himalaya-date-face font-lock-constant-face
  "Font face for himalaya dates."
  :type 'face
  :group 'himalaya)

(defcustom himalaya-unseen-face font-lock-string-face
  "Font face for unseen email symbol."
  :type 'face
  :group 'himalaya)

(defcustom himalaya-flagged-face font-lock-warning-face
  "Font face for flagged email symbol."
  :type 'face
  :group 'himalaya)

(defcustom himalaya-deleted-face font-lock-keyword-face
  "Font face for deleted email symbol."
  :type 'face
  :group 'himalaya)

(defcustom himalaya-headers-face font-lock-constant-face
  "Font face for headers when reading an email."
  :type 'face
  :group 'himalaya)

(defcustom himalaya-unseen-symbol "●"
  "Symbol to display in the flags column when an email hasn't been
read yet."
  :type 'text
  :group 'himalaya)

(defcustom himalaya-answered-symbol "↵"
  "Symbol to display in the flags column when an email has been
replied to."
  :type 'text
  :group 'himalaya)

(defcustom himalaya-flagged-symbol "⚑"
  "Symbol to display in the flags column when an email has been
flagged."
  :type 'text
  :group 'himalaya)

(defcustom himalaya-deleted-symbol "✘"
  "Symbol to display in the flags column when an email has been
marked for deletion."
  :type 'text
  :group 'himalaya)

(defcustom himalaya-subject-width 70
  "Width of the subject column in the email list."
  :type 'number
  :group 'himalaya)

(defcustom himalaya-from-width 30
  "Width of the from column in the email list."
  :type 'number
  :group 'himalaya)


(defvar himalaya-id nil
  "The current email id.")

(defvar himalaya-reply nil
  "True if the current email is a reply.")


(defvar-local himalaya-marked-ids nil
  "The current marked email ids.")

(defvar-local himalaya-subject nil
  "The current email subject.")

(defvar-local himalaya-page 1
  "The current folder page.")

(defun himalaya--extract-headers (email)
  "Extract email headers from EMAIL."
  (with-temp-buffer
    (insert email)
    (goto-char (point-min))
    (mail-header-extract-no-properties)))

(defun himalaya--prepare-email-write-buffer (buffer)
  "Setup BUFFER to be used to write an email.
Sets the mail function correctly, adds mail header, etc."
  (with-current-buffer buffer
    (goto-char (point-min))
    (search-forward "\n\n")
    (himalaya-message-write-mode)
    (set-buffer-modified-p nil)))

(defun himalaya--envelope-list (&optional account folder page)
  "Return a list of emails from ACCOUNT in FOLDER.
Paginate using PAGE of PAGE-SIZE.
If ACCOUNT, FOLDER, or PAGE are nil, the default values are used."
  (himalaya--run-blocking
   "envelope"
   "list"
   (when account (list "--account" account))
   (when page (list "--page" (format "%s" page)))
   (when himalaya-page-size (list "--page-size" (prin1-to-string himalaya-page-size)))
   (when folder folder)))

(defun himalaya--message-read (id callback &optional account folder raw html)
  "Return the contents of email with ID from FOLDER on ACCOUNT.
If ACCOUNT or FOLDER are nil, use the defaults. If RAW is
non-nil, return the raw contents of the email including headers.
If HTML is non-nil, return the HTML version of the email,
otherwise return the plain text version."
  (message "Reading message %s…" id)
  (himalaya--run
   (lambda (msg) (funcall callback (replace-regexp-in-string "" "" msg)))
   nil
   "message"
   "read"
   (when account (list "--account" account))
   (when folder (list "--folder" folder))
   (when raw "--raw")
   (when html "--html")
   (format "%s" id))) ; force id as a string

(defun himalaya--message-copy (ids source target callback &optional account)
  "Copy message(s) with envelope IDS from SOURCE to TARGET folder on
ACCOUNT. If ACCOUNT is nil, use the defaults."
  (message "Copying message(s) from %s to %s…" source target)
  (himalaya--run
   callback
   nil
   "message"
   "copy"
   (when account (list "--account" account))
   source
   target
   ids))

(defun himalaya--message-move (ids source target callback &optional account)
  "Move message(s) with envelope IDS from SOURCE to TARGET folder on
ACCOUNT. If ACCOUNT is nil, use the defaults."
  (message "Moving message(s) from %s to %s…" source target)
  (himalaya--run
   callback
   nil
   "message"
   "move"
   (when account (list "--account" account))
   source
   target
   ids))

(defun himalaya--message-delete (ids callback &optional account folder)
  "Delete emails with IDS from FOLDER on ACCOUNT.
If ACCOUNT or FOLDER are nil, use the defaults.
IDS is a list of numbers."
  (message "Deleting message(s)…")
  (himalaya--run
   callback
   nil
   "message"
   "delete"
   (when account (list "--account" account))
   (when folder (list "--folder" folder))
   ids))

(defun himalaya--attachment-download (ids callback &optional account folder)
  "Download attachments from email with ID.
If ACCOUNT or FOLDER are nil, use the defaults."
  (message "Downloading attachments…")
  (himalaya--run
   callback
   nil
   "attachment"
   "download"
   (when account (list "--account" account))
   (when folder (list "--folder" folder))
   ids))

(defun himalaya--template-write (callback &optional account)
  "Return a template for writing a new email using ACCOUNT."
  (message "Generating new template…")
  (himalaya--run
   callback
   nil
   "template"
   "write"
   (when account (list "--account" account))))

(defun himalaya--template-reply (id callback &optional account folder reply-all)
  "Return a reply template for email with ID from FOLDER on ACCOUNT.
If ACCOUNT or FOLDER are nil, use the defaults.
If REPLY-ALL is non-nil, the template will be generated as a reply all email."
  (message "Generating reply template…")
  (himalaya--run
   callback
   nil
   "template"
   "reply"
   (when account (list "--account" account))
   (when folder (list "--folder" folder))
   (when reply-all "--all")
   (format "%s" id)))

(defun himalaya--template-forward (id callback &optional account folder)
  "Return a forward template for email with ID from FOLDER on ACCOUNT.
If ACCOUNT or FOLDER are nil, use the defaults."
  (message "Generating forward template…")
  (himalaya--run
   callback
   nil
   "template"
   "forward"
   (when account (list "--account" account))
   (when folder (list "--folder" folder))
   (format "%s" id)))

(defun himalaya--template-send (template callback &optional account)
  "Send TEMPLATE using ACCOUNT."
  (message "Sending template…")
  (himalaya--run
   callback
   template
   "template"
   "send"
   (when account (list "--account" account))))

(defun himalaya--flag-add (id flags callback &optional account folder)
  "Add FLAGS to email ID."
  (himalaya--run
   callback
   nil
   "flags"
   "add"
   (when account (list "--account" account))
   (when folder (list "--folder" folder))
   id
   flags))

(defun himalaya--envelope-flag-symbols (flags)
  "Generate a display string for FLAGS."
  (concat
   (if (member "Seen" flags) " " (propertize himalaya-unseen-symbol 'face himalaya-unseen-face))
   (if (member "Answered" flags) himalaya-answered-symbol " ")
   (if (member "Flagged" flags) (propertize himalaya-flagged-symbol 'face himalaya-flagged-face) " ")
   (if (member "Deleted" flags) (propertize himalaya-deleted-symbol 'face himalaya-deleted-face) " ")))

(defun himalaya--envelope-list-build-table ()
  "Construct the email list table."
  (when (consp current-prefix-arg)
    (setq himalaya-page 1)
    (goto-char (point-min)))
  (let ((emails (himalaya--envelope-list himalaya-account himalaya-folder himalaya-page)) entries)
    (dolist (email emails entries)
      (push (list (plist-get email :id)
                  (vector
                   (propertize (plist-get email :id) 'face himalaya-id-face)
                   (himalaya--envelope-flag-symbols (plist-get email :flags))
                   (plist-get email :subject)
                   (himalaya--envelope-list-build-table-sender-column email)
                   (propertize (plist-get email :date) 'face himalaya-date-face)))
            entries))
    (if himalaya-list-envelopes-order
        entries
      (nreverse entries))))

(defun himalaya--envelope-list-build-table-sender-column (email)
  "Construct the sender"
  (let* ((from (plist-get email :from))
         (name (plist-get from :name))
         (addr (plist-get from :addr)))
    (propertize (if (eq name :null) addr name) 'face himalaya-sender-face)))

;;;###autoload
(defun himalaya-list-envelopes (&optional account folder page)
  "List emails in FOLDER on ACCOUNT."
  (interactive)
  (switch-to-buffer "*Himalaya Envelopes*")
  (himalaya-list-envelopes-mode)
  (setq mode-line-process (format " Account[%s] Folder[%s] Page[%s]" (or himalaya-account "-") (or himalaya-folder "-") himalaya-page))
  (revert-buffer))

(defun himalaya--save-face-property (beg end)
  ;; We need to distinguish ,,not set'' from ''no face''.
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

;;;###autoload
(defalias 'himalaya #'himalaya-list-envelopes)

(defun himalaya-read-message-raw (id &optional account folder pre-hook)
  "Display raw email ID from FOLDER on ACCOUNT.
If ACCOUNT or FOLDER are nil, use the defaults."
  (himalaya--message-read
   id
   (lambda (msg)
     (when pre-hook (funcall pre-hook))
     (let* ((headers (himalaya--extract-headers msg))
	    (subject (alist-get 'subject headers)))
       (switch-to-buffer (format "*Raw: %s*" subject))
       (let ((inhibit-read-only t))
	 (erase-buffer)
	 (insert msg)
	 (set-buffer-modified-p nil))
       (himalaya-read-message-raw-mode)
       (setq himalaya-account account)
       (setq himalaya-folder folder)
       (setq himalaya-id id)))
   account
   folder
   t))

(defun himalaya-read-message-switch-raw ()
  "Read a raw version of the current email."
  (interactive)
  (himalaya-read-message-raw himalaya-id himalaya-account himalaya-folder #'kill-current-buffer))

(defun himalaya-read-message-switch-plain ()
  "Read a plain version of the current email."
  (interactive)
  (himalaya-read-message himalaya-id himalaya-account himalaya-folder #'kill-current-buffer))

(defun himalaya-read-message-download-attachment ()
  "Download all attachments of current email."
  (interactive)
  (himalaya--attachment-download
   (list himalaya-id)
   (lambda (output) (message "%s" output))
   himalaya-account
   himalaya-folder))

(defun himalaya-read-message-reply (&optional reply-all)
  "Open a new buffer with a reply template to the current email.
If called with \\[universal-argument], email will be REPLY-ALL."
  (interactive "P")
  (himalaya--template-reply
   himalaya-id
   (lambda (tpl)
     (setq himalaya-reply t)
     (switch-to-buffer (generate-new-buffer (format "*Reply: %s*" himalaya-subject)))
     (insert tpl)
     (set-buffer-modified-p nil)
     (himalaya--prepare-email-write-buffer (current-buffer)))
   himalaya-account
   himalaya-folder
   reply-all))

(defun himalaya-read-message-forward ()
  "Open a new buffer with a forward template to the current email."
  (interactive)
  (himalaya--template-forward
   himalaya-id
   (lambda (tpl)
     (setq himalaya-reply nil)
     (switch-to-buffer (generate-new-buffer (format "*Forward: %s*" himalaya-subject)))
     (insert tpl)
     (set-buffer-modified-p nil)
     (himalaya--prepare-email-write-buffer (current-buffer)))
   himalaya-account
   himalaya-folder))

(defun himalaya-next-email ()
  "Go to the next email."
  (interactive)
  (condition-case nil
      (himalaya-read-message (prin1-to-string (1+ (string-to-number himalaya-id)))
                             himalaya-account
                             himalaya-folder)
    (t (user-error "At end of folder"))))

(defun himalaya-previous-email ()
  "Go to the previous email."
  (interactive)
  (when (string= himalaya-id "1")
    (user-error "At beginning of folder"))
  (himalaya-read-message (prin1-to-string (max 1 (1- (string-to-number himalaya-id))))
			 himalaya-account
			 himalaya-folder))

;; Message utils

(defun himalaya-read-message (id &optional account folder pre-hook)
  "Read message from envelope ID and FOLDER on ACCOUNT.
If ACCOUNT or FOLDER are nil, use the defaults."
  (himalaya--message-read
   id
   (lambda (msg)
     (when pre-hook (funcall pre-hook))
     (let* ((headers (himalaya--extract-headers msg))
	    (subject (alist-get 'subject headers)))
       (switch-to-buffer (format "*%s*" subject))
       (erase-buffer)
       (insert msg)
       (set-buffer-modified-p nil)
       (himalaya-read-message-mode)
       (goto-char (point-min))
       (setq buffer-read-only t)
       (setq himalaya-account account)
       (setq himalaya-folder folder)
       (setq himalaya-id id)
       (setq himalaya-subject subject)))
   account
   folder))

;; Envelope listing mode

(defun himalaya-forward-envelopes-page-map ()
  "Go to the next envelope listing page of the current folder."
  (interactive)
  (himalaya-list-envelopes himalaya-account himalaya-folder (1+ himalaya-page)))

(defun himalaya-backward-envelopes-page-map ()
  "Go to the previous envelopes listing page of the current folder."
  (interactive)
  (himalaya-list-envelopes himalaya-account himalaya-folder (max 1 (1- himalaya-page))))

(defun himalaya-jump-to-envelopes-page-map (page)
  "Jump to envelopes listing PAGE of current folder."
  (interactive "nJump to page: ")
  (himalaya-list-envelopes himalaya-account himalaya-folder (max 1 page)))

(defun himalaya-read-message-map ()
  "Pick the envelope at point and read its associated message."
  (interactive)
  (let* ((envelope (tabulated-list-get-entry))
         (id (substring-no-properties (elt envelope 0))))
    (himalaya-read-message id himalaya-account himalaya-folder)))

(defun himalaya-write-message-map ()
  "Open a new buffer for writing a new message."
  (interactive)
  (himalaya--template-write
   (lambda (tpl)
     (setq himalaya-reply nil)
     (switch-to-buffer (generate-new-buffer "*Himalaya New Message*"))
     (insert tpl)
     (set-buffer-modified-p nil)
     (himalaya--prepare-email-write-buffer (current-buffer)))
   himalaya-account))

(defun himalaya-reply-message-map (&optional reply-all)
  "Reply to the message at point.
If called with \\[universal-argument], email will be REPLY-ALL."
  (interactive "P")
  (let* ((email (tabulated-list-get-entry))
         (id (substring-no-properties (elt email 0)))
         (subject (substring-no-properties (elt email 2))))
    (setq himalaya-id id)
    (setq himalaya-subject subject)
    (himalaya-read-message-reply reply-all)))

(defun himalaya-forward-message-map ()
  "Forward the email at point."
  (interactive)
  (let* ((email (tabulated-list-get-entry))
         (id (substring-no-properties (elt email 0)))
         (subject (substring-no-properties (elt email 2))))
    (setq himalaya-id id)
    (setq himalaya-subject subject)
    (himalaya-read-message-forward)))

(defun himalaya-copy-messages-map ()
  "Copy message(s) associated to marked envelope(s), or to the
envelope at point if mark is not set."
  (interactive)
  (himalaya--pick-folder
   "Copy to folder: "
   (lambda (target)
     (himalaya--message-copy
      (or himalaya-marked-ids (list (tabulated-list-get-id)))
      himalaya-folder
      target
      (lambda (status)
	(message "%s" status)
	(himalaya-unmark-all t)
	(revert-buffer t t t))
      himalaya-account))))

(defun himalaya-move-messages-map ()
  "Move message(s) associated to marked envelope(s), or to the
envelope at point if mark is not set."
  (interactive)
  (himalaya--pick-folder
   "Move to folder: "
   (lambda (target)
     (himalaya--message-move
      (or himalaya-marked-ids (list (tabulated-list-get-id)))
      himalaya-folder
      target
      (lambda (status)
	(message "%s" status)
	(himalaya-unmark-all t)
	(revert-buffer t t t))
      himalaya-account))))

(defun himalaya-delete-messages-map ()
  "Delete message(s) associated to marked envelope(s), or to the
envelope at point if mark is not set."
  (interactive)
  (let* ((envelope (tabulated-list-get-entry))
         (subject (substring-no-properties (elt envelope 2))))
    (when (y-or-n-p (format "Delete message(s) %s? " (if himalaya-marked-ids (string-join himalaya-marked-ids ", ") subject)))
      (himalaya--message-delete
       (or himalaya-marked-ids (tabulated-list-get-id))
       (lambda (status)
	 (message "%s" status)
	 (himalaya-unmark-all t)
	 (revert-buffer t t t))
       himalaya-account
       himalaya-folder))))

(defun himalaya-download-attachments-map ()
  "Download attachment(s) associated to marked envelope(s), or to
the envelope at point if mark is not set."
  (interactive)
  (himalaya--attachment-download
   (or himalaya-marked-ids (list (tabulated-list-get-id)))
   (lambda (status)
     (message "%s" status)
     (himalaya-unmark-all t))
   himalaya-account
   himalaya-folder))

(defun himalaya-mark-forward-map ()
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

(defun himalaya-unmark-backward-map ()
  "Unmark the envelope at point and move the cursor to the previous
line."
  (interactive)
  (previous-line)
  (goto-char (point-at-bol))
  (let ((inhibit-read-only t))
    (setq himalaya-marked-ids (remove (tabulated-list-get-id) himalaya-marked-ids))
    (tabulated-list-put-tag "")
    (himalaya--restore-face-property (point-at-bol) (point-at-eol))))

(defun himalaya-unmark-forward-map ()
  "Unmark the envelope at point and move the cursor to the next
line."
  (interactive)
  (let ((inhibit-read-only t))
    (setq himalaya-marked-ids (remove (tabulated-list-get-id) himalaya-marked-ids))
    (tabulated-list-put-tag "")
    (himalaya--restore-face-property (point-at-bol) (point-at-eol)))
  (next-line)
  (goto-char (point-at-bol)))

(defun himalaya-unmark-all (&optional quiet)
  "Unmark all marked envelopes."
  (interactive)
  (when himalaya-marked-ids
    (save-excursion
      (let ((inhibit-read-only t))
        (goto-char (point-min))
        (while (re-search-forward (format "^[%s]" (string dired-marker-char)) nil t)
          (himalaya--restore-face-property (point-at-bol) (point-at-eol)))
        (tabulated-list-clear-all-tags)))
    (unless quiet (message "%d marks removed" (length himalaya-marked-ids)))
    (setq himalaya-marked-ids nil)))

(defvar himalaya-list-envelopes-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c a") #'himalaya-switch-account-then-reload)
    (define-key map (kbd "s"    ) #'himalaya-sync-account-then-reload)

    (define-key map (kbd "C-c f") #'himalaya-switch-folder-then-reload)
    (define-key map (kbd "e"    ) #'himalaya-expunge-folder-then-reload)

    (define-key map (kbd "f") #'himalaya-forward-envelopes-page-map)
    (define-key map (kbd "b") #'himalaya-backward-envelopes-page-map)
    (define-key map (kbd "j") #'himalaya-jump-to-envelopes-page-map)

    (define-key map (kbd "RET") #'himalaya-read-message-map)
    (define-key map (kbd "w"  ) #'himalaya-write-message-map)
    (define-key map (kbd "R"  ) #'himalaya-reply-message-map)
    (define-key map (kbd "F"  ) #'himalaya-forward-message-map)
    (define-key map (kbd "C"  ) #'himalaya-copy-messages-map)
    (define-key map (kbd "M"  ) #'himalaya-move-messages-map)
    (define-key map (kbd "D"  ) #'himalaya-delete-messages-map)

    (define-key map (kbd "a") #'himalaya-download-attachments-map)

    (define-key map (kbd "m"  ) #'himalaya-mark-forward-map)
    (define-key map (kbd "DEL") #'himalaya-unmark-backward-map)
    (define-key map (kbd "u"  ) #'himalaya-unmark-forward-map)
    (define-key map (kbd "U"  ) #'himalaya-unmark-all)

    map))

(define-derived-mode himalaya-list-envelopes-mode tabulated-list-mode "Himalaya-Envelopes"
  "Himalaya email client email list mode."
  (setq tabulated-list-format
	(vector
         (list "ID" 5 nil :right-align t)
         (list "Flags" 6 nil)
         (list "Subject" himalaya-subject-width nil)
         (list "From" himalaya-from-width nil)
         (list "Date" 19 nil)))
  (setq tabulated-list-sort-key nil)
  (setq tabulated-list-entries #'himalaya--envelope-list-build-table)
  (setq tabulated-list-padding 1)
  (tabulated-list-init-header)
  (hl-line-mode))

(defvar himalaya-read-message-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a") #'himalaya-read-message-download-attachment)
    (define-key map (kbd "R") #'himalaya-read-message-switch-raw)
    (define-key map (kbd "r") #'himalaya-read-message-reply)
    (define-key map (kbd "f") #'himalaya-read-message-forward)
    (define-key map (kbd "q") #'kill-current-buffer)
    (define-key map (kbd "n") #'himalaya-next-email)
    (define-key map (kbd "p") #'himalaya-previous-email)
    map))

(define-derived-mode himalaya-read-message-mode message-mode "Himalaya-Read"
  "Himalaya email client reading mode.")

(defvar himalaya-read-message-raw-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a") #'himalaya-read-message-download-attachment)
    (define-key map (kbd "R") #'himalaya-read-message-switch-plain)
    (define-key map (kbd "r") #'himalaya-read-message-reply)
    (define-key map (kbd "f") #'himalaya-read-message-forward)
    (define-key map (kbd "q") #'kill-current-buffer)
    (define-key map (kbd "n") #'himalaya-next-email)
    (define-key map (kbd "p") #'himalaya-previous-email)
    map))

(define-derived-mode himalaya-read-message-raw-mode message-mode "Himalaya-Read-Raw"
  "Himalaya email client raw message mode.")

;; Message write mode

(defun himalaya-send-buffer ()
  "Send the current buffer."
  (interactive)
  (himalaya--template-send
   (buffer-string)
   (lambda (status)
     (message "%s" status)
     (set-buffer-modified-p nil)
     (kill-current-buffer))
   himalaya-account))

(defvar himalaya-message-write-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'himalaya-send-buffer)
    map))

(define-derived-mode himalaya-message-write-mode message-mode "Himalaya-Write"
  "Himalaya email client writing mode.")

(provide 'himalaya)
;;; himalaya.el ends here
