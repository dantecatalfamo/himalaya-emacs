;;; himalaya-process.el --- Process management of email client Himalaya CLI  -*- lexical-binding: t -*-

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

(defun himalaya--clear-io-buffers ()
  (with-current-buffer (get-buffer-create "*Himalaya stdout*")
    (let ((inhibit-read-only t)) (erase-buffer)))
  (with-current-buffer (get-buffer-create "*Himalaya stderr*")
    (let ((inhibit-read-only t)) (erase-buffer))))

(defun himalaya--run (callback input &rest args)
  "Asynchronously run Himalaya CLI with ARGS. CALLBACK is called with
stdout string when Himalaya CLI exits. INPUT is sent to stdin if not
nil. Signals a Lisp error and displays the output on non-zero exit."
  (himalaya--clear-io-buffers)
  (let* ((args (list (when himalaya-config-path (list "-c" himalaya-config-path)) "-o" "json" args))
	 (command (cons himalaya-executable (flatten-list args)))
	 (sentinel (lambda (process event) (himalaya--run-sentinel process callback)))
	 (process (make-process
		   :name "himalaya"
		   :buffer (get-buffer-create "*Himalaya stdout*")
		   :stderr (get-buffer-create "*Himalaya stderr*")
		   :connection-type 'pipe
		   :command command
		   :sentinel sentinel)))
    (when input
      (process-send-string process input)
      (process-send-eof process))))

(defun himalaya--run-sentinel (process callback)
  "Sentinel function for himalaya--async-run make-process."
  (when (eq (process-status process) 'exit)
    (message nil)
    (unless (eq 0 (process-exit-status process))
      (display-buffer "*Himalaya stderr*")
      (error "Himalaya exited with a non-zero status"))
    (funcall callback (with-current-buffer (process-buffer process) (json-parse-string (buffer-string) :object-type 'plist :array-type 'list)))))

(defun himalaya--run-plain (callback input &rest args)
  "Asynchronously run Himalaya CLI with ARGS. CALLBACK is called with
stdout string when Himalaya CLI exits. INPUT is sent to stdin if not
nil. Signals a Lisp error and displays the output on non-zero exit."
  (himalaya--clear-io-buffers)
  (let* ((args (list (when himalaya-config-path (list "-c" himalaya-config-path)) "-o" "plain" args))
	 (command (cons himalaya-executable (flatten-list args)))
	 (sentinel (lambda (process event) (himalaya--run-plain-sentinel process callback)))
	 (process (make-process
		   :name "himalaya"
		   :buffer (get-buffer-create "*Himalaya stdout*")
		   :stderr (get-buffer-create "*Himalaya stderr*")
		   :connection-type 'pipe
		   :command command
		   :sentinel sentinel)))
    (when input
      (process-send-string process input)
      (process-send-eof process))))

(defun himalaya--run-plain-sentinel (process callback)
  "Sentinel function for himalaya--run-plain make-process."
  (when (eq (process-status process) 'exit)
    (message nil)
    (unless (eq 0 (process-exit-status process))
      (display-buffer "*Himalaya stderr*")
      (error "Himalaya exited with a non-zero status"))
    (funcall callback (with-current-buffer (process-buffer process) (buffer-string)))))

(defun himalaya--run-blocking (&rest args)
  "Blocking version of 'himalaya--run'."
  (himalaya--clear-io-buffers)
  (with-temp-buffer
    (let* ((args (list (when himalaya-config-path (list "-c" himalaya-config-path)) "-o" "json" args))
           (exit-status (apply #'call-process himalaya-executable nil t nil (flatten-list args)))
	   (output (buffer-string)))
      (unless (eq 0 exit-status)
        (with-current-buffer-window "*Himalaya stderr*" nil nil (insert output))
        (error "Himalaya exited with a non-zero status"))
      (json-parse-string output :object-type 'plist :array-type 'list))))

(provide 'himalaya-process)
;;; himalaya-process.el ends here
