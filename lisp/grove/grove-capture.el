;;; grove-capture.el --- Quick note capture for grove -*- lexical-binding: t -*-

;; Copyright 2026 Jonathan Chu

;; Author: Jonathan Chu <me@jonathanchu.is>
;; URL: https://github.com/jonathanchu/grove

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Quick note capture for grove.  Opens a temporary org buffer where
;; the user can type freely.  On save, the first non-empty line becomes
;; the title and the note is saved to the inbox directory.

;;; Code:

(require 'grove)

(defvar grove-capture-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'grove-capture-finalize)
    (define-key map (kbd "C-c C-k") #'grove-capture-cancel)
    map)
  "Keymap for `grove-capture-mode'.")

(define-minor-mode grove-capture-mode
  "Minor mode active in grove capture buffers."
  :lighter " Grove-Capture"
  :keymap grove-capture-mode-map)

(defun grove-capture--sanitize-filename (title)
  "Convert TITLE into a safe filename.
Downcases, replaces spaces with hyphens, strips non-alphanumeric characters."
  (let ((name (downcase (string-trim title))))
    (setq name (replace-regexp-in-string "[^a-z0-9 -]" "" name))
    (setq name (replace-regexp-in-string "\\s-+" "-" name))
    (setq name (replace-regexp-in-string "^-+\\|-+$" "" name))
    (if (string-empty-p name) "untitled" name)))

(defun grove-capture--unique-path (directory filename)
  "Return a unique file path in DIRECTORY for FILENAME.
Appends a numeric suffix if the file already exists."
  (let ((base (file-name-sans-extension filename))
        (ext (file-name-extension filename t))
        (path (expand-file-name filename directory)))
    (if (not (file-exists-p path))
        path
      (let ((n 1))
        (while (file-exists-p
                (setq path (expand-file-name
                            (concat base (format "-%d" n) ext)
                            directory)))
          (setq n (1+ n)))
        path))))

;;;###autoload
(defun grove-capture ()
  "Open a buffer for quick note capture.
Type freely, then press \\[grove-capture-finalize] to save or
\\[grove-capture-cancel] to discard."
  (interactive)
  (grove--ensure-directory)
  (let ((buf (get-buffer-create "*grove-capture*")))
    (switch-to-buffer buf)
    (org-mode)
    (grove-capture-mode 1)
    (let ((inhibit-read-only t))
      (erase-buffer))
    (setq-local header-line-format
                (substitute-command-keys
                 "Capture: \\[grove-capture-finalize] to save, \\[grove-capture-cancel] to discard"))
    (message "Type your note. First line becomes the title.")))

(defun grove-capture-finalize ()
  "Save the capture buffer as a new note in the inbox."
  (interactive)
  (let ((content (string-trim (buffer-string))))
    (if (string-empty-p content)
        (progn
          (grove-capture-cancel)
          (message "Empty note discarded"))
      (let* ((lines (split-string content "\n"))
             (title (string-trim (car lines)))
             (body (string-join (cdr lines) "\n"))
             (filename (concat (grove-capture--sanitize-filename title) ".org"))
             (path (grove-capture--unique-path (grove--inbox-path) filename)))
        (with-temp-file path
          (insert "#+title: " title "\n")
          (unless (string-empty-p body)
            (insert "\n" body "\n")))
        (kill-buffer (current-buffer))
        (find-file path)
        (message "Note saved: %s" (file-name-nondirectory path))))))

(defun grove-capture-cancel ()
  "Discard the capture buffer without saving."
  (interactive)
  (kill-buffer (current-buffer))
  (message "Capture cancelled"))

(provide 'grove-capture)
;;; grove-capture.el ends here
