;;; grove-backlink.el --- Ripgrep-powered backlinks for grove -*- lexical-binding: t -*-

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

;; Backlinks for grove.  Uses ripgrep to find all notes that link to
;; the current note via [[wikilinks]].  Results are displayed in a
;; dedicated side window with context snippets.

;;; Code:

(require 'grove)

(defconst grove-backlink-buffer-name "*grove-backlinks*"
  "Name of the backlinks buffer.")

;;;; Core

(defun grove-backlink--title-for-file (file)
  "Return the grove title for FILE.
Uses the cache if available, otherwise the filename."
  (let ((meta (gethash file grove--cache)))
    (if meta
        (plist-get meta :title)
      (file-name-sans-extension (file-name-nondirectory file)))))

(defun grove-backlink--find (title)
  "Return a list of backlink results for TITLE.
Each result is a plist (:file :line :context) found via ripgrep."
  (grove--ensure-directory)
  (let* ((pattern (format "\\[\\[%s\\]\\]" (regexp-quote title)))
         (cmd (format "rg --no-heading --line-number --context 1 --glob=*.org %s %s"
                      (shell-quote-argument pattern)
                      (shell-quote-argument grove-directory)))
         (output (shell-command-to-string cmd))
         results current-file)
    (dolist (line (split-string output "\n" t))
      (cond
       ;; Context separator
       ((string-match-p "^--$" line))
       ;; Match line: file:line:content
       ((string-match "^\\(.+\\.org\\):\\([0-9]+\\):\\(.*\\)$" line)
        (let ((file (match-string 1 line))
              (lnum (string-to-number (match-string 2 line)))
              (context (string-trim (match-string 3 line))))
          ;; Skip self-references
          (unless (and current-file
                       (string= file current-file))
            (push (list :file file :line lnum :context context)
                  results))))
       ;; Context line: file-line-content
       ((string-match "^\\(.+\\.org\\)-\\([0-9]+\\)-\\(.*\\)$" line))))
    (setq current-file (buffer-file-name))
    ;; Filter out self-references
    (cl-remove-if
     (lambda (r)
       (and current-file
            (string= (plist-get r :file) current-file)))
     (nreverse results))))

;;;; Display

(defvar grove-backlink-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'grove-backlink--visit)
    (define-key map (kbd "q") #'grove-backlink-close)
    (define-key map (kbd "n") #'next-line)
    (define-key map (kbd "p") #'previous-line)
    map)
  "Keymap for `grove-backlink-mode'.")

(define-derived-mode grove-backlink-mode special-mode "Grove-Backlinks"
  "Major mode for the grove backlinks buffer."
  :group 'grove
  (setq-local truncate-lines t))

(defun grove-backlink--visit ()
  "Visit the backlink at point."
  (interactive)
  (when-let ((file (get-text-property (point) 'grove-backlink-file))
             (line (get-text-property (point) 'grove-backlink-line)))
    (let ((win (or (grove-backlink--main-window)
                   (next-window))))
      (select-window win)
      (find-file file)
      (goto-char (point-min))
      (forward-line (1- line)))))

(defun grove-backlink--main-window ()
  "Return a non-side window suitable for visiting files."
  (catch 'found
    (walk-windows
     (lambda (win)
       (unless (window-parameter win 'window-side)
         (unless (string= (buffer-name (window-buffer win))
                           grove-backlink-buffer-name)
           (throw 'found win)))))))

(defun grove-backlink--render (title results)
  "Render RESULTS for TITLE into the backlinks buffer."
  (let ((buf (get-buffer-create grove-backlink-buffer-name)))
    (with-current-buffer buf
      (grove-backlink-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize (format "Backlinks to \"%s\"" title)
                            'face 'bold)
                "\n\n")
        (if (null results)
            (insert (propertize "No backlinks found." 'face 'shadow))
          (dolist (r results)
            (let ((file (plist-get r :file))
                  (line (plist-get r :line))
                  (context (plist-get r :context))
                  (start (point)))
              (insert (propertize (grove-backlink--title-for-file file)
                                  'face 'font-lock-function-name-face)
                      "\n")
              (insert "  " (propertize context 'face 'shadow) "\n\n")
              (add-text-properties
               start (point)
               (list 'grove-backlink-file file
                     'grove-backlink-line line)))))
        (goto-char (point-min))))
    buf))

;;;; Commands

;;;###autoload
(defun grove-backlinks ()
  "Show backlinks for the current note."
  (interactive)
  (unless (and (buffer-file-name) (grove-file-p (buffer-file-name)))
    (user-error "Not visiting a grove note"))
  (grove--refresh-cache)
  (let* ((meta (gethash (buffer-file-name) grove--cache))
         (title (or (plist-get meta :title)
                    (file-name-sans-extension
                     (file-name-nondirectory (buffer-file-name)))))
         (results (grove-backlink--find title))
         (buf (grove-backlink--render title results)))
    (display-buffer-in-side-window
     buf
     '((side . bottom)
       (slot . 0)
       (window-height . 12)
       (window-parameters
        . ((no-delete-other-windows . t)))))
    (message "Found %d backlink(s)" (length results))))

(defun grove-backlink-close ()
  "Close the backlinks panel."
  (interactive)
  (when-let ((win (get-buffer-window grove-backlink-buffer-name)))
    (delete-window win))
  (when-let ((buf (get-buffer grove-backlink-buffer-name)))
    (kill-buffer buf)))

(provide 'grove-backlink)
;;; grove-backlink.el ends here
