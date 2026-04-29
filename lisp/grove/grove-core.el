;;; grove-core.el --- Core definitions for grove -*- lexical-binding: t -*-

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

;; Shared customization options, vault cache, and utility functions
;; used across all grove modules.

;;; Code:

(require 'cl-lib)

;;;; Customization

(defgroup grove nil
  "Obsidian-like note-taking for org files."
  :group 'org
  :prefix "grove-")

(defcustom grove-directory nil
  "Root directory for grove notes.
All org files in this directory and its subdirectories are
considered part of the vault."
  :type '(choice (const nil) directory)
  :group 'grove)

(defcustom grove-inbox-directory "inbox"
  "Subdirectory of `grove-directory' for captured notes.
Relative to `grove-directory'."
  :type 'string
  :group 'grove)

(defcustom grove-daily-directory "daily"
  "Subdirectory of `grove-directory' for daily notes.
Relative to `grove-directory'."
  :type 'string
  :group 'grove)

(defcustom grove-daily-format "%Y-%m-%d"
  "Format string for daily note filenames.
Passed to `format-time-string'."
  :type 'string
  :group 'grove)

;;;; Vault cache

(defvar grove--cache (make-hash-table :test #'equal)
  "Hash table mapping absolute file paths to note metadata plists.
Each value is a plist with keys :title :tags :links :mtime.")

(defun grove--ensure-directory ()
  "Ensure `grove-directory' is set and exists, or prompt the user."
  (unless grove-directory
    (setq grove-directory
          (read-directory-name "Grove vault directory: ")))
  (unless (file-directory-p grove-directory)
    (if (y-or-n-p (format "Directory %s does not exist.  Create it? "
                          grove-directory))
        (make-directory grove-directory t)
      (user-error "Grove requires a vault directory")))
  (setq grove-directory (file-name-as-directory
                         (expand-file-name grove-directory))))

(defun grove--inbox-path ()
  "Return the absolute path to the inbox directory, creating it if needed."
  (grove--ensure-directory)
  (let ((path (expand-file-name grove-inbox-directory grove-directory)))
    (unless (file-directory-p path)
      (make-directory path t))
    (file-name-as-directory path)))

(defun grove--daily-path ()
  "Return the absolute path to the daily notes directory, creating it if needed."
  (grove--ensure-directory)
  (let ((path (expand-file-name grove-daily-directory grove-directory)))
    (unless (file-directory-p path)
      (make-directory path t))
    (file-name-as-directory path)))

(defun grove--parse-note (file)
  "Parse an org FILE and return a metadata plist.
Returns (:title TITLE :tags TAGS :links LINKS :mtime MTIME)."
  (let ((mtime (file-attribute-modification-time (file-attributes file)))
        title tags links)
    (with-temp-buffer
      (insert-file-contents file nil 0 4096)
      ;; Extract #+title:
      (goto-char (point-min))
      (when (re-search-forward "^#\\+title:\\s-*\\(.+\\)" nil t)
        (setq title (string-trim (match-string 1))))
      ;; Extract #+filetags: or inline #hashtags
      (goto-char (point-min))
      (when (re-search-forward "^#\\+filetags:\\s-*\\(.+\\)" nil t)
        (setq tags (split-string (match-string 1) ":" t "\\s-*")))
      ;; Extract [[wikilinks]]
      (goto-char (point-min))
      (while (re-search-forward "\\[\\[\\([^]]+\\)\\]\\]" nil t)
        (let ((link (match-string 1)))
          ;; Skip standard org links (contain a colon protocol)
          (unless (string-match-p ":" link)
            (push link links)))))
    (list :title (or title (file-name-sans-extension
                            (file-name-nondirectory file)))
          :tags tags
          :links (nreverse links)
          :mtime mtime)))

(defun grove--refresh-cache ()
  "Refresh the vault cache by scanning `grove-directory'.
Only re-parses files whose mtime has changed."
  (grove--ensure-directory)
  (let ((files (directory-files-recursively grove-directory "\\.org\\'"))
        (seen (make-hash-table :test #'equal)))
    ;; Update or add entries
    (dolist (file files)
      (puthash file t seen)
      (let* ((cached (gethash file grove--cache))
             (current-mtime (file-attribute-modification-time
                             (file-attributes file)))
             (cached-mtime (plist-get cached :mtime)))
        (when (or (null cached)
                  (not (equal current-mtime cached-mtime)))
          (puthash file (grove--parse-note file) grove--cache))))
    ;; Remove deleted files
    (maphash (lambda (key _val)
               (unless (gethash key seen)
                 (remhash key grove--cache)))
             grove--cache)))

(defun grove--note-titles ()
  "Return an alist of (TITLE . PATH) for all cached notes."
  (let (result)
    (maphash (lambda (path meta)
               (push (cons (plist-get meta :title) path) result))
             grove--cache)
    (sort result (lambda (a b) (string< (car a) (car b))))))

(defun grove--sanitize-filename (title)
  "Convert TITLE into a safe filename.
Downcases, replaces spaces with hyphens, strips non-alphanumeric characters."
  (let ((name (downcase (string-trim title))))
    (setq name (replace-regexp-in-string "[^a-z0-9 -]" "" name))
    (setq name (replace-regexp-in-string "\\s-+" "-" name))
    (setq name (replace-regexp-in-string "^-+\\|-+$" "" name))
    (if (string-empty-p name) "untitled" name)))

(defun grove-file-p (file)
  "Return non-nil if FILE is inside `grove-directory'."
  (and grove-directory
       file
       (string-prefix-p (expand-file-name grove-directory)
                        (expand-file-name file))))

(provide 'grove-core)
;;; grove-core.el ends here
