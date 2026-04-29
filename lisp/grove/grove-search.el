;;; grove-search.el --- Search integration for grove -*- lexical-binding: t -*-

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

;; Search integration for grove.  Provides full-text ripgrep search
;; and note finding, with optional consult integration for live
;; preview and narrowing.

;;; Code:

(require 'grove-core)

(declare-function consult--grep "consult")
(declare-function consult--grep-make-builder "consult")
(declare-function consult--find "consult")
(defvar consult-ripgrep-args)

;;;; Full-text search

;;;###autoload
(defun grove-search (&optional initial)
  "Search note contents in the vault.
With optional INITIAL input string.  Uses `consult-ripgrep' when
available, otherwise falls back to `grep'."
  (interactive)
  (grove--ensure-directory)
  (if (featurep 'consult)
      (grove-search--consult-ripgrep initial)
    (grove-search--grep initial)))

(defun grove-search--consult-ripgrep (&optional initial)
  "Search vault with consult-ripgrep.  INITIAL is the initial input."
  (let ((consult-ripgrep-args
         (concat consult-ripgrep-args " --glob=*.org")))
    (consult--grep "Grove search" #'consult--grep-make-builder
                   grove-directory initial)))

(defun grove-search--grep (&optional initial)
  "Search vault with grep.  INITIAL is the initial input."
  (let ((pattern (read-string "Grove search: " initial)))
    (grep (format "rg --no-heading --line-number --glob=*.org %s %s"
                  (shell-quote-argument pattern)
                  (shell-quote-argument grove-directory)))))

;;;; Find note by title

;;;###autoload
(defun grove-find ()
  "Find a note by title using `completing-read' over cached titles."
  (interactive)
  (grove--ensure-directory)
  (grove--refresh-cache)
  (let* ((titles (grove--note-titles))
         (choice (completing-read "Find note: " (mapcar #'car titles) nil t)))
    (when-let ((path (cdr (assoc choice titles))))
      (find-file path))))

;;;; Tag search

;;;###autoload
(defun grove-search-tag (&optional initial)
  "Search for notes by tag.
With optional INITIAL input string.  Searches for both org-style
:tag: and inline #tag patterns."
  (interactive)
  (grove--ensure-directory)
  (let* ((tag (or initial (read-string "Tag: ")))
         (pattern (format "(#%s\\b|:%s:)" tag tag)))
    (if (featurep 'consult)
        (let ((consult-ripgrep-args
               (concat consult-ripgrep-args " --glob=*.org")))
          (consult--grep "Grove tags" #'consult--grep-make-builder
                         grove-directory pattern))
      (grep (format "rg --no-heading --line-number --glob=*.org %s %s"
                    (shell-quote-argument pattern)
                    (shell-quote-argument grove-directory))))))

(provide 'grove-search)
;;; grove-search.el ends here
