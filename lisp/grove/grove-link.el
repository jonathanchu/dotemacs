;;; grove-link.el --- Wikilink support for grove -*- lexical-binding: t -*-

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

;; Wikilink support for grove.  Provides font-lock highlighting and
;; follow-link behavior for [[wikilinks]] in org buffers that are
;; part of a grove vault.

;;; Code:

(require 'grove-core)

;;;; Faces

(defface grove-link
  '((t :inherit link))
  "Face for grove wikilinks."
  :group 'grove)

;;;; Font-lock

(defconst grove-link--regexp
  "\\[\\[\\([^]:\n]+\\)\\]\\]"
  "Regexp matching grove wikilinks.
Matches [[text]] but not [[protocol:text]] (standard org links).")

(defvar grove-link--keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'grove-link-follow-at-point)
    (define-key map (kbd "RET") #'grove-link-follow-at-point)
    map)
  "Keymap active on grove wikilinks.")

(defun grove-link--fontify (limit)
  "Font-lock matcher for grove wikilinks up to LIMIT.
Adds the grove-link face and a clickable keymap."
  (while (re-search-forward grove-link--regexp limit t)
    (let ((start (match-beginning 0))
          (end (match-end 0)))
      (add-text-properties
       start end
       (list 'face 'grove-link
             'mouse-face 'highlight
             'keymap grove-link--keymap
             'help-echo (format "grove: %s" (match-string 1))
             'grove-link-target (match-string 1)))))
  nil)

(defun grove-link-setup-font-lock ()
  "Add grove wikilink font-lock keywords to the current buffer."
  (font-lock-add-keywords
   nil
   '((grove-link--fontify))
   'append)
  (font-lock-flush))

(defun grove-link-remove-font-lock ()
  "Remove grove wikilink font-lock keywords from the current buffer."
  (font-lock-remove-keywords
   nil
   '((grove-link--fontify)))
  (font-lock-flush))

;;;; Follow

(defun grove-link--resolve (title)
  "Resolve TITLE to a file path in the vault.
Returns the path if found, or nil.  If multiple matches exist,
prompt the user to choose."
  (grove--refresh-cache)
  (let ((matches
         (cl-remove-if-not
          (lambda (pair)
            (string-equal-ignore-case (car pair) title))
          (grove--note-titles))))
    (cond
     ((null matches) nil)
     ((= (length matches) 1) (cdar matches))
     (t (let ((choice (completing-read
                       (format "Multiple matches for \"%s\": " title)
                       (mapcar #'car matches)
                       nil t)))
          (cdr (assoc choice matches)))))))

(defun grove-link--target-at-point ()
  "Return the grove link target at point, or nil."
  (get-text-property (point) 'grove-link-target))

;;;###autoload
(defun grove-link-follow-at-point ()
  "Follow the grove wikilink at point."
  (interactive)
  (if-let ((target (grove-link--target-at-point)))
      (grove-link-follow target)
    (user-error "No grove link at point")))

(defun grove-link-follow (title)
  "Follow a grove wikilink to TITLE.
If the note doesn't exist, offer to create it."
  (let ((path (grove-link--resolve title)))
    (if path
        (find-file path)
      (if (y-or-n-p (format "Note \"%s\" not found.  Create it? " title))
          (let* ((filename (concat (grove--sanitize-filename title) ".org"))
                 (path (expand-file-name filename grove-directory)))
            (find-file path)
            (insert "#+title: " title "\n\n"))
        (message "Link not followed")))))

;;;; Insert

;;;###autoload
(defun grove-link-insert ()
  "Insert a grove wikilink by choosing from existing notes."
  (interactive)
  (grove--refresh-cache)
  (let* ((titles (grove--note-titles))
         (choice (completing-read "Link to: " (mapcar #'car titles) nil nil)))
    (insert "[[" choice "]]")))

(provide 'grove-link)
;;; grove-link.el ends here
