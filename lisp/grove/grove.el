;;; grove.el --- Obsidian-like note-taking for org files -*- lexical-binding: t -*-

;; Copyright 2026 Jonathan Chu

;; Author: Jonathan Chu <me@jonathanchu.is>
;; URL: https://github.com/jonathanchu/grove
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: notes outlines tools

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

;; Grove is a simple, fast note-taking mode for Emacs that provides an
;; Obsidian-like experience for org files.  One keybinding opens a full
;; UI with a file tree sidebar and note editing area.
;;
;; Features:
;; - Built-in file tree sidebar
;; - Quick note capture
;; - Wikilinks with backlinks (ripgrep-powered, no database)
;; - Daily notes
;; - Full-text and tag search (Consult integration optional)
;; - Graph view (Graphviz-based)
;; - Inbox review for triaging untagged notes
;;
;; Usage:
;;   (setq grove-directory "~/notes/")
;;   (global-grove-mode 1)   ; auto-enable grove-mode in vault files
;;   (grove-open)

;;; Code:

(require 'grove-core)
(require 'grove-ui)
(require 'grove-capture)
(require 'grove-search)
(require 'grove-daily)
(require 'grove-backlink)
(require 'grove-inbox)
(require 'grove-link)
(require 'grove-graph)

;;;; Minor mode

(defvar grove-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-l") #'grove-link-insert)
    map)
  "Keymap for `grove-mode'.")

(define-minor-mode grove-mode
  "Minor mode active in org buffers that are part of a grove vault."
  :lighter " Grove"
  :keymap grove-mode-map
  (if grove-mode
      (grove-link-setup-font-lock)
    (grove-link-remove-font-lock)))

(defun grove--turn-on ()
  "Turn on `grove-mode' if the current buffer is visiting a grove file."
  (when (and (derived-mode-p 'org-mode)
             (buffer-file-name)
             (grove-file-p (buffer-file-name)))
    (grove-mode 1)))

;;;###autoload
(define-globalized-minor-mode global-grove-mode
  grove-mode
  grove--turn-on
  :group 'grove)

;;;; Global keymap

(defvar grove-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "v") #'grove-open)
    (define-key map (kbd "q") #'grove-close)
    (define-key map (kbd "n") #'grove-capture)
    (define-key map (kbd "f") #'grove-find)
    (define-key map (kbd "s") #'grove-search)
    (define-key map (kbd "t") #'grove-search-tag)
    (define-key map (kbd "d") #'grove-daily)
    (define-key map (kbd "b") #'grove-backlinks)
    (define-key map (kbd "i") #'grove-inbox-review)
    (define-key map (kbd "l") #'grove-link-insert)
    (define-key map (kbd "g") #'grove-graph)
    map)
  "Keymap for grove commands, bound under a prefix key.
Bind this to a prefix key in your init file, e.g.:
  (global-set-key (kbd \"C-c v\") grove-command-map)")

(provide 'grove)
;;; grove.el ends here
