;;; docket.el --- Todoist-like GTD for org files -*- lexical-binding: t -*-

;; Copyright 2026 Jonathan Chu

;; Author: Jonathan Chu <me@jonathanchu.is>
;; URL: https://github.com/jonathanchu/docket
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: outlines convenience

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

;; Docket is a Todoist-like GTD task management system for Emacs built
;; on top of org files.  It provides a polished UI with a projects
;; sidebar, today/upcoming views, quick-add with natural language
;; parsing, and saved filters.
;;
;; Usage:
;;   (setq docket-files '("~/org/gtd.org"))
;;   (global-set-key (kbd "C-c d") docket-command-map)
;;   (docket-open)

;;; Code:

(require 'docket-core)
(require 'docket-ui)
(require 'docket-capture)
(require 'docket-today)
(require 'docket-sidebar)
(require 'docket-upcoming)
(require 'docket-filter)
(require 'docket-transient)

;;;; Command map

(defvar docket-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a") #'docket-capture)
    (define-key map (kbd "d") #'docket-open)
    (define-key map (kbd "f") #'docket-filter)
    (define-key map (kbd "l") #'docket-view-labels)
    (define-key map (kbd "p") #'docket-create-project)
    (define-key map (kbd "q") #'docket-close)
    (define-key map (kbd "r") #'docket-refresh)
    (define-key map (kbd "t") #'docket-view-today)
    (define-key map (kbd "u") #'docket-view-upcoming)
    map)
  "Keymap for docket commands, bound under a prefix key.
Bind this to a prefix key in your init file, e.g.:
  (global-set-key (kbd \"C-c d\") docket-command-map)")

;;;###autoload
(defun docket-view-today ()
  "Show the today view."
  (interactive)
  (docket--ensure-files)
  (unless docket--cache (docket--refresh-cache))
  (docket--render-today))

(provide 'docket)

;;; docket.el ends here
