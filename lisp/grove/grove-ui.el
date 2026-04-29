;;; grove-ui.el --- Layout management for grove -*- lexical-binding: t -*-

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

;; Layout management for grove.  Provides `grove-open' and `grove-close'
;; to create and tear down the Obsidian-like workspace layout.

;;; Code:

(require 'grove-core)
(require 'grove-tree)

;;;; Window configuration

(defvar grove--saved-window-config nil
  "Saved window configuration from before `grove-open' was called.")

(defvar grove--active-p nil
  "Non-nil when the grove UI is currently active.")

;;;; Open / Close

;;;###autoload
(defun grove-open ()
  "Open the grove workspace.
Saves the current window configuration, opens the tree sidebar,
and displays a note in the main area."
  (interactive)
  (grove--ensure-directory)
  (setq grove--saved-window-config (current-window-configuration))
  (delete-other-windows)
  (grove--refresh-cache)
  (grove-tree-open)
  ;; Open a note in the main window
  (let ((main-win (or (grove-tree--main-window)
                      (selected-window))))
    (select-window main-win)
    (grove--open-initial-note)
    ;; Update the tree to highlight the note we just opened
    (when-let ((file (buffer-file-name (window-buffer main-win))))
      (grove-tree--set-current-file file)))
  (setq grove--active-p t)
  (message "Grove opened: %s" (abbreviate-file-name grove-directory)))

(defun grove--open-initial-note ()
  "Open an initial note in the current window.
Tries the daily note, then the first note in the vault, otherwise
shows a welcome message."
  (let ((daily (expand-file-name
                (concat (format-time-string grove-daily-format) ".org")
                (grove--daily-path))))
    (cond
     ;; If today's daily note exists, open it
     ((file-exists-p daily)
      (find-file daily))
     ;; Otherwise open the first org file found
     ((let ((notes (grove--note-titles)))
        (when notes
          (find-file (cdar notes))
          t)))
     ;; Empty vault — show a welcome buffer
     (t
      (grove--show-welcome)))))

(defun grove--show-welcome ()
  "Display a welcome buffer for an empty vault."
  (let ((buf (get-buffer-create "*grove-welcome*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "Welcome to Grove\n\n" 'face 'bold))
        (insert "Your vault is empty.  Get started:\n\n")
        (insert "  C-c v n  — Capture a new note\n")
        (insert "  C-c v d  — Create today's daily note\n"))
      (special-mode))
    (switch-to-buffer buf)))

;;;###autoload
(defun grove-close ()
  "Close the grove workspace and restore the previous window configuration."
  (interactive)
  (grove-tree-close)
  (when-let ((buf (get-buffer "*grove-graph*")))
    (when-let ((win (get-buffer-window buf)))
      (delete-window win))
    (kill-buffer buf))
  (when-let ((buf (get-buffer "*grove-welcome*")))
    (kill-buffer buf))
  (when grove--saved-window-config
    (set-window-configuration grove--saved-window-config)
    (setq grove--saved-window-config nil))
  (setq grove--active-p nil)
  (message "Grove closed"))

;;;###autoload
(defun grove-toggle ()
  "Toggle the grove workspace on or off."
  (interactive)
  (if grove--active-p
      (grove-close)
    (grove-open)))

(provide 'grove-ui)
;;; grove-ui.el ends here
