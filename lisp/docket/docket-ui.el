;;; docket-ui.el --- Layout management for docket -*- lexical-binding: t -*-

;; Copyright 2026 Jonathan Chu

;; Author: Jonathan Chu <me@jonathanchu.is>
;; URL: https://github.com/jonathanchu/docket

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

;; Layout management for docket.  Provides `docket-open' and `docket-close'
;; to create and tear down the GTD workspace layout with a projects sidebar
;; and main task view.

;;; Code:

(require 'docket-core)

;;;; Window configuration

(defvar docket--saved-window-config nil
  "Saved window configuration from before `docket-open' was called.")

(defvar docket--active-p nil
  "Non-nil when the docket UI is currently active.")

(defvar docket--main-buffer nil
  "The buffer currently displayed in the main view area.")

;;;; Buffer names

(defconst docket--sidebar-buffer-name "*docket-sidebar*"
  "Buffer name for the projects sidebar.")

(defconst docket--today-buffer-name "*docket-today*"
  "Buffer name for the today view.")

(defconst docket--upcoming-buffer-name "*docket-upcoming*"
  "Buffer name for the upcoming view.")

(defconst docket--filter-buffer-name "*docket-filter*"
  "Buffer name for filter results.")

;;;; Sidebar window management

(defun docket--sidebar-window ()
  "Return the sidebar window if it exists, or nil."
  (get-buffer-window docket--sidebar-buffer-name))

(defun docket--main-window ()
  "Return the main view window (the non-sidebar window)."
  (let ((sidebar-win (docket--sidebar-window)))
    (if sidebar-win
        (let ((main nil))
          (walk-windows
           (lambda (w)
             (unless (eq w sidebar-win)
               (setq main w)))
           nil (window-frame sidebar-win))
          main)
      (selected-window))))

(defun docket--display-in-main (buffer)
  "Display BUFFER in the main view window."
  (let ((win (docket--main-window)))
    (when win
      (set-window-buffer win buffer)
      (setq docket--main-buffer buffer))))

;;;; Open / Close

(declare-function docket-sidebar-open "docket-sidebar")
(declare-function docket-sidebar-close "docket-sidebar")
(declare-function docket--render-today "docket-today")

;;;###autoload
(defun docket-open ()
  "Open the docket workspace.
Saves the current window configuration, opens the sidebar,
and displays the today view."
  (interactive)
  (docket--ensure-files)
  (setq docket--saved-window-config (current-window-configuration))
  (delete-other-windows)
  (docket--refresh-cache)
  (require 'docket-sidebar)
  (require 'docket-today)
  (docket-sidebar-open)
  ;; Show today view in the main window
  (let ((main-win (docket--main-window)))
    (when main-win
      (select-window main-win)
      (docket--render-today)
      (setq docket--main-buffer (current-buffer))))
  (setq docket--active-p t)
  (message "Docket: %d tasks from %d files"
           (length docket--cache) (length docket-files)))

;;;###autoload
(defun docket-close ()
  "Close the docket workspace and restore the previous window configuration."
  (interactive)
  (require 'docket-sidebar)
  (docket-sidebar-close)
  ;; Kill docket view buffers
  (dolist (name (list docket--today-buffer-name
                      docket--upcoming-buffer-name
                      docket--filter-buffer-name))
    (when-let ((buf (get-buffer name)))
      (kill-buffer buf)))
  (when docket--saved-window-config
    (set-window-configuration docket--saved-window-config)
    (setq docket--saved-window-config nil))
  (setq docket--active-p nil)
  (setq docket--main-buffer nil)
  (message "Docket closed"))

;;;###autoload
(defun docket-toggle ()
  "Toggle the docket workspace on or off."
  (interactive)
  (if docket--active-p
      (docket-close)
    (docket-open)))

(provide 'docket-ui)
;;; docket-ui.el ends here
