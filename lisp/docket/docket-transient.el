;;; docket-transient.el --- Transient help menus for docket -*- lexical-binding: t -*-

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

;; Transient-based contextual help menus for docket views and sidebar.

;;; Code:

(require 'transient)
(require 'docket)

;;;; View help menu

(defun docket-view-help ()
  "Show help menu for docket task views."
  (interactive)
  (let ((transient-display-buffer-action
         '(display-buffer-at-bottom)))
    (call-interactively #'docket-view-help-transient)))

(transient-define-prefix docket-view-help-transient ()
  "Help menu for docket task views."
  ["Task Actions"
   ("x" "Toggle done" docket-view-toggle-done)
   ("RET" "Jump to org" docket-view-jump-to-task)
   ("e" "Cycle priority" docket-view-cycle-priority)
   ("t" "Change state" docket-view-set-state)
   ("/" "Move to project" docket-view-refile)]
  ["Dates & Tags"
   ("s" "Set scheduled" docket-view-set-scheduled)
   ("d" "Set deadline" docket-view-set-deadline)
   ("#" "Edit tags" docket-view-set-tags)]
  ["View"
   ("S" "Cycle sort" docket-view-cycle-sort)
   ("H" "Toggle completed" docket-view-toggle-show-done)
   ("g" "Refresh" docket-view-refresh)
   ("q" "Close workspace" docket-close)])

;;;; Sidebar help menu

(defun docket-sidebar-help ()
  "Show help menu for the docket sidebar."
  (interactive)
  (let ((transient-display-buffer-action
         '(display-buffer-at-bottom)))
    (call-interactively #'docket-sidebar-help-transient)))

(transient-define-prefix docket-sidebar-help-transient ()
  "Help menu for the docket sidebar."
  ["Sidebar"
   ("RET" "Open view" docket-sidebar--activate)
   ("n" "Next item" docket-sidebar-next)
   ("p" "Previous item" docket-sidebar-previous)
   ("g" "Refresh" docket-sidebar-refresh)
   ("q" "Close sidebar" docket-sidebar-close)])

;;;; Global help menu

(transient-define-prefix docket-help ()
  "Help menu for docket commands."
  ["Workspace"
   ("d" "Open docket" docket-open)
   ("q" "Close docket" docket-close)]
  ["Views"
   ("t" "Today" docket-view-today)
   ("u" "Upcoming" docket-view-upcoming)
   ("f" "Filter" docket-filter)
   ("l" "Labels" docket-view-labels)]
  ["Actions"
   ("a" "Quick add" docket-capture)
   ("p" "New project" docket-create-project)
   ("r" "Refresh cache" docket-refresh)])

(provide 'docket-transient)
;;; docket-transient.el ends here
