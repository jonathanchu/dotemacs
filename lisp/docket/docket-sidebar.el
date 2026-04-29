;;; docket-sidebar.el --- Projects sidebar for docket -*- lexical-binding: t -*-

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

;; An ewoc-based projects sidebar displayed in a side window.
;; Shows views (Today, Upcoming), projects with task counts, and labels.

;;; Code:

(require 'cl-lib)
(require 'ewoc)
(require 'docket)

(declare-function hl-line-highlight "hl-line")

;;;; Faces

(defface docket-sidebar-section
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for section headers in the sidebar."
  :group 'docket)

(defface docket-sidebar-item
  '((t :inherit default))
  "Face for items in the sidebar."
  :group 'docket)

(defface docket-sidebar-count
  '((t :inherit font-lock-comment-face))
  "Face for task counts in the sidebar."
  :group 'docket)

(defface docket-sidebar-active
  '((t :inherit font-lock-constant-face :weight bold))
  "Face for the currently active view/filter."
  :group 'docket)

;;;; Data model

(cl-defstruct (docket-sidebar-node (:constructor docket-sidebar-node-create))
  "A node in the sidebar."
  type      ; 'section, 'view, 'project, or 'label
  name      ; display name
  key       ; identifier for filtering (project name, tag, or view keyword)
  count)    ; number of tasks (nil for sections)

;;;; State

(defvar-local docket-sidebar--ewoc nil
  "The ewoc instance for the sidebar buffer.")

(defvar-local docket-sidebar--active nil
  "The currently active filter key in the sidebar.")

(defconst docket-sidebar--buffer-name "*docket-sidebar*"
  "Name of the sidebar buffer.")

;;;; Rendering

(defun docket-sidebar--print (node)
  "Print NODE as a line in the sidebar ewoc."
  (let* ((type (docket-sidebar-node-type node))
         (name (docket-sidebar-node-name node))
         (key (docket-sidebar-node-key node))
         (count (docket-sidebar-node-count node))
         (active-p (and docket-sidebar--active
                        (equal key docket-sidebar--active))))
    (pcase type
      ('section
       (insert (propertize name 'face 'docket-sidebar-section)))
      (_
       (let ((indent (if (eq type 'view) "  " "  "))
             (face (if active-p 'docket-sidebar-active 'docket-sidebar-item))
             (count-str (if count
                            (propertize (format " %d" count)
                                        'face 'docket-sidebar-count)
                          "")))
         (insert indent (propertize name 'face face) count-str))))))

(defun docket-sidebar--build-nodes ()
  "Build the list of sidebar nodes from the current cache."
  (let ((nodes '())
        (today-count (length (docket--today-tasks)))
        (upcoming-count (length (docket--upcoming-tasks)))
        (active-count (length (docket--active-tasks)))
        (inbox-tasks (length (docket--tasks-for-project docket-inbox-heading))))
    ;; Views section
    (push (docket-sidebar-node-create :type 'section :name "Views") nodes)
    (push (docket-sidebar-node-create :type 'view :name "Inbox"
                                      :key "inbox" :count inbox-tasks)
          nodes)
    (push (docket-sidebar-node-create :type 'view :name "Today"
                                      :key "today" :count today-count)
          nodes)
    (push (docket-sidebar-node-create :type 'view :name "Upcoming"
                                      :key "upcoming" :count upcoming-count)
          nodes)
    (push (docket-sidebar-node-create :type 'view :name "All"
                                      :key "all" :count active-count)
          nodes)
    ;; Projects section
    (push (docket-sidebar-node-create :type 'section :name "Projects") nodes)
    (dolist (project docket--projects)
      (unless (equal project docket-inbox-heading)
        (let ((count (length (docket--tasks-for-project project))))
          (when (> count 0)
            (push (docket-sidebar-node-create
                   :type 'project :name project
                   :key project :count count)
                  nodes)))))
    ;; Labels section
    (let ((tags (docket--all-tags)))
      (when tags
        (push (docket-sidebar-node-create :type 'section :name "Labels") nodes)
        (dolist (tag-count tags)
          (push (docket-sidebar-node-create
                 :type 'label :name (car tag-count)
                 :key (car tag-count) :count (cdr tag-count))
                nodes))))
    (nreverse nodes)))

;;;; Interaction

(declare-function docket-sidebar-help "docket-transient")
(declare-function docket--render-today "docket-today")
(declare-function docket--render-upcoming "docket-upcoming")
(declare-function docket--render-filter "docket-filter")
(declare-function docket--display-in-main "docket-ui")
(declare-function docket--main-window "docket-ui")

(defun docket-sidebar--activate ()
  "Activate the item at point in the sidebar."
  (interactive)
  (when-let ((ewoc-node (and docket-sidebar--ewoc
                             (ewoc-locate docket-sidebar--ewoc))))
    (let* ((node (ewoc-data ewoc-node))
           (type (docket-sidebar-node-type node))
           (key (docket-sidebar-node-key node)))
      (unless (eq type 'section)
        (setq docket-sidebar--active key)
        (let ((inhibit-read-only t)
              (saved-pos (point)))
          (ewoc-refresh docket-sidebar--ewoc)
          (goto-char saved-pos)
          (hl-line-highlight))
        (pcase key
          ("today"
           (require 'docket-today)
           (docket--render-today))
          ("upcoming"
           (require 'docket-upcoming)
           (docket--render-upcoming))
          ("inbox"
           (require 'docket-filter)
           (docket--render-filter
            :title "Inbox"
            :predicate (lambda (task)
                         (equal (docket-task-project task)
                                docket-inbox-heading))))
          ("all"
           (require 'docket-filter)
           (docket--render-filter
            :title "All Tasks"
            :predicate (lambda (task)
                         (member (docket-task-state task)
                                 docket-todo-states))))
          (_
           (pcase type
             ('project
              (require 'docket-filter)
              (docket--render-filter
               :title key
               :predicate (lambda (task)
                            (equal (docket-task-project task) key))))
             ('label
              (require 'docket-filter)
              (docket--render-filter
               :title (concat "#" key)
               :predicate (lambda (task)
                            (member key (docket-task-tags task))))))))
        ;; Focus the main view window
        (when-let ((main-win (docket--main-window)))
          (select-window main-win))))))

;;;; Navigation

(defun docket-sidebar-next ()
  "Move to the next item, skipping section headers."
  (interactive)
  (forward-line 1)
  (while (and (not (eobp))
              (let ((node (and docket-sidebar--ewoc
                               (ewoc-locate docket-sidebar--ewoc))))
                (and node (eq (docket-sidebar-node-type (ewoc-data node))
                              'section))))
    (forward-line 1)))

(defun docket-sidebar-previous ()
  "Move to the previous item, skipping section headers."
  (interactive)
  (forward-line -1)
  (while (and (not (bobp))
              (let ((node (and docket-sidebar--ewoc
                               (ewoc-locate docket-sidebar--ewoc))))
                (and node (eq (docket-sidebar-node-type (ewoc-data node))
                              'section))))
    (forward-line -1)))

;;;; Refresh

(defun docket-sidebar-refresh ()
  "Rebuild the sidebar from the current cache."
  (interactive)
  (let ((buf (get-buffer docket-sidebar--buffer-name)))
    (when buf
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (setq docket-sidebar--ewoc
                (ewoc-create #'docket-sidebar--print "" ""))
          (dolist (node (docket-sidebar--build-nodes))
            (ewoc-enter-last docket-sidebar--ewoc node)))))))

;;;; Mode

(defvar docket-sidebar-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'docket-sidebar--activate)
    (define-key map (kbd "g") #'docket-sidebar-refresh)
    (define-key map (kbd "q") #'docket-sidebar-close)
    (define-key map (kbd "n") #'docket-sidebar-next)
    (define-key map (kbd "p") #'docket-sidebar-previous)
    (define-key map (kbd "C-n") #'docket-sidebar-next)
    (define-key map (kbd "C-p") #'docket-sidebar-previous)
    (define-key map (kbd "?") #'docket-sidebar-help)
    map)
  "Keymap for `docket-sidebar-mode'.")

(define-derived-mode docket-sidebar-mode special-mode "Docket-Sidebar"
  "Major mode for the docket projects sidebar."
  :group 'docket
  (setq-local cursor-type nil
              truncate-lines t
              mode-line-format nil
              header-line-format (propertize " Docket" 'face 'bold))
  (hl-line-mode 1))

;;;; Open / Close

(defun docket-sidebar-open ()
  "Open the projects sidebar."
  (let ((buf (get-buffer-create docket-sidebar--buffer-name)))
    (with-current-buffer buf
      (docket-sidebar-mode)
      (setq docket-sidebar--active "today")
      (docket-sidebar-refresh))
    (let ((win (display-buffer-in-side-window
                buf
                `((side . left)
                  (slot . 0)
                  (window-width . ,docket-sidebar-width)
                  (window-parameters
                   . ((no-other-window . nil)
                      (no-delete-other-windows . t)))))))
      (when win
        (window-preserve-size win t t))
      buf)))

(defun docket-sidebar-close ()
  "Close the projects sidebar."
  (interactive)
  (let ((win (get-buffer-window docket-sidebar--buffer-name)))
    (when win
      (delete-window win)))
  (when-let ((buf (get-buffer docket-sidebar--buffer-name)))
    (kill-buffer buf)))

(provide 'docket-sidebar)
;;; docket-sidebar.el ends here
