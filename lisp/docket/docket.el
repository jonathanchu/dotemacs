;;; docket.el --- Todoist-like GTD for org files -*- lexical-binding: t -*-

;; Copyright 2026 Jonathan Chu

;; Author: Jonathan Chu <me@jonathanchu.is>
;; URL: https://github.com/jonathanchu/docket
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: org-mode gtd productivity

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

(require 'cl-lib)
(require 'org)
(require 'org-element)

;;;; Customization

(defgroup docket nil
  "Todoist-like GTD for org files."
  :group 'org
  :prefix "docket-")

(defcustom docket-files nil
  "List of org files to use as GTD sources.
Each entry should be an absolute path to an org file."
  :type '(repeat file)
  :group 'docket)

(defcustom docket-inbox-heading "Inbox"
  "Name of the heading used as the inbox in GTD files.
New tasks captured without a project are filed under this heading."
  :type 'string
  :group 'docket)

(defcustom docket-todo-states '("TODO" "NEXT" "WAIT" "SOMEDAY")
  "Active TODO states recognized by docket."
  :type '(repeat string)
  :group 'docket)

(defcustom docket-done-states '("DONE" "CANCELLED")
  "Done states recognized by docket."
  :type '(repeat string)
  :group 'docket)

(defcustom docket-sidebar-width 32
  "Width of the projects sidebar in columns."
  :type 'integer
  :group 'docket)

;;;; Task structure

(cl-defstruct (docket-task (:constructor docket-task-create))
  "A single task parsed from an org heading."
  id title state priority tags deadline scheduled closed
  project file point level)

;;;; Cache

(defvar docket--cache nil
  "List of `docket-task' structs parsed from `docket-files'.")

(defvar docket--projects nil
  "List of project names extracted from `docket-files'.")

(defvar docket--cache-tick 0
  "Counter incremented on each cache refresh.")

(defun docket--ensure-files ()
  "Ensure `docket-files' is configured and files exist."
  (unless docket-files
    (user-error "Please set `docket-files' to a list of org file paths"))
  (dolist (f docket-files)
    (unless (file-exists-p f)
      (user-error "Docket file does not exist: %s" f))))

(defun docket--parse-timestamp (ts)
  "Parse an org timestamp element TS into an Emacs time value.
Returns nil if TS is nil."
  (when ts
    (let ((raw (org-element-property :raw-value ts)))
      (when (and raw (string-match org-ts-regexp-both raw))
        (encode-time (org-parse-time-string raw))))))

(defun docket--parse-file (file)
  "Parse FILE and return a list of `docket-task' structs."
  (let (tasks current-project
        (title-counts (make-hash-table :test #'equal)))
    (with-temp-buffer
      (insert-file-contents file)
      (delay-mode-hooks (org-mode))
      (org-element-map (org-element-parse-buffer 'headline) 'headline
        (lambda (hl)
          (let* ((level (org-element-property :level hl))
                 (title (org-element-property :raw-value hl))
                 (todo (org-element-property :todo-keyword hl))
                 (priority (org-element-property :priority hl))
                 (tags (org-element-property :tags hl))
                 (deadline (docket--parse-timestamp
                            (org-element-property :deadline hl)))
                 (scheduled (docket--parse-timestamp
                             (org-element-property :scheduled hl)))
                 (closed (docket--parse-timestamp
                          (org-element-property :closed hl)))
                 (pos (org-element-property :begin hl)))
            ;; Level 1 headings are projects
            (when (= level 1)
              (setq current-project title))
            ;; Collect tasks (headings with a TODO keyword)
            (when todo
              (let* ((base-id (format "%s::%s::%s"
                                      (file-name-nondirectory file)
                                      (or current-project "") title))
                     (count (gethash base-id title-counts 0))
                     (id (if (zerop count) base-id
                           (format "%s::%d" base-id count))))
                (puthash base-id (1+ count) title-counts)
                (push (docket-task-create
                       :id id
                       :title title
                       :state todo
                       :priority (when priority (char-to-string priority))
                       :tags tags
                       :deadline deadline
                       :scheduled scheduled
                       :closed closed
                       :project (if (= level 1) title current-project)
                       :file file
                       :point pos
                       :level level)
                      tasks)))))))
    (nreverse tasks)))

(defun docket--refresh-cache ()
  "Re-parse all `docket-files' and update the cache."
  (docket--ensure-files)
  (let (all-tasks projects-set)
    (dolist (file docket-files)
      (let ((tasks (docket--parse-file file)))
        (setq all-tasks (nconc all-tasks tasks))))
    ;; Extract unique project names
    (dolist (task all-tasks)
      (when-let ((proj (docket-task-project task)))
        (cl-pushnew proj projects-set :test #'equal)))
    (setq docket--cache all-tasks)
    (setq docket--projects (sort projects-set #'string<))
    (cl-incf docket--cache-tick)
    docket--cache))

(defun docket--tasks ()
  "Return the current task list, refreshing cache if needed."
  (unless docket--cache
    (docket--refresh-cache))
  docket--cache)

(defun docket-refresh ()
  "Force refresh the docket cache from org files."
  (interactive)
  (docket--refresh-cache)
  (message "Docket: refreshed %d tasks from %d files"
           (length docket--cache) (length docket-files)))

;;;; Task queries

(defun docket--active-tasks ()
  "Return tasks in active (non-done) states."
  (cl-remove-if-not
   (lambda (task)
     (member (docket-task-state task) docket-todo-states))
   (docket--tasks)))

(defun docket--today-tasks ()
  "Return tasks relevant to today's view.
Includes: overdue, due today (deadline/scheduled), and NEXT tasks."
  (let* ((today (current-time))
         (decoded (decode-time today))
         (day-end (encode-time 59 59 23
                               (decoded-time-day decoded)
                               (decoded-time-month decoded)
                               (decoded-time-year decoded))))
    (cl-remove-if-not
     (lambda (task)
       (and (member (docket-task-state task) docket-todo-states)
            (or
             ;; NEXT state tasks
             (string= (docket-task-state task) "NEXT")
             ;; Deadline today or overdue
             (when-let ((dl (docket-task-deadline task)))
               (time-less-p dl day-end))
             ;; Scheduled today or earlier
             (when-let ((sc (docket-task-scheduled task)))
               (time-less-p sc day-end)))))
     (docket--tasks))))

(defun docket--upcoming-tasks ()
  "Return tasks with a deadline or scheduled date in the future."
  (let ((now (current-time)))
    (cl-remove-if-not
     (lambda (task)
       (and (member (docket-task-state task) docket-todo-states)
            (or (docket-task-deadline task)
                (docket-task-scheduled task))))
     (docket--tasks))))

(defun docket--tasks-for-project (project)
  "Return active tasks belonging to PROJECT."
  (cl-remove-if-not
   (lambda (task)
     (and (equal (docket-task-project task) project)
          (member (docket-task-state task) docket-todo-states)))
   (docket--tasks)))

(defun docket--tasks-with-tag (tag)
  "Return active tasks that have TAG."
  (cl-remove-if-not
   (lambda (task)
     (and (member tag (docket-task-tags task))
          (member (docket-task-state task) docket-todo-states)))
   (docket--tasks)))

(defun docket--all-tags ()
  "Return an alist of (TAG . COUNT) across all active tasks."
  (let ((counts (make-hash-table :test #'equal)))
    (dolist (task (docket--active-tasks))
      (dolist (tag (docket-task-tags task))
        (puthash tag (1+ (gethash tag counts 0)) counts)))
    (let (result)
      (maphash (lambda (k v) (push (cons k v) result)) counts)
      (sort result (lambda (a b) (string< (car a) (car b)))))))

;;;; Task mutations

(defun docket--set-task-state (task new-state)
  "Set TASK to NEW-STATE in its org file."
  (let ((file (docket-task-file task))
        (pos (docket-task-point task)))
    (with-current-buffer (find-file-noselect file)
      (save-excursion
        (goto-char pos)
        (when (looking-at org-heading-regexp)
          (org-todo new-state)
          (save-buffer))))
    (docket--refresh-cache)))

(defun docket--toggle-task-done (task)
  "Toggle TASK between its current state and DONE."
  (if (member (docket-task-state task) docket-done-states)
      (docket--set-task-state task "TODO")
    (docket--set-task-state task "DONE")))

(defun docket--jump-to-task (task)
  "Open the org file and jump to TASK's heading."
  (let ((file (docket-task-file task))
        (pos (docket-task-point task)))
    (find-file file)
    (goto-char pos)
    (org-reveal)
    (org-show-entry)))

;;;; After-save hook for auto-refresh

(defun docket--after-save-hook ()
  "Refresh cache when a docket file is saved."
  (when (and docket-files
             (buffer-file-name)
             (member (expand-file-name (buffer-file-name))
                     (mapcar #'expand-file-name docket-files)))
    (docket--refresh-cache)))

;;;; Minor mode

(define-minor-mode docket-mode
  "Minor mode for docket GTD buffers."
  :lighter " Docket"
  :group 'docket
  (if docket-mode
      (add-hook 'after-save-hook #'docket--after-save-hook nil t)
    (remove-hook 'after-save-hook #'docket--after-save-hook t)))

(defun docket--maybe-enable ()
  "Enable `docket-mode' if the current buffer is a docket file."
  (when (and docket-files
             (buffer-file-name)
             (member (expand-file-name (buffer-file-name))
                     (mapcar #'expand-file-name docket-files)))
    (docket-mode 1)))

(add-hook 'find-file-hook #'docket--maybe-enable)

;;;; Command map (populated by submodules)

(defvar docket-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "d") #'docket-open)
    (define-key map (kbd "q") #'docket-close)
    (define-key map (kbd "t") #'docket-view-today)
    (define-key map (kbd "r") #'docket-refresh)
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
  (require 'docket-today)
  (docket--render-today))

(provide 'docket)

(require 'docket-ui)
(require 'docket-sidebar)
(require 'docket-today)
(require 'docket-capture)
(require 'docket-upcoming)
(require 'docket-filter)
(require 'docket-transient)

;;; docket.el ends here
