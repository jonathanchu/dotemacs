;;; docket-today.el --- Today view for docket -*- lexical-binding: t -*-

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

;; The Today view shows tasks that need attention now: overdue tasks,
;; tasks scheduled or due today, and tasks in the NEXT state.
;; Each task can be toggled done, re-prioritized, or jumped to in-place.

;;; Code:

(require 'cl-lib)
(require 'ewoc)
(require 'docket)

(declare-function docket--display-in-main "docket-ui")
(declare-function docket--render-upcoming "docket-upcoming")
(declare-function docket--render-filter "docket-filter")
(declare-function docket-capture--parse-relative-date "docket-capture")
(declare-function docket-capture--find-project-heading "docket-capture")
(declare-function docket-close "docket-ui")
(defvar docket-filter--title)
(defvar docket-filter--predicate)
(defvar docket-upcoming--ewoc)
(defvar docket-filter--ewoc)
(defvar docket-view--show-done)

;;;; Faces

(defface docket-task-title
  '((t :inherit default))
  "Face for task titles."
  :group 'docket)

(defface docket-task-done
  '((t :inherit font-lock-comment-face :strike-through t))
  "Face for completed tasks."
  :group 'docket)

(defface docket-task-project
  '((t :inherit font-lock-comment-face))
  "Face for the project name on a task line."
  :group 'docket)

(defface docket-task-tag
  '((t :inherit font-lock-type-face))
  "Face for tags on a task line."
  :group 'docket)

(defface docket-priority-a
  '((t :inherit font-lock-warning-face :weight bold))
  "Face for priority A (p1) tasks."
  :group 'docket)

(defface docket-priority-b
  '((((class color) (min-colors 88)) :foreground "#e5a00d" :weight bold)
    (t :inherit font-lock-variable-name-face :weight bold))
  "Face for priority B (p2) tasks."
  :group 'docket)

(defface docket-priority-c
  '((t :inherit font-lock-function-name-face))
  "Face for priority C (p3) tasks."
  :group 'docket)

(defface docket-overdue
  '((t :inherit font-lock-warning-face))
  "Face for overdue date indicators."
  :group 'docket)

(defface docket-date-today
  '((t :inherit success))
  "Face for dates that are today."
  :group 'docket)

(defface docket-date
  '((t :inherit font-lock-comment-face))
  "Face for future date indicators."
  :group 'docket)

(defface docket-section-header
  '((t :inherit font-lock-keyword-face :weight bold :height 1.1))
  "Face for section headers in task views."
  :group 'docket)

;;;; State indicators

(defconst docket--state-indicators
  '(("TODO"      . "○")
    ("NEXT"      . "●")
    ("WAIT"      . "◑")
    ("SOMEDAY"   . "◌")
    ("DONE"      . "◉")
    ("CANCELLED" . "⊘"))
  "Alist mapping TODO states to display indicators.")

(defun docket--state-indicator (state)
  "Return the display indicator for STATE."
  (or (cdr (assoc state docket--state-indicators)) "○"))

;;;; Priority face

(defun docket--priority-face (priority)
  "Return the face for PRIORITY string (\"A\", \"B\", \"C\")."
  (pcase priority
    ("A" 'docket-priority-a)
    ("B" 'docket-priority-b)
    ("C" 'docket-priority-c)
    (_ 'docket-task-title)))

;;;; Date formatting

(defun docket--format-date (time)
  "Format TIME as a human-readable relative date string.
Uses \"Today\", \"Tomorrow\", day names for this week, or \"Mon DD\" otherwise."
  (let* ((now (decode-time))
         (today (encode-time 0 0 0
                             (decoded-time-day now)
                             (decoded-time-month now)
                             (decoded-time-year now)))
         (tomorrow (time-add today (* 24 60 60)))
         (next-week (time-add today (* 7 24 60 60))))
    (cond
     ((time-less-p time today) (format-time-string "%b %d" time))
     ((time-less-p time tomorrow) "Today")
     ((time-less-p time (time-add tomorrow (* 24 60 60))) "Tomorrow")
     ((time-less-p time next-week) (format-time-string "%A" time))
     (t (format-time-string "%b %d" time)))))

(defun docket--date-face (time)
  "Return the face for a date at TIME."
  (let* ((now (decode-time))
         (today (encode-time 0 0 0
                             (decoded-time-day now)
                             (decoded-time-month now)
                             (decoded-time-year now)))
         (tomorrow (time-add today (* 24 60 60))))
    (cond
     ((time-less-p time today) 'docket-overdue)
     ((time-less-p time tomorrow) 'docket-date-today)
     (t 'docket-date))))

;;;; Task rendering

(defun docket--format-task-line (task)
  "Format TASK as a single display line and insert it."
  (let* ((state (docket-task-state task))
         (done-p (member state docket-done-states))
         (indicator (docket--state-indicator state))
         (priority (docket-task-priority task))
         (title (docket-task-title task))
         (project (docket-task-project task))
         (tags (docket-task-tags task))
         (deadline (docket-task-deadline task))
         (scheduled (docket-task-scheduled task))
         (title-face (if done-p 'docket-task-done 'docket-task-title))
         (pri-str (if priority
                      (propertize (format "[%s] " priority)
                                  'face (if done-p 'docket-task-done
                                          (docket--priority-face priority)))
                    "    "))
         (indicator-face (if done-p 'docket-task-done
                           (if priority
                               (docket--priority-face priority)
                             'docket-task-title)))
         (date-str (unless done-p
                     (let ((date (or deadline scheduled)))
                       (when date
                         (propertize (format "  <%s>" (docket--format-date date))
                                     'face (docket--date-face date))))))
         (tag-str (when tags
                    (propertize
                     (concat " " (mapconcat (lambda (tag) (concat "#" tag))
                                            tags " "))
                     'face (if done-p 'docket-task-done 'docket-task-tag))))
         (proj-str (when project
                     (propertize (concat "  " project)
                                 'face (if done-p 'docket-task-done
                                         'docket-task-project)))))
    (insert (propertize indicator 'face indicator-face)
            " "
            pri-str
            (propertize title 'face title-face)
            (or date-str "")
            (or tag-str "")
            (or proj-str ""))))

;;;; Section headers

(cl-defstruct (docket-view-node (:constructor docket-view-node-create))
  "A node in a docket view — either a section header or a task."
  type    ; 'section or 'task
  label   ; section label (for section nodes)
  task)   ; docket-task struct (for task nodes)

(defun docket--view-print (node)
  "Print a view NODE (section header or task)."
  (pcase (docket-view-node-type node)
    ('section
     (insert (propertize (docket-view-node-label node)
                         'face 'docket-section-header)))
    ('task
     (docket--format-task-line (docket-view-node-task node)))))

;;;; Sorting

(defvar-local docket-view--sort-mode 'priority
  "Current sort mode for task views.
One of `priority', `date', or `project'.")

(defconst docket--sort-modes '(priority date project)
  "Available sort modes, cycled by `docket-view-cycle-sort'.")

(defun docket--task-sort-key (task)
  "Return a sort key for TASK (lower = higher priority)."
  (let ((pri (docket-task-priority task))
        (state (docket-task-state task)))
    (+ (pcase pri ("A" 0) ("B" 100) ("C" 200) (_ 300))
       (pcase state ("NEXT" 0) ("TODO" 10) ("WAIT" 20) ("SOMEDAY" 30) (_ 50)))))

(defun docket--task-date-key (task)
  "Return a time value for sorting TASK by date.
Tasks without dates sort to the end."
  (or (docket-task-deadline task)
      (docket-task-scheduled task)
      (encode-time 0 0 0 1 1 2100)))

(defun docket--sort-tasks (tasks)
  "Sort TASKS according to `docket-view--sort-mode'."
  (let ((sorted (copy-sequence tasks)))
    (pcase docket-view--sort-mode
      ('priority
       (sort sorted (lambda (a b)
                      (< (docket--task-sort-key a)
                         (docket--task-sort-key b)))))
      ('date
       (sort sorted (lambda (a b)
                      (time-less-p (docket--task-date-key a)
                                   (docket--task-date-key b)))))
      ('project
       (sort sorted (lambda (a b)
                      (string< (or (docket-task-project a) "")
                               (or (docket-task-project b) "")))))
      (_ sorted))))

;;;; Today view

(defvar-local docket-today--ewoc nil
  "The ewoc instance for the today view.")

(defun docket--today-build-nodes ()
  "Build the list of view nodes for the today view."
  (let* ((now (current-time))
         (decoded (decode-time now))
         (today-start (encode-time 0 0 0
                                   (decoded-time-day decoded)
                                   (decoded-time-month decoded)
                                   (decoded-time-year decoded)))
         (today-end (encode-time 59 59 23
                                 (decoded-time-day decoded)
                                 (decoded-time-month decoded)
                                 (decoded-time-year decoded)))
         overdue-tasks today-tasks next-tasks done-tasks nodes)
    ;; Categorize tasks
    (dolist (task (docket--active-tasks))
      (let ((dl (docket-task-deadline task))
            (sc (docket-task-scheduled task))
            (state (docket-task-state task)))
        (cond
         ;; Overdue: deadline or scheduled before start of today
         ((or (and dl (time-less-p dl today-start))
              (and sc (time-less-p sc today-start)))
          (push task overdue-tasks))
         ;; Due/scheduled today
         ((or (and dl (time-less-p dl today-end))
              (and sc (time-less-p sc today-end)))
          (push task today-tasks))
         ;; NEXT state
         ((string= state "NEXT")
          (push task next-tasks)))))
    ;; Collect completed tasks when toggled on
    (when docket-view--show-done
      (dolist (task (docket--tasks))
        (when (member (docket-task-state task) docket-done-states)
          (push task done-tasks))))
    ;; Build nodes
    (when overdue-tasks
      (push (docket-view-node-create :type 'section
                                     :label (format "Overdue (%d)"
                                                    (length overdue-tasks)))
            nodes)
      (dolist (task (docket--sort-tasks overdue-tasks))
        (push (docket-view-node-create :type 'task :task task) nodes)))
    (when today-tasks
      (push (docket-view-node-create :type 'section
                                     :label (format "Today (%d)"
                                                    (length today-tasks)))
            nodes)
      (dolist (task (docket--sort-tasks today-tasks))
        (push (docket-view-node-create :type 'task :task task) nodes)))
    (when next-tasks
      (push (docket-view-node-create :type 'section
                                     :label (format "Next Actions (%d)"
                                                    (length next-tasks)))
            nodes)
      (dolist (task (docket--sort-tasks next-tasks))
        (push (docket-view-node-create :type 'task :task task) nodes)))
    (when done-tasks
      (push (docket-view-node-create :type 'section
                                     :label (format "Completed (%d)"
                                                    (length done-tasks)))
            nodes)
      (dolist (task done-tasks)
        (push (docket-view-node-create :type 'task :task task) nodes)))
    (nreverse nodes)))

(defun docket--render-today ()
  "Render the today view in the main window."
  (let ((buf (get-buffer-create "*docket-today*")))
    (with-current-buffer buf
      (unless (derived-mode-p 'docket-view-mode)
        (docket-view-mode))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (setq docket-today--ewoc
              (ewoc-create #'docket--view-print "" ""))
        (let* ((nodes (docket--today-build-nodes))
               (task-count (cl-count 'task nodes
                                     :key #'docket-view-node-type)))
          (if nodes
              (dolist (node nodes)
                (ewoc-enter-last docket-today--ewoc node))
            (insert (propertize "No tasks for today — you're all caught up!"
                                'face 'font-lock-comment-face)))
          (when docket-today--ewoc
            (ewoc-refresh docket-today--ewoc))
          (goto-char (point-min))
          (setq-local header-line-format
                      (propertize (format " Today (%d)" task-count)
                                  'face 'bold)))))
    (require 'docket-ui)
    (docket--display-in-main buf)))

;;;; View interaction commands

(defun docket-view--task-at-point ()
  "Return the `docket-task' at point, or nil."
  (when-let ((ewoc (or (bound-and-true-p docket-today--ewoc)
                        (bound-and-true-p docket-upcoming--ewoc)
                        (bound-and-true-p docket-filter--ewoc))))
    (when-let ((ewoc-node (ewoc-locate ewoc)))
      (let ((node (ewoc-data ewoc-node)))
        (when (eq (docket-view-node-type node) 'task)
          (docket-view-node-task node))))))

(defun docket-view--current-ewoc ()
  "Return the ewoc for the current view buffer."
  (or (bound-and-true-p docket-today--ewoc)
      (bound-and-true-p docket-upcoming--ewoc)
      (bound-and-true-p docket-filter--ewoc)))

(defun docket-view-toggle-done ()
  "Toggle the task at point between TODO and DONE."
  (interactive)
  (when-let ((task (docket-view--task-at-point)))
    (docket--toggle-task-done task)
    (when-let ((ewoc (docket-view--current-ewoc)))
      (let ((inhibit-read-only t)
            (ewoc-node (ewoc-locate ewoc)))
        ;; Update the node with refreshed task data
        (setf (docket-view-node-task (ewoc-data ewoc-node))
              (docket--find-task-by-id (docket-task-id task)))
        (ewoc-invalidate ewoc ewoc-node)))
    ;; Update sidebar counts
    (when (get-buffer "*docket-sidebar*")
      (docket-sidebar-refresh))))

(defun docket--find-task-by-id (id)
  "Find a task in the cache by ID."
  (cl-find id (docket--tasks) :key #'docket-task-id :test #'equal))

(defun docket-view-jump-to-task ()
  "Jump to the org heading for the task at point."
  (interactive)
  (when-let ((task (docket-view--task-at-point)))
    (docket--jump-to-task task)))

(defun docket-view-cycle-priority ()
  "Cycle the priority of the task at point: A → B → C → none → A."
  (interactive)
  (when-let ((task (docket-view--task-at-point)))
    (let* ((current (docket-task-priority task))
           (new-pri (pcase current
                      ("A" "B") ("B" "C") ("C" nil) (_ "A")))
           (file (docket-task-file task))
           (pos (docket-task-point task)))
      (with-current-buffer (find-file-noselect file)
        (save-excursion
          (goto-char pos)
          (org-priority (if new-pri (string-to-char new-pri) ?\s)))
        (save-buffer))
      (docket--refresh-cache)
      (when-let ((ewoc (docket-view--current-ewoc)))
        (let ((inhibit-read-only t)
              (ewoc-node (ewoc-locate ewoc)))
          (setf (docket-view-node-task (ewoc-data ewoc-node))
                (docket--find-task-by-id (docket-task-id task)))
          (ewoc-invalidate ewoc ewoc-node))))))

(defun docket-view-set-state ()
  "Change the TODO state of the task at point."
  (interactive)
  (when-let ((task (docket-view--task-at-point)))
    (let* ((all-states (append docket-todo-states docket-done-states))
           (new-state (completing-read "State: " all-states nil t)))
      (docket--set-task-state task new-state)
      (when-let ((ewoc (docket-view--current-ewoc)))
        (let ((inhibit-read-only t)
              (ewoc-node (ewoc-locate ewoc)))
          (setf (docket-view-node-task (ewoc-data ewoc-node))
                (docket--find-task-by-id (docket-task-id task)))
          (ewoc-invalidate ewoc ewoc-node))))))

(defun docket-view--read-date (prompt)
  "Read a date string using PROMPT.
Accepts the same inputs as `docket-capture--parse-relative-date'
\(today, tomorrow, day names, +Nd) as well as YYYY-MM-DD dates.
Returns an Emacs time value."
  (require 'docket-capture)
  (let* ((input (read-string prompt))
         (time (or (docket-capture--parse-relative-date input)
                   (and (string-match-p "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}$" input)
                        (encode-time (org-parse-time-string (concat input " 00:00"))))
                   (user-error "Unrecognized date: %s" input))))
    time))

(defun docket-view--set-date (task keyword time)
  "Set KEYWORD (\"SCHEDULED\" or \"DEADLINE\") on TASK to TIME.
TIME is an Emacs time value."
  (let ((file (docket-task-file task))
        (pos (docket-task-point task))
        (ts (format-time-string "<%F %a>" time)))
    (with-current-buffer (find-file-noselect file)
      (save-excursion
        (goto-char pos)
        (let ((end (save-excursion (org-end-of-subtree t) (point))))
          ;; Remove existing keyword line if present
          (save-excursion
            (when (re-search-forward
                   (format "^[ \t]*%s: " keyword) end t)
              (beginning-of-line)
              (delete-region (point) (progn (forward-line 1) (point)))))
          ;; Insert new keyword line after heading
          (forward-line 1)
          (insert (format "   %s: %s\n" keyword ts))))
      (save-buffer))
    (docket--refresh-cache)
    (when-let ((ewoc (docket-view--current-ewoc)))
      (let ((inhibit-read-only t)
            (ewoc-node (ewoc-locate ewoc)))
        (setf (docket-view-node-task (ewoc-data ewoc-node))
              (docket--find-task-by-id (docket-task-id task)))
        (ewoc-invalidate ewoc ewoc-node)))))

(defun docket-view-set-scheduled ()
  "Set the scheduled date of the task at point."
  (interactive)
  (when-let ((task (docket-view--task-at-point)))
    (docket-view--set-date task "SCHEDULED"
                           (docket-view--read-date "Scheduled: "))))

(defun docket-view-set-deadline ()
  "Set the deadline of the task at point."
  (interactive)
  (when-let ((task (docket-view--task-at-point)))
    (docket-view--set-date task "DEADLINE"
                           (docket-view--read-date "Deadline: "))))

(defun docket-view-set-tags ()
  "Edit the tags of the task at point."
  (interactive)
  (when-let ((task (docket-view--task-at-point)))
    (let ((file (docket-task-file task))
          (pos (docket-task-point task)))
      (with-current-buffer (find-file-noselect file)
        (save-excursion
          (goto-char pos)
          (org-set-tags-command))
        (save-buffer))
      (docket--refresh-cache)
      (when-let ((ewoc (docket-view--current-ewoc)))
        (let ((inhibit-read-only t)
              (ewoc-node (ewoc-locate ewoc)))
          (setf (docket-view-node-task (ewoc-data ewoc-node))
                (docket--find-task-by-id (docket-task-id task)))
          (ewoc-invalidate ewoc ewoc-node))))))

(defun docket-view-next ()
  "Move to the next task, skipping section headers."
  (interactive)
  (when-let ((ewoc (docket-view--current-ewoc)))
    (let ((node (ewoc-locate ewoc)))
      (while (and (setq node (ewoc-next ewoc node))
                  (eq (docket-view-node-type (ewoc-data node)) 'section)))
      (when node
        (ewoc-goto-node ewoc node)))))

(defun docket-view-previous ()
  "Move to the previous task, skipping section headers."
  (interactive)
  (when-let ((ewoc (docket-view--current-ewoc)))
    (let ((node (ewoc-locate ewoc)))
      (while (and (setq node (ewoc-prev ewoc node))
                  (eq (docket-view-node-type (ewoc-data node)) 'section)))
      (when node
        (ewoc-goto-node ewoc node)))))

(defun docket-view-refile ()
  "Move the task at point to a different project."
  (interactive)
  (require 'docket-capture)
  (when-let ((task (docket-view--task-at-point)))
    (let* ((projects (cons docket-inbox-heading
                           (cl-remove docket-inbox-heading
                                      docket--projects :test #'equal)))
           (target (completing-read "Move to project: " projects nil t))
           (file (docket-task-file task))
           (pos (docket-task-point task))
           (target-file nil)
           (target-pos nil))
      ;; Find target project heading
      (dolist (f docket-files)
        (when-let ((p (docket-capture--find-project-heading target f)))
          (setq target-file f target-pos p)))
      (unless target-pos
        (user-error "Project \"%s\" not found" target))
      ;; Cut the subtree from source
      (with-current-buffer (find-file-noselect file)
        (save-excursion
          (goto-char pos)
          (org-cut-subtree)))
      ;; Paste under target project
      (with-current-buffer (find-file-noselect target-file)
        (save-excursion
          (goto-char target-pos)
          (org-end-of-subtree t)
          (unless (bolp) (insert "\n"))
          (org-paste-subtree 2))
        (save-buffer))
      ;; Save source if different file
      (unless (equal file target-file)
        (with-current-buffer (find-file-noselect file)
          (save-buffer)))
      (docket--refresh-cache)
      (docket-view-refresh)
      (message "Moved \"%s\" → %s" (docket-task-title task) target))))

(defun docket-view-cycle-sort ()
  "Cycle the sort mode: priority → date → project → priority."
  (interactive)
  (let* ((current docket-view--sort-mode)
         (idx (cl-position current docket--sort-modes))
         (next (nth (mod (1+ (or idx 0)) (length docket--sort-modes))
                    docket--sort-modes)))
    (setq docket-view--sort-mode next)
    (docket-view-refresh)
    (message "Sort: %s" next)))

(declare-function docket-sidebar-refresh "docket-sidebar")
(declare-function docket-view-help "docket-transient")

(defun docket-view-refresh ()
  "Refresh the current view and update sidebar counts."
  (interactive)
  (docket--refresh-cache)
  (cond
   ((bound-and-true-p docket-today--ewoc)
    (docket--render-today))
   ((bound-and-true-p docket-upcoming--ewoc)
    (require 'docket-upcoming)
    (docket--render-upcoming))
   ((bound-and-true-p docket-filter--ewoc)
    (require 'docket-filter)
    (when (and docket-filter--title docket-filter--predicate)
      (docket--render-filter :title docket-filter--title
                             :predicate docket-filter--predicate))))
  ;; Update sidebar counts
  (when (get-buffer "*docket-sidebar*")
    (docket-sidebar-refresh)))

;;;; Show/hide completed tasks

(defvar-local docket-view--show-done nil
  "When non-nil, include completed tasks in the current view.")

(defun docket-view-toggle-show-done ()
  "Toggle showing completed tasks in the current view."
  (interactive)
  (setq docket-view--show-done (not docket-view--show-done))
  (docket-view-refresh)
  (message "Completed tasks: %s" (if docket-view--show-done "shown" "hidden")))

;;;; Mode

(defvar docket-view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "x") #'docket-view-toggle-done)
    (define-key map (kbd "RET") #'docket-view-jump-to-task)
    (define-key map (kbd "e") #'docket-view-cycle-priority)
    (define-key map (kbd "t") #'docket-view-set-state)
    (define-key map (kbd "s") #'docket-view-set-scheduled)
    (define-key map (kbd "d") #'docket-view-set-deadline)
    (define-key map (kbd "#") #'docket-view-set-tags)
    (define-key map (kbd "n") #'docket-view-next)
    (define-key map (kbd "p") #'docket-view-previous)
    (define-key map (kbd "C-n") #'docket-view-next)
    (define-key map (kbd "C-p") #'docket-view-previous)
    (define-key map (kbd "g") #'docket-view-refresh)
    (define-key map (kbd "/") #'docket-view-refile)
    (define-key map (kbd "H") #'docket-view-toggle-show-done)
    (define-key map (kbd "S") #'docket-view-cycle-sort)
    (define-key map (kbd "?") #'docket-view-help)
    (define-key map (kbd "q") #'docket-close)
    map)
  "Keymap for docket view buffers (today, upcoming, filter).")

(define-derived-mode docket-view-mode special-mode "Docket-View"
  "Major mode for docket task views."
  :group 'docket
  (setq-local cursor-type nil
              truncate-lines t)
  (hl-line-mode 1))

(provide 'docket-today)
;;; docket-today.el ends here
