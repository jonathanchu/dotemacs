;;; docket-upcoming.el --- Upcoming view for docket -*- lexical-binding: t -*-

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

;; The Upcoming view shows tasks grouped by date for the next 7 days,
;; plus a "Later" section for anything beyond that window.

;;; Code:

(require 'cl-lib)
(require 'ewoc)
(require 'docket)
(require 'docket-today)

;;;; Upcoming view

(defvar-local docket-upcoming--ewoc nil
  "The ewoc instance for the upcoming view.")

(defun docket--upcoming-task-date (task)
  "Return the effective date for TASK (deadline or scheduled)."
  (or (docket-task-deadline task) (docket-task-scheduled task)))

(defun docket--upcoming-build-nodes ()
  "Build the list of view nodes for the upcoming view."
  (let* ((now (current-time))
         (tasks (cl-remove-if-not
                 (lambda (task)
                   (and (member (docket-task-state task) docket-todo-states)
                        (docket--upcoming-task-date task)))
                 (docket--tasks)))
         ;; Sort by date
         (sorted (sort (copy-sequence tasks)
                       (lambda (a b)
                         (time-less-p (docket--upcoming-task-date a)
                                      (docket--upcoming-task-date b)))))
         ;; Group by date
         (day-groups (make-hash-table :test #'equal))
         (later-tasks '())
         (seven-days (time-add now (* 7 24 60 60)))
         nodes day-keys)
    ;; Bucket tasks into date groups
    (dolist (task sorted)
      (let ((date (docket--upcoming-task-date task)))
        (if (time-less-p date seven-days)
            (let ((day-key (format-time-string "%Y-%m-%d" date)))
              (puthash day-key
                       (append (gethash day-key day-groups) (list task))
                       day-groups)
              (cl-pushnew day-key day-keys :test #'equal))
          (push task later-tasks))))
    ;; Sort day keys chronologically
    (setq day-keys (sort day-keys #'string<))
    ;; Build nodes for each day
    (dolist (day-key day-keys)
      (let* ((day-tasks (gethash day-key day-groups))
             (date (docket--upcoming-task-date (car day-tasks)))
             (label (docket--upcoming-day-label date day-key)))
        (push (docket-view-node-create :type 'section
                                       :label (format "%s (%d)" label
                                                      (length day-tasks)))
              nodes)
        (dolist (task (docket--sort-tasks day-tasks))
          (push (docket-view-node-create :type 'task :task task) nodes))))
    ;; Later section
    (when later-tasks
      (setq later-tasks (nreverse later-tasks))
      (push (docket-view-node-create :type 'section
                                     :label (format "Later (%d)"
                                                    (length later-tasks)))
            nodes)
      (dolist (task (docket--sort-tasks later-tasks))
        (push (docket-view-node-create :type 'task :task task) nodes)))
    (nreverse nodes)))

(defun docket--upcoming-day-label (date day-key)
  "Return a human-readable label for DATE with DAY-KEY."
  (let ((today (format-time-string "%Y-%m-%d"))
        (tomorrow (format-time-string "%Y-%m-%d"
                                      (time-add (current-time) (* 24 60 60)))))
    (cond
     ((string= day-key today) "Today")
     ((string= day-key tomorrow) "Tomorrow")
     (t (format-time-string "%A, %b %d" date)))))

(defun docket--render-upcoming ()
  "Render the upcoming view in the main window."
  (let ((buf (get-buffer-create "*docket-upcoming*")))
    (with-current-buffer buf
      (unless (derived-mode-p 'docket-view-mode)
        (docket-view-mode))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (setq docket-upcoming--ewoc
              (ewoc-create #'docket--view-print "" ""))
        (let ((nodes (docket--upcoming-build-nodes)))
          (if nodes
              (dolist (node nodes)
                (ewoc-enter-last docket-upcoming--ewoc node))
            (insert (propertize "No upcoming tasks scheduled."
                                'face 'font-lock-comment-face))))
        (when docket-upcoming--ewoc
          (ewoc-refresh docket-upcoming--ewoc))
        (goto-char (point-min))
        (setq-local header-line-format
                    (propertize " Upcoming" 'face 'bold))))
    (require 'docket-ui)
    (docket--display-in-main buf)))

;; Register in command map
(define-key docket-command-map (kbd "u") #'docket-view-upcoming)

;;;###autoload
(defun docket-view-upcoming ()
  "Show the upcoming tasks view."
  (interactive)
  (docket--ensure-files)
  (unless docket--cache (docket--refresh-cache))
  (docket--render-upcoming))

(provide 'docket-upcoming)
;;; docket-upcoming.el ends here
