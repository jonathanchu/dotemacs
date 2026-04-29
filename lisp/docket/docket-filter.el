;;; docket-filter.el --- Filters and labels for docket -*- lexical-binding: t -*-

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

;; Provides filtered task views and a label browser.
;; Filters can be predefined (priority, state) or custom (saved to
;; custom-file via customize).

;;; Code:

(require 'cl-lib)
(require 'ewoc)
(require 'docket)
(require 'docket-today)

(declare-function docket--display-in-main "docket-ui")

;;;; Customization

(defcustom docket-saved-filters nil
  "List of saved custom filters.
Each entry is a plist (:name NAME :type TYPE :value VALUE) where:
  TYPE is one of `tag', `state', `priority', `project'
  VALUE is the filter value (tag name, state string, priority letter, etc.)"
  :type '(repeat (plist :key-type symbol :value-type sexp))
  :group 'docket)

;;;; Filter view

(defvar-local docket-filter--ewoc nil
  "The ewoc instance for the filter view.")

(defvar-local docket-filter--title nil
  "Title of the current filter view.")

(defvar-local docket-filter--predicate nil
  "Predicate function for the current filter.")

(cl-defun docket--render-filter (&key title predicate)
  "Render a filtered task view in the main window.
TITLE is the header, PREDICATE filters tasks."
  (let ((buf (get-buffer-create "*docket-filter*")))
    (with-current-buffer buf
      (unless (derived-mode-p 'docket-view-mode)
        (docket-view-mode))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (setq docket-filter--ewoc
              (ewoc-create #'docket--view-print "" ""))
        (setq docket-filter--title title)
        (setq docket-filter--predicate predicate)
        (let* ((tasks (cl-remove-if-not predicate (docket--tasks)))
               (active (cl-remove-if-not
                        (lambda (task)
                          (member (docket-task-state task) docket-todo-states))
                        tasks))
               (done (when docket-view--show-done
                       (cl-remove-if-not
                        (lambda (task)
                          (member (docket-task-state task) docket-done-states))
                        tasks)))
               (sorted (docket--sort-tasks active))
               (task-count (+ (length sorted) (length done))))
          (if (or sorted done)
              (progn
                (ewoc-enter-last
                 docket-filter--ewoc
                 (docket-view-node-create
                  :type 'section
                  :label (format "%s (%d)" title task-count)))
                (dolist (task sorted)
                  (ewoc-enter-last
                   docket-filter--ewoc
                   (docket-view-node-create :type 'task :task task)))
                (when done
                  (ewoc-enter-last
                   docket-filter--ewoc
                   (docket-view-node-create
                    :type 'section
                    :label (format "Completed (%d)" (length done))))
                  (dolist (task done)
                    (ewoc-enter-last
                     docket-filter--ewoc
                     (docket-view-node-create :type 'task :task task)))))
            (insert (propertize (format "No tasks matching \"%s\"." title)
                                'face 'font-lock-comment-face)))
          (when docket-filter--ewoc
            (ewoc-refresh docket-filter--ewoc))
          (goto-char (point-min))
          (setq-local header-line-format
                      (propertize (format " %s (%d)" title task-count)
                                  'face 'bold)))))
    (require 'docket-ui)
    (docket--display-in-main buf)))

;;;; Predefined filters

(defun docket--filter-by-priority (priority)
  "Return a predicate that matches tasks with PRIORITY."
  (lambda (task)
    (equal (docket-task-priority task) priority)))

(defun docket--filter-by-state (state)
  "Return a predicate that matches tasks with STATE."
  (lambda (task)
    (equal (docket-task-state task) state)))

(defun docket--filter-by-tag (tag)
  "Return a predicate that matches tasks with TAG."
  (lambda (task)
    (member tag (docket-task-tags task))))

(defun docket--filter-by-project (project)
  "Return a predicate that matches tasks in PROJECT."
  (lambda (task)
    (equal (docket-task-project task) project)))

;;;; Interactive filter command

;;;###autoload
(defun docket-filter ()
  "Apply a filter to view tasks.
Prompts for filter type and value."
  (interactive)
  (docket--ensure-files)
  (unless docket--cache (docket--refresh-cache))
  (let* ((filter-type (completing-read
                        "Filter by: "
                        '("Priority" "State" "Tag" "Project" "Saved")
                        nil t))
         (predicate nil)
         (title nil))
    (pcase filter-type
      ("Priority"
       (let ((pri (completing-read "Priority: " '("A" "B" "C") nil t)))
         (setq title (format "Priority %s" pri)
               predicate (docket--filter-by-priority pri))))
      ("State"
       (let ((state (completing-read "State: "
                                     (append docket-todo-states docket-done-states)
                                     nil t)))
         (setq title (format "State: %s" state)
               predicate (docket--filter-by-state state))))
      ("Tag"
       (let* ((tags (docket--all-tags))
              (tag (completing-read "Tag: " (mapcar #'car tags) nil t)))
         (setq title (format "#%s" tag)
               predicate (docket--filter-by-tag tag))))
      ("Project"
       (let ((project (completing-read "Project: " docket--projects nil t)))
         (setq title project
               predicate (docket--filter-by-project project))))
      ("Saved"
       (if docket-saved-filters
           (let* ((names (mapcar (lambda (f) (plist-get f :name))
                                 docket-saved-filters))
                  (name (completing-read "Filter: " names nil t))
                  (filter (cl-find name docket-saved-filters
                                   :key (lambda (f) (plist-get f :name))
                                   :test #'equal)))
             (setq title name
                   predicate (docket--build-saved-filter filter)))
         (user-error "No saved filters.  Use `docket-filter-save' to create one"))))
    (when (and title predicate)
      (docket--render-filter :title title :predicate predicate))))

(defun docket--build-saved-filter (filter)
  "Build a predicate from a saved FILTER plist."
  (let ((type (plist-get filter :type))
        (value (plist-get filter :value)))
    (pcase type
      ('tag (docket--filter-by-tag value))
      ('state (docket--filter-by-state value))
      ('priority (docket--filter-by-priority value))
      ('project (docket--filter-by-project value)))))

;;;; Save filter

;;;###autoload
(defun docket-filter-save ()
  "Save the current filter for reuse."
  (interactive)
  (let* ((type (completing-read "Filter type: "
                                '("tag" "state" "priority" "project") nil t))
         (value (pcase type
                  ("tag"
                   (completing-read "Tag: "
                                    (mapcar #'car (docket--all-tags)) nil t))
                  ("state"
                   (completing-read "State: "
                                    (append docket-todo-states docket-done-states)
                                    nil t))
                  ("priority"
                   (completing-read "Priority: " '("A" "B" "C") nil t))
                  ("project"
                   (completing-read "Project: " docket--projects nil t))))
         (name (read-string "Filter name: ")))
    (customize-save-variable
     'docket-saved-filters
     (cons (list :name name :type (intern type) :value value)
           docket-saved-filters))
    (message "Saved filter: %s" name)))

;;;; Label browser

;;;###autoload
(defun docket-view-labels ()
  "Browse all labels (tags) and their task counts."
  (interactive)
  (docket--ensure-files)
  (unless docket--cache (docket--refresh-cache))
  (let* ((tags (docket--all-tags))
         (tag (completing-read
               "Label: "
               (mapcar (lambda (tc)
                         (format "%s (%d)" (car tc) (cdr tc)))
                       tags)
               nil t)))
    ;; Extract tag name from "tagname (count)" format
    (when (string-match "^\\([^ ]+\\)" tag)
      (let ((tag-name (match-string 1 tag)))
        (docket--render-filter
         :title (format "#%s" tag-name)
         :predicate (docket--filter-by-tag tag-name))))))

(provide 'docket-filter)
;;; docket-filter.el ends here
