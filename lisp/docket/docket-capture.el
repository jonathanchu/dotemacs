;;; docket-capture.el --- Quick add with natural language parsing -*- lexical-binding: t -*-

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

;; Quick task capture with natural language parsing.
;; Parses input like "Buy milk #errands p1 tomorrow /Shopping"
;; into a structured org heading with priority, tags, scheduled date,
;; and project placement.

;;; Code:

(require 'cl-lib)
(require 'docket)

;;;; Date parsing

(defconst docket-capture--day-names
  '("monday" "tuesday" "wednesday" "thursday" "friday" "saturday" "sunday"
    "mon" "tue" "wed" "thu" "fri" "sat" "sun")
  "Day names recognized for date parsing.")

(defun docket-capture--parse-relative-date (word)
  "Parse WORD as a relative date and return a time value, or nil.
Recognizes: today, tomorrow, day names, +Nd (e.g. +3d)."
  (let ((word-down (downcase word)))
    (cond
     ((string= word-down "today")
      (current-time))
     ((string= word-down "tomorrow")
      (time-add (current-time) (* 24 60 60)))
     ((string-match "^\\+\\([0-9]+\\)d$" word-down)
      (time-add (current-time)
                (* (string-to-number (match-string 1 word-down)) 24 60 60)))
     ;; Day names
     ((member word-down docket-capture--day-names)
      (docket-capture--next-weekday word-down)))))

(defun docket-capture--next-weekday (day-str)
  "Return the time value of the next occurrence of DAY-STR."
  (let* ((day-alist '(("monday" . 1) ("mon" . 1)
                      ("tuesday" . 2) ("tue" . 2)
                      ("wednesday" . 3) ("wed" . 3)
                      ("thursday" . 4) ("thu" . 4)
                      ("friday" . 5) ("fri" . 5)
                      ("saturday" . 6) ("sat" . 6)
                      ("sunday" . 0) ("sun" . 0)))
         (target (cdr (assoc (downcase day-str) day-alist)))
         (today (decoded-time-weekday (decode-time)))
         (delta (mod (- target today) 7)))
    (when (zerop delta) (setq delta 7))
    (time-add (current-time) (* delta 24 60 60))))

;;;; Input parsing

(cl-defstruct (docket-capture-result (:constructor docket-capture-result-create))
  "Parsed result from quick-add input."
  title        ; task title (cleaned)
  priority     ; "A", "B", "C", or nil
  tags         ; list of tag strings
  date         ; time value or nil
  date-type    ; 'scheduled or 'deadline
  project)     ; project name or nil

(defun docket-capture--parse (input)
  "Parse INPUT string into a `docket-capture-result'.
Syntax:
  #tag        → org tag
  p1/p2/p3    → priority A/B/C
  !deadline   → next date word sets deadline instead of scheduled
  /project    → refile under project heading
  today/tomorrow/monday/+3d → scheduled date"
  (let ((words (split-string input))
        title-words tags priority date date-type project
        use-deadline)
    (dolist (word words)
      (cond
       ;; Tags: #errands
       ((string-prefix-p "#" word)
        (push (substring word 1) tags))
       ;; Priority: p1, p2, p3
       ((and (string-match-p "^p[1-3]$" (downcase word))
             (null priority))
        (setq priority (pcase (downcase word)
                         ("p1" "A") ("p2" "B") ("p3" "C"))))
       ;; Deadline modifier
       ((string= (downcase word) "!deadline")
        (setq use-deadline t))
       ;; Project: /ProjectName
       ((string-prefix-p "/" word)
        (setq project (substring word 1)))
       ;; Date words
       ((docket-capture--parse-relative-date word)
        (setq date (docket-capture--parse-relative-date word))
        (setq date-type (if use-deadline 'deadline 'scheduled)))
       ;; Normal title word
       (t
        (push word title-words))))
    (docket-capture-result-create
     :title (string-join (nreverse title-words) " ")
     :priority priority
     :tags (nreverse tags)
     :date date
     :date-type (or date-type 'scheduled)
     :project project)))

;;;; Insertion

(defun docket-capture--find-project-heading (project file)
  "Find the position of PROJECT heading in FILE, or nil."
  (with-current-buffer (find-file-noselect file)
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward
             (format "^\\* .*%s" (regexp-quote project)) nil t)
        (point)))))

(defun docket-capture--find-inbox (file)
  "Find the position of the inbox heading in FILE, creating it if needed."
  (with-current-buffer (find-file-noselect file)
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward
           (format "^\\* %s" (regexp-quote docket-inbox-heading)) nil t)
          (point)
        ;; Create inbox heading at end of file
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (insert (format "* %s\n" docket-inbox-heading))
        (point)))))

(defun docket-capture--insert-task (result)
  "Insert a task from RESULT into the appropriate org file."
  (let* ((title (docket-capture-result-title result))
         (priority (docket-capture-result-priority result))
         (tags (docket-capture-result-tags result))
         (date (docket-capture-result-date result))
         (date-type (docket-capture-result-date-type result))
         (project (docket-capture-result-project result))
         (file (car docket-files))
         (target-pos nil))
    ;; Find where to insert
    (when project
      (dolist (f docket-files)
        (when-let ((pos (docket-capture--find-project-heading project f)))
          (setq file f target-pos pos)))
      (unless target-pos
        (if (y-or-n-p (format "Project \"%s\" doesn't exist.  Create it? " project))
            (with-current-buffer (find-file-noselect file)
              (save-excursion
                (goto-char (point-max))
                (unless (bolp) (insert "\n"))
                (insert (format "* %s\n" project))
                (setq target-pos (point))))
          (setq project nil))))
    (unless target-pos
      (setq target-pos (docket-capture--find-inbox file)))
    ;; Build the heading
    (let ((heading (concat "** TODO "
                           (if priority (format "[#%s] " priority) "")
                           title
                           (when tags
                             (concat "  :" (mapconcat #'identity tags ":") ":"))))
          (timestamp (when date
                       (let ((ts-str (format-time-string "<%Y-%m-%d %a>" date)))
                         (if (eq date-type 'deadline)
                             (format "DEADLINE: %s" ts-str)
                           (format "SCHEDULED: %s" ts-str))))))
      ;; Insert into file
      (with-current-buffer (find-file-noselect file)
        (save-excursion
          (goto-char target-pos)
          (org-end-of-subtree t)
          (unless (bolp) (insert "\n"))
          (insert heading "\n")
          (when timestamp
            (insert "   " timestamp "\n")))
        (save-buffer))))
  (docket--refresh-cache))

;;;; Interactive command

;;;###autoload
(defun docket-capture ()
  "Quick-add a task with natural language parsing.
Syntax: Buy milk #errands p1 tomorrow /Shopping

  #tag        → org tag
  p1/p2/p3    → priority A/B/C
  !deadline   → next date sets deadline (default: scheduled)
  /project    → file under project heading
  today/tomorrow/monday/+3d → date"
  (interactive)
  (docket--ensure-files)
  (let* ((input (read-string "Task: "))
         (result (docket-capture--parse input)))
    (when (string-empty-p (docket-capture-result-title result))
      (user-error "Task title cannot be empty"))
    (docket-capture--insert-task result)
    (message "Added: %s%s%s"
             (docket-capture-result-title result)
             (if (docket-capture-result-project result)
                 (format " → %s" (docket-capture-result-project result))
               (format " → %s" docket-inbox-heading))
             (if (docket-capture-result-date result)
                 (format " [%s]"
                         (format-time-string
                          "%b %d"
                          (docket-capture-result-date result)))
               ""))))

;;;###autoload
(defun docket-create-project (name)
  "Create a new empty project heading named NAME."
  (interactive "sProject name: ")
  (docket--ensure-files)
  (let ((file (car docket-files)))
    ;; Check if project already exists
    (dolist (f docket-files)
      (when (docket-capture--find-project-heading name f)
        (user-error "Project \"%s\" already exists" name)))
    (with-current-buffer (find-file-noselect file)
      (save-excursion
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (insert (format "* %s\n" name)))
      (save-buffer))
    (docket--refresh-cache)
    (message "Created project: %s" name)))

;; Register in command map
(define-key docket-command-map (kbd "a") #'docket-capture)
(define-key docket-command-map (kbd "p") #'docket-create-project)

(provide 'docket-capture)
;;; docket-capture.el ends here
