;; init-org.el --- My personal org-mode setup.
;;
;; Copyright (c) 2019, 2020
;;
;; Author: Jonathan Chu <me@jonathanchu.is>
;; URL: https://github.com/jonathanchu/dotemacs
;; Version: 1.0

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This is the whole #!

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(eval-and-compile
  (require 'cl-lib)
  (require 'use-package)
  (setq use-package-verbose nil)
  (setq use-package-expand-minimally t))

(eval-when-compile
  (require 'cl)
  (setplist 'string-to-multibyte
            (use-package-plist-delete
             (symbol-plist 'string-to-multibyte) 'byte-obsolete-info)))

(require 'org)
(require 'org-agenda)

(message "hello!")

;; Setup my org agenda files
(require 'find-lisp)
(defvar jc/org-agenda-directory "~/Dropbox/org/gtd/")
(setq org-agenda-files
      (find-lisp-find-files jc/org-agenda-directory "\.org$"))

;; (setq org-agenda-files '("~/Dropbox/org/inbox.org"
;;                          "~/Dropbox/org/todo.org"
;;                          "~/Dropbox/org/gtd.org"
;;                          "~/Dropbox/org/simplehealth.org"))

(setq org-refile-targets '(("~/Dropbox/org/gtd/next.org" :level . 0)
                           ("~/Dropbox/org/gtd/someday.org" :level . 0)
                           ("~/Dropbox/org/gtd/emails.org" :level . 0)
                           ("~/Dropbox/org/gtd/projects.org" :maxlevel . 1)
                           ))

;; (setq org-outline-path-complete-in-steps nil)         ; Refile in a single go
;; (setq org-refile-use-outline-path t)                  ; Show full paths for refiling

(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)
(setq org-log-state-notes-into-drawer t)  ;; Changes to task states might get logged, so we log them in a drawer and not the content of the note.
(setq org-deadline-warning-days 7)
(setq org-agenda-span (quote day))

(setq org-tag-alist '((:startgroup . nil)
                      ("@home" . ?h) ("@work" . ?w)
                      ("@phone" . ?p) ("bills" . ?b)
                      (:endgroup . nil)
                      ("email" . ?e) ("errand" . ?x)))

;; TODO Put this in org-settings.el
(setq org-todo-keywords
      '((sequence "TODO(t)" "STARTED(s)" "WAITING(w)" "PROJECT"
                  "DELEGATED" "DEFERRED" "SOMEDAY" "|" "DONE(d)" "CANCELED(c)")
        (sequence "⚑(T)" "🏴(S)" "❓(W)" "|" "✔(D)" "✘(C)")))

(setq org-todo-keyword-faces '(("❓" . warning)
                               ("TODO" :foreground "medium blue" :weight bold)
                               ("STARTED" :foreground "dark orange" :weight bold)
                               ("WAITING" :foreground "red" :weight bold)
                               ("PROJECT" :foreground "royal blue" :weight bold)
                               ("DELEGATED" :foreground "dark violet" :weight bold)
                               ("DEFERRED" :foreground "dark blue" :weight bold)
                               ("SOMEDAY" :foreground "dark blue" :weight bold)
                               ("DONE" :foreground "dark violet" :weight bold)
                               ("CANCELED" :foreground "black" :weight bold)
                               ))
;; (setq org-priority-faces '((?A . error)
;;                            (?B . warning)
;;                            (?C . success)))

(setq org-tags-column -80)

(defvar org-capture-templates '(("t" "Todo [inbox]" entry
                                 (file+headline "~/Dropbox/org/gtd/inbox.org" "Tasks")
                                 "* TODO %i%?
:PROPERTIES:
:ID:       %(shell-command-to-string \"uuidgen\"):CREATED:  %U
:END:"
                                 )
                                ))

(require 'org-habit)

(setq jc/org-agenda-todo-view
      `(" " "Agenda"
        ((agenda ""
                 ((org-agenda-span 'day)
                  (org-deadline-warning-days 365)))
         (todo "TODO"
               ((org-agenda-overriding-header "To Refile")
                (org-agenda-files '(,(concat jc/org-agenda-directory "inbox.org")))))
         (todo "TODO"
               ((org-agenda-overriding-header "Emails")
                (org-agenda-files '(,(concat jc/org-agenda-directory "emails.org")))))
         (todo "NEXT"
               ((org-agenda-overriding-header "In Progress")
                (org-agenda-files '(,(concat jc/org-agenda-directory "someday.org")
                                    ,(concat jc/org-agenda-directory "projects.org")
                                    ,(concat jc/org-agenda-directory "next.org")))
                ))
         (todo "TODO"
               ((org-agenda-overriding-header "Projects")
                (org-agenda-files '(,(concat jc/org-agenda-directory "projects.org")))
                ))
         (todo "TODO"
               ((org-agenda-overriding-header "One-off Tasks")
                (org-agenda-files '(,(concat jc/org-agenda-directory "next.org")))
                (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled))))
         nil)))

(add-to-list 'org-agenda-custom-commands `,jc/org-agenda-todo-view)

(use-package org-super-agenda
  :disabled
  ;; :after org-agenda
  :init
  ;; (setq org-super-agenda-groups '((:name "To Refile"
  ;;                                        :and (:scheduled nil :deadline nil)
  ;;                                        )))

  ;; (setq org-super-agenda-groups '((:name "Today"
  ;;                                        ;; :time-grid t
  ;;                                        ;; :todo "TODAY"
  ;;                                        :deadline today)
  ;;                                 ;; (:name "Important"
  ;;                                 ;;        ;; Single arguments given alone
  ;;                                 ;;        ;; :tag "bills"
  ;;                                 ;;        :priority "A"
  ;;                                 ;;        )
  ;;                                 (:name "Bills"
  ;;                                        :tag "bills"
  ;;                                        :children todo)
  ;;                                 (:name "Past due"
  ;;                                        :scheduled past
  ;;                                        :deadline past)
  ;;                                 ))

  ;; (setq org-super-agenda-groups '((:name "Today"
  ;;                                        :time-grid t
  ;;                                        :scheduled today)
  ;;                                 (:name "Due today"
  ;;                                        :deadline today)
  ;;                                 (:name "Important"
  ;;                                        :priority "A")
  ;;                                 (:name "Overdue"
  ;;                                        :deadline past)
  ;;                                 (:name "Due soon"
  ;;                                        :deadline future)
  ;;                                 (:name "Waiting"
  ;;                                        :todo "WAIT")))
  ;; (let ((org-super-agenda-groups
  ;;        '((:log t)  ; Automatically named "Log"
  ;;          (:name "Schedule"
  ;;                 :time-grid t)
  ;;          (:name "Today"
  ;;                 :scheduled today)
  ;;          (:habit t)
  ;;          (:name "Due today"
  ;;                 :deadline today)
  ;;          (:name "Overdue"
  ;;                 :deadline past)
  ;;          (:name "Due soon"
  ;;                 :deadline future)
  ;;          (:name "Unimportant"
  ;;                 :todo ("SOMEDAY" "MAYBE" "CHECK" "TO-READ" "TO-WATCH")
  ;;                 :order 100)
  ;;          (:name "Waiting..."
  ;;                 :todo "WAITING"
  ;;                 :order 98)
  ;;          (:name "Scheduled earlier"
  ;;                 :scheduled past))))
  ;;   (org-agenda-list))
  :config
  (org-super-agenda-mode t))

(provide 'init-org)
;;; init-org.el ends here
