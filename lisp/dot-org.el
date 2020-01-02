;; dot-org.el --- My personal org-mode setup.
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
  (setq use-package-expand-minimally t)
  (load "~/.emacs.d/lisp/org-settings"))

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

(use-package org-super-agenda
  ;; :after org-agenda
  :init
  ;; (setq org-super-agenda-groups '(
  ;;                                 (:name "Today"  ; Optionally specify section name
  ;;                                        :time-grid t  ; Items that appear on the time grid
  ;;                                        :todo "TODAY")  ; Items that have this TODO keyword
  ;;                                 (:name "Important"
  ;;                                        ;; Single arguments given alone
  ;;                                        :tag "bills"
  ;;                                        :priority "A")
  ;;                                 ;; Set order of multiple groups at once
  ;;                                 ;; (:order-multi (2 (:name "Shopping in town"
  ;;                                 ;;                         ;; Boolean AND group matches items that match all subgroups
  ;;                                 ;;                         :and (:tag "shopping" :tag "@town"))
  ;;                                 ;;                  (:name "Food-related"
  ;;                                 ;;                         ;; Multiple args given in list with implicit OR
  ;;                                 ;;                         :tag ("food" "dinner"))
  ;;                                 ;;                  (:name "Personal"
  ;;                                 ;;                         :habit t
  ;;                                 ;;                         :tag "personal")
  ;;                                 ;;                  (:name "Space-related (non-moon-or-planet-related)"
  ;;                                 ;;                         ;; Regexps match case-insensitively on the entire entry
  ;;                                 ;;                         :and (:regexp ("space" "NASA")
  ;;                                 ;;                                       ;; Boolean NOT also has implicit OR between selectors
  ;;                                 ;;                                       :not (:regexp "moon" :tag "planet")))))
  ;;                                 ;; Groups supply their own section names when none are given
  ;;                                 (:todo "WAITING" :order 8)  ; Set order of this section
  ;;                                 (:todo ("SOMEDAY" "TO-READ" "CHECK" "TO-WATCH" "WATCHING")
  ;;                                        ;; Show this group at the end of the agenda (since it has the
  ;;                                        ;; highest number). If you specified this group last, items
  ;;                                        ;; with these todo keywords that e.g. have priority A would be
  ;;                                        ;; displayed in that group instead, because items are grouped
  ;;                                        ;; out in the order the groups are listed.
  ;;                                        :order 9)
  ;;                                 (:name "All other things"
  ;;                                        :deadline nil
  ;;                                        :scheduled nil)
  ;;                                 (:priority<= "B"
  ;;                                              ;; Show this section after "Today" and "Important", because
  ;;                                              ;; their order is unspecified, defaulting to 0. Sections
  ;;                                              ;; are displayed lowest-number-first.
  ;;                                              :order 1)
  ;;                                 ;; After the last group, the agenda will display items that didn't
  ;;                                 ;; match any of these groups, with the default order position of 99
  ;;                                 ))
  (setq org-super-agenda-groups '(

                                  (:name "Overdue"
                                         :deadline past
                                         :scheduled past
                                         )
                                  (:name "Due Today"
                                         :deadline today
                                         )
                                  (:name "Scheduled Today"
                                         :scheduled today
                                         )

                                  (:name "Important"
                                         :priority "A"
                                         :discard (:anything t)
                                         )
                                  )


        )

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

(provide 'dot-org)
;;; dot-org.el ends here
