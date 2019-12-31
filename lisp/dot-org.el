;;;_ , Org-mode

(eval-and-compile
  (require 'cl-lib)
  (require 'use-package)
  (setq use-package-verbose nil)
  (setq use-package-expand-minimally t)
  (load "org-settings"))

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
(setq org-priority-faces '((?A . error)
                           (?B . warning)
                           (?C . success)))

(setq org-tags-column -80)

(setq org-capture-templates '(("t" "Todo [inbox]" entry
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
  (setq org-super-agenda-groups '((:name "Today"
                                         :time-grid t
                                         :todo "TODAY"
                                         :deadline today)
                                  (:name "Important"
                                         ;; Single arguments given alone
                                         :tag "bills"
                                         ;; :priority "A"
                                         )
                                  (:name "Past due"
                                         :scheduled past
                                         :deadline past)))
  :config
  (org-super-agenda-mode t))

(provide 'dot-org)
;;; dot-org.el ends here
