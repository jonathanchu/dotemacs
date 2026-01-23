;;; init.el --- My personal Emacs configuration for the terminal -*- lexical-binding: t -*-
;;
;; Copyright (c) 2026
;;
;; Author: Jonathan Chu <me@jonathanchu.is>
;; URL: https://github.com/jonathanchu/dotemacs
;; Version: 1.0

;; This file is not part of GNU Emacs.

;;; Commentary:

;; A barebones emacs config based on my gui config

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

;;; Bootstrapping

(defconst emacs-start-time (current-time))

;;; Packaging

(require 'package)
(setq load-prefer-newer t
      package-enable-at-startup nil
      package-user-dir (concat user-emacs-directory "elpa")
      package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")))

(package-initialize)

;; Bootstrap `use-package' when emacs v < 29
(when (< emacs-major-version 29)
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package)))

(setq-default use-package-verbose nil
              use-package-expand-minimally t
              use-package-always-ensure t)
(eval-when-compile
  (require 'use-package))

;; for now
(setq package-check-signature 'allow-unsigned)
(setq package-unsigned-archives '("gnu" "nongnu"))

;;; Core

;;;; Performance

(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))

;; native compile settings
(when (featurep 'native-compile)
  (setq native-comp-async-report-warnings-errors nil))

;; long line handling
(setq bidi-inhibit-bpa t)

;;;; Encoding

(prefer-coding-system 'utf-8)

;;;; Files & Backups

;; custom settings in a separate file
(setq-default custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file :no-error-if-file-is-missing)

;; recent files mode
(setq recentf-max-saved-items 100)
(recentf-mode 1)

;;;; Editing Defaults

(setq-default indent-tabs-mode nil)

(setq sentence-end-double-space nil
      require-final-newline t
      show-trailing-whitespace t
      ring-bell-function 'ignore
      initial-scratch-message nil)

;; only type 'y' or 'n' instead of 'yes' or 'no'
(setq use-short-answers t)

;; delete selection, insert text
(delete-selection-mode t)

;; reload the file in buffer when they change on disk
(global-auto-revert-mode t)

;; delete trailing whitespace in all modes
(add-hook 'before-save-hook #'delete-trailing-whitespace)

;;;; Scrolling

(setq scroll-conservatively 10000)

;;;; Windows & Frames

(setq window-combination-resize t)

;; consider all themes as safe
(setq custom-safe-themes t)

;;;; Org

;; conservative indention for org src blocks
(setq org-src-preserve-indentation t)

(use-package org
  :ensure nil
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c i" . org-time-stamp-inactive))
  :custom-face
  (org-table ((t (:inherit fixed-pitch))))
  (org-code ((t (:inherit fixed-pitch))))
  (org-block ((t (:inherit fixed-pitch))))
  (org-verbatim ((t (:inherit fixed-pitch))))
  :config
  ;; keep org and tasks separate from (De)notes
  (setq org-directory "~/Dropbox/org/"
        org-default-notes-file (concat org-directory "inbox.org")
        org-log-done 'time
        org-log-into-drawer t
        org-deadline-warning-days 7
        org-agenda-span 'day)

  ;; TODO states
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "WAITING(w@/!)" "|" "DONE(d)" "CANCELLED(c@)")))

  ;; Tags
  (setq org-tag-alist '(("work" . ?w)
                        ("personal" . ?p)
                        ("idea" . ?i)))

  ;; Formatting for org agenda views
  ;; Example: %-25:c
  ;; - = left-align
  ;; 25 = width in characters
  ;; : = truncate if too long
  ;; c = category
  (setq org-agenda-prefix-format
        '((agenda . " %i %-12:c%?-12t% s")
          (todo   . " %i %-25:c")
          (tags   . " %i %-12:c")
          (search . " %i %-12:c")))

  ;; Agenda files
  (setq org-agenda-files (list org-directory))

  ;; Archive to subdirectory
  (setq org-archive-location "archive/%s_archive::")

  ;; Refile to headings in agenda files
  (setq org-refile-targets '((org-agenda-files :maxlevel . 2))
        org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil)

  ;; Capture templates
  (setq org-capture-templates
        '(("t" "Task" entry (file+headline org-default-notes-file "Inbox")
           "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n")
          ("w" "Work log" entry (file+olp+datetree "~/Dropbox/org/work_log.org")
           "* %U %?\n")
          ("i" "Idea" entry (file+headline "~/Dropbox/org/someday.org" "Ideas")
           "* %? :idea:\n:PROPERTIES:\n:CREATED: %U\n:END:\n")))

  ;; Custom agenda views
  (setq org-agenda-custom-commands
        '(("d" "Dashboard"
           ((agenda "" ((org-agenda-span 'day)))
            (todo "NEXT" ((org-agenda-overriding-header "Next Actions")))
            (todo "WAITING" ((org-agenda-overriding-header "Waiting On")))))
          ("w" "Work" tags-todo "+work")
          ("p" "Personal" tags-todo "+personal")
          ("i" "Ideas" tags "+idea"))))

;;; UI & Appearance

;;;; Frame Defaults

(setq inhibit-splash-screen t)
(menu-bar-mode -1)

;;;; Theme

(use-package catppuccin-theme
  :config
  (setq catppuccin-flavor 'latte)
  (load-theme 'catppuccin t))

;;; Completion Framework

;;;; Vertico

(use-package vertico
  :init
  (vertico-mode)
  :config
  (setq vertico-cycle t
        vertico-count 20))

;;;; Orderless

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;;;; Marginalia

(use-package marginalia
  :init
  (marginalia-mode))

;;;; Consult

(use-package consult
  :bind (("C-c k" . consult-ripgrep)
         ("C-x C-r" . consult-recent-file)
         ("C-s" . consult-line)
         ("C-c C-r" . consult-history)
         ("C-x b" . consult-buffer)
         ("C-c o" . consult-outline))
  :config
  (setq consult-narrow-key "<"))

;;;; Embark

(use-package embark
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim)))

(use-package embark-consult
  :after (embark consult)
  :demand t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;;; Editor

;;;; Line Numbers

(use-package display-line-numbers
  :defer
  :ensure nil
  :custom
  (display-line-numbers-width-start t)
  :hook
  (prog-mode . display-line-numbers-mode)
  (tex-mode . display-line-numbers-mode)
  (markdown-mode . display-line-numbers-mode)
  (conf-mode . display-line-numbers-mode)
  (org-mode . display-line-numbers-mode))

;;;; Parens & Delimiters

(use-package smartparens
  :hook ((emacs-lisp-mode . smartparens-strict-mode)
         (org-mode . smartparens-mode))
  :config
  (require 'smartparens-config))

;;;; Navigation

(use-package outline
  :ensure nil
  :hook (emacs-lisp-mode . outline-minor-mode)
  :bind (:map outline-minor-mode-map
              ("TAB" . outline-cycle)
              ("<backtab>" . outline-cycle-buffer)))

;;;; Windows & Buffers

(use-package ibuffer
  :ensure nil
  :bind ("C-x C-b" . ibuffer))

(use-package fullframe
  :after magit
  :config
  (fullframe magit-status magit-mode-quit-window)
  (fullframe ibuffer ibuffer-quit))

;;; Project Management

(use-package projectile
  :defer t
  :bind-keymap ("C-c p" . projectile-command-map)
  :bind (:map projectile-command-map
              ("f" . consult-projectile-find-file)
              ("p" . consult-projectile-switch-project)
              ("b" . consult-projectile-switch-to-buffer))
  :init
  (setq projectile-completion-system 'default)
  :config
  (setq projectile-project-search-path '("~/projects/")
        projectile-switch-project-action #'projectile-dired)
  (projectile-mode +1))

(use-package consult-projectile
  :after (consult projectile))

;;; Version Control

;;;; Magit

(use-package magit
  :bind
  (("C-x g" . magit-status)
   ("C-c C-a" . magit-commit-amend)
   ("C-c g" . magit-file-dispatch)
   ("s-P" . magit-status-with-prefix-arg))
  :custom
  (magit-diff-refine-hunk 'all)
  (magit-repository-directories '(("~/projects" . 3)))
  (magit-status-margin '(t "%Y-%m-%d %H:%M " magit-log-margin-width t 18))
  (magit-log-margin '(t "%Y-%m-%d %H:%M " magit-log-margin-width t 18))
  (magit-section-initial-visibility-alist
   '((stashes . hide)
     (unpulled . show)
     (unpushed . show)
     (untracked . show)
     (recent . show)))
  (magit-status-show-hashes-in-headers t)
  (magit-log-section-commit-count 15)
  (magit-revision-insert-related-refs nil)
  (magit-refresh-status-buffer nil)
  (magit-section-cache-visibility t)
  :config
  (defun magit-status-with-prefix-arg ()
    "Call `magit-status` with a prefix."
    (interactive)
    (let ((current-prefix-arg '(4)))
      (call-interactively #'magit-status))))

;;;; Diff Highlighting

(use-package diff-hl
  :init
  (global-diff-hl-mode 1)
  :hook
  (magit-pre-refresh . diff-hl-magit-pre-refresh)
  (magit-post-refresh . diff-hl-magit-post-refresh))

;;; Key Bindings

;; eval region
(global-set-key (kbd "C-c e r") #'eval-region)

;;; Finalization

(unless window-system
  (let ((elapsed (float-time (time-subtract (current-time)
                                            emacs-start-time))))
    (message "Loading %s...done (%.3fs)" load-file-name elapsed))

  (add-hook 'after-init-hook
            `(lambda ()
               (let ((elapsed
                      (float-time
                       (time-subtract (current-time) emacs-start-time))))
                 (message "Loading %s...done (%.3fs) [after-init]"
                          ,load-file-name elapsed))) t))

;;; init.el ends here
