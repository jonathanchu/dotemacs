;;; init.el --- My personal Emacs configuration. -*- lexical-binding: t -*-
;;
;; Copyright (c) 2015 - 2026
;;
;; Author: Jonathan Chu <me@jonathanchu.is>
;; URL: https://github.com/jonathanchu/dotemacs
;; Version: 2.0
;; Package-Requires: ((emacs "29.1"))

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

;;; Bootstrapping

(when (< emacs-major-version 29)
  (error "This configuration requires Emacs 29 or newer"))

(defconst emacs-start-time (current-time))

(setq user-full-name "Jonathan Chu"
      user-mail-address "me@jonathanchu.is")

(defvar current-user
  (getenv
   (if (equal system-type 'windows-nt) "USERNAME" "USER")))

(message "Your Emacs is powering up... Be patient, Master %s!" current-user)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

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

;; use-package is built-in since Emacs 29
(setq-default use-package-verbose nil
              use-package-expand-minimally t
              use-package-always-ensure t)
(require 'use-package)

;; for now
(setq package-check-signature 'allow-unsigned)
(setq package-unsigned-archives '("gnu" "nongnu"))

;;; Core

;;;; Performance

(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))

;; reset GC threshold after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 16777216)))

;; Scroll performance
(setq fast-but-imprecise-scrolling t
      redisplay-skip-fontification-on-input t
      inhibit-compacting-font-caches t
      auto-window-vscroll nil
      jit-lock-defer-time 0.05)

;; try to improve handling of long lines
(setq bidi-inhibit-bpa t)

;; protect against performance issues with minified files
(global-so-long-mode 1)

;; native compile settings
(when (featurep 'native-compile)
  (setq native-comp-async-report-warnings-errors nil))

;; Warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;;;; Encoding

(prefer-coding-system 'utf-8)
(define-coding-system-alias 'UTF-8 'utf-8)
(define-coding-system-alias 'utf8 'utf-8)

;;;; Files & Backups

(setq auto-save-default nil
      create-lockfiles nil)

;; Write temp files to directory to not clutter the filesystem
(defvar user-temporary-file-directory
  (concat temporary-file-directory user-login-name "/"))
(make-directory user-temporary-file-directory t)
(setq backup-by-copying t)
(with-eval-after-load 'tramp
  (add-to-list 'backup-directory-alist
               `(,tramp-file-name-regexp nil)))
(setq backup-directory-alist
      `(("." . ,user-temporary-file-directory)))
(setq auto-save-list-file-prefix
      (concat user-temporary-file-directory ".auto-saves-"))
(setq auto-save-file-name-transforms
      `((".*" ,user-temporary-file-directory t)))

;; custom settings in a separate file
(setq-default custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file :no-error-if-file-is-missing)

(use-package recentf
  :ensure nil
  :config
  (setq recentf-max-saved-items 250
        recentf-max-menu-items 15
        recentf-auto-cleanup 300
        recentf-exclude (list "^/var/folders\\.*"
                              "COMMIT_EDITMSG\\'"
                              ".*-autoloads\\.el\\'"
                              "[/\\]\\.elpa/"
                              "/\\.git/.*\\'"
                              "ido.last"
                              ".emacs.d"))
  (recentf-mode))

(use-package saveplace
  :ensure nil
  :init
  (setq save-place-file (expand-file-name "saved-places" user-emacs-directory))
  :config
  (save-place-mode t))

;; desktop save mode
(desktop-save-mode t)
(setq desktop-restore-eager 5
      desktop-save t)

;;;; Scrolling

(setq scroll-preserve-screen-position t
      mouse-wheel-scroll-amount '(1 ((shift) . 1))
      mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse t
      scroll-step 1
      scroll-conservatively 10000
      scroll-margin 3)

(pixel-scroll-precision-mode 1)

;;;; Editing Defaults

(setq-default indent-tabs-mode nil
              truncate-lines t
              line-spacing 4
              cursor-type 'box)

(setq fill-column 80
      tab-always-indent 'complete
      require-final-newline t
      show-trailing-whitespace t
      sentence-end-double-space nil
      ring-bell-function 'ignore
      echo-keystrokes 0.02
      initial-major-mode 'emacs-lisp-mode
      initial-scratch-message nil)

;; only type 'y' or 'n' instead of 'yes' or 'no'
(setq use-short-answers t)

;; show line and column in mode line
(line-number-mode 1)
(column-number-mode 1)
(size-indication-mode t)

;; delete selection, insert text
(delete-selection-mode t)

;; make sure looking at most recent changes
(global-auto-revert-mode t)

;; delete trailing whitespace in all modes
(add-hook 'before-save-hook #'delete-trailing-whitespace)

;; useful for camelCase
(add-hook 'prog-mode-hook #'subword-mode)

;; turn on visual line mode for text
(add-hook 'text-mode-hook #'visual-line-mode)

;; variable pitch mode for text
(add-hook 'text-mode-hook #'variable-pitch-mode)

;;;; Completion Behavior

(setq read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t)
(mapc (lambda (x)
        (add-to-list 'completion-ignored-extensions x))
      '(".gz" ".pyc" ".elc" ".exe"))

;;;; Windows & Frames

(setq window-combination-resize t
      confirm-kill-processes nil)

;; consider all themes as safe
(setq custom-safe-themes t)

;; Suppress warnings for functions redefined with defadvice
(setq ad-redefinition-action 'accept)

;;;; Language Defaults

(setq js-indent-level 2
      css-indent-offset 2)

;;;; Server

(when (display-graphic-p)
  (require 'server)
  (message "Starting up server...")
  (unless (server-running-p)
    (server-start)))

;;;; Required Libraries

(eval-when-compile
  (require 'cl-lib))
(require 'bind-key)

(use-package diminish
  :defer t)

(use-package exec-path-from-shell
  :defer 1
  :if (memq window-system '(mac ns))
  :config
  (exec-path-from-shell-initialize))

;;; macOS Specific

(when (eq system-type 'darwin)
  (setq mac-option-modifier 'meta
        mac-command-modifier 'super
        mac-allow-anti-aliasing t
        ns-pop-up-frames nil)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . light)))

;;; UI & Appearance

;;;; Frame Defaults

(setq inhibit-splash-screen t)
(menu-bar-mode -1)
(when (functionp 'tool-bar-mode)
  (tool-bar-mode -1))
(when window-system
  (scroll-bar-mode -1))

;; Reduce the clutter in the fringes
(setq indicate-buffer-boundaries nil
      indicate-empty-lines nil)

;; Doesn't exist in terminal Emacs
(unless (fboundp 'define-fringe-bitmap)
  (fset 'define-fringe-bitmap #'ignore))

;;;; Fonts

(add-to-list 'default-frame-alist '(font . "JetBrains Mono-15"))

;;;; Icons

(use-package nerd-icons)

;;;; Modeline

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 30
        doom-modeline-indent-info t))

;;;; Theme

(use-package catppuccin-theme
  :config
  (setq catppuccin-flavor 'macchiato)
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

(use-package paren
  :ensure nil
  :config
  (show-paren-mode t))

(use-package smartparens
  :hook ((emacs-lisp-mode . smartparens-strict-mode)
         (org-mode . smartparens-mode))
  :config
  (require 'smartparens-config))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;;;; Whitespace

(use-package whitespace
  :ensure nil
  :hook (prog-mode . whitespace-mode)
  :config
  (setq whitespace-action '(auto-cleanup)
        whitespace-style '(trailing space-before-tab indentation empty space-after-tab)))

;;;; Highlighting

(use-package hl-line
  :hook (prog-mode . hl-line-mode)
  :config
  (setq hl-line-sticky-flag nil
        global-hl-line-sticky-flag nil))

(use-package volatile-highlights
  :config
  (volatile-highlights-mode t))

;;;; Selection & Regions

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package easy-kill
  :bind ([remap kill-ring-save] . easy-kill))

;;;; Navigation

(use-package outline
  :ensure nil
  :hook (emacs-lisp-mode . outline-minor-mode)
  :bind (:map outline-minor-mode-map
              ("TAB" . outline-cycle)
              ("<backtab>" . outline-cycle-buffer)))

(use-package beginend
  :config
  (beginend-global-mode))

(use-package goto-chg
  :bind
  ("C-c b ," . goto-last-change)
  ("C-c b ." . goto-last-change-reverse))

;;;; Text Manipulation

(use-package move-text
  :bind
  (("M-p" . move-text-up)
   ("M-n" . move-text-down)))

(use-package smart-comment
  :bind ("s-/" . smart-comment))

(use-package aggressive-indent
  :hook (clojure-mode . aggressive-indent-mode))

;;;; Folding

(use-package origami
  :hook (prog-mode . origami-mode)
  :bind
  ("s-[" . origami-close-node-recursively)
  ("s-]" . origami-open-node-recursively)
  ("M-[" . origami-close-all-nodes)
  ("M-]" . origami-open-all-nodes))

;;;; Multiple Cursors

(use-package multiple-cursors
  :bind ("C-c m c" . mc/edit-lines))

;;;; Windows & Buffers

(use-package popwin
  :config
  (popwin-mode t))

(use-package fullframe
  :config
  (fullframe magit-status magit-mode-quit-window)
  (fullframe ibuffer ibuffer-quit))

(use-package ibuffer
  :ensure nil
  :bind ("C-x C-b" . ibuffer))

(use-package ibuffer-vc
  :defer t
  :init
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-vc-set-filter-groups-by-vc-root)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic)))))

;;;; Search

(use-package deadgrep
  :bind ("s-F" . deadgrep))

;;; Project Management

(use-package project
  :ensure nil
  :bind-keymap ("C-c p" . project-prefix-map)
  :bind (("s-t" . project-find-file))
  :config
  (setq project-switch-commands '((project-find-file "Find file")
                                  (project-find-regexp "Find regexp")
                                  (project-dired "Dired")
                                  (magit-project-status "Magit" ?m)))
  (setq project-vc-extra-root-markers '(".project" ".projectile" "package.json")))

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
     (untracked . show)
     (unpulled . show)
     (unpushed . show)
     (recent . show)))
  (magit-revision-insert-related-refs nil)
  (magit-refresh-status-buffer nil)
  (magit-section-cache-visibility t)
  (magit-log-section-commit-count 15)
  (magit-status-show-hashes-in-headers t)
  :config
  (defun magit-status-with-prefix-arg ()
    "Call `magit-status` with a prefix."
    (interactive)
    (let ((current-prefix-arg '(4)))
      (call-interactively #'magit-status)))
  :hook
  (magit-mode . (lambda () (setq-local left-fringe-width 16))))

(use-package magit-git-toolbelt
  :load-path "lisp/"
  :after magit
  :init
  (setq magit-git-toolbelt-key "."))

(use-package magit-gh
  :load-path "lisp/"
  :after magit
  :init
  (setq magit-gh-key ","))

;;;; Git Gutter

(defvar +vc-gutter-default-style t
  "If non-nil, enable the default look of the vc gutter.")

(use-package git-gutter-fringe
  :config
  (when +vc-gutter-default-style
    (if (fboundp 'fringe-mode) (fringe-mode '4))
    (setq-default fringes-outside-margins t
                  highlight-nonselected-windows nil)
    (define-fringe-bitmap 'git-gutter-fr:added [224]
      nil nil '(center repeated))
    (define-fringe-bitmap 'git-gutter-fr:modified [224]
      nil nil '(center repeated))
    (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240]
      nil nil 'bottom)))

(use-package git-gutter
  :after git-gutter-fringe
  :hook (focus-in . git-gutter:update-all-windows)
  :config
  (global-git-gutter-mode +1))

;;;; Git Utilities

(use-package git-messenger
  :defer t
  :bind ("C-x v m" . git-messenger:popup-message))

(use-package github-browse-file
  :defer t)

;;; Programming Languages

;;;; Syntax Checking

(use-package flycheck
  :commands (flycheck-mode flycheck-list-errors flycheck-buffer)
  :defer 2
  :init
  (setq flycheck-indication-mode 'right-fringe
        flycheck-check-syntax-automatically '(save mode-enabled)
        flycheck-highlighting-mode 'symbols
        flycheck-disabled-checkers '(emacs-lisp emacs-lisp-checkdoc make javascript-jshint))
  :config
  (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
    [0 0 0 0 0 4 12 28 60 124 252 124 60 28 12 4 0 0 0 0])
  (global-flycheck-mode 1))

(use-package flycheck-pos-tip
  :after flycheck
  :config
  (setq flycheck-pos-tip-timeout 10
        flycheck-display-errors-delay 0.5)
  (flycheck-pos-tip-mode +1))

(use-package package-lint
  :defer t
  :commands (package-lint-current-buffer))

(use-package flycheck-package
  :after flycheck
  :config (flycheck-package-setup))

;;;; Snippets

(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :after yasnippet)

;;;; Python

(use-package elpy
  :defer t
  :init (advice-add 'python-mode :before 'elpy-enable)
  :config
  (setq elpy-rpc-python-command "python3"))

(use-package jinja2-mode)

;;;; Go

(use-package go-mode)

;;;; Web

(use-package web-mode
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2)
  (add-to-list 'auto-mode-alist '("\\.hb\\.html\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode)))

(use-package json-mode)

;;;; Markup

(use-package markdown-mode)
(use-package yaml-mode)
(use-package toml-mode)

;;;; Shell & Config

(use-package make-mode :ensure nil)
(use-package sh-script :ensure nil)

(use-package fish-mode
  :defer t
  :hook
  (fish-mode . (lambda ()
                 (add-hook 'before-save-hook #'fish_indent-before-save nil t))))

;;; Productivity

;;;; Org Mode

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
  ;; fix the line number rendering in GUI only
  (set-face-attribute 'line-number nil :inherit 'fixed-pitch)
  (set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch)

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
          ("i" "Idea" entry (file+headline "~/Dropbox/org/ideas.org" "Ideas")
           "* %? :idea:\n:PROPERTIES:\n:CREATED: %U\n:END:\n")))

  ;; Custom agenda views
  (setq org-agenda-custom-commands
        '(("d" "Dashboard"
           ((agenda "" ((org-agenda-span 'day)))
            (todo "NEXT" ((org-agenda-overriding-header "Next Actions")))
            (todo "WAITING" ((org-agenda-overriding-header "Waiting On")))))
          ("w" "Work" tags-todo "+work")
          ("p" "Personal" tags-todo "+personal")
          ("i" "Ideas" tags "+idea")))

  ;; Babel
  (setq org-confirm-babel-evaluate nil)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)))
  (setq org-babel-python-command "python3")

  (use-package ob-typescript
    :after org))

;;;; Org Export

(use-package ox-hugo
  :after ox)

;;;; Denote

(use-package denote
  :hook (dired-mode . denote-dired-mode)
  :bind
  (("C-c n n" . denote)
   ("C-c n r" . denote-rename-file)
   ("C-c n l" . denote-link)
   ("C-c n b" . denote-backlinks)
   ("C-c n d" . denote-dired)
   ("C-c n g" . denote-grep))
  :config
  (setq denote-directory (expand-file-name "~/Dropbox/Notes/")
        denote-sort-keywords t)
  (denote-rename-buffer-mode 1))

(use-package consult-denote
  :after (denote consult)
  :bind
  (("C-c n s" . consult-denote-find)
   ("C-c n g" . consult-denote-grep)))

;;;; Dired

(use-package dired-narrow
  :bind (:map dired-mode-map
              ("/" . dired-narrow)))

;;; Custom Functions

;;;; Navigation

(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.
Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.
If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))
  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(defun switch-to-previous-buffer ()
  "Switch to the most recent buffer.  Toggle back and forth between the two most recent buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

;;;; Windows

(defun transpose-windows ()
  "If you have two windows, it swaps them."
  (interactive)
  (let ((this-buffer (window-buffer (selected-window)))
        (other-buffer (prog2
                          (other-window +1)
                          (window-buffer (selected-window))
                        (other-window -1))))
    (switch-to-buffer other-buffer)
    (switch-to-buffer-other-window this-buffer)
    (other-window -1)))

(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

;;;; Text Manipulation

(defun dcaps-to-scaps ()
  "Convert word in DOuble CApitals to Single Capitals."
  (interactive)
  (and (= ?w (char-syntax (char-before)))
       (save-excursion
         (and (if (called-interactively-p 1)
                  (skip-syntax-backward "w")
                (= -3 (skip-syntax-backward "w")))
              (let (case-fold-search)
                (looking-at "\\b[[:upper:]]\\{2\\}[[:lower:]]"))
              (capitalize-word 1)))))

(add-hook 'post-self-insert-hook #'dcaps-to-scaps)

(defun copy-buffer-file-name-as-kill (choice)
  "Copy the buffer-file-name to the kill-ring."
  (interactive "cCopy Buffer Name (f) full, (p) path, (n) name")
  (let ((new-kill-string)
        (name (if (eq major-mode 'dired-mode)
                  (dired-get-filename)
                (or (buffer-file-name) ""))))
    (cond ((eq choice ?f)
           (setq new-kill-string name))
          ((eq choice ?p)
           (setq new-kill-string (file-name-directory name)))
          ((eq choice ?n)
           (setq new-kill-string (file-name-nondirectory name)))
          (t (message "Quit")))
    (when new-kill-string
      (message "%s copied" new-kill-string)
      (kill-new new-kill-string))))

;;;; Utilities

(defun reload-config()
  "Reload Emacs configuration"
  (interactive)
  (load-file "~/.emacs.d/init.el"))

(defun stop-using-minibuffer ()
  "Kill the minibuffer when you use the mouse in another window."
  (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
    (abort-recursive-edit)))

(add-hook 'mouse-leave-buffer-hook #'stop-using-minibuffer)

;;;; Dired

(defun mydired-sort ()
  "Sort dired listings with directories first."
  (save-excursion
    (let (buffer-read-only)
      (forward-line 2)
      (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max)))
    (set-buffer-modified-p nil)))

(advice-add 'dired-readin :after #'mydired-sort)

;;;; Org

(defun my-org-config/after-org-archive ()
  (org-save-all-org-buffers))

(add-hook 'org-archive-hook 'my-org-config/after-org-archive)

;;; Advice

(defun my/no-query-kill-emacs (orig-fun &rest args)
  "Prevent active process query on quit."
  (cl-letf (((symbol-function 'process-list) (lambda () nil)))
    (apply orig-fun args)))

(advice-add 'save-buffers-kill-emacs :around #'my/no-query-kill-emacs)

(defun my/pop-to-mark-ensure-new-position (orig-fun &rest args)
  "Continue popping until cursor moves. Skip expand-region cruft after copy."
  (let ((p (point)))
    (when (eq last-command #'kill-ring-save)
      (dotimes (_ 3) (apply orig-fun args)))
    (dotimes (_ 10)
      (when (= p (point)) (apply orig-fun args)))))

(advice-add 'pop-to-mark-command :around #'my/pop-to-mark-ensure-new-position)

(setq set-mark-command-repeat-pop t)

;;; Key Bindings

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line] #'smarter-move-beginning-of-line)

;; duplicate line or region
(global-set-key (kbd "C-c d") #'duplicate-dwim)

;; switch to previous buffer
(global-set-key (kbd "C-`") #'switch-to-previous-buffer)

;; toggle window split
(global-set-key (kbd "C-x |") #'toggle-window-split)

;; sorting
(global-set-key (kbd "M-`") #'sort-lines)

;; font-size
(define-key global-map (kbd "s-=") #'text-scale-increase)
(define-key global-map (kbd "s--") #'text-scale-decrease)

;; fullscreen toggle
(global-set-key [(s return)] #'toggle-frame-fullscreen)

;; fixup whitespace
(global-set-key (kbd "C-c w") #'fixup-whitespace)

;; kill the current buffer
(global-set-key (kbd "C-x C-k") #'kill-current-buffer)

;; zap to char
(global-set-key (kbd "M-z") #'zap-up-to-char)

;; evaluate the region
(global-set-key (kbd "C-c e r") #'eval-region)

;;; Finalization

(when window-system
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
