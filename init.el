;; init.el --- My personal Emacs configuration.
;;
;; Copyright (c) 2015, 2016, 2017
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

;;----------------------------------------------------------------------------
;; Initialization
;;----------------------------------------------------------------------------

(defconst emacs-start-time (current-time))

(setq user-full-name "Jonathan Chu"
      user-mail-address "me@jonathanchu.is")

(add-to-list 'exec-path "/usr/local/bin")

(defvar current-user
  (getenv
   (if (equal system-type 'windows-nt) "USERNAME" "USER")))

(message "Your Emacs is powering up... Be patient, Master %s!" current-user)

;;----------------------------------------------------------------------------
;; Core
;;----------------------------------------------------------------------------

;; set encoding
(prefer-coding-system 'utf-8)

;; and tell emacs to play nice with encoding
(define-coding-system-alias 'UTF-8 'utf-8)
(define-coding-system-alias 'utf8 'utf-8)

;; save nothing
(setq auto-save-default nil
      create-lockfiles nil)

;; no splash screen
(setq inhibit-splash-screen t)

;; no message on startup
(setq initial-scratch-message nil)

(require 'cl-lib)

;; Reduce the frequency of garbage collection by making it happen on
;; each 25MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 25000000)
(setq gc-cons-percentage 0.6)

;;----------------------------------------------------------------------------
;; Packages
;;----------------------------------------------------------------------------

(require 'package)
;; (setq package-enable-at-startup nil)
;; (setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
;;                          ("melpa" . "https://melpa.org/packages/")
;;                          ("melpa-stable" . "https://stable.melpa.org/packages/")))

(setq load-prefer-newer t
      package-enable-at-startup nil
      package-user-dir (concat user-emacs-directory "elpa")
      package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")))

(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; use-package
(eval-when-compile
  (require 'use-package))
(setq use-package-verbose t
      use-package-enable-imenu-support t)
(require 'diminish)
(require 'bind-key)

;; (require 'core (concat user-emacs-directory "core/core"))

;; FIXME: installing powerline early :-/
(use-package powerline
  :ensure t)

(use-package dash
  :ensure t)

(use-package all-the-icons
  :ensure t)

(use-package popup
  :ensure t)

;;----------------------------------------------------------------------------
;; Global Config
;;----------------------------------------------------------------------------

;; load in custom-set-variables early. FIXME
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(magit-branch-arguments nil)
 '(package-selected-packages
   (quote
    (add-node-modules-path git-gutter-fringe+ helm parinfer shackle-mode fish-mode helm-ag paperless flycheck-pos-tip fringe-helper nlinum highlight-numbers xterm-color web-mode volatile-highlights use-package smex smartparens smart-comment scratch rich-minority restclient rainbow-mode rainbow-delimiters python-mode powerline popwin paradox origami org-bullets neotree markdown-mode magit less-css-mode latex-preview-pane key-chord js2-mode imenu-anywhere ido-vertical-mode ibuffer-vc highlight-tail helm-projectile gitignore-mode github-browse-file gitconfig-mode git-timemachine git-messenger git-gutter-fringe fullframe frame-fns flycheck flx-ido fix-word fireplace f expand-region exec-path-from-shell evil esup elpy easy-kill dired-single dired-quick-sort dired+ diff-hl deft counsel command-log-mode clj-refactor blank-mode beacon atom-one-dark-theme anzu all-the-icons aggressive-indent ag ace-window)))
 '(paradox-github-token t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; set paths from shell
(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns))
  :config
  (exec-path-from-shell-initialize))


(when (display-graphic-p)
  (require 'server)
  (message "Starting up server...")
  (unless (server-running-p)
    (server-start)))

;;----------------------------------------------------------------------------
;; Editor Config
;;----------------------------------------------------------------------------

;; Warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; M-q
(setq fill-column 80)

;; no word wrap
(setq-default truncate-lines t)

(setq-default line-spacing 4)

;; no tabs
(setq-default indent-tabs-mode nil)

(setq ring-bell-function 'ignore)

;; show extra whitespace
(setq show-trailing-whitespace t)

;; ensure last line is a return
(setq require-final-newline t)

;; show file size
(size-indication-mode t)

;; make sure looking at most recent changes
(global-auto-revert-mode t)

(setq window-combination-resize t)

;;keep cursor at same position when scrolling
(setq scroll-preserve-screen-position t)

;; scroll one line at a time
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
(setq scroll-conservatively 10000)
(setq scroll-margin 3)

;; open with in original frame, not new window
(setq ns-pop-up-frames nil)

;; sentences end with single space
(setq sentence-end-double-space nil)

;; useful for camelCase
(subword-mode t)

;; delete selection, insert text
(delete-selection-mode t)

;; prevent active process query on quit
(require 'cl)
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  (cl-flet ((process-list ())) ad-do-it))

;; instantly display current key sequence in mini buffer
(setq echo-keystrokes 0.02)

;; desktop save mode
(desktop-save-mode t)
(setq desktop-restore-eager 5)
(setq desktop-save t)

(setq initial-major-mode 'emacs-lisp-mode)

;; improve filename completion
(setq read-file-name-completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)
(mapc (lambda (x)
        (add-to-list 'completion-ignored-extensions x))
      '(".gz" ".pyc" ".elc" ".exe"))

;; Suppress warnings for functions redefined with defadvice
(setq ad-redefinition-action 'accept)

(setq tab-always-indent 'complete)

;; try to improve handling of long lines
(setq bidi-display-reordering nil)

;; delete trailing whitespace in all modes
(add-hook 'before-save-hook #'delete-trailing-whitespace)

;;----------------------------------------------------------------------------
;; UI
;;----------------------------------------------------------------------------

;; only type 'y' or 'n' instead of 'yes' or 'no'
(fset 'yes-or-no-p 'y-or-n-p)

;; no menu bar
(menu-bar-mode -1)

;; no toolbar
(when (functionp 'tool-bar-mode)
  (tool-bar-mode -1))  ;; no toolbar

;; disable scroll bars
(if window-system
    (progn
      (scroll-bar-mode -1)
      ;;(set-frame-font "Inconsolata 15"))) ;; set font
      ))

(setq-default cursor-type 'bar)

;; nice fonts in OS X
(setq mac-allow-anti-aliasing t)

;; show line number in mode line
(line-number-mode 1)

;; show column number in the mode line
(column-number-mode 1)

;; highlight current line
(global-hl-line-mode +1)

(add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono for Powerline-13"))

;; mode line modifications based on powerline
(defvar mode-line-height 30)
(defvar-local doom--env-version nil)
(defvar-local doom--env-command nil)

(eval-when-compile (require 'powerline))
(defvar mode-line-bar          (pl/percent-xpm mode-line-height 100 0 100 0 3 "#00B3EF" nil))
(defvar mode-line-eldoc-bar    (pl/percent-xpm mode-line-height 100 0 100 0 3 "#B3EF00" nil))
(defvar mode-line-inactive-bar (pl/percent-xpm mode-line-height 100 0 100 0 3 nil nil))

(defface mode-line-is-modified nil "Face for mode-line modified symbol")
(defface mode-line-buffer-path nil "Face for mode-line buffer file path")
(defface mode-line-highlight nil "")
(defface mode-line-2 nil "")

;;
;; Mode-line segments
;;

(defun *buffer-path ()
  (when buffer-file-name
    (propertize
     (f-dirname
      (let ((buffer-path (file-relative-name buffer-file-name (doom/project-root)))
            (max-length (truncate (/ (window-body-width) 1.75))))
        (concat (projectile-project-name) "/"
                (if (> (length buffer-path) max-length)
                    (let ((path (reverse (split-string buffer-path "/" t)))
                          (output ""))
                      (when (and path (equal "" (car path)))
                        (setq path (cdr path)))
                      (while (and path (<= (length output) (- max-length 4)))
                        (setq output (concat (car path) "/" output))
                        (setq path (cdr path)))
                      (when path
                        (setq output (concat "../" output)))
                      (when (string-suffix-p "/" output)
                        (setq output (substring output 0 -1)))
                      output)
                  buffer-path))))
     'face (if active 'mode-line-buffer-path))))

(defun *buffer-state ()
  (when buffer-file-name
    (propertize
     (concat (if (not (file-exists-p buffer-file-name))
                 "∄"
               (if (buffer-modified-p) "✱"))
             (if buffer-read-only ""))
     'face 'mode-line-is-modified)))

(defun *buffer-name ()
  "The buffer's name."
  (s-trim-left (format-mode-line "%b")))

(defun *buffer-pwd ()
  "Displays `default-directory'."
  (propertize
   (concat "[" (abbreviate-file-name default-directory) "]")
   'face 'mode-line-2))

(defun *major-mode ()
  "The major mode, including process, environment and text-scale info."
  (concat (format-mode-line mode-name)
          (if (stringp mode-line-process) mode-line-process)
          (if doom--env-version (concat " " doom--env-version))
          (and (featurep 'face-remap)
               (/= text-scale-mode-amount 0)
               (format " (%+d)" text-scale-mode-amount))))

(use-package f
  :ensure t)

;;----------------------------------------------------------------------------
;; Doom Stuff
;;----------------------------------------------------------------------------

(load-file "~/.emacs.d/doom.el")

(add-to-list 'load-path "~/.emacs.d/themes")
(require 'doom-themes)
(load-theme 'doom-one t) ;; or doom-dark, etc.

;;; OPTIONAL
;; brighter source buffers
(add-hook 'find-file-hook 'doom-buffer-mode)
;; brighter minibuffer when active
(add-hook 'minibuffer-setup-hook 'doom-buffer-mode)
;; Custom neotree theme
(require 'doom-neotree)
(setq doom-neotree-enable-file-icons t)

;;----------------------------------------------------------------------------
;; UNSORTED MESS
;;----------------------------------------------------------------------------

;; javascript
(setq js-indent-level 2)

;; css
(setq css-indent-offset 2)

;; prevent active process query on quit
;; (require 'cl)
;; (defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
;;   (cl-flet ((process-list ())) ad-do-it))

;; instantly display current key sequence in mini buffer
;; (setq echo-keystrokes 0.02)

;; server mode
;; (if (not server-mode)
;;     (server-start nil t))

;; (setq initial-major-mode 'emacs-lisp-mode)

;; cua mode
(cua-mode t)
(setq cua-enable-cua-keys nil)
;;(setq cua-highlight-region-shift-only t)
;;(setq cua-toggle-set-mark nil)

;; debugging
;; (setq debug-on-error t)

;; desktop save mode
;; (desktop-save-mode t)
;; (setq desktop-restore-eager 5)
;; (setq desktop-save t)

;; improve filename completion
;; (setq read-file-name-completion-ignore-case t)
;; (setq read-buffer-completion-ignore-case t)
;; (mapc (lambda (x)
;;         (add-to-list 'completion-ignored-extensions x))
;;       '(".gz" ".pyc" ".elc" ".exe"))

;; Suppress warnings for functions redefined with defadvice
;; (setq ad-redefinition-action 'accept)

;; (setq tab-always-indent 'complete)

;; highlight current line
;; core-ui
;; (global-hl-line-mode +1)

;; try to improve handling of long lines
;; (setq bidi-display-reordering nil)

;; set paths from shell
(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns))
  :config
  (exec-path-from-shell-initialize))

(use-package clojure-mode
  :ensure t
  :config
  (define-clojure-indent
    (defroutes 'defun)
    (GET 2)
    (POST 2)
    (PUT 2)
    (DELETE 2)
    (HEAD 2)
    (ANY 2)
    (context 2)))

(use-package clj-refactor
  :ensure t
  :config
  (defun my-clojure-mode-hook ()
    (clj-refactor-mode 1)
    (yas-minor-mode 1) ; for adding require/use/import
    (cljr-add-keybindings-with-prefix "C-c C-m"))
  (add-hook 'clojure-mode-hook #'my-clojure-mode-hook))

(use-package cider
  :ensure t
  :config
  (progn
    (setq nrepl-log-messages t)
    (setq nrepl-hide-special-buffers t)
    (add-hook 'cider-mode-hook #'eldoc-mode)))

(use-package python-mode
  :ensure t
  :config
  (add-hook 'python-mode-hook
            '(lambda ()
               (setq fill-column 80)))
  (add-to-list 'auto-mode-alist '("\\.py" . python-mode)))

(use-package elpy
  :ensure t
  :config
  (elpy-enable))

(use-package magit
  :ensure t
  :config
  (progn
    (setq magit-push-always-verify nil)
    (setq magit-completing-read-function #'ivy-completing-read)
    (setq magit-last-seen-setup-instructions "1.4.0")
    (setq magit-diff-refine-hunk t))
  :bind
  ("C-x g" . magit-status)
  ("C-c C-a" . magit-commit-amend))

(use-package web-mode
  :ensure t
  :config
  (progn
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (add-to-list 'auto-mode-alist '("\\.hb\\.html\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))))


(use-package less-css-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.less\\'" . less-css-mode)))

(use-package org
  :defer t
  :bind
  ("C-c c" . org-capture)
  ("C-c a" . org-agenda)
  ("C-c l" . org-store-link)
  :config
  (setq org-directory "~/Dropbox/org")
  (setq org-log-done t)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "|" "DONE(d)")
          (sequence "WAITING(w)" "|" "CANCELED(c)")
          (sequence "NEXT(n)" "|" "HOLD(h)")
          ))
  (setq org-todo-keyword-faces
        '(("TODO" :foreground "green" :weight bold)
          ("NEXT" :foreground "blue" :weight bold)
          ("WAITING" :foreground "orange" :weight bold)
          ("HOLD" :foreground "magenta" :weight bold)
          ("CANCELED" :foreground "red" :weight bold)))
  (setq org-completion-use-ido t)
  (setq org-startup-folded nil)
  (setq org-ellipsis "⤵")
  (setq org-agenda-files '("~/Dropbox/org"))
  (setq org-agenda-window-setup (quote current-window))
  (setq org-deadline-warning-days 7)
  (setq org-agenda-span (quote fortnight))
  (setq org-agenda-skip-scheduled-if-deadline-is-shown t)
  (setq org-agenda-skip-deadline-prewarning-if-scheduled (quote pre-scheduled))
  (setq org-agenda-todo-ignore-deadlines (quote all))
  (setq org-agenda-todo-ignore-scheduled (quote all))
  (setq org-agenda-sorting-strategy
        (quote
         ((agenda deadline-up priority-down)
          (todo priority-down category-keep)
          (tags priority-down category-keep)
          (search category-keep))))
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)
  (add-hook 'org-mode-hook
            (lambda ()
              (make-variable-buffer-local 'yas/trigger-key)
              (setq yas/trigger-key [tab])
              (add-to-list 'org-tab-first-hook 'yas/org-very-safe-expand)
              (define-key yas/keymap [tab] 'yas/next-field))))

(use-package yasnippet
  :ensure t
  :config
  (progn
    (yas-global-mode 1)
    (setq yas-snippet-dirs (append yas-snippet-dirs
                                   '("~/.emacs.d/snippets")))))

(use-package flycheck-pos-tip
  :ensure t
  :config
  (setq flycheck-pos-tip-timeout 10)
  (setq flycheck-display-errors-delay 0.5)
  (flycheck-pos-tip-mode +1))

(use-package flycheck
  :ensure t
  :commands
  (flycheck-mode flycheck-list-errors flycheck-buffer)
  :defer 2
  :init
  (progn
    (setq flycheck-indication-mode 'right-fringe)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (setq flycheck-highlighting-mode 'symbols)
    (setq flycheck-disabled-checkers '(emacs-lisp emacs-lisp-checkdoc make javascript-jshint)))
  :config
  (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
    [0 0 0 0 0 4 12 28 60 124 252 124 60 28 12 4 0 0 0 0])
  (global-flycheck-mode 1)
  (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules))

(add-to-list 'load-path "~/.emacs.d/vendor")
(require 'prettier-js)
(setq prettier-js-args '("--print-width" "80"
                         "--tab-width" "2"
                         "--single-quote" "true"
                         "--trailing-comma" "all"
                         "--jsx-bracket-same-line" "false"))

(use-package flx-ido
  :ensure t)

(use-package ido
  :config
  (progn
    (ido-mode t)
    (ido-everywhere t)
    (flx-ido-mode t)
    (setq ido-enable-flex-matching t)
    (setq ido-use-faces nil)))

(use-package ido-vertical-mode
  :ensure t
  :config
  (progn
    (ido-vertical-mode 1)
    (setq ido-vertical-define-keys #'C-n-and-C-p-only)))

(use-package smex
  :ensure t
  :init
  (smex-initialize))

(use-package imenu-anywhere
  :ensure t
  :bind
  ("C-c i" . imenu-anywhere))

(use-package uniquify
  :config
  (progn
    (setq uniquify-buffer-name-style 'reverse)
    (setq uniquify-separator " • ")
    (setq uniquify-after-kill-buffer-p t)
    (setq uniquify-ignore-buffers-re "^\\*")))

;; (use-package ag
;;   :ensure t
;;   :config
;;   (progn
;;     (setq ag-reuse-buffers t)
;;     (setq ag-highlight-search t)
;;     (add-hook 'ag-mode-hook
;;               (lambda ()
;;                 (copy-face 'lazy-highlight #'ag-match-face))))
;;   :bind
;;   ("s-F" . ag-project))

;; (use-package projectile
;;   :ensure t
;;   :diminish ""
;;   :config
;;   (projectile-global-mode 1)
;;   :init
;;   (bind-key "s-t" #'projectile-find-file)
;;   (setq projectile-keymap-prefix (kbd "C-x p")))

(use-package fish-mode
  :ensure t
  :defer t
  :config
  (add-hook 'fish-mode-hook (lambda ()
                              (add-hook 'before-save-hook 'fish_indent-before-save))))

(use-package git-messenger
  :ensure t
  :defer t
  :bind
  ("C-x v m" . git-messenger:popup-message))

(use-package company
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'company-mode))

(use-package whitespace
  :config
  (progn
    (global-whitespace-mode t)
    (setq whitespace-action '(auto-cleanup))
    (setq whitespace-style '(trailing space-before-tab indentation empty space-after-tab))))

(use-package paren
  :config
  (show-paren-mode t))

(use-package saveplace
  :config
  (progn
    (setq-default save-place t)
    (setq save-place-file "~/.emacs.d/saved-places")))

(use-package command-log-mode
  :ensure t)

(use-package gitconfig-mode
  :ensure t)

(use-package gitignore-mode
  :ensure t)

(use-package git-timemachine
  :ensure t)

(use-package github-browse-file
  :ensure t)

(use-package restclient
  :ensure t)

(use-package make-mode
  :config
  (add-to-list 'auto-mode-alist '("\\Makefile\\'" . makefile-mode)))

(use-package neotree
  :ensure t
  :bind
  ("C-c n" . neotree-toggle))

(use-package dired+
  :ensure t)

(use-package dired-single
  :ensure t)

(use-package undo-tree
  :ensure t
  :config
  (progn
    (global-undo-tree-mode t)
    (setq undo-tree-visualizer-diff t)
    (setq undo-tree-visualizer-timestamps t)))

(use-package ibuffer
  :bind
  ("C-x C-b" . ibuffer))

(use-package ibuffer-vc
  :ensure t
  :defer t
  :init
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-vc-set-filter-groups-by-vc-root)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic)))))

(use-package fullframe
  :ensure t
  :config
  (progn
    (fullframe magit-status magit-mode-quit-window)
    (fullframe ibuffer ibuffer-quit)
    (fullframe paradox-list-packages paradox-quit-and-close)))

(use-package recentf
  :config
  (setq recentf-max-saved-items 250
        recentf-max-menu-items 15
        ;; Cleanup recent files only when Emacs is idle, but not when the mode
        ;; is enabled, because that unnecessarily slows down Emacs. My Emacs
        ;; idles often enough to have the recent files list clean up regularly
        recentf-auto-cleanup 300
        recentf-exclude (list "^/var/folders\\.*"
                              "COMMIT_EDITMSG\\'"
                              ".*-autoloads\\.el\\'"
                              "[/\\]\\.elpa/"
                              "/\\.git/.*\\'"
                              "ido.last"
                              ".emacs.d"))
  (recentf-mode))

(use-package beacon
  :ensure t
  :config
  (progn
    (beacon-mode 1)
    (setq beacon-push-mark 35)
    (setq beacon-color "#61AFEF")))

(use-package expand-region
  :ensure t
  :bind
  ("C-=" . er/expand-region))

(use-package markdown-mode
  :ensure t
  :mode "\\.md\\'")

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package smartparens
  :ensure t
  :init
  (smartparens-global-mode t)
  :config
  (progn
    (sp-local-pair 'web-mode "{%" "%}")
    (use-package smartparens-config)
    (setq sp-autoskip-closing-pair 'always)
    (setq sp-hybrid-kill-entire-symbol nil)))

(use-package paredit
  :ensure t
  :config
  (autoload 'enable-paredit-mode "paredit" t)
  (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  (add-hook 'ielm-mode-hook #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
  (add-hook 'scheme-mode-hook #'enable-paredit-mode)
  (add-hook 'clojure-mode-hook #'enable-paredit-mode)
  (add-hook 'org-mode-hook #'enable-paredit-mode)
  (add-hook 'python-mode-hook
            (lambda () (local-set-key (kbd "C-k") #'paredit-kill))))

(use-package latex-preview-pane
  :ensure t
  :config
  (latex-preview-pane-enable))

(use-package swiper
  :init
  (ivy-mode 1)
  :ensure t
  :bind
  ("C-s" . counsel-grep-or-swiper)
  ("C-r" . swiper)
  ("C-c C-r" . ivy-resume)
  :config
  (progn
    (setq ivy-use-virtual-buffers t)
    (setq ivy-format-function #'ivy-format-function-arrow)
    ;;(setq ivy-re-builders-alist '((t . ivy--regex-fuzzy)))
    (setq ivy-initial-inputs-alist nil)
    (advice-add 'swiper :after 'recenter)))

(use-package ace-window
  :ensure t
  :init
  (setq aw-keys '(?a ?s ?d ?f ?j ?k ?l))
  :bind
  ("C-x C-o" . ace-window))

(use-package avy
  :ensure t
  :init
  (setq avy-keys '(?a ?s ?d ?e ?f ?h ?j ?k ?l ?n ?m ?v ?r ?u))
  :config
  (progn
    (avy-setup-default)
    (setq avy-background t)
    (setq avy-styles-alist '((avy-goto-word-or-subword-1 . de-brujin)))
    (setq avy-styles-alist '((avy-got-char-2 . post)))
    (setq avy-all-windows nil)))

(use-package scratch
  :ensure t
  :config
  (autoload 'scratch "scratch" nil t))

(use-package flyspell
  :config
  (add-hook 'text-mode-hook #'flyspell-mode))

(use-package anzu
  :ensure t
  :config
  (progn
    (global-anzu-mode t)
    (set-face-attribute 'anzu-mode-line nil :foreground "yellow" :weight 'bold))
  :bind
  ("M-%" . anzu-query-replace)
  ("C-M-%" . anzu-query-replace-regexp))

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  (setq org-bullets-bullet-list '("●"
                                  "○"
                                  "◉"
                                  "◆")))

(use-package aggressive-indent
  :ensure t
  :init
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
  (add-hook 'clojure-mode-hook #'aggressive-indent-mode))

(use-package paradox
  :ensure t
  :config
  (setq paradox-execute-asynchronously t))

(use-package counsel
  :ensure t
  :bind
  ("M-x" . counsel-M-x)
  ("C-x C-f" . counsel-find-file)
  ("C-c g" . counsel-git-grep)
  ("C-c k" . counsel-ag))

(use-package easy-kill
  :ensure t
  :config
  (global-set-key [remap kill-ring-save] 'easy-kill))

(use-package fix-word
  :ensure t
  :bind
  ("M-u" . fix-word-upcase)
  ("M-l" . fix-word-downcase)
  ("M-c" . fix-word-capitalize))

(use-package evil :ensure t)

(use-package helm
  :ensure t
  :diminish helm-mode
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x C-r" . helm-recentf)
         ("C-x b" . helm-buffers-list))
  :config
  (helm-mode 1)
  (helm-autoresize-mode 1)
  ;;(add-to-list 'helm-completing-read-handlers-alist '(find-file . helm-completing-read-symbols))
  ;; helm-recentf-fuzzy-match var is broken: redeclare it manually
  ;; (setq helm-source-recentf
  ;;       (helm-make-source "Recentf" 'helm-recentf-source
  ;;         :fuzzy-match t))
  (setq helm-M-x-fuzzy-match t
        helm-M-x-always-save-history t
        helm-recentf-fuzzy-match t
        helm-buffers-fuzzy-matching t
        helm-display-header-line nil))

(use-package helm-ag
  :ensure t
  :bind ("s-F" . helm-do-ag-project-root)
  :config
  ;; (helm-ag-use-agignore t)
  )

(use-package helm-projectile
  :ensure t
  :init
  (helm-projectile-on)
  (bind-key "s-t" #'helm-projectile-find-file))

(use-package js2-mode
  :ensure t
  :mode
  "\\.js$"
  "\\.jsx$"
  :commands js2-mode
  :config
  (progn
    (setq-default
     js2-auto-indent-flag nil
     js2-basic-offset 4
     js2-electric-keys nil
     js2-mirror-mode nil
     js2-mode-show-parse-errors nil
     js2-mode-show-strict-warnings nil
     js2-strict-missing-semi-warning nil
     js2-strict-trailing-comma-warning nil
     js2-highlight-external-variables nil)
    ;; (add-hook 'js2-mode-hook 'prettier-js-mode)
    (add-hook 'js2-mode-hook 'prettier-js-save-hook)
    (add-hook 'js2-mode-hook
              (defun my-js2-mode-setup ()
                (flycheck-select-checker 'javascript-eslint)))
    (add-hook 'after-save-hook 'flow-save-hook)))

(defun prettier-js-save-hook ()
  "Invoke prettier-js hook if prettier executable is found."
  (when (executable-find "prettier")
    (prettier-js-mode)))

(defun flow-save-hook ()
  "Invoke flow-status after save when in js2-mode."
  (when (and (eq major-mode 'js2-mode)
             (executable-find "flow"))
    (flow-status)))

;; TODO: This is specifically for flow, but let's just modify the flow package instead of messing with the compilation buffer on a global level.
;;
;; (defun bury-compile-buffer-if-successful (buffer string)
;;   "Bury a compilation buffer if succeeded without warnings."
;;   (if (and
;;        (string-match "compilation" (buffer-name buffer))
;;        (string-match "No errors!" (with-current-buffer buffer
;;          (buffer-string)))
;;       )
;;       (run-with-timer 1 nil
;;                             (lambda (buf)
;;                               (bury-buffer buf)
;;                               (switch-to-prev-buffer (get-buffer-window buf) 'kill))
;;                             buffer)))
;; (add-hook 'compilation-finish-functions 'bury-compile-buffer-if-successful)



(use-package origami
  :ensure t
  :config
  (global-origami-mode t)
  :bind
  ("s-[" . origami-close-node-recursively)
  ("s-]" . origami-open-node-recursively)
  ("M-[" . origami-close-all-nodes)
  ("M-]" . origami-open-all-nodes))

(use-package sh-script
  :config
  (add-to-list 'auto-mode-alist '("\\.envrc\\'" . shell-script-mode)))

(use-package goto-chg
  :ensure t
  :bind
  ("C-c b ," . goto-last-change)
  ("C-c b ." . goto-last-change-reverse))

(use-package rainbow-mode
  :ensure t
  :config
  (add-hook 'css-mode-hook #'rainbow-mode))

(use-package highlight-tail
  :ensure t
  :config
  (progn
    (setq highlight-tail-steps 8)
    (setq highlight-tail-timer 0.05)))

(use-package deft
  :ensure t
  :config
  (progn
    (setq deft-directory "~/Dropbox/org")
    (setq deft-extension "org")
    (setq deft-text-mode 'org-mode)
    (setq deft-use-filename-as-title t)
    (setq deft-auto-save-interval 0)))

(use-package fireplace
  :ensure t)

(use-package popwin
  :ensure t
  :config
  (popwin-mode t))

(use-package gnus
  :config
  (setq gnus-select-method '(nntp "ger.gmane.org")))

(use-package smart-comment
  :ensure t
  :bind
  ("s-/" . smart-comment))

(use-package key-chord
  :ensure t
  :init
  (progn
    (key-chord-mode 1)
    (key-chord-define-global "hj" 'undo)
    (key-chord-define-global ",." "<>\C-b")
    (key-chord-define-global "--" 'my/insert-underscore)
    (key-chord-define-global "jj" 'avy-goto-word-1)
    (key-chord-define-global "jl" 'avy-goto-line)
    (key-chord-define-global "jk" 'avy-goto-char)
    (key-chord-define-global "uu" 'undo-tree-visualize)))

(use-package esup
  :ensure t)

(use-package volatile-highlights
  :ensure t
  :config
  (volatile-highlights-mode t))

(use-package blank-mode
  :ensure t)

;; (use-package all-the-icons
;;   :ensure t)

(use-package paperless
  :ensure t
  :config
  (progn
    (setq paperless-capture-directory "~/Documents/ScanSnap Inbox")
    (setq paperless-root-directory "~/Dropbox/Documents")))

(use-package parinfer
  :ensure t
  :bind
  (("C-," . parinfer-toggle-mode))
  :init
  (progn
    (setq parinfer-extensions
          '(defaults       ; should be included.
             pretty-parens  ; different paren styles for different modes.
             paredit        ; Introduce some paredit commands.
             smart-tab      ; C-b & C-f jump positions and smart shift with tab & S-tab.
             smart-yank))   ; Yank behavior depend on mode.
    (add-hook 'clojure-mode-hook #'parinfer-mode)
    (add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
    (add-hook 'common-lisp-mode-hook #'parinfer-mode)
    (add-hook 'scheme-mode-hook #'parinfer-mode)
    (add-hook 'lisp-mode-hook #'parinfer-mode)))

;; (use-package dash
;;   :ensure t)

(use-package add-node-modules-path
  :ensure t)

(eval-after-load 'js2-mode
  '(add-hook 'js2-mode-hook #'add-node-modules-path))

;; http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
(defun my/use-eslint-from-node-modules ()
  "Use local eslint from node_modeules."
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))


(load-file "~/.emacs.d/vendor/flow.el")
(load-file "~/.emacs.d/vendor/flycheck-flow.el")


(defun my/insert-underscore ()
  "Insert an underscore."
  (interactive)
  (insert "_"))

(defun yas/org-very-safe-expand ()
  (let ((yas/fallback-behavior 'return-nil)) (yas/expand)))

;; make zap-to-char act like zap-up-to-char
(defadvice zap-to-char (after my-zap-to-char-advice (arg char) activate)
  "Kill up to the ARG'th occurence of CHAR, and leave CHAR.
The CHAR is replaced and the point is put before CHAR."
  (insert char)
  (forward-char -1))

;; smarter navigation to the beginning of a line
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

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; Write temp files to directory to not clutter the filesystem
(defvar user-temporary-file-directory
  (concat temporary-file-directory user-login-name "/"))
(make-directory user-temporary-file-directory t)
(setq backup-by-copying t)
(setq backup-directory-alist
      `(("." . ,user-temporary-file-directory)
        (,tramp-file-name-regexp nil)))
(setq auto-save-list-file-prefix
      (concat user-temporary-file-directory ".auto-saves-"))
(setq auto-save-file-name-transforms
      `((".*" ,user-temporary-file-directory t)))

;; duplicate the current line function
(defun duplicate-line ()
  "Duplicate the current line."
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (forward-line 1)
  (yank))

;; use ido selection for recentf
(defun ido-choose-from-recentf ()
  "Use ido to select a recently visited file from the `recentf-list'."
  (interactive)
  (find-file (ido-completing-read "Open file: " recentf-list nil t)))

;; swaps windows
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

;; Convert word DOuble CApitals to Single Capitals
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

;; timestamps in *Messages*
;; via http://www.reddit.com/r/emacs/comments/1auqgm/speeding_up_your_emacs_startup/
;;     (defun current-time-microseconds ()
;;       (let* ((nowtime (current-time))
;;              (now-ms (nth 2 nowtime)))
;;         (concat (format-time-string "[%Y-%m-%dT%T" nowtime) (format ".%d] " now-ms))))
;;
;;     (defadvice message (before test-symbol activate)
;;       (if (not (string-equal (ad-get-arg 0) "%s%s"))
;;           (let ((inhibit-read-only t)
;;                 (deactivate-mark nil))
;;             (with-current-buffer "*Messages*"
;;               (goto-char (point-max))
;;               (if (not (bolp))
;;                   (newline))
;;               (insert (current-time-microseconds))))))

;; Copy the buffer filename to the kill ring
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

;; toggle between most recent buffers
(defun switch-to-previous-buffer ()
  "Switch to the most recent buffer.  Toggle back and forth between the two most recent buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

;; toggle window split
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

;; When popping the mark, continue popping until the cursor actually moves
;; Also, if the last command was a copy - skip past all the expand-region cruft.
(defadvice pop-to-mark-command (around ensure-new-position activate)
  (let ((p (point)))
    (when (eq last-command #'kill-ring-save)
      ad-do-it
      ad-do-it
      ad-do-it)
    (dotimes (i 10)
      (when (= p (point)) ad-do-it))))

(setq set-mark-command-repeat-pop t)

;; Sort directories first in dired-mode
(defun mydired-sort ()
  "Sort dired listings with directories first."
  (save-excursion
    (let (buffer-read-only)
      (forward-line 2) ;; beyond dir. header
      (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max)))
    (set-buffer-modified-p nil)))

(defadvice dired-readin
    (after dired-after-updating-hook first () activate)
  "Sort dired listings with directories first before adding marks."
  (mydired-sort))

;; Kill the current buffer
(defun kill-current-buffer ()
  "Kills the current buffer"
  (interactive)
  (kill-buffer (buffer-name)))

;; transpose the last two words when at end of line
(defadvice transpose-words
    (before my/transpose-words)
  "Transpose the last two words when at the end of line."
  (if (looking-at "$")
      (backward-word 1)))

;; Kill the minibuffer when you use the mouse in another window
;; http://trey-jackson.blogspot.com/2010/04/emacs-tip-36-abort-minibuffer-when.html
(defun stop-using-minibuffer ()
  "kill the minibuffer"
  (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
    (abort-recursive-edit)))

(add-hook 'mouse-leave-buffer-hook #'stop-using-minibuffer)

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line] #'smarter-move-beginning-of-line)

;; duplicate the current line
(global-set-key (kbd "C-c d") #'duplicate-line)

;; recentf with ido selection
;; bind to infrequently used find-file-read-only.
;; (global-set-key (kbd "C-x C-r") #'ido-choose-from-recentf)

;; switch to previous buffer
(global-set-key (kbd "C-`") #'switch-to-previous-buffer)

;; toggle window split
(global-set-key (kbd "C-x |") #'toggle-window-split)

;; sorting
(global-set-key (kbd "M-`") #'sort-lines)

;; font-size
(define-key global-map (kbd "s-=") #'text-scale-increase)
(define-key global-map (kbd "s--") #'text-scale-decrease)

;; scroll window up/down by one line
(global-set-key (kbd "M-n") (kbd "C-u 1 C-v"))
(global-set-key (kbd "M-p") (kbd "C-u 1 M-v"))

;; fullscreen toggle
(global-set-key [(s return)] #'toggle-frame-fullscreen)

;; fixup whitespace
(global-set-key (kbd "C-c w") #'fixup-whitespace)

;; kill the current buffer
(global-set-key (kbd "C-x C-k") #'kill-current-buffer)

;;----------------------------------------------------------------------------
;; Finalization
;;----------------------------------------------------------------------------

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
