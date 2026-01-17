;; init.el --- My personal Emacs configuration.
;;
;; Copyright (c) 2015 - 2026
;;
;; Author: Jonathan Chu <me@jonathanchu.is>
;; URL: https://github.com/jonathanchu/dotemacs
;; Version: 2.0

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
;; Bootstrapping
;;----------------------------------------------------------------------------

(defconst emacs-start-time (current-time))

(setq user-full-name "Jonathan Chu"
      user-mail-address "me@jonathanchu.is")

(defvar current-user
  (getenv
   (if (equal system-type 'windows-nt) "USERNAME" "USER")))

(message "Your Emacs is powering up... Be patient, Master %s!" current-user)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;;----------------------------------------------------------------------------
;; Packaging
;;----------------------------------------------------------------------------

(require 'package)
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

;; Bootstrap `use-package'
(setq-default use-package-verbose nil ; Don't report loading details
              use-package-expand-minimally t  ; make the expanded code as minimal as possible
              use-package-always-ensure t)
(eval-when-compile
  (require 'use-package))

;; for now
(setq package-check-signature 'allow-unsigned)
(setq package-unsigned-archives '("gnu" "nongnu"))

;;----------------------------------------------------------------------------
;; Core
;;----------------------------------------------------------------------------

;; native compile settings
(when (featurep 'native-compile)
  (setq native-comp-async-report-warnings-errors nil))

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

;; no menu bar
(menu-bar-mode -1)

;; no toolbar
(when (functionp 'tool-bar-mode)
  (tool-bar-mode -1))  ;; no toolbar

;; disable scroll bars
(if window-system
    (scroll-bar-mode -1))

(setq-default cursor-type 'bar)

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . light))

(eval-when-compile
  (require 'cl-lib))
(require 'bind-key)

;; Reduce the frequency of garbage collection by making it happen on
;; each 100MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb
;; reset GC threshold after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 16777216))) ; 16MB

;; turn on visual line mode
;; (global-visual-line-mode t)
(add-hook 'text-mode-hook #'visual-line-mode)
;; (add-hook 'org-mode-hook #'visual-line-mode)

;; set paths from shell
(use-package exec-path-from-shell
  :defer 1  ; load after 1 second of idle time
  :ensure t
  :if (memq window-system '(mac ns))
  :config
  (exec-path-from-shell-initialize))

;; Start up emacs server
(when (display-graphic-p)
  (require 'server)
  (message "Starting up server...")
  (unless (server-running-p)
    (server-start)))

(use-package recentf
  :ensure nil  ; builtin
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

(use-package saveplace
  :config
  (setq-default save-place t)
  (setq save-place-file "~/.emacs.d/saved-places"))

;; Automatically kill running processes on exit
(setq confirm-kill-processes nil)

;; require diminish mode for some packages
 (use-package diminish
    :ensure t
    :defer t)

;; Warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; M-q
(setq fill-column 80)

;; no word wrap
(setq-default truncate-lines t)

(setq-default line-spacing 4)

;; show line number in mode line
(line-number-mode 1)

;; show column number in the mode line
(column-number-mode 1)

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

;; allow windows to resize evenly when closed
(setq window-combination-resize t)

;;keep cursor at same position when scrolling
(setq scroll-preserve-screen-position t)

;; for smoother scrolling
(when (>= emacs-major-version 29)
  (pixel-scroll-precision-mode 1))

;; scroll one line at a time
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
(setq scroll-conservatively 10000)
(setq scroll-margin 3)

;; Scroll performance
(setq fast-but-imprecise-scrolling t)
(setq redisplay-skip-fontification-on-input t)
(setq inhibit-compacting-font-caches t)
(setq auto-window-vscroll nil)
(setq jit-lock-defer-time 0.05)

;; open with in original frame, not new window
(setq ns-pop-up-frames nil)

;; sentences end with single space
(setq sentence-end-double-space nil)

;; useful for camelCase
(add-hook 'prog-mode-hook #'subword-mode)

;; delete selection, insert text
(delete-selection-mode t)

;; consider all themes as safe
(setq custom-safe-themes t)

;; custom settings in a separate file and load the custom settings
(setq-default custom-file (expand-file-name
                             "custom.el"
                             user-emacs-directory))
(load custom-file :no-error-if-file-is-missing)

;; prevent active process query on quit
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent active process query on quit."
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
(setq bidi-inhibit-bpa t)

;; delete trailing whitespace in all modes
(add-hook 'before-save-hook #'delete-trailing-whitespace)

;; javascript
(setq js-indent-level 2)

;; css
(setq css-indent-offset 2)

;; only type 'y' or 'n' instead of 'yes' or 'no'
(setq use-short-answers t)

;; conservative indention for org src blocks
;; (setq org-src-preserve-indentation t)

;; protect against performance issues with minified files
(global-so-long-mode 1)

;; variable pitch mode
(add-hook 'text-mode-hook
          (lambda ()
            (variable-pitch-mode 1)))

;; Reduce the clutter in the fringes; we'd like to reserve that space for more
;; useful information, like git-gutter and flycheck.
(setq indicate-buffer-boundaries nil
      indicate-empty-lines nil)

;; Doesn't exist in terminal Emacs, so we define it to prevent void-function
;; errors emitted from packages use it without checking for it first.
(unless (fboundp 'define-fringe-bitmap)
  (fset 'define-fringe-bitmap #'ignore))

;;----------------------------------------------------------------------------
;; UI & Appearance
;;----------------------------------------------------------------------------

;; nice fonts in OS X
(setq mac-allow-anti-aliasing t)

(add-to-list 'default-frame-alist '(font . "JetBrains Mono-14"))

(use-package nerd-icons)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 30)
  (setq doom-modeline-indent-info t))

(use-package catppuccin-theme
  :ensure t
  :config
  ;; 'latte, 'macchiato, or 'mocha
  (setq catppuccin-flavor 'latte)
  (load-theme 'catppuccin t))

;;----------------------------------------------------------------------------
;; Editor
;;----------------------------------------------------------------------------

(use-package display-line-numbers
  :defer
  :ensure nil  ; builtin
  :custom
    (display-line-numbers-width-start t)
  :hook
    (prog-mode . display-line-numbers-mode)
    (tex-mode . display-line-numbers-mode)
    (markdown-mode . display-line-numbers-mode)
    (conf-mode . display-line-numbers-mode)
    (org-mode . display-line-numbers-mode))

(use-package paren
  :ensure nil  ; built-in
  :config
  (show-paren-mode t))

(use-package popwin
  :ensure t
  :config
  (popwin-mode t))

(use-package whitespace
  :ensure nil  ; builtin
  :config
  (setq whitespace-action '(auto-cleanup))
  (setq whitespace-style '(trailing space-before-tab indentation empty space-after-tab))
  :hook
  (prog-mode . whitespace-mode))

(use-package aggressive-indent
  :ensure t
  :hook (clojure-mode . aggressive-indent-mode))

(use-package ibuffer
  :ensure nil  ; builtin
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

(use-package beginend
  :ensure t
  :config
  (beginend-global-mode))

(use-package expand-region
  :ensure t
  :bind
  ("C-=" . er/expand-region))

(use-package fullframe
  :ensure t
  :config
  (fullframe magit-status magit-mode-quit-window)
  (fullframe ibuffer ibuffer-quit))

(use-package deadgrep
  :ensure t
  :bind
  ("s-F" . deadgrep))

(use-package multiple-cursors
  :ensure t
  :bind
  ("C-c m c" . mc/edit-lines))

;; narrow dired to match filter
(use-package dired-narrow
  :ensure t
  :bind
  (:map dired-mode-map
        ("/" . dired-narrow)))

(use-package origami
  :ensure t
  :hook
  (prog-mode . origami-mode)
  :bind
  ("s-[" . origami-close-node-recursively)
  ("s-]" . origami-open-node-recursively)
  ("M-[" . origami-close-all-nodes)
  ("M-]" . origami-open-all-nodes))

(use-package goto-chg
  :ensure t
  :bind
  ("C-c b ," . goto-last-change)
  ("C-c b ." . goto-last-change-reverse))

(use-package move-text
  :ensure t
  :bind
  (("M-p" . move-text-up)
   ("M-n" . move-text-down)))

(use-package hl-line
  :hook (prog-mode . hl-line-mode)
  :config
  ;; Doesn't seem to play nice in emacs 25+
  (setq hl-line-sticky-flag nil
        global-hl-line-sticky-flag nil))

(use-package volatile-highlights
  :ensure t
  :config
  (volatile-highlights-mode t))

(use-package easy-kill
  :ensure t
  :config
  (global-set-key [remap kill-ring-save] 'easy-kill))

(use-package smart-comment
  :ensure t
  :bind
  ("s-/" . smart-comment))

(use-package smartparens
  :ensure t
  :hook ((emacs-lisp-mode . smartparens-strict-mode)
         (org-mode . smartparens-mode))
  :config
  (require 'smartparens-config))

(use-package vertico
  :init
  (vertico-mode)
  :config
  (setq vertico-cycle t)  ; Cycle from bottom to top
  (setq vertico-count 20)) ; Number of candidates to display

;; Orderless - Flexible matching (replaces Ivy's fuzzy matching)
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;; Marginalia - Rich annotations in minibuffer
(use-package marginalia
  :init
  (marginalia-mode))

;; Consult - Enhanced commands (replaces Counsel)
(use-package consult
  :bind (("C-c k" . consult-ripgrep)  ; Replaces counsel-ag
         ("C-x C-r" . consult-recent-file)  ; Replaces counsel-recentf
         ("C-s" . consult-line)  ; Replaces counsel-grep-or-swiper
         ("C-r" . consult-line)  ; Replaces swiper
         ("C-c C-r" . consult-history)
         ("C-x b" . consult-buffer))
  :config
  (setq consult-narrow-key "<"))

;; Embark - Context actions on completion candidates
(use-package embark
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim)))

;; Embark-Consult integration
(use-package embark-consult
  :after (embark consult)
  :demand t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package projectile
  :defer t
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (setq projectile-completion-system 'default)  ; Uses vertico via completing-read
  :config
  (setq projectile-project-search-path '("~/projects/"))
  (setq projectile-switch-project-action #'projectile-dired)
  (projectile-mode +1))

;; Consult-Projectile (optional, for projectile integration)
(use-package consult-projectile
  :after (consult projectile)
  :bind (("s-t" . consult-projectile-find-file)))

;;----------------------------------------------------------------------------
;; Programming Modes
;;----------------------------------------------------------------------------

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

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
  (global-flycheck-mode 1))

(use-package flycheck-pos-tip
  :ensure t
  :after flycheck
  :config
  (setq flycheck-pos-tip-timeout 10)
  (setq flycheck-display-errors-delay 0.5)
  (flycheck-pos-tip-mode +1))

(use-package git-messenger
  :ensure t
  :defer t
  :bind
  ("C-x v m" . git-messenger:popup-message))

(use-package github-browse-file
  :ensure t
  :defer t)

(use-package magit
  :ensure t
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
  ;; Visibility
  (magit-section-initial-visibility-alist
   '((stashes . hide)
     (untracked . show)
     (unpulled . show)
     (unpushed . show)
     (recent . show)))
  ;; Performance
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
  :load-path "lisp/")

(defvar +vc-gutter-default-style t
  "If non-nil, enable the default look of the vc gutter.
This means subtle thin bitmaps on the left, an arrow bitmap for flycheck, and
flycheck indicators moved to the right fringe.")

;; subtle diff indicators in the fringe
(use-package git-gutter-fringe
  :ensure t
  :config
  (when +vc-gutter-default-style
    ;; standardize default fringe width
    (if (fboundp 'fringe-mode) (fringe-mode '4))

    ;; places the git gutter outside the margins.
    (setq-default fringes-outside-margins t
              highlight-nonselected-windows nil)
    ;; thin fringe bitmaps
    ;; thin green vertical line, 3px wide on left side
    (define-fringe-bitmap 'git-gutter-fr:added [224]
      nil nil '(center repeated))
    ;; thin red vertical line, 3px wide on left side
    (define-fringe-bitmap 'git-gutter-fr:modified [224]
      nil nil '(center repeated))
    ;; red triangular wedge pointing down
    (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240]
      nil nil 'bottom)))

(use-package git-gutter
  :ensure t
  :config
  (require 'git-gutter-fringe)
  (global-git-gutter-mode +1)
  ;; Just a reminder that this can cause a slowdown for big changesets
  :hook (focus-in . git-gutter:update-all-windows))

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

(use-package elpy
  :ensure t
  :defer t
  :init (advice-add 'python-mode :before 'elpy-enable)
  :config
  (setq elpy-rpc-python-command "python3"))

(use-package jinja2-mode)

(use-package toml-mode)

(use-package yaml-mode)

(use-package make-mode)

(use-package sh-script)

(use-package fish-mode
  :ensure t
  :defer t
  :config
  (add-hook 'fish-mode-hook (lambda ()
                              (add-hook 'before-save-hook 'fish_indent-before-save))))

(use-package go-mode)

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

(use-package markdown-mode)

(use-package json-mode)

;;----------------------------------------------------------------------------
;; Productivity
;;----------------------------------------------------------------------------

(use-package ox-hugo
  :ensure t
  :after ox)

(use-package denote
  :hook
  (dired-mode . denote-dired-mode)
  :bind
  (("C-c n n" . denote)
   ("C-c n r" . denote-rename-file)
   ("C-c n l" . denote-link)
   ("C-c n b" . denote-backlinks)
   ("C-c n d" . denote-dired)
   ("C-c n g" . denote-grep))
  :config
  (setq denote-directory (expand-file-name "~/Dropbox/Notes/"))

  ;; Automatically rename Denote buffers when opening them so that
  ;; instead of their long file name they have, for example, a literal
  ;; "[D]" followed by the file's title.  Read the doc string of
  ;; `denote-rename-buffer-format' for how to modify this.
  (denote-rename-buffer-mode 1)

  (setq denote-sort-keywords t))

(use-package consult-denote
  :ensure t
  :after (denote consult)
  :bind
  (("C-c n s" . consult-denote-find)
   ("C-c n g" . consult-denote-grep)))

;;----------------------------------------------------------------------------
;; Functions
;;----------------------------------------------------------------------------

(defun reload-config()
  "Reload Emacs configuration"
  (interactive)
  (load-file "~/.emacs.d/init.el"))

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

;; Kill the minibuffer when you use the mouse in another window
;; http://trey-jackson.blogspot.com/2010/04/emacs-tip-36-abort-minibuffer-when.html
(defun stop-using-minibuffer ()
  "kill the minibuffer"
  (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
    (abort-recursive-edit)))
(add-hook 'mouse-leave-buffer-hook #'stop-using-minibuffer)

(defun my-org-config/after-org-archive ()
  (org-save-all-org-buffers))
(add-hook 'org-archive-hook 'my-org-config/after-org-archive)

;;----------------------------------------------------------------------------
;; Key Bindings
;;----------------------------------------------------------------------------

;; Mac-specific
(setq-default mac-option-modifier 'meta)
(setq-default mac-command-modifier 'super)

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line] #'smarter-move-beginning-of-line)

;; duplicate the current line
(global-set-key (kbd "C-c d") #'duplicate-line)

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
