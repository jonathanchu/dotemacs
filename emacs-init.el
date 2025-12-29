(defun reload-config()
  "Reload Emacs configuration"
  (interactive)
  (load-file "~/.emacs.d/init.el"))

(use-package cus-edit
  :defer t
  :config
  (setq custom-file "~/.emacs.d/custom.el")

  (unless (file-exists-p custom-file)
    (write-region "" nil custom-file))

  (load custom-file))

(defconst emacs-start-time (current-time))

(setq user-full-name "Jonathan Chu"
      user-mail-address "me@jonathanchu.is")

(add-to-list 'exec-path "/usr/local/bin")

(defvar current-user
  (getenv
   (if (equal system-type 'windows-nt) "USERNAME" "USER")))

(message "Your Emacs is powering up... Be patient, Master %s!" current-user)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(defvar gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(require 'package)
(setq load-prefer-newer t
      package-enable-at-startup nil
      package-user-dir (concat user-emacs-directory "elpa")
      package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Bootstrap `use-package'
(setq-default use-package-verbose nil ; Don't report loading details
              use-package-expand-minimally t  ; make the expanded code as minimal as possible
              use-package-enable-imenu-support t) ; Let imenu finds use-package definitions
(eval-when-compile
  (require 'use-package))

(require 'bind-key)

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
;; each 100MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; turn on visual line mode
(global-visual-line-mode t)

;; set paths from shell
(use-package exec-path-from-shell
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

(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns))
  :config
  (exec-path-from-shell-initialize))

(use-package ido
  :config
  (progn
    (ido-mode t)
    ;; (flx-ido-mode t)
    (setq ido-enable-flex-matching t)
    (setq ido-use-faces nil)))

(use-package ido-vertical-mode
  :ensure t
  :config
  (progn
    (ido-vertical-mode 1)
    (setq ido-vertical-define-keys #'C-n-and-C-p-only)))

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

(use-package saveplace
  :config
  (progn
    (setq-default save-place t)
    (setq save-place-file "~/.emacs.d/saved-places")))

;; Automatically kill running processes on exit
(setq confirm-kill-processes nil)

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
(require 'cl-lib)
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent active process query on quit."
  (cl-flet ((process-list ())) ad-do-it))

;; instantly display current key sequence in mini buffer
(setq echo-keystrokes 0.02)

;; desktop save mode
(desktop-save-mode t)
(defvar desktop-restore-eager 5)
(defvar desktop-save t)

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

;; javascript
(defvar js-indent-level 2)

;; css
(defvar css-indent-offset 2)

;; cua mode
;; (cua-mode t)
;; (setq cua-enable-cua-keys nil)

;; variable pitch mode
(add-hook 'text-mode-hook
          (lambda ()
            (variable-pitch-mode 1)))

(use-package aggressive-indent
  :ensure t
  :init
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
  (add-hook 'clojure-mode-hook #'aggressive-indent-mode))

(use-package anzu
  :ensure t
  :config
  (progn
    (global-anzu-mode t)
    (set-face-attribute 'anzu-mode-line nil :foreground "yellow" :weight 'bold))
  :bind
  ("M-%" . anzu-query-replace)
  ("C-M-%" . anzu-query-replace-regexp))

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

;; TODO add back in highlight-tail?

(use-package beginend
  :ensure t
  :config
  (beginend-global-mode))

(use-package expand-region
  :ensure t
  :bind
  ("C-=" . er/expand-region))

(use-package flyspell
  :config
  (add-hook 'text-mode-hook #'flyspell-mode))

(use-package fullframe
  :ensure t
  :config
  (progn
    (fullframe magit-status magit-mode-quit-window)
    (fullframe ibuffer ibuffer-quit)
    (fullframe paradox-list-packages paradox-quit-and-close)))

(use-package goto-chg
  :ensure t
  :bind
  ("C-c b ," . goto-last-change)
  ("C-c b ." . goto-last-change-reverse))

(use-package key-chord
  :ensure t
  :init
  (progn
    (key-chord-mode 1)
    (key-chord-define-global "hj" 'undo)
    (key-chord-define-global ",." "<>\C-b")
    (key-chord-define-global "jj" 'avy-goto-word-1)
    (key-chord-define-global "jl" 'avy-goto-line)
    (key-chord-define-global "jk" 'avy-goto-char)
    ))

(use-package move-text
  :ensure t
  :bind
  (("M-p" . move-text-up)
   ("M-n" . move-text-down))
  )

(use-package multiple-cursors
  :ensure t
  :bind
  ("C-c m c" . mc/edit-lines))

(use-package neotree
  :ensure t
  :config
  (setq neo-window-fixed-size nil
        neo-create-file-auto-open t
        neo-banner-message nil
        neo-mode-line-type 'none
        neo-smart-open t
        neo-show-hidden-files t
        neo-auto-indent-point t)
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  :bind
  ("C-c n" . neotree-toggle)
  )

(use-package olivetti
  :ensure t)

(use-package origami
  :ensure t
  :config
  (global-origami-mode t)
  :bind
  ("s-[" . origami-close-node-recursively)
  ("s-]" . origami-open-node-recursively)
  ("M-[" . origami-close-all-nodes)
  ("M-]" . origami-open-all-nodes))

(use-package paren
  :config
  (show-paren-mode t))

(use-package popwin
  :ensure t
  :config
  (popwin-mode t))

(use-package undo-tree
  :ensure t
  :config
  (progn
    (global-undo-tree-mode t)
    (setq undo-tree-visualizer-diff t)
    (setq undo-tree-visualizer-timestamps t)))

(use-package uniquify
  :config
  (progn
    (setq uniquify-buffer-name-style 'reverse)
    (setq uniquify-separator " • ")
    (setq uniquify-after-kill-buffer-p t)
    (setq uniquify-ignore-buffers-re "^\\*")))

(use-package whitespace
  :config
  (progn
    (global-whitespace-mode t)
    (setq whitespace-action '(auto-cleanup))
    (setq whitespace-style '(trailing space-before-tab indentation empty space-after-tab))))

(use-package vi-tilde-fringe
  :disabled
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'vi-tilde-fringe-mode))

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . light))

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
;; (global-hl-line-mode +1)

;; (add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono for Powerline-13"))
;; (add-to-list 'default-frame-alist '(font . "Fira Mono for Powerline-13"))
;; (add-to-list 'default-frame-alist '(font . "Fira Code-13"))
;; (add-to-list 'default-frame-alist '(font . "Fira Mono-13"))
;; (add-to-list 'default-frame-alist '(font . "Operator Mono-14"))
(add-to-list 'default-frame-alist '(font . "IBM Plex Mono-14"))

;; Reduce the clutter in the fringes; we'd like to reserve that space for more
;; useful information, like git-gutter and flycheck.
(setq indicate-buffer-boundaries nil
      indicate-empty-lines nil)

;; Doesn't exist in terminal Emacs, so we define it to prevent void-function
;; errors emitted from packages use it without checking for it first.
(unless (fboundp 'define-fringe-bitmap)
  (fset 'define-fringe-bitmap #'ignore))

(use-package hl-line
  :init (add-hook 'prog-mode-hook 'hl-line-mode)
  :config
  ;; Doesn't seem to play nice in emacs 25+
  (setq hl-line-sticky-flag nil
        global-hl-line-sticky-flag nil))

(use-package ace-window
  :ensure t
  :init
  (setq aw-keys '(?a ?s ?d ?f ?j ?k ?l))
  :bind
  ("C-x C-o" . ace-window))

(use-package lsp-mode
  :commands lsp-mode
  :ensure t
  :hook (prog-mode . (lambda ()
                       (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode)
                         (lsp-deferred))))
  :bind (:map lsp-mode-map
              ("C-c C-d" . lsp-describe-thing-at-point))
  :init
  (setq lsp-auto-guess-root t)  ;; Detect project root
  (setq lsp-keep-workspace-alive nil)  ;; Auto-kill LSP server
  (setq lsp-prefer-flymake nil)  ;; Use lsp-ui and flycheck
  (setq flymake-fringe-indicator-position 'right-fringe)
  (setq lsp-print-performance t)
  :config
    )

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :init
  ;; (add-hook 'lsp-mode-hook 'flycheck-mode)
  (setq lsp-ui-doc-enable nil
        lsp-ui-doc-use-webkit nil
        lsp-ui-doc-delay 0.2
        lsp-ui-doc-include-signature t
        lsp-ui-doc-position 'at-point
        lsp-ui-doc-border (face-foreground 'default)
        lsp-eldoc-enable-hover nil ; Disable eldoc displays in minibuffer

        lsp-ui-imenu-enable t
        lsp-ui-imenu-colors `(,(face-foreground 'font-lock-keyword-face)
                              ,(face-foreground 'font-lock-string-face)
                              ,(face-foreground 'font-lock-constant-face)
                              ,(face-foreground 'font-lock-variable-name-face))

        lsp-ui-sideline-enable t
        lsp-ui-sideline-show-hover nil
        lsp-ui-sideline-show-diagnostics nil
        lsp-ui-sideline-ignore-duplicate t)
  :config
  (add-to-list 'lsp-ui-doc-frame-parameters '(right-fringe . 8))

  ;; `C-g'to close doc
  (advice-add #'keyboard-quit :before #'lsp-ui-doc-hide)

  ;; Reset `lsp-ui-doc-background' after loading theme
  (add-hook 'after-load-theme-hook
            (lambda ()
              (setq lsp-ui-doc-border (face-foreground 'default))
              (set-face-background 'lsp-ui-doc-background
                                   (face-background 'tooltip))))

  ;; WORKAROUND Hide mode-line of the lsp-ui-imenu buffer
  ;; @see https://github.com/emacs-lsp/lsp-ui/issues/243
  (defun my-lsp-ui-imenu-hide-mode-line ()
    "Hide the mode-line in lsp-ui-imenu."
    (setq mode-line-format nil))
  (advice-add #'lsp-ui-imenu :after #'my-lsp-ui-imenu-hide-mode-line)
  )

;; Microsoft python-language-server support
(use-package lsp-python-ms
  :ensure t
  :hook (python-mode . (lambda () (require 'lsp-python-ms)))
  :init
  (when (executable-find "python3")
    (setq lsp-python-ms-python-executable-cmd "python3")))

(use-package company
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'company-mode))

(use-package counsel
  ;; :disabled
  :ensure t
  :bind (
         ;; ("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ;; ("C-c g" . counsel-git-grep)
         ("C-c k" . counsel-ag)
         ("C-x C-r" . counsel-recentf)))

(use-package counsel-projectile
  ;; :disabled
  :ensure t
  :init
  ;; (bind-key "s-F" #'counsel-projectile-ag)
  (bind-key "s-t" #'counsel-projectile-find-file)
  ;; (bind-key "C-x b" #'counsel-projectile-switch-to-buffer)
  :config
  (counsel-projectile-mode 1))

(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (progn
    (setq ivy-use-virtual-buffers t)
    (setq ivy-count-format "(%d/%d)")
    (setq enable-recursive-minibuffers t)
    (setq ivy-initial-inputs-alist nil)
    (setq ivy-format-function #'ivy-format-function-arrow)
    (setq ivy-re-builders-alist
          '((swiper . ivy--regex-plus)
            (t      . ivy--regex-fuzzy)))  ;; enable fuzzy search everywhere except for Swiper
    )
  :bind
  ("C-c C-r" . ivy-resume))

(use-package swiper
  :ensure t
  :bind
  ("C-s" . counsel-grep-or-swiper)
  ("C-r" . swiper)
  :config
  ;; (advice-add 'swiper :after 'recenter)
  )
