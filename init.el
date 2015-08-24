;; init.el --- My personal Emacs configuration.
;;
;; Copyright (c) 2015
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
(setq user-full-name "Jonathan Chu"
      user-mail-address "me@jonathanchu.is")

(add-to-list 'load-path "~/.emacs.d/vendor")
(add-to-list 'exec-path "/usr/local/bin")

(defvar current-user
  (getenv
   (if (equal system-type 'windows-nt) "USERNAME" "USER")))

(message "Your Emacs is powering up... Be patient, Master %s!" current-user)

;;----------------------------------------------------------------------------
;; Packages
;;----------------------------------------------------------------------------

(require 'package)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(
                      ace-window  ;; done
                      ag  ;; done
                      aggressive-indent  ;; done
                      anaconda-mode  ;; done
                      anzu  ;; done
                      atom-one-dark-theme  ;; done
                      cider  ;; done
                      clj-refactor  ;; done
                      command-log-mode  ;; done
                      company  ;; done
                      company-anaconda  ;; done
                      counsel  ;; done
                      deft  ;; done
                      dired+  ;; done
                      dired-single  ;; done
                      easy-kill  ;; done
                      exec-path-from-shell  ;; done
                      expand-region  ;; done
                      flx-ido  ;; done
                      flycheck  ;; done
                      fullframe  ;; done
                      gitconfig-mode  ;; done
                      github-browse-file  ;; done
                      gitignore-mode  ;; done
                      git-timemachine  ;; done
                      grizzl  ;; done
                      guru-mode  ;; done
                      ido-ubiquitous  ;; done
                      ido-vertical-mode  ;; done
                      latex-preview-pane  ;; done
                      magit  ;; done
                      markdown-mode  ;; done
                      nav  ;; done
                      paradox  ;; done
                      paredit  ;; done
                      projectile  ;; done
                      py-isort  ;; done
                      python-mode  ;; done
                      rainbow-delimiters  ;; done
                      restclient  ;; done
                      scratch  ;; done
                      smart-mode-line  ;; done
                      smartparens
                      smex  ;; done
                      swiper  ;; done
                      textmate  ;; done
                      undo-tree  ;; done
                      use-package
                      web-mode  ;; done
                      yasnippet
                      )
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; use-package
(require 'use-package)

;; set paths from shell
(use-package exec-path-from-shell
  :ensure
  :if (memq window-system '(mac ns))
  :config
  (exec-path-from-shell-initialize))

;; load in custom-set-variables early. FIXME
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "c5a044ba03d43a725bd79700087dea813abcb6beb6be08c7eb3303ed90782482" "6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" default)))
 '(magit-branch-arguments nil)
 '(paradox-github-token t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;----------------------------------------------------------------------------
;; Global Config
;;----------------------------------------------------------------------------

;; only type 'y' or 'n' instead of 'yes' or 'no'
(fset 'yes-or-no-p 'y-or-n-p)

;; no splash screen
(setq inhibit-splash-screen t)

;; no message on startup
(setq initial-scratch-message nil)

;; no menu bar
(menu-bar-mode -1)

;; start position and frame size
(add-to-list 'default-frame-alist '(left . 0))
(add-to-list 'default-frame-alist '(top . 0))
(add-to-list 'default-frame-alist '(height . 43))
(add-to-list 'default-frame-alist '(width . 150))

;; M-q
(setq fill-column 80)

;; no toolbar
(when (functionp 'tool-bar-mode)
  (tool-bar-mode -1))  ;; no toolbar

;; disable scroll bars
(if window-system
    (progn
      (scroll-bar-mode -1)
      (set-frame-font "Inconsolata 15"))) ;; set font

;; make the font more visually pleasing
(set-face-attribute 'default nil :height 160)

;; nice fonts in OS X
(setq mac-allow-anti-aliasing t)

;; no word wrap
(setq-default truncate-lines 1)

(setq-default line-spacing 4)

;; no tabs
(setq-default indent-tabs-mode nil)

;; delete trailing whitespace in all modes
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; show line number in mode line
(line-number-mode 1)

;; show column number in the mode line
(column-number-mode 1)

;; show extra whitespace
(setq show-trailing-whitespace t)

;; ensure last line is a return
(setq require-final-newline t)

;; set encoding
(prefer-coding-system 'utf-8)

;; and tell emacs to play nice with encoding
(define-coding-system-alias 'UTF-8 'utf-8)
(define-coding-system-alias 'utf8 'utf-8)

;; cursor
(setq-default cursor-type 'bar)

;; highlight brackets
(require 'paren)
(show-paren-mode 1)

;; make sure looking at most recent changes
(global-auto-revert-mode 1)

;; whitespace cleanup
(global-whitespace-mode 1)
(setq whitespace-action '(auto-cleanup)) ;; automatically clean up bad whitespace
(setq whitespace-style '(trailing space-before-tab indentation empty space-after-tab)) ;; only show bad whitespace

(setq window-combination-resize t)

;;keep cursor at same position when scrolling
(setq scroll-preserve-screen-position 1)

;; scroll one line at a time
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
(setq scroll-conservatively 10000)

;; save place
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file "~/.emacs.d/saved-places")

;; color theme
(use-package atom-one-dark-theme
  :ensure
  :config
  (load-theme 'atom-one-dark t))

;; open with in original frame, not new window
(setq ns-pop-up-frames nil)

;; sentences end with single space
(setq sentence-end-double-space nil)

;; useful for camelCase
(subword-mode 1)

;; delete selection, insert text
(delete-selection-mode 1)

;; prevent active process query on quit
(require 'cl)
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  (flet ((process-list ())) ad-do-it))


;;----------------------------------------------------------------------------
;; Modes
;;----------------------------------------------------------------------------

;; command-log-mode
(use-package command-log-mode
  :ensure)

;; gitconfig-mode
(use-package gitconfig-mode
  :ensure)

;; gitignore-mode
(use-package gitignore-mode
  :ensure)

;; git-timemachine
(use-package git-timemachine
  :ensure)

;; github-browse-fle
(use-package github-browse-file
  :ensure)

;; restclient
(use-package restclient
  :ensure)

;; flycheck
(use-package flycheck
  :ensure
  :defer 2
  :config
  (global-flycheck-mode 1))

;; uniquify
(use-package uniquify
  :config
  (progn
    (setq uniquify-buffer-name-style 'reverse)
    (setq uniquify-separator " • ")
    (setq uniquify-after-kill-buffer-p t)
    (setq uniquify-ignore-buffers-re "^\\*"))
  )

;; flx-ido
(use-package flx-ido
  :ensure)

;; ido-mode
(ido-mode t)
(ido-everywhere t)
(setq ido-use-faces nil)

;; ido-vertical-mode
(use-package ido-vertical-mode
  :ensure
  :config
  (ido-vertical-mode 1)
  (setq ido-vertical-define-keys 'C-n-and-C-p-only))

;; ido-ubiquitous
(use-package ido-ubiquitous
  :ensure
  :config
  (ido-ubiquitous-mode 1))

;; python
(use-package python-mode
  :ensure
  :config
  (add-hook 'python-mode-hook
            '(lambda ()
               (setq fill-column 80)))
  (add-to-list 'auto-mode-alist '("\\.py" . python-mode)))

;; anaconda-mode
(use-package anaconda-mode
  :ensure
  :config
  (add-hook 'python-mode-hook
            '(lambda ()
               (anaconda-mode 1)
               (eldoc-mode 1))))

;; javascript
(setq js-indent-level 2)

;; web-mode
(use-package web-mode
  :ensure
  :config
  (add-to-list 'auto-mode-alist '("\\.hb\\.html\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode)))

;; everything is indented 2 spaces
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)

;; makefile-mode
(add-to-list 'auto-mode-alist '("\\Makefile\\'" . makefile-mode))

;; css
(add-to-list 'auto-mode-alist '("\\.less\\'" . css-mode))
(setq css-indent-offset 2)

;; textmate-mode
(use-package textmate
  :ensure
  :config
  (textmate-mode 1))

;; magit
(use-package magit
  :ensure
  :config
  (progn
    (setq magit-push-always-verify nil)
    (setq magit-completing-read-function 'ivy-completing-read)
    (setq magit-last-seen-setup-instructions "1.4.0"))
  :bind
  ("C-x g" . magit-status)
  ("C-c C-a" . magit-commit-amend))

;; deft
(use-package deft
  :ensure
  :config
  (setq deft-directory "~/Dropbox/Simplenote")
  (setq deft-extension "txt")
  (setq deft-text-mode 'org-mode)
  (setq deft-use-filename-as-title t)
  (setq deft-auto-save-interval 0))

;; nav-mode
(use-package nav
  :ensure
  :config
  (nav-disable-overeager-window-splitting))

;; smex
(use-package smex
  :ensure
  :config
  (smex-initialize)
  :bind
  ("M-x" . smex)
  ("M-x" . smex-major-mode-commands)
  ("C-c C-c M-x" . execute-extended-command))

;; server mdoe
(if (not server-mode)
    (server-start nil t))

;; yasnippet
(require 'yasnippet)
(yas-global-mode 1)
(setq yas-snippet-dirs (append yas-snippet-dirs
                               '("~/.emacs.d/snippets")))

;; smart-mode-line
(use-package smart-mode-line
  :ensure
  :config
  (setq sml/no-confirm-load-theme t)
  (setq sml/theme 'respectful)
  (add-hook 'after-init-hook 'sml/setup))

;; ag
(use-package ag
  :ensure
  :config
  (progn
    (setq ag-highlight-search t
          ag-reuse-buffers t)))

;; grizzl
(use-package grizzl
  :ensure)

;; projectile
(use-package projectile
  :ensure
  :diminish ""
  :config
  (projectile-global-mode 1)
  (setq projectile-completion-system 'grizzl))

;; py-isort
;; (use-package py-isort
;;   :ensure
;;   :config
;;   (add-hook 'before-save-hook 'py-isort-before-save))

;; org-mode
(setq org-directory "~/Dropbox/org")
(setq org-log-done t)
(setq org-todo-keywords
      '((sequence "TODO(t)" "|" "DONE(d)")
        (sequence "WAITING(w)" "|" "CANCELED(c)")))
(setq org-completion-use-ido t)

;; linum mode
(require 'linum)
(global-linum-mode 1)
(setq linum-format
      (lambda (line) (propertize
                      (format (let ((w (length (number-to-string (count-lines (point-min) (point-max))))))
                                (concat " %" (number-to-string w) "d ")) line) 'face 'linum)))

;; dired+ mode
(use-package dired+
  :ensure)

;; dired-single
(use-package dired-single
  :ensure)

;; undo-tree mode
(use-package undo-tree
  :ensure
  :config
  (global-undo-tree-mode 1)
  (setq undo-tree-visualizer-diff t
        undo-tree-visualizer-timestamps t))

;; fullframe
(use-package fullframe
  :ensure
  :config
  (fullframe magit-status magit-mode-quit-window))

;; recentf
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
                              "/\\.git/.*\\'"))
  (recentf-mode))

;; cider
(use-package cider
  :ensure
  :config
  (setq nrepl-log-messages t)
  (setq nrepl-hide-special-buffers t)
  (add-hook 'cider-mode-hook 'eldoc-mode))

;; expand-region
(use-package expand-region
  :ensure
  :bind
  ("C-=" . er/expand-region))

;; markdown-mode
(use-package markdown-mode
  :ensure
  :mode "\\.md\\'")

;; rainbow delimiters
(use-package rainbow-delimiters
  :ensure
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; smartparens mode
(smartparens-global-mode t)
(sp-local-pair 'web-mode "{%" "%}")
(require 'smartparens-config)
(setq sp-autoskip-closing-pair 'always
      ;; Don't kill the entire symbol on C-k
      sp-hybrid-kill-entire-symbol nil)

;; clojure-mode
(require 'clojure-mode)
(define-clojure-indent
  (defroutes 'defun)
  (GET 2)
  (POST 2)
  (PUT 2)
  (DELETE 2)
  (HEAD 2)
  (ANY 2)
  (context 2))

;; paredit-mode
(use-package paredit
  :ensure
  :config
  (autoload 'enable-paredit-mode "paredit" t)
  (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook 'enable-paredit-mode)
  (add-hook 'ielm-mode-hook 'enable-paredit-mode)
  (add-hook 'lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
  (add-hook 'scheme-mode-hook 'enable-paredit-mode)
  (add-hook 'clojure-mode-hook 'enable-paredit-mode)
  (add-hook 'python-mode-hook
            (lambda () (local-set-key (kbd "C-k") 'paredit-kill))))

;; latex-preview-pane
(use-package latex-preview-pane
  :ensure
  :config
  (latex-preview-pane-enable))

;; swiper
(use-package swiper
  :init
  (ivy-mode 1)
  :ensure
  :bind
  ("C-s" . swiper)
  ("C-r" . swiper)
  ("C-c C-r" . ivy-resume)
  :config
  ;; (ivy-mode 1)
  (setq ivy-use-virtual-buffers t))

;; ace-window
(use-package ace-window
  :ensure
  :init
  (setq aw-keys '(?a ?s ?d ?f ?j ?k ?l))
  :bind
  (("C-x C-o" . ace-window)))

;; avy
(use-package avy
  :ensure
  :init
  (setq avy-keys '(?a ?s ?d ?e ?f ?h ?j ?k ?l ?n ?m ?v ?r ?u))
  :config
  (avy-setup-default)
  (setq avy-background t)
  (setq avy-styles-alist '((avy-goto-word-or-subword-1 . de-brujin)))
  :bind
  ("C-c j" . avy-goto-word-or-subword-1))

;; multiple scratch buffers
(use-package scratch
  :ensure
  :config
  (autoload 'scratch "scratch" nil t))

;; flyspell
(add-hook 'text-mode-hook 'flyspell-mode)

;; anzu
(use-package anzu
  :ensure
  :config
  (global-anzu-mode 1)
  (set-face-attribute 'anzu-mode-line nil :foreground "yellow" :weight 'bold)
  :bind
  (("M-%" . anzu-query-replace)
   ("C-M-%" . anzu-query-replace-regexp)))

;; org bullets (custom)
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; clj-refactor
(use-package clj-refactor
  :ensure
  :config
  (defun my-clojure-mode-hook ()
    (clj-refactor-mode 1)
    (yas-minor-mode 1) ; for adding require/use/import
    (cljr-add-keybindings-with-prefix "C-c C-m"))
  (add-hook 'clojure-mode-hook 'my-clojure-mode-hook))

;; company-mode and backends
(use-package company
  :ensure
  :config
  (global-company-mode)
  (eval-after-load 'company
    (progn
      '(add-to-list 'company-backends 'company-anaconda)
      )))

;; aggressive-indent
(use-package aggressive-indent
  :ensure
  :init
  (add-hook 'emacs-lisp-mode-hook 'aggressive-indent-mode)
  (add-hook 'clojure-mode-hook 'aggressive-indent-mode))

;; guru-mode
(use-package guru-mode
  :ensure
  :config
  (guru-global-mode 1))

;; paradox
(use-package paradox
  :ensure
  :config
  (setq paradox-execute-asynchronously t))

;; counsel
(use-package counsel
  :ensure
  :bind
  ("C-c g" . counsel-git-grep))

;; easy-kill
(use-package easy-kill
  :ensure
  :config
  (global-set-key [remap kill-ring-save] 'easy-kill))


;;----------------------------------------------------------------------------
;; Defuns
;;----------------------------------------------------------------------------

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

;; Highlight the call to ipdb
;; src http://pedrokroger.com/2010/07/configuring-emacs-as-a-python-ide-2/
(defun annotate-pdb ()
  (interactive)
  (highlight-lines-matching-regexp "import ipdb")
  (highlight-lines-matching-regexp "pdb.set_trace()"))
(add-hook 'python-mode-hook 'annotate-pdb)

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
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank))

;; use ido selection for recentf
(defun ido-choose-from-recentf ()
  "Use ido to select a recently visited file from the `recentf-list'"
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
         (and (if (called-interactively-p)
                  (skip-syntax-backward "w")
                (= -3 (skip-syntax-backward "w")))
              (let (case-fold-search)
                (looking-at "\\b[[:upper:]]\\{2\\}[[:lower:]]"))
              (capitalize-word 1)))))

(add-hook 'post-self-insert-hook 'dcaps-to-scaps)

;; timestamps in *Messages*
;; via http://www.reddit.com/r/emacs/comments/1auqgm/speeding_up_your_emacs_startup/
(defun current-time-microseconds ()
  (let* ((nowtime (current-time))
         (now-ms (nth 2 nowtime)))
    (concat (format-time-string "[%Y-%m-%dT%T" nowtime) (format ".%d] " now-ms))))

(defadvice message (before test-symbol activate)
  (if (not (string-equal (ad-get-arg 0) "%s%s"))
      (let ((inhibit-read-only t)
            (deactivate-mark nil))
        (with-current-buffer "*Messages*"
          (goto-char (point-max))
          (if (not (bolp))
              (newline))
          (insert (current-time-microseconds))))))


;;----------------------------------------------------------------------------
;; Key Bindings
;;----------------------------------------------------------------------------

;; duplicate the current line
(global-set-key [C-return] 'duplicate-line)

;; sorting
(global-set-key (kbd "M-`") 'sort-lines)

;; font-size
(define-key global-map (kbd "s-=") 'text-scale-increase)
(define-key global-map (kbd "s--") 'text-scale-decrease)

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line] 'smarter-move-beginning-of-line)

;; recentf with ido selection
;; bind to infrequently used find-file-read-only.
(global-set-key (kbd "C-x C-r") 'ido-choose-from-recentf)

;;scroll window up/down by one line
(global-set-key (kbd "M-n") (kbd "C-u 1 C-v"))
(global-set-key (kbd "M-p") (kbd "C-u 1 M-v"))


;;; init.el ends here
