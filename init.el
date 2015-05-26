;; init.el --- My personal Emacs configuration.
;;
;; Copyright (c) 2015
;;
;; Author: Jonathan Chu <jonathan.chu@me.com>
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

(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'exec-path "/usr/local/bin")

(defvar current-user
      (getenv
       (if (equal system-type 'windows-nt) "USERNAME" "USER")))

(message "Your Emacs is powering up... Be patient, Master %s!" current-user)

;;----------------------------------------------------------------------------
;; Packages
;;----------------------------------------------------------------------------

(require 'package)

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(
                      ag
                      atom-dark-theme
                      auto-complete
                      dedicated
                      deft
                      dired+
                      dired-single
                      exec-path-from-shell
                      flx-ido
                      flycheck
                      fullframe
                      github-browse-file
                      git-timemachine
                      grizzl
                      helm
                      helm-projectile
                      idle-highlight-mode
                      ido-ubiquitous
                      ido-vertical-mode
                      magit
                      nav
                      paredit
                      projectile
                      py-isort
                      pymacs
                      python-mode
                      rainbow-delimiters
                      scratch
                      smart-mode-line
                      smex
                      textmate
                      undo-tree
                      web-mode
                      yasnippet
                      )
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; set paths from shell
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; load in custom-set-variables early. FIXME
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "c5a044ba03d43a725bd79700087dea813abcb6beb6be08c7eb3303ed90782482" "6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" default))))
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
(blink-cursor-mode 1)

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

;; color theme atom dark
(load-theme 'atom-dark t)

;; open with in original frame, not new window
(setq ns-pop-up-frames nil)

;; sentences end with single space
(setq sentence-end-double-space nil)

;; useful for camelCase
(subword-mode 1)


;;----------------------------------------------------------------------------
;; Modes
;;----------------------------------------------------------------------------

;; flycheck
(require 'flycheck)

;; uniquify
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator " • ")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

;; ido-mode
(ido-mode t)
(ido-everywhere t)
(setq ido-use-faces nil)

;; ido-vertical-mode
(require 'ido-vertical-mode)
(ido-vertical-mode 1)

;; ido-ubiquitous
(ido-ubiquitous-mode 1)

;; python
(require 'python-mode)

(add-hook 'python-mode-hook (lambda ()
                            (flycheck-mode 1)
                            (setq fill-column 80)))

(add-to-list 'auto-mode-alist '("\\.py" . python-mode))

;; javascript
(add-hook 'js-mode-hook
          (lambda () (flycheck-mode t)))
(setq js-indent-level 2)

;; web mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.hb\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))

;; everything is indented 2 spaces
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)

;; makefile-mode
(add-to-list 'auto-mode-alist '("\\Makefile\\'" . makefile-mode))

;; css
(add-to-list 'auto-mode-alist '("\\.less\\'" . css-mode))
(setq css-indent-offset 2)

(require 'dedicated) ;; sticky windows

(textmate-mode)

;; magit
(global-set-key "\C-xg" 'magit-status)

(eval-after-load 'magit
  '(progn
     (set-face-foreground 'magit-diff-add "green3")
     (set-face-foreground 'magit-diff-del "red3")))

;; deft
(require 'deft)
(setq deft-extension "txt")
(setq deft-directory "~/Dropbox/Simplenote")
(setq deft-text-mode 'text-mode)
(setq deft-use-filename-as-title t)

;; nav-mode
(require 'nav)
(nav-disable-overeager-window-splitting)

;; smex
(require 'smex)
(smex-initialize)

;; server mdoe
(if (not server-mode)
    (server-start nil t))

;; yasnippet
(require 'yasnippet)
(yas-global-mode 1)
(setq yas-snippet-dirs (append yas-snippet-dirs
                               '("~/.emacs.d/snippets")))

;; smart-mode-line
(require 'smart-mode-line)
(add-hook 'after-init-hook 'sml/setup)
(sml/apply-theme 'respectful)

;; projectile
(projectile-global-mode)
(setq projectile-completion-system 'grizzl)

;; pymacs
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)
(autoload 'pymacs-autoload "pymacs")
(setenv "PYMACS_PYTHON" "/usr/local/bin/python")
(setq py-load-pymacs-p 'nil)

;; ropemacs
(require 'pymacs)
(pymacs-load "ropemacs" "rope-")
(setq ropemacs-enable-autoimport t)

;; auto-complete
(require 'auto-complete)
(global-auto-complete-mode t)

;; py-isort
(require 'py-isort)
(add-hook 'before-save-hook 'py-isort-before-save)

;; org-mode
(setq org-directory "~/Dropbox/org")
(setq org-log-done t)
(setq org-todo-keywords
      '((sequence "TODO(t)" "|" "DONE(d)")
        (sequence "WAITING(w)" "|" "CANCELED(c)")))

;; linum mode
(require 'linum)
(global-linum-mode 1)
(setq linum-format
    (lambda (line) (propertize
        (format (let ((w (length (number-to-string (count-lines (point-min) (point-max))))))
            (concat " %" (number-to-string w) "d ")) line) 'face 'linum)))

;; dired+ mode
(require 'dired+)

;; undo-tree mode
(require 'undo-tree)
(global-undo-tree-mode)

;; fullframe
(require 'fullframe)
(fullframe magit-status magit-mode-quit-window)

;; magit
(setq magit-last-seen-setup-instructions "1.4.0")

;; recentf
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)

;; helm
(require 'helm)
(require 'helm-config)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)

(helm-mode 1)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-m") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-h f") 'helm-apropos)
(global-set-key (kbd "C-h r") 'helm-info-emacs)
(global-set-key (kbd "C-h C-l") 'helm-locate-library)
(global-set-key (kbd "C-x C-r") 'helm-recentf)

(set-face-attribute 'helm-selection nil
                    :background "purple"
                    :foreground "white")

(helm-autoresize-mode 1)


;;----------------------------------------------------------------------------
;; Defuns
;;----------------------------------------------------------------------------

; make zap-to-char act like zap-up-to-char
(defadvice zap-to-char (after my-zap-to-char-advice (arg char) activate)
  "Kill up to the ARG'th occurence of CHAR, and leave CHAR.
  The CHAR is replaced and the point is put before CHAR."
  (insert char)
  (forward-char -1))

; smarter navigation to the beginning of a line
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

; Highlight the call to ipdb
; src http://pedrokroger.com/2010/07/configuring-emacs-as-a-python-ide-2/
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
(defun duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank)
  )

;; highlight comment annotations
;; from http://emacsredux.com/blog/2013/07/24/highlight-comment-annotations/
(defun font-lock-comment-annotations ()
  "Highlight a bunch of well known comment annotations.

This functions should be added to the hooks of major modes for programming."
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|OPTIMIZE\\|HACK\\|REFACTOR\\):"
          1 font-lock-warning-face t))))
(add-hook 'prog-mode-hook 'font-lock-comment-annotations)


;;----------------------------------------------------------------------------
;; Key Bindings
;;----------------------------------------------------------------------------

;; duplicate the current line
(global-set-key [C-return] 'duplicate-line)

;; sorting
(global-set-key (kbd "M-`") 'sort-lines)

;; font-size
(define-key global-map (kbd "C-=") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

;; smex
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)

;;; init.el ends here
