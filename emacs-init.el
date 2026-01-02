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

;; for now
;; (setq package-check-signature nil)
;; (setq package-check-signature 'allow-unsigned)
;; (setq package-unsigned-archives '("gnu" "nongnu"))

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

(use-package smex
  :ensure t
  :init
  (smex-initialize))

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

;; require diminish mode for some packages
 (use-package diminish
    :ensure t)

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

(defun my-org-config/after-org-archive ()
  (org-save-all-org-buffers))

(add-hook 'org-archive-hook 'my-org-config/after-org-archive)

;;----------------------------------------------------------------------------
;; Key bindings
;;----------------------------------------------------------------------------

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
;; (global-set-key (kbd "M-n") (kbd "C-u 1 C-v"))
;; (global-set-key (kbd "M-p") (kbd "C-u 1 M-v"))

;; fullscreen toggle
(global-set-key [(s return)] #'toggle-frame-fullscreen)

;; fixup whitespace
(global-set-key (kbd "C-c w") #'fixup-whitespace)

;; kill the current buffer
(global-set-key (kbd "C-x C-k") #'kill-current-buffer)

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

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package volatile-highlights
  :ensure t
  :config
  (volatile-highlights-mode t))

(use-package ace-window
  :ensure t
  :init
  (setq aw-keys '(?a ?s ?d ?f ?j ?k ?l))
  :bind
  ("C-x C-o" . ace-window))

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
  (global-flycheck-mode 1))

(use-package forge
  :ensure t
  :after magit)

(use-package git-messenger
  :ensure t
  :defer t
  :bind
  ("C-x v m" . git-messenger:popup-message))

(use-package git-timemachine
  :ensure t)

(use-package github-browse-file
  :ensure t)

(use-package magit
  :ensure t
  :config
  (progn
    (setq magit-completing-read-function #'ivy-completing-read)
    (setq magit-diff-refine-hunk t)
    (setq magit-status-margin
          '(t "%Y-%m-%d %H:%M " magit-log-margin-width t 18))
    (setq magit-log-margin '(t "%Y-%m-%d %H:%M " magit-log-margin-width t 18)))
  :bind
  ("C-x g" . magit-status)
  ("C-c C-a" . magit-commit-amend)
  ("C-c g" . magit-file-dispatch)
  )

(setq magit-repository-directories '(("\~/projects" . 3)))
(defun magit-status-with-prefix-arg ()
  "Call `magit-status` with a prefix."
  (interactive)
  (let ((current-prefix-arg '(4)))
    (call-interactively #'magit-status)))
(global-set-key (kbd "s-P") 'magit-status-with-prefix-arg)


(use-package magit-git-toolbelt
  ;; :disabled
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
    (setq-default fringes-outside-margins t)
    ;; thin fringe bitmaps
    (define-fringe-bitmap 'git-gutter-fr:added [224]
      nil nil '(center repeated))
    (define-fringe-bitmap 'git-gutter-fr:modified [224]
      nil nil '(center repeated))
    (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240]
      nil nil 'bottom)))

(setq-default fringes-outside-margins t
              highlight-nonselected-windows nil)

(use-package git-gutter
  :ensure t
  :config
  (require 'git-gutter-fringe)
  (global-git-gutter-mode +1)
  (add-hook 'focus-in-hook 'git-gutter:update-all-windows))

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

(use-package yasnippet
  :ensure t
  :config
  (progn
    (yas-global-mode 1)
    ;; (setq yas-snippet-dirs (append yas-snippet-dirs
                                   ;; '("~/.emacs.d/snippets")))
))


  (use-package yasnippet-snippets
    :ensure t)

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

(use-package helm
  :ensure t
  :diminish helm-mode
  :bind (
         ;; ("M-x" . helm-M-x)
         ;; ("C-x C-f" . helm-find-files)
         ;; ("C-x C-r" . helm-recentf)
         ;; ("C-x b" . helm-buffers-list)
         ("C-c i" . helm-imenu))
  :config
  (helm-mode 1)
  (helm-autoresize-mode 1)
  (defvar helm-M-x-fuzzy-match t)
  (defvar helm-M-x-always-save-history t)
  (defvar helm-recentf-fuzzy-match t)
  (defvar lm-buffers-fuzzy-matching t)
  (defvar helm-imenu-fuzzy-match t)
  (defvar helm-display-header-line nil)
  ;; (defvar helm-completion-in-region-fuzzy-match t)
  ;; (defvar helm-mode-fuzzy-match t)
  (setq helm-candidate-number-limit 30))

(use-package helm-projectile
  :ensure t
  :init
  (helm-projectile-on)
  ;; (bind-key "s-t" #'helm-projectile-find-file)
  ;; (bind-key "s-P" #'helm-projectile-switch-project)
  )

(use-package dumb-jump
  :ensure t
  :bind
  (("M-g o" . dumb-jump-go-other-window)
   ("M-g j" . dumb-jump-go)
   ("M-g i" . dumb-jump-go-prompt)
   ("M-g x" . dumb-jump-go-prefer-external)
   ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :config
  (setq dumb-jump-selector 'ivy)
  )

(use-package easy-kill
  :ensure t
  :config
  (global-set-key [remap kill-ring-save] 'easy-kill))

(use-package edit-indirect
  :ensure t)

(use-package smart-comment
  :ensure t
  :bind
  ("s-/" . smart-comment))

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
  ;; (add-hook 'org-mode-hook #'enable-paredit-mode)
  (add-hook 'python-mode-hook
            (lambda () (local-set-key (kbd "C-k") #'paredit-kill))))

(use-package elpy
  :disabled
  :ensure t
  :config
  (elpy-enable)
  (setq elpy-rpc-python-command "python3"))

(use-package jinja2-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.j2\\'" . jinja2-mode)))

(use-package python-mode
  :ensure t
  :config
  (add-hook 'python-mode-hook
            '(lambda ()
               (setq fill-column 80)))
  (add-to-list 'auto-mode-alist '("\\.py" . python-mode)))

(use-package toml-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.toml\\'" . toml-mode)))

(use-package yaml-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

(use-package make-mode
  :config
  (add-to-list 'auto-mode-alist '("\\Makefile\\'" . makefile-mode)))

(use-package sh-script
  :config
  (add-to-list 'auto-mode-alist '("\\.envrc\\'" . shell-script-mode)))

(use-package fish-mode
  :ensure t
  :defer t
  :config
  (add-hook 'fish-mode-hook (lambda ()
                              (add-hook 'before-save-hook 'fish_indent-before-save))))

(use-package go-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode)))

(use-package less-css-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.less\\'" . less-css-mode)))

(use-package restclient
  :ensure t)

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

(use-package markdown-mode
  :ensure t
  :mode "\\.md\\'")

(use-package cider
  :ensure t
  :config
  (progn
    (setq nrepl-log-messages t)
    (setq nrepl-hide-special-buffers t)
    (add-hook 'cider-mode-hook #'eldoc-mode)))

(use-package clj-refactor
  :ensure t
  :config
  (defun my-clojure-mode-hook ()
    (clj-refactor-mode 1)
    (yas-minor-mode 1) ; for adding require/use/import
    (cljr-add-keybindings-with-prefix "C-c C-m"))
  (add-hook 'clojure-mode-hook #'my-clojure-mode-hook))

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

(use-package json-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode)))

(use-package esup
  :ensure t)

(use-package fix-word
  :ensure t
  :bind
  ("M-u" . fix-word-upcase)
  ("M-l" . fix-word-downcase)
  ("M-c" . fix-word-capitalize))

(use-package uuidgen
  :ensure t
  :defer t)

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (setq which-key-use-C-h-commands nil)
  ;; Prevent
  ;; https://github.com/justbur/emacs-which-key/issues/130
  (setq inhibit-compacting-font-caches nil)
  (setq which-key-allow-imprecise-window-fit t)
  (which-key-mode))

(use-package crux
  :ensure t
  :defer t
  :bind
  ("C-c s" . crux-find-shell-init-file)
  ("C-c e" . crux-find-user-init-file))

(use-package scratch
  :ensure t
  :config
  (autoload 'scratch "scratch" nil t))
