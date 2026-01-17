;; init.el --- My personal Emacs configuration for the terminal
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

;;----------------------------------------------------------------------------
;; Bootstrapping
;;----------------------------------------------------------------------------

(defconst emacs-start-time (current-time))

(prefer-coding-system 'utf-8)

;; GC thresholds
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))

;; native compile settings
(when (featurep 'native-compile)
    (setq native-comp-async-report-warnings-errors nil))

;; long line handling
(setq bidi-inhibit-bpa t)

;; no menu bar
(menu-bar-mode -1)

;; no splash screen
(setq inhibit-splash-screen t)

;; no message on startup
(setq initial-scratch-message nil)

;; reload the file in buffer when they change on disk
(global-auto-revert-mode t)

;; misc useful settings
(setq-default indent-tabs-mode nil)
(setq sentence-end-double-space nil)
(setq scroll-conservatively 10000)
(delete-selection-mode t)

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
              use-package-always-ensure t) ; make sure packages always installed
(eval-when-compile
  (require 'use-package))

;; for now
(setq package-check-signature 'allow-unsigned)
(setq package-unsigned-archives '("gnu" "nongnu"))

;;----------------------------------------------------------------------------
;; Packages
;;----------------------------------------------------------------------------

(use-package ibuffer
  :ensure nil  ; built-in
  :bind
  ("C-x C-b" . ibuffer))

;; consider all themes as safe
(setq custom-safe-themes t)

(use-package catppuccin-theme
  :config
  ;; 'latte, 'macchiato, or 'mocha
  (setq catppuccin-flavor 'latte)
  (load-theme 'catppuccin t))

;; show extra whitespace
(setq show-trailing-whitespace t)

;; delete trailing whitespace in all modes
(add-hook 'before-save-hook #'delete-trailing-whitespace)

;; ensure last line is a return
(setq require-final-newline t)

;; no cowbell
(setq ring-bell-function 'ignore)

;; recent files mode
(setq recentf-max-saved-items 100)
(recentf-mode 1)

;; only type 'y' or 'n' instead of 'yes' or 'no'
(setq use-short-answers t)

;; conservative indention for org src blocks
(setq org-src-preserve-indentation t)

;; allow windows to resize evenly when closed
(setq window-combination-resize t)

;; custom settings in a separate file and load the custom settings
(setq-default custom-file (expand-file-name
                           "custom.el"
                           user-emacs-directory))
(load custom-file :no-error-if-file-is-missing)

;; line numbers in left fringe
(use-package display-line-numbers
  :defer
  :ensure nil  ; built-in
  :custom
    (display-line-numbers-width-start t)
  :hook
    (prog-mode . display-line-numbers-mode)
    (tex-mode . display-line-numbers-mode)
    (markdown-mode . display-line-numbers-mode)
    (conf-mode . display-line-numbers-mode)
    (org-mode . display-line-numbers-mode))

(use-package diff-hl
  :ensure t
  :init
  (global-diff-hl-mode 1)
  :hook
  (magit-pre-refresh . diff-hl-magit-pre-refresh)
  (magit-post-refresh . diff-hl-magit-post-refresh))

;; smartparens mode
(use-package smartparens
  :hook ((emacs-lisp-mode . smartparens-strict-mode)
         (org-mode . smartparens-mode))
  :config
  (require 'smartparens-config))

;; Magit
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
     (unpulled . show)
     (unpushed . show)
     (untracked . show)
     (recent . show)))
  (magit-status-show-hashes-in-headers t)
  (magit-log-section-commit-count 15)
  ;; Performance
  (magit-revision-insert-related-refs nil)
  (magit-refresh-status-buffer nil)
  (magit-section-cache-visibility t)
  :config
  (defun magit-status-with-prefix-arg ()
    "Call `magit-status` with a prefix."
    (interactive)
    (let ((current-prefix-arg '(4)))
      (call-interactively #'magit-status))))

;; Full frame buffers
(use-package fullframe
  :after magit
  :config
  (progn
    (fullframe magit-status magit-mode-quit-window)
    (fullframe ibuffer ibuffer-quit)))

;; Vertico - Vertical completion UI
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
  :bind
  (:map projectile-command-map
        ("f" . consult-projectile-find-file)
        ("p" . consult-projectile-switch-project)
        ("b" . consult-projectile-switch-to-buffer))
  :init
  (setq projectile-completion-system 'default)
  :config
  (setq projectile-project-search-path '("~/projects/"))
  (setq projectile-switch-project-action #'projectile-dired)
  (projectile-mode +1))

;; Consult-Projectile (optional, for projectile integration)
(use-package consult-projectile
  :after (consult projectile))


;;----------------------------------------------------------------------------
;; Finalization
;;----------------------------------------------------------------------------

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
