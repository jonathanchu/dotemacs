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

;; no menu bar
(menu-bar-mode -1)

;; no splash screen
(setq inhibit-splash-screen t)

;; no message on startup
(setq initial-scratch-message nil)

;; reload the file in buffer when they change on disk
(global-auto-revert-mode t)

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
              use-package-always-ensure t)
(eval-when-compile
  (require 'use-package))

;; for now
(setq package-check-signature nil)
(setq package-check-signature 'allow-unsigned)
(setq package-unsigned-archives '("gnu" "nongnu"))

;;----------------------------------------------------------------------------
;; Packages
;;----------------------------------------------------------------------------

(use-package ibuffer
  :bind
  ("C-x C-b" . ibuffer))

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

;; consider all themes as safe
(setq custom-safe-themes t)

(recentf-mode 1)
(setq recentf-max-saved-items 100)  ; Optional: increase history size

;; custom settings in a separate file and load the custom settings
(setq-default custom-file (expand-file-name
                             "custom.el"
                             user-emacs-directory))
(load custom-file :no-error-if-file-is-missing)

;; smartparens mode
(use-package smartparens
  :hook ((emacs-lisp-mode . smartparens-strict-mode)
         (org-mode . smartparens-mode))
  :config
  (require 'smartparens-config))

;; Magit
(use-package magit
  :config
  (progn
    (setq magit-completing-read-function #'completing-read-default)
    (setq magit-diff-refine-hunk t)
    (setq magit-status-margin
          '(t "%Y-%m-%d %H:%M " magit-log-margin-width t 18))
    (setq magit-log-margin '(t "%Y-%m-%d %H:%M " magit-log-margin-width t 18)))
  :bind
  ("C-x g" . magit-status)
  ("C-c C-a" . magit-commit-amend)
  ("C-c g" . magit-file-dispatch))

;; Full frame buffers
(use-package fullframe
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
  :bind (("C-x C-f" . find-file)  ; Keep default or use consult-find
         ("C-c k" . consult-ripgrep)  ; Replaces counsel-ag
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
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; Consult-Projectile (optional, for projectile integration)
(use-package consult-projectile
  :after (consult projectile)
  :bind (("s-t" . consult-projectile-find-file)))


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
