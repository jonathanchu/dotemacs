;; init-ui.el --- My personal UI setup.
;;
;; Copyright (c) 2019-2020
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
;; UI
;;----------------------------------------------------------------------------

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
(add-to-list 'default-frame-alist '(font . "Operator Mono-14"))

;; mode line modifications based on powerline
;; (defvar mode-line-height 30)
;; (defvar-local doom--env-version nil)
;; (defvar-local doom--env-command nil)

;; (eval-when-compile (require 'powerline))
;; (defvar mode-line-bar          (pl/percent-xpm mode-line-height 100 0 100 0 3 "#00B3EF" nil))
;; (defvar mode-line-eldoc-bar    (pl/percent-xpm mode-line-height 100 0 100 0 3 "#B3EF00" nil))
;; (defvar mode-line-inactive-bar (pl/percent-xpm mode-line-height 100 0 100 0 3 nil nil))

;; (defface mode-line-is-modified nil "Face for mode-line modified symbol")
;; (defface mode-line-buffer-path nil "Face for mode-line buffer file path")
;; (defface mode-line-highlight nil "")
;; (defface mode-line-2 nil "")

;; ;;
;; ;; Mode-line segments
;; ;;

;; (defun *buffer-path ()
;;   (when buffer-file-name
;;     (propertize
;;      (f-dirname
;;       (let ((buffer-path (file-relative-name buffer-file-name (doom/project-root)))
;;             (max-length (truncate (/ (window-body-width) 1.75))))
;;         (concat (projectile-project-name) "/"
;;                 (if (> (length buffer-path) max-length)
;;                     (let ((path (reverse (split-string buffer-path "/" t)))
;;                           (output ""))
;;                       (when (and path (equal "" (car path)))
;;                         (setq path (cdr path)))
;;                       (while (and path (<= (length output) (- max-length 4)))
;;                         (setq output (concat (car path) "/" output))
;;                         (setq path (cdr path)))
;;                       (when path
;;                         (setq output (concat "../" output)))
;;                       (when (string-suffix-p "/" output)
;;                         (setq output (substring output 0 -1)))
;;                       output)
;;                   buffer-path))))
;;      'face (if active 'mode-line-buffer-path))))

;; (defun *buffer-state ()
;;   (when buffer-file-name
;;     (propertize
;;      (concat (if (not (file-exists-p buffer-file-name))
;;                  "∄"
;;                (if (buffer-modified-p) "✱"))
;;              (if buffer-read-only ""))
;;      'face 'mode-line-is-modified)))

;; (defun *buffer-name ()
;;   "The buffer's name."
;;   (s-trim-left (format-mode-line "%b")))

;; (defun *buffer-pwd ()
;;   "Displays `default-directory'."
;;   (propertize
;;    (concat "[" (abbreviate-file-name default-directory) "]")
;;    'face 'mode-line-2))

;; (defun *major-mode ()
;;   "The major mode, including process, environment and text-scale info."
;;   (concat (format-mode-line mode-name)
;;           (if (stringp mode-line-process) mode-line-process)
;;           (if doom--env-version (concat " " doom--env-version))
;;           (and (featurep 'face-remap)
;;                (/= text-scale-mode-amount 0)
;;                (format " (%+d)" text-scale-mode-amount))))

(use-package centaur-tabs
  :disabled
  :ensure t
  :config
  (setq centaur-tabs-background-color (face-background 'default))
  ;; (centaur-tabs-inherit-tabbar-faces)
  (setq centaur-tabs-style "bar")
  (setq centaur-tabs-height 32)
  (setq centaur-tabs-set-icons t)
  (setq centaur-tabs-set-bar t)
  (setq centaur-tabs-set-modified-marker t)
  (setq centaur-tabs-modified-marker "*")
  (setq centaur-tabs-set-close-button nil)
  (setq centaur-tabs-gray-out-icons 'buffer)
  (setq centaur-tabs-set-bar 'left)
  (centaur-tabs-mode t)
  (centaur-tabs-headline-match)
  (set-face-attribute 'centaur-tabs-modified-marker-selected nil :foreground (face-background 'doom-modeline-bar))
  (set-face-attribute 'centaur-tabs-modified-marker-unselected nil :foreground (face-background 'doom-modeline-bar))
  (defun centaur-tabs-buffer-groups ()
    "`centaur-tabs-buffer-groups' control buffers' group rules.

 Group centaur-tabs with mode if buffer is derived from `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
 All buffer name start with * will group to \"Emacs\".
 Other buffer group by `centaur-tabs-get-group-name' with project name."
    (list
     (cond
      ((or (string-equal "*" (substring (buffer-name) 0 1))
           (memq major-mode '(magit-process-mode
                              magit-status-mode
                              magit-diff-mode
                              magit-log-mode
                              magit-file-mode
                              magit-blob-mode
                              magit-blame-mode
                              )))
       "Emacs")
      (t
       (centaur-tabs-get-group-name (current-buffer))))))
  :hook
  (dashboard-mode . centaur-tabs-local-mode)
  (term-mode . centaur-tabs-local-mode)
  (calendar-mode . centaur-tabs-local-mode)
  (dired-mode . centaur-tabs-local-mode)
  (org-agenda-mode . centaur-tabs-local-mode)
  (magit-log-mode . centaur-tabs-local-mode)
  (magit-diff-mode . centaur-tabs-local-mode)
  (magit-status-mode . centaur-tabs-local-mode)
  (magit-process-mode . centaur-tabs-local-mode)
  (magit-stashes-mode . centaur-tabs-local-mode)
  (helpful-mode . centaur-tabs-local-mode)
  (help-mode . centaur-tabs-local-mode)
  (fundamental-mode . centaur-tabs-local-mode)
  (lisp-interaction-mode . centaur-tabs-local-mode)
  :bind
  ("s-{" . centaur-tabs-backward)
  ("s-}" . centaur-tabs-forward)
  :custom-face
  (centaur-tabs-active-bar-face ((t (:inherit doom-modeline-bar)))))

(use-package modus-operandi-theme
  :ensure t
  :config
  ;; (load-theme 'modus-operandi t)
  )

(use-package leuven-theme
  :ensure t
  :config
  ;; (load-theme 'leuven t)
  )

(use-package acme-theme
  :load-path "themes"
  :config
  ;; (load-theme 'acme t)
  )

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-one-light t)
  )


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


(provide 'init-ui)
;;; init-ui.el ends here
