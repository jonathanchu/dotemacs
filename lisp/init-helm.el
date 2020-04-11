;; init-helm.el --- My personal Helm setup.
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

(use-package helm
  :ensure t
  :diminish helm-mode
  :bind (
         ;; ("M-x" . helm-M-x)
         ;; ("C-x C-f" . helm-find-files)
         ;; ("C-x C-r" . helm-recentf)
         ("C-x b" . helm-buffers-list)
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

(use-package helm-ag
  :ensure t
  ;; :bind ("s-F" . helm-do-ag-project-root)
  :config
  ;; (helm-ag-use-agignore t)
  )

(use-package helm-projectile
  :ensure t
  :init
  (helm-projectile-on)
  ;; (bind-key "s-t" #'helm-projectile-find-file)
  ;; (bind-key "s-P" #'helm-projectile-switch-project)
  )

(provide 'init-helm)
;;; init-helm.el ends here
