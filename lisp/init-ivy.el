;; init-ivy.el --- My custom Ivy config.
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

(provide 'init-ivy)
;;; init-ivy.el ends here
