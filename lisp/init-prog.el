;; init-prog.el --- My custom programming config.
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

(use-package make-mode
  :config
  (add-to-list 'auto-mode-alist '("\\Makefile\\'" . makefile-mode)))

(use-package scratch
  :ensure t
  :config
  (autoload 'scratch "scratch" nil t))

(use-package sh-script
  :config
  (add-to-list 'auto-mode-alist '("\\.envrc\\'" . shell-script-mode)))

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

(use-package toml-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.toml\\'" . toml-mode)))

(use-package yaml-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

(provide 'init-prog)
;;; init-prog.el ends here
