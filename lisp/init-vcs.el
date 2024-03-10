;; init-vcs.el --- My custom VCS config.
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

(use-package git-undo
  :load-path "vendor/git-undo-el"
  :bind ("C-. C-/" . git-undo))

;; (use-package gitconfig-mode
;;   :ensure t)

(use-package github-browse-file
  :ensure t)

;; (use-package gitignore-mode
;;   :ensure t)

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
  :disabled
  :load-path "vendor/")

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

(provide 'init-vcs)
;;; init-vcs.el ends here
