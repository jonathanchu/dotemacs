;; init-javascript.el --- My custom YavaScripts config.
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
;; YavaScripts
;;----------------------------------------------------------------------------

(use-package add-node-modules-path
  :ensure t
  :config
  (add-hook 'js2-mode-hook #'add-node-modules-path)
  (add-hook 'js-mode-hook #'add-node-modules-path)
  (add-hook 'rjsx-mode-hook #'add-node-modules-path))

(use-package flow-minor-mode
  :ensure t
  :config
  (add-hook 'js2-mode-hook 'flow-minor-enable-automatically)
  (add-hook 'rjsx-mode-hook 'flow-minor-enable-automatically))

(use-package indium
  :ensure t)

(use-package js2-mode
  :ensure t
  :mode
  "\\.js$"
  "\\.jsx$"
  :commands js2-mode
  :config
  (progn
    (setq-default
     js2-auto-indent-flag nil
     js2-basic-offset 2
     js2-electric-keys nil
     js2-mirror-mode nil
     js2-mode-show-parse-errors nil
     js2-mode-show-strict-warnings nil
     js2-strict-missing-semi-warning nil
     js2-strict-trailing-comma-warning nil
     js2-highlight-external-variables nil)
    (add-hook 'js2-mode-hook 'prettier-js-mode)
    ;; (add-hook 'js2-mode-hook 'prettier-js-save-hook)
    (add-hook 'js2-mode-hook
              (defun my-js2-mode-setup ()
                (flycheck-select-checker 'javascript-eslint)))
    ;; (add-hook 'after-save-hook 'flow-save-hook)
    ))

(defun flow-save-hook ()
  "Invoke flow-status after save when in js2-mode."
  (when (and (eq major-mode 'js2-mode)
             (executable-find "flow"))
    (flow-status)))

(use-package js2-refactor
  :ensure t
  :init
  (add-hook 'js2-mode-hook #'js2-refactor-mode))

(use-package json-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode)))

(defun prettier-js-save-hook ()
  "Invoke prettier-js hook if prettier executable is found."
  (when (executable-find "prettier")
    (prettier-js-mode)))

(use-package rjsx-mode
  :ensure t
  :config
  (progn
    (add-to-list 'auto-mode-alist '("\\/components\\/.*\\.js\\'" . rjsx-mode))
    (add-hook 'rjsx-mode-hook 'prettier-js-mode)
    ;; (add-hook 'js2-mode-hook 'prettier-js-save-hook)
    (add-hook 'rjsx-mode-hook
              (defun my-rjsx-mode-setup ()
                (flycheck-select-checker 'javascript-eslint)))))

(use-package flow-js2-mode
  :ensure t
  :config
  (add-hook 'js2-mode-hook #'flow-js2-mode))

(provide 'init-javascript)
;;; init-javascript.el ends here
