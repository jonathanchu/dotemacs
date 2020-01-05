;; init-lsp.el --- My custom LSP config.
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
;; LSP
;;----------------------------------------------------------------------------

(use-package company-lsp
  :disabled
  :ensure t
  :config
  (push 'company-lsp company-backends)
  )

(use-package lsp-mode
  ;; :disabled
  :commands lsp-mode
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'lsp-mode)
  :config
  (add-hook 'js-mode-hook #'lsp)
  (use-package lsp-ui
    :ensure t
    :commands lsp-ui-mode
    :init
    (add-hook 'lsp-mode-hook 'flycheck-mode)
    :config
    (progn
      (add-hook 'js-mode-hook #'flycheck-mode)
      (add-hook 'js2-mode-hook #'flycheck-mode) ;; for js2-mode support
      (add-hook 'rjsx-mode #'flycheck-mode) ;; for rjsx-mode support
      (setq lsp-ui-sideline-ignore-duplicate t)
      (add-hook 'lsp-mode-hook 'lsp-ui-mode)
      ;; (add-hook 'js-mode-hook #'lsp)
      )
    :after flycheck
    )
  (use-package company-lsp
    :ensure t
    :commands company-lsp
    :config
    (push 'company-lsp company-backends))
  )

(use-package lsp-javascript-flow
  :disabled
  :ensure t
  :after lsp-mode
  :config
  (progn
    (add-hook 'js-mode-hook #'lsp-javascript-flow-enable)
    (add-hook 'js2-mode-hook #'lsp-javascript-flow-enable) ;; for js2-mode support
    (add-hook 'rjsx-mode #'lsp-javascript-flow-enable) ;; for rjsx-mode support
    )
  )

(provide 'init-lsp)
;;; init-lsp.el ends here
