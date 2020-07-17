;; init-utils.el --- My custom utils config.
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
  :defer t)

(provide 'init-utils)
;;; init-utils.el ends here
