;; init-doom.el --- My custom Doom Emacs config.
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
;; Doom Stuff
;;----------------------------------------------------------------------------

(use-package dash
  :ensure t)

(use-package all-the-icons
  :ensure t)

(use-package popup
  :ensure t)

(load-file "~/.emacs.d/doom.el")
;; (load-file "~/.config/emacs/doom.el")

;; (use-package doom-themes
;;   ;; :ensure t
;;   :load-path "vendor/emacs-doom-themes-modified")
;; ;; (load-theme 'doom-one-light t)
(use-package doom-themes)

(use-package doom-modeline
  :ensure t
  :hook
  (after-init . doom-modeline-mode))

(setq-default frame-title-format '(""))

;; Global settings (defaults)
(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
      doom-themes-enable-italic t) ; if nil, italics is universally disabled


;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each theme
;; may have their own settings.
;; (load-theme 'doom-nord-light t)
;; (load-theme 'doom-one-light t)
;; (load-theme 'doom-outrun-electric t)
;; (load-theme 'doom-city-lights t)
;; (use-package atom-one-dark-theme
;;   :ensure t
;;   :config
;;   (load-theme 'atom-one-dark t))

;; Enable flashing mode-line on errors
(doom-themes-visual-bell-config)

;; Enable custom neotree theme
;; (doom-themes-neotree-config)  ; all-the-icons fonts must be installed!

;; Corrects (and improves) org-mode's native fontification.
(doom-themes-org-config)

;; (add-to-list 'custom-theme-load-path "~/.emacs.d/vendor/emacs-horizon-theme")
;; (load-file "~/.emacs.d/vendor/emacs-horizon-theme/horizon-themes.el")
;; (require 'horizon-themes)
;; (load-theme 'horizon t)
;; (use-package horizon-themes
;;   :load-path "vendor/emacs-horizon-theme"
;;   :config
;;   (load-theme 'horizon t))

(provide 'init-doom)
;;; init-doom.el ends here
