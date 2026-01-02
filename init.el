;; init.el --- My personal Emacs configuration.
;;
;; Copyright (c) 2015 - 2025
;;
;; Author: Jonathan Chu <me@jonathanchu.is>
;; URL: https://github.com/jonathanchu/dotemacs
;; Version: 2.0

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
;; Bootstrapping
;;----------------------------------------------------------------------------

;; load literate config first
(org-babel-load-file (expand-file-name "~/.emacs.d/emacs-init.org"))

(require 'elegant)

;;----------------------------------------------------------------------------
;; Packages
;;----------------------------------------------------------------------------

(add-to-list 'default-frame-alist '(internal-border-width . 20))
(defun mode-line-align (left right)
  "Return a string with LEFT and RIGHT at the edges of the
current window."
  (format (format "%%s %%%ds" (- (window-total-width) (length left) 2))
          left right))

(setq-default mode-line-format
              '(:eval
                (mode-line-align
                 (format-mode-line
                  (list " " mode-line-buffer-identification
                        " " mode-line-modified
                        " " mode-name))
                 (format-mode-line
                  (list minor-mode-alist
                        " " mode-line-misc-info)))))

;;; When we set a face, we take care of removing any previous settings
;;; -------------------------------------------------------------------
(defun set-face (face style)
  "Reset a FACE and make it inherit STYLE."
  (set-face-attribute face nil
                      :foreground 'unspecified :background 'unspecified
                      :family     'unspecified :slant      'unspecified
                      :weight     'unspecified :height     'unspecified
                      :underline  'unspecified :overline   'unspecified
                      :box        'unspecified :inherit    style))
;;; -------------------------------------------------------------------


(defun set-modeline-faces ()
  "Mode line at top."
  (set-face 'header-line                                 'face-strong)
  (set-face-attribute 'header-line nil
                      :underline (face-foreground 'default))
  (set-face-attribute 'mode-line nil
                      :height 10
                      :underline (face-foreground 'default)
                      :overline nil
                      :box nil
                      :foreground (face-background 'default)
                      :background (face-background 'default))
  (set-face 'mode-line-inactive                            'mode-line)
  (set-face-attribute 'cursor nil
                      :background (face-foreground 'default))
  (set-face-attribute 'window-divider nil
                      :foreground (face-background 'mode-line))
  (set-face-attribute 'window-divider-first-pixel nil
                      :foreground (face-background 'default))
  (set-face-attribute 'window-divider-last-pixel nil
                      :foreground (face-background 'default)))


(set-modeline-faces)

(setq frame-resize-pixelwise t)

;;----------------------------------------------------------------------------
;; Finalization
;;----------------------------------------------------------------------------

(when window-system
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
