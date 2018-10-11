;;; magit-git-toolbelt.el --- A Magit interface git-toolbelt

;; Copyright 2018 Jonathan Chu

;; Author: Jonathan Chu <me@jonathanchu.is>
;; URL: https://github.com/jonathanchu/magit-git-toolbelt
;; Version: 0.1.0

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A Magit interface for Vincent Driessen's git-toolbelt:
;; https://github.com/nvie/git-toolbelt


;;; Code:

(require 'magit)
(require 'magit-popup)


;;; Variables

(defgroup magit-git-toolbelt nil
  "Invoke git-toolbelt commands from Magit"
  :prefix "magit-git-toolbelt"
  :group 'magit-extensions)


;;; Popups

(magit-define-popup magit-git-toolbelt-popup
                    "Popup console for git-toolbelt commands."
                    'magit-popups
                    :actions '((?c "Cleanup")))

(defun magit-git-toolbelt-cleanup ()
  "Runs git cleanup"
  (interactive)
  (magit-run-git "cleanup"))
