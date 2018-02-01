;;; my-flow.el --- An Emacs plugin for Flow, a static typechecker in JavaScript

;; Copyright 2017 Jonathan Chu

;; Author: Jonathan Chu <me@jonathanchu.is>
;; URL: XXXX
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

;; An Emacs plugin for Flow, a static typechecker in JavaScript

;;; Code:

(defgroup flow nil
  "A frontend for Flow."
  :group 'tools)

(defcustom flow-executable "flow"
  "Name of the flow executable to use."
  :type 'string
  :group 'flow)

(defun flow-start ()
  "Start the Flow server."
  (shell-command (format "%s start" flow-executable)))

;; (defun my-flow-status ()
;;   "Run flow status."
;;   (interactive)
;;   (flow-start)
;;   (compile (format "%s status; exit 0;" flow-executable)))

;; (defun brian-compile-finish (buffer outstr)
;;   (unless (string-match "finished" outstr)
;;     (switch-to-buffer-other-window buffer))
;;   t)

;; (setq compilation-finish-functions 'brian-compile-finish)

;; (require 'cl)

;; (defadvice compilation-start
;;     (around inhibit-display
;;             (command &optional mode name-function highlight-regexp))
;;   (if (not (string-match "^\\(find\\|grep\\)" command))
;;       (noflet ((display-buffer)
;;                (set-window-point)
;;                (goto-char))
;;         (fset 'display-buffer 'ignore)
;;         (fset 'goto-char 'ignore)
;;         (fset 'set-window-point 'ignore)
;;         (save-window-excursion
;;           ad-do-it))
;;     ad-do-it))

;; (ad-activate 'compilation-start)

(defun my-new-flow-status ()
  "Start something."
  (interactive)
  (flow-start)
  (compilation-start
   (format "%s status --from emacs; exit 0;" flow-executable)
   nil
   (lambda (mode-name) "*HAHAHA*")))

(define-compilation-mode flow-mode "Flow"
  "Flow results compilation mode.")

;; Local Variables:
;; no-byte-compile: t
;; End:
;;; my-flow.el ends here
