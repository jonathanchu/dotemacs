;;; grove-daily.el --- Daily notes for grove -*- lexical-binding: t -*-

;; Copyright 2026 Jonathan Chu

;; Author: Jonathan Chu <me@jonathanchu.is>
;; URL: https://github.com/jonathanchu/grove

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

;; Daily notes for grove.  Provides commands to open today's daily note
;; and navigate to yesterday's or tomorrow's.

;;; Code:

(require 'grove)

;;;###autoload
(defun grove-daily (&optional time)
  "Open or create the daily note for TIME.
TIME defaults to the current time.  If the note doesn't exist,
it is created with a title template."
  (interactive)
  (grove--ensure-directory)
  (let* ((time (or time (current-time)))
         (filename (concat (format-time-string grove-daily-format time) ".org"))
         (path (expand-file-name filename (grove--daily-path)))
         (new-p (not (file-exists-p path))))
    (find-file path)
    (when new-p
      (insert "#+title: " (format-time-string "%A, %B %e, %Y" time) "\n")
      (insert "#+date: " (format-time-string "%F" time) "\n\n")
      (save-buffer))))

;;;###autoload
(defun grove-daily-yesterday ()
  "Open or create yesterday's daily note."
  (interactive)
  (grove-daily (time-subtract (current-time) (days-to-time 1))))

;;;###autoload
(defun grove-daily-tomorrow ()
  "Open or create tomorrow's daily note."
  (interactive)
  (grove-daily (time-add (current-time) (days-to-time 1))))

(provide 'grove-daily)
;;; grove-daily.el ends here
