;;; magit-git-toolbelt.el --- A Magit interface for git-toolbelt -*- lexical-binding: t -*-

;; Copyright 2025 Jonathan Chu

;; Author: Jonathan Chu <me@jonathanchu.is>
;; URL: https://github.com/jonathanchu/magit-git-toolbelt
;; Version: 0.2.0
;; Package-Requires: ((emacs "26.1") (magit "3.0.0") (transient "0.3.0"))
;; Keywords: git tools vc

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
;;
;; Usage:
;;   M-x magit-git-toolbelt or press "@" in Magit buffers
;;
;; git-toolbelt must be installed and available in your PATH.
;; See: https://github.com/nvie/git-toolbelt#installation

;;; Code:

(require 'magit)
(require 'transient)

;;; Custom Variables

(defgroup magit-git-toolbelt nil
  "Magit interface for git-toolbelt commands."
  :prefix "magit-git-toolbelt-"
  :group 'magit-extensions)

(defcustom magit-git-toolbelt-recent-branches-since "1.week.ago"
  "Default time range for recent-branches command."
  :type 'string
  :group 'magit-git-toolbelt)

;;; Transient Menu

;;;###autoload (autoload 'magit-git-toolbelt "magit-git-toolbelt" nil t)
;; Define the submenus
(transient-define-prefix magit-git-toolbelt-commit-info ()
  "Commit info commands."
  ["Commit Info"
   ("i" "Initial commit" magit-git-toolbelt-initial-commit)
   ("s" "Current SHA" magit-git-toolbelt-sha)
   ("l" "Local commits" magit-git-toolbelt-local-commits)
   ])

(transient-define-prefix magit-git-toolbelt-merge-status ()
  "Merge status commands."
  ["Merge Status"
   ("m" "Merged" magit-git-toolbelt-merged)
   ("u" "Unmerged" magit-git-toolbelt-unmerged)
   ])

(transient-define-prefix magit-git-toolbelt-branches ()
  "Branches"
  ["Branches"
   ("l" "Local branches" magit-git-toolbelt-local-branches)
   ("b" "Current branch" magit-git-toolbelt-current-branch)
   ("m" "Main branch" magit-git-toolbelt-main-branch)
   ("r" "Remote branches" magit-git-toolbelt-remote-branches)
   ("t" "Remote tracking branch" magit-git-toolbelt-remote-tracking-branch)
   ])

(transient-define-prefix magit-git-toolbelt-diff-inspection ()
  "Diff & Inspection"
  ["Diff & Inspection"
   ("d" "Deltas (commits ahead/behind" magit-git-toolbelt-deltas)
   ("m" "Modified files" magit-git-toolbelt-modified)
   ("u" "Untracked files" magit-git-toolbelt-untracked)
   ])

;; Main menu references the submenu
(transient-define-prefix magit-git-toolbelt ()
  "Git toolbelt commands."
  ["Quick Commands"
   ("c" "Cleanup merged branches" magit-git-toolbelt-cleanup)
   ("r" "Recent branches" magit-git-toolbelt-recent-branches)
   ("a" "Active branches" magit-git-toolbelt-active-branches)
   ]
  ["Submenus"
   ("C" "Commit Info..." magit-git-toolbelt-commit-info)
   ("M" "Merge Status..." magit-git-toolbelt-merge-status)
   ("B" "Branches..." magit-git-toolbelt-branches)
   ("D" "Diff & Inspection..." magit-git-toolbelt-diff-inspection)
   ]
  ["Actions"
   ("z" "Undo last commit" magit-git-toolbelt-undo-commit)]
  )

;;; Branch Commands

(defun magit-git-toolbelt-cleanup ()
  "Delete branches already merged into master or develop."
  (interactive)
  (when (yes-or-no-p "Delete all branches merged into master/develop? ")
    (magit-git-command "cleanup" default-directory)))

(defun magit-git-toolbelt-recent-branches ()
  "Show branches modified recently."
  (interactive)
  (let ((output (magit-git-string "recent-branches")))
    (if output
        (magit-git-toolbelt--display-output "Recent Branches" output)
      (message "No recent branches found"))))

(defun magit-git-toolbelt-local-branches ()
  "List local branches."
  (interactive)
  (let ((output (shell-command-to-string "git local-branches")))
    (magit-git-toolbelt--display-output "Local Branches" output)))

(defun magit-git-toolbelt-remote-branches ()
  "List remote branches."
  (interactive)
  (let ((output (shell-command-to-string "git remote-branches")))
    (magit-git-toolbelt--display-output "Remote Branches" output)))

(defun magit-git-toolbelt-remote-tracking-branch ()
  "Print the name of the remote tracking branch."
  (interactive)
  (let ((output (shell-command-to-string "git remote-tracking-branch")))
    (magit-git-toolbelt--display-output "Remote Tracking Branch" output)))

(defun magit-git-toolbelt-active-branches ()
  "Returns a list of local or remote branches."
  (interactive)
  (let ((output (shell-command-to-string "git active-branches")))
    (magit-git-toolbelt--display-output "Active Branches" output)))

(defun magit-git-toolbelt-current-branch ()
  "Show the current branch name."
  (interactive)
  (let ((branch (magit-git-string "current-branch")))
    (if branch
        (message "Current branch: %s" branch)
      (message "Not on any branch"))))

(defun magit-git-toolbelt-main-branch ()
  "Show the main branch name."
  (interactive)
  (let ((branch (magit-git-string "main-branch")))
    (if branch
        (message "Main branch: %s" branch)
      (message "No main branch found"))))

;;; Commit Info Commands

(defun magit-git-toolbelt-initial-commit ()
  "Show the initial commit for the repo."
  (interactive)
  (let ((commit (magit-git-string "initial-commit")))
    (if commit
        (magit-show-commit commit)
      (message "Could not find initial commit"))))

(defun magit-git-toolbelt-sha ()
  "Show the current commit SHA."
  (interactive)
  (let ((sha (magit-git-string "sha")))
    (if sha
        (progn
          (kill-new sha)
          (message "SHA: %s (copied to kill ring)" sha))
      (message "Could not get SHA"))))

(defun magit-git-toolbelt-local-commits ()
  "Returns a list of commits that are still in local, but not yet pushed."
  (interactive)
  (let ((output (shell-command-to-string "git local-commits")))
    (magit-git-toolbelt--display-output "Local Commits" output)))

;;; Diff & Inspection Commands

(defun magit-git-toolbelt-deltas ()
  "Show commits ahead/behind of upstream."
  (interactive)
  (let ((output (shell-command-to-string "git deltas")))
    (if (string-empty-p (string-trim output))
        (message "No deltas (branch is in sync)")
      (message "Deltas: %s" (string-trim output)))))

(defun magit-git-toolbelt-merged ()
  "Show branches that have been merged."
  (interactive)
  (let ((output (shell-command-to-string "git merged")))
    (if (string-empty-p (string-trim output))
        (message "No merged branches found")
      (magit-git-toolbelt--display-output "Merged Branches" output))))

(defun magit-git-toolbelt-unmerged ()
  "Show branches that have not been merged."
  (interactive)
  (let ((output (shell-command-to-string "git unmerged")))
    (if (string-empty-p (string-trim output))
        (message "No unmerged branches found")
      (magit-git-toolbelt--display-output "Unmerged Branches" output))))

;;; File Status Commands

(defun magit-git-toolbelt-modified ()
  "Show modified files."
  (interactive)
  (let ((output (shell-command-to-string "git modified")))
    (if (string-empty-p (string-trim output))
        (message "No modified files")
      (magit-git-toolbelt--display-output "Modified Files" output))))

(defun magit-git-toolbelt-untracked ()
  "Show untracked files."
  (interactive)
  (let ((output (shell-command-to-string "git untracked")))
    (if (string-empty-p (string-trim output))
        (message "No untracked files")
      (magit-git-toolbelt--display-output "Untracked Files" output))))

;;; Action Commands

(defun magit-git-toolbelt-undo-commit ()
  "Undo the last commit (keeps changes staged)."
  (interactive)
  (when (yes-or-no-p "Undo the last commit? ")
    (magit-git-command "undo-commit" default-directory)
    (magit-refresh)))

;;; Helper Functions

(defun magit-git-toolbelt--display-output (title output)
  "Display OUTPUT in a buffer with TITLE."
  (let ((buf (get-buffer-create (format "*magit-git-toolbelt: %s*" title))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert output)
        (goto-char (point-min)))
      (special-mode))
    (display-buffer buf)))

;;; Integration with Magit

;;;###autoload
(with-eval-after-load 'magit
  (transient-append-suffix 'magit-dispatch "!"
    '("@" "Git Toolbelt" magit-git-toolbelt))
  (define-key magit-mode-map "@" #'magit-git-toolbelt))

(provide 'magit-git-toolbelt)

;;; magit-git-toolbelt.el ends here
