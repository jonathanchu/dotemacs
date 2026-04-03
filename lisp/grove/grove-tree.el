;;; grove-tree.el --- File tree sidebar for grove -*- lexical-binding: t -*-

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

;; An ewoc-based file tree sidebar displayed in a side window.
;; Shows the vault directory structure with expand/collapse for directories.

;;; Code:

(require 'cl-lib)
(require 'ewoc)
(require 'grove)

;;;; Customization

(defcustom grove-tree-width 30
  "Width of the tree sidebar window."
  :type 'integer
  :group 'grove)

(defcustom grove-tree-icons nil
  "Whether to show nerd font icons in the tree sidebar.
Requires a Nerd Font to be installed and active."
  :type 'boolean
  :group 'grove)

;;;; Faces

(defface grove-tree-directory
  '((t :inherit font-lock-function-name-face :weight bold))
  "Face for directory names in the tree sidebar."
  :group 'grove)

(defface grove-tree-file
  '((t :inherit default))
  "Face for file names in the tree sidebar."
  :group 'grove)

(defface grove-tree-marker
  '((t :inherit font-lock-comment-face))
  "Face for expand/collapse markers in the tree sidebar."
  :group 'grove)

(defface grove-tree-guide
  '((t :inherit font-lock-comment-face))
  "Face for indent guide lines in the tree sidebar."
  :group 'grove)

(defface grove-tree-current
  '((t :inherit font-lock-constant-face :weight bold))
  "Face for the currently open file in the tree sidebar."
  :group 'grove)

;;;; Data model

(cl-defstruct grove-tree-node
  "A node in the grove file tree."
  path        ; absolute path
  name        ; display name
  depth       ; nesting level (0 = root children)
  directory-p ; t for directories
  expanded-p) ; t if children are visible (directories only)

;;;; Ewoc

(defvar-local grove-tree--ewoc nil
  "The ewoc instance for the current tree buffer.")

(defvar-local grove-tree--expanded (make-hash-table :test #'equal)
  "Set of expanded directory paths in the tree.")

(defvar-local grove-tree--current-file nil
  "Path of the file currently open in the main window.")

(defconst grove-tree-buffer-name "*grove-tree*"
  "Name of the tree sidebar buffer.")

(defun grove-tree--indent-string (depth)
  "Return an indent guide string for DEPTH levels of nesting."
  (if (zerop depth)
      ""
    (propertize (apply #'concat (make-list depth "│ "))
                'face 'grove-tree-guide)))

(defun grove-tree--item-count (directory)
  "Return the number of visible items (org files and subdirs) in DIRECTORY."
  (let ((count 0))
    (dolist (file (directory-files directory nil))
      (unless (string-prefix-p "." file)
        (when (or (file-directory-p (expand-file-name file directory))
                  (string-suffix-p ".org" file))
          (cl-incf count))))
    count))

(defun grove-tree--icon (dir-p expanded)
  "Return an icon string for a tree node.
DIR-P is non-nil for directories, EXPANDED is non-nil if expanded."
  (if (not grove-tree-icons)
      ""
    (concat (cond
             ((not dir-p) "\xe612 ")         ; nf-seti-text (file)
             (expanded    "\xf115 ")         ; nf-fa-folder_open
             (t           "\xf114 "))        ; nf-fa-folder
            )))

(defun grove-tree--print (node)
  "Print NODE as a line in the ewoc buffer."
  (let* ((depth (grove-tree-node-depth node))
         (indent (grove-tree--indent-string depth))
         (dir-p (grove-tree-node-directory-p node))
         (expanded (and dir-p (gethash (grove-tree-node-path node)
                                       grove-tree--expanded)))
         (name (grove-tree-node-name node))
         (icon (grove-tree--icon dir-p expanded))
         (marker (cond
                  ((not dir-p) "  ")
                  (expanded "▾ ")
                  (t "▸ "))))
    (let ((current-p (and (not dir-p)
                          grove-tree--current-file
                          (string= (grove-tree-node-path node)
                                   grove-tree--current-file)))
          (count (when dir-p
                   (grove-tree--item-count (grove-tree-node-path node)))))
      (insert indent
              (propertize marker 'face 'grove-tree-marker)
              (propertize icon 'face (if dir-p
                                         'grove-tree-directory
                                       'grove-tree-file))
              (propertize name 'face (cond
                                      (current-p 'grove-tree-current)
                                      (dir-p 'grove-tree-directory)
                                      (t 'grove-tree-file)))
              (if count
                  (propertize (format " (%d)" count) 'face 'grove-tree-guide)
                "")))))

(defun grove-tree--list-entries (directory depth)
  "Return a sorted list of `grove-tree-node' structs for DIRECTORY at DEPTH.
Directories come first, then files.  Hidden files are excluded."
  (let (dirs files)
    (dolist (file (directory-files directory t))
      (let ((name (file-name-nondirectory file)))
        (unless (string-prefix-p "." name)
          (if (file-directory-p file)
              (push (make-grove-tree-node
                     :path file
                     :name name
                     :depth depth
                     :directory-p t
                     :expanded-p nil)
                    dirs)
            (when (string-suffix-p ".org" name)
              (push (make-grove-tree-node
                     :path file
                     :name (file-name-sans-extension name)
                     :depth depth
                     :directory-p nil
                     :expanded-p nil)
                    files))))))
    (append (sort dirs (lambda (a b)
                         (string< (grove-tree-node-name a)
                                  (grove-tree-node-name b))))
            (sort files (lambda (a b)
                          (string< (grove-tree-node-name a)
                                   (grove-tree-node-name b)))))))

;;;; Tree operations

(defun grove-tree--node-at-point ()
  "Return the ewoc node at point, or nil."
  (and grove-tree--ewoc
       (ewoc-locate grove-tree--ewoc)))

(defun grove-tree--has-children-p (ewoc-node node)
  "Return non-nil if EWOC-NODE has visible children in the ewoc."
  (let ((next (ewoc-next grove-tree--ewoc ewoc-node)))
    (and next
         (> (grove-tree-node-depth (ewoc-data next))
            (grove-tree-node-depth node)))))

(defun grove-tree--toggle-expand ()
  "Toggle expand/collapse for the directory node at point."
  (interactive)
  (let ((inhibit-read-only t)
        (ewoc-node (grove-tree--node-at-point)))
    (when ewoc-node
      (let ((node (ewoc-data ewoc-node)))
        (when (grove-tree-node-directory-p node)
          (if (grove-tree--has-children-p ewoc-node node)
              (grove-tree--collapse ewoc-node node)
            (grove-tree--expand ewoc-node node))
          (ewoc-invalidate grove-tree--ewoc ewoc-node))))))

(defun grove-tree--expand (ewoc-node node)
  "Expand NODE by inserting its children after EWOC-NODE."
  (puthash (grove-tree-node-path node) t grove-tree--expanded)
  (let ((children (grove-tree--list-entries
                   (grove-tree-node-path node)
                   (1+ (grove-tree-node-depth node))))
        (prev ewoc-node))
    (dolist (child children)
      (setq prev (ewoc-enter-after grove-tree--ewoc prev child)))))

(defun grove-tree--collapse (ewoc-node node)
  "Collapse NODE by removing all its descendants after EWOC-NODE."
  (let ((next (ewoc-next grove-tree--ewoc ewoc-node))
        (target-depth (grove-tree-node-depth node)))
    (while (and next
                (> (grove-tree-node-depth (ewoc-data next)) target-depth))
      (let ((to-delete next))
        (setq next (ewoc-next grove-tree--ewoc next))
        (ewoc-delete grove-tree--ewoc to-delete))))
  (remhash (grove-tree-node-path node) grove-tree--expanded))

(defun grove-tree--preview ()
  "Preview the file at point in the main window without switching focus."
  (interactive)
  (let ((ewoc-node (grove-tree--node-at-point)))
    (when ewoc-node
      (let ((node (ewoc-data ewoc-node)))
        (unless (grove-tree-node-directory-p node)
          (let ((path (grove-tree-node-path node))
                (win (or (grove-tree--main-window) (next-window))))
            (with-selected-window win
              (find-file path))))))))

(defun grove-tree--open-file ()
  "Open the file at point in the main window and focus it."
  (interactive)
  (let ((ewoc-node (grove-tree--node-at-point)))
    (when ewoc-node
      (let ((node (ewoc-data ewoc-node)))
        (if (grove-tree-node-directory-p node)
            (grove-tree--toggle-expand)
          (let ((path (grove-tree-node-path node)))
            (grove-tree--set-current-file path)
            (select-window
             (or (grove-tree--main-window)
                 (next-window)))
            (find-file path)))))))

(defun grove-tree--main-window ()
  "Return the main (non-sidebar) window, or nil."
  (let ((tree-win (get-buffer-window grove-tree-buffer-name)))
    (catch 'found
      (walk-windows
       (lambda (win)
         (unless (eq win tree-win)
           (unless (window-parameter win 'window-side)
             (throw 'found win))))))))

;;;; Navigation

(defun grove-tree--skip-blank-lines (direction)
  "Move in DIRECTION (1 or -1) skipping blank lines."
  (forward-line direction)
  (while (and (not (eobp)) (not (bobp))
              (looking-at-p "^$"))
    (forward-line direction)))

(defun grove-tree-next ()
  "Move to the next entry and preview it."
  (interactive)
  (grove-tree--skip-blank-lines 1)
  (grove-tree--preview))

(defun grove-tree-previous ()
  "Move to the previous entry and preview it."
  (interactive)
  (grove-tree--skip-blank-lines -1)
  (grove-tree--preview))

;;;; Refresh

(defun grove-tree-refresh ()
  "Rebuild the tree sidebar from scratch."
  (interactive)
  (grove--ensure-directory)
  (let ((buf (get-buffer grove-tree-buffer-name)))
    (when buf
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (setq grove-tree--ewoc
                (ewoc-create #'grove-tree--print "" ""))
          (dolist (node (grove-tree--list-entries grove-directory 0))
            (ewoc-enter-last grove-tree--ewoc node)))))))

;;;; Current file tracking

(defun grove-tree--set-current-file (file)
  "Set FILE as the current file and refresh the tree display."
  (let ((buf (get-buffer grove-tree-buffer-name)))
    (when (and buf (buffer-live-p buf))
      (with-current-buffer buf
        (unless (equal file grove-tree--current-file)
          (setq grove-tree--current-file file)
          (when grove-tree--ewoc
            (let ((inhibit-read-only t)
                  (pos (point)))
              (ewoc-refresh grove-tree--ewoc)
              (goto-char pos)
              (hl-line-highlight))))))))

;;;; Mode

(defvar grove-tree-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'grove-tree--open-file)
    (define-key map (kbd "TAB") #'grove-tree--toggle-expand)
    (define-key map (kbd "g") #'grove-tree-refresh)
    (define-key map (kbd "q") #'grove-tree-close)
    (define-key map (kbd "n") #'grove-tree-next)
    (define-key map (kbd "p") #'grove-tree-previous)
    (define-key map (kbd "C-n") #'grove-tree-next)
    (define-key map (kbd "C-p") #'grove-tree-previous)
    map)
  "Keymap for `grove-tree-mode'.")

(define-derived-mode grove-tree-mode special-mode "Grove-Tree"
  "Major mode for the grove file tree sidebar."
  :group 'grove
  (setq-local cursor-type nil
              truncate-lines t
              mode-line-format nil
              header-line-format (propertize " Grove" 'face 'bold))
  (hl-line-mode 1))

;;;; Open / Close

(defun grove-tree-open ()
  "Open the tree sidebar for the grove vault."
  (grove--ensure-directory)
  (let ((buf (get-buffer-create grove-tree-buffer-name)))
    (with-current-buffer buf
      (grove-tree-mode)
      (grove-tree-refresh)
      (let ((main-win (grove-tree--main-window)))
        (when main-win
          (grove-tree--set-current-file
           (buffer-file-name (window-buffer main-win))))))
    (let ((win (display-buffer-in-side-window
                buf
                `((side . left)
                  (slot . 0)
                  (window-width . ,grove-tree-width)
                  (window-parameters
                   . ((no-other-window . nil)
                      (no-delete-other-windows . t)))))))
      (when win
        (window-preserve-size win t t))
      buf)))

(defun grove-tree-close ()
  "Close the tree sidebar."
  (interactive)
  (let ((win (get-buffer-window grove-tree-buffer-name)))
    (when win
      (delete-window win)))
  (when-let ((buf (get-buffer grove-tree-buffer-name)))
    (kill-buffer buf)))

(provide 'grove-tree)
;;; grove-tree.el ends here
