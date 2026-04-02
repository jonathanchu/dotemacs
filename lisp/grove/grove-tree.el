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

(defconst grove-tree-buffer-name "*grove-tree*"
  "Name of the tree sidebar buffer.")

(defun grove-tree--print (node)
  "Print NODE as a line in the ewoc buffer."
  (let* ((depth (grove-tree-node-depth node))
         (indent (make-string (* depth 2) ?\s))
         (dir-p (grove-tree-node-directory-p node))
         (expanded (grove-tree-node-expanded-p node))
         (name (grove-tree-node-name node))
         (marker (cond
                  ((not dir-p) "  ")
                  (expanded "▾ ")
                  (t "▸ "))))
    (insert indent
            (propertize marker 'face 'grove-tree-marker)
            (propertize name 'face (if dir-p
                                       'grove-tree-directory
                                     'grove-tree-file)))))

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

(defun grove-tree--toggle-expand ()
  "Toggle expand/collapse for the directory node at point."
  (let ((ewoc-node (grove-tree--node-at-point)))
    (when ewoc-node
      (let ((node (ewoc-data ewoc-node)))
        (when (grove-tree-node-directory-p node)
          (if (grove-tree-node-expanded-p node)
              (grove-tree--collapse ewoc-node node)
            (grove-tree--expand ewoc-node node))
          (ewoc-invalidate grove-tree--ewoc ewoc-node))))))

(defun grove-tree--expand (ewoc-node node)
  "Expand NODE by inserting its children after EWOC-NODE."
  (setf (grove-tree-node-expanded-p node) t)
  (let ((children (grove-tree--list-entries
                   (grove-tree-node-path node)
                   (1+ (grove-tree-node-depth node))))
        (prev ewoc-node))
    (dolist (child children)
      (setq prev (ewoc-enter-after grove-tree--ewoc prev child)))))

(defun grove-tree--collapse (ewoc-node node)
  "Collapse NODE by removing all its descendants after EWOC-NODE."
  (setf (grove-tree-node-expanded-p node) nil)
  (let ((next (ewoc-next grove-tree--ewoc ewoc-node))
        (target-depth (grove-tree-node-depth node)))
    (while (and next
                (> (grove-tree-node-depth (ewoc-data next)) target-depth))
      (let ((to-delete next))
        (setq next (ewoc-next grove-tree--ewoc next))
        (ewoc-delete grove-tree--ewoc to-delete)))))

(defun grove-tree--open-file ()
  "Open the file at point in the main window."
  (let ((ewoc-node (grove-tree--node-at-point)))
    (when ewoc-node
      (let ((node (ewoc-data ewoc-node)))
        (if (grove-tree-node-directory-p node)
            (grove-tree--toggle-expand)
          (let ((path (grove-tree-node-path node)))
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
                (ewoc-create #'grove-tree--print "" "" t))
          (dolist (node (grove-tree--list-entries grove-directory 0))
            (ewoc-enter-last grove-tree--ewoc node)))))))

;;;; Mode

(defvar grove-tree-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'grove-tree--open-file)
    (define-key map (kbd "TAB") #'grove-tree--toggle-expand)
    (define-key map (kbd "g") #'grove-tree-refresh)
    (define-key map (kbd "q") #'grove-tree-close)
    (define-key map (kbd "n") #'next-line)
    (define-key map (kbd "p") #'previous-line)
    map)
  "Keymap for `grove-tree-mode'.")

(define-derived-mode grove-tree-mode special-mode "Grove-Tree"
  "Major mode for the grove file tree sidebar."
  :group 'grove
  (setq-local cursor-type nil
              truncate-lines t
              mode-line-format nil
              header-line-format (propertize " Grove" 'face 'bold)))

;;;; Open / Close

(defun grove-tree-open ()
  "Open the tree sidebar for the grove vault."
  (grove--ensure-directory)
  (let ((buf (get-buffer-create grove-tree-buffer-name)))
    (with-current-buffer buf
      (grove-tree-mode)
      (grove-tree-refresh))
    (display-buffer-in-side-window
     buf
     `((side . left)
       (slot . 0)
       (window-width . ,grove-tree-width)
       (window-parameters
        . ((no-other-window . nil)
           (no-delete-other-windows . t)))))
    buf))

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
