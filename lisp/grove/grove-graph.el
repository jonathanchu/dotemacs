;;; grove-graph.el --- Graphviz-based graph view for grove -*- lexical-binding: t -*-

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

;; Graph view for grove using Graphviz.  Extracts wikilinks via ripgrep,
;; generates a DOT graph, and renders it as SVG for display in Emacs.

;;; Code:

(require 'grove)

;;;; Customization

(defcustom grove-graph-executable "dot"
  "Path to the Graphviz dot executable."
  :type 'string
  :group 'grove)

(defcustom grove-graph-layout "neato"
  "Graphviz layout engine.
Common options: \"dot\" (hierarchical), \"neato\" (force-directed),
\"fdp\" (force-directed), \"sfdp\" (scalable force-directed)."
  :type '(choice (const "dot")
                 (const "neato")
                 (const "fdp")
                 (const "sfdp"))
  :group 'grove)

(defcustom grove-graph-node-color "#89b4fa"
  "Fill color for graph nodes."
  :type 'string
  :group 'grove)

(defcustom grove-graph-edge-color "#585b70"
  "Color for graph edges."
  :type 'string
  :group 'grove)

(defcustom grove-graph-bg-color "#1e1e2e"
  "Background color for the graph."
  :type 'string
  :group 'grove)

(defcustom grove-graph-display 'auto
  "How to display the graph buffer.
`auto' uses a right side window when the frame is wide enough,
otherwise opens a full buffer.  `side' always uses a right side
window.  `buffer' always opens a full buffer."
  :type '(choice (const :tag "Auto (side if wide enough)" auto)
                 (const :tag "Right side window" side)
                 (const :tag "Full buffer" buffer))
  :group 'grove)

(defcustom grove-graph-min-width 160
  "Minimum frame width (in columns) for auto side window display."
  :type 'integer
  :group 'grove)

;;;; Build adjacency list

(defun grove-graph--adjacency-list ()
  "Build an adjacency list from wikilinks in the vault via ripgrep.
Returns an alist of (SOURCE-TITLE . (TARGET-TITLE ...))."
  (grove--ensure-directory)
  (grove--refresh-cache)
  (let ((adjacency (make-hash-table :test #'equal))
        (all-titles (make-hash-table :test #'equal)))
    ;; Collect all note titles
    (maphash (lambda (_path meta)
               (puthash (plist-get meta :title) t all-titles))
             grove--cache)
    ;; Build edges from cached link data
    (maphash (lambda (_path meta)
               (let ((source (plist-get meta :title))
                     (links (plist-get meta :links)))
                 (dolist (target links)
                   (when (gethash target all-titles)
                     (push target (gethash source adjacency))))))
             grove--cache)
    ;; Convert to alist and include isolated nodes
    (let (result)
      (maphash (lambda (title _)
                 (push (cons title (gethash title adjacency)) result))
               all-titles)
      result)))

;;;; Generate DOT

(defun grove-graph--dot-escape (str)
  "Escape STR for use in a DOT label."
  (replace-regexp-in-string "\"" "\\\\\"" str))

(defun grove-graph--generate-dot (adjacency)
  "Generate a DOT graph string from ADJACENCY list."
  (let ((node-id (make-hash-table :test #'equal))
        (counter 0))
    ;; Assign IDs
    (dolist (entry adjacency)
      (unless (gethash (car entry) node-id)
        (puthash (car entry) (format "n%d" counter) node-id)
        (cl-incf counter)))
    (with-temp-buffer
      (insert "graph vault {\n")
      (insert (format "  bgcolor=\"%s\";\n" grove-graph-bg-color))
      (insert "  overlap=false;\n")
      (insert "  splines=true;\n")
      (insert (format "  node [shape=box style=\"filled,rounded\" fillcolor=\"%s\" "
                      grove-graph-node-color))
      (insert "fontcolor=\"#1e1e2e\" fontname=\"sans-serif\" fontsize=11];\n")
      (insert (format "  edge [color=\"%s\"];\n" grove-graph-edge-color))
      ;; Nodes
      (dolist (entry adjacency)
        (let ((id (gethash (car entry) node-id))
              (label (grove-graph--dot-escape (car entry))))
          (insert (format "  %s [label=\"%s\"];\n" id label))))
      ;; Edges
      (let ((seen (make-hash-table :test #'equal)))
        (dolist (entry adjacency)
          (let ((source-id (gethash (car entry) node-id)))
            (dolist (target (cdr entry))
              (let* ((target-id (gethash target node-id))
                     (edge-key (if (string< source-id target-id)
                                   (concat source-id "--" target-id)
                                 (concat target-id "--" source-id))))
                (when (and target-id (not (gethash edge-key seen)))
                  (puthash edge-key t seen)
                  (insert (format "  %s -- %s;\n" source-id target-id))))))))
      (insert "}\n")
      (buffer-string))))

;;;; Render

(defun grove-graph--render-svg (dot-string)
  "Render DOT-STRING to SVG using Graphviz.  Returns the SVG string."
  (unless (executable-find grove-graph-executable)
    (user-error "Graphviz not found.  Install it and ensure `%s' is on your PATH"
                grove-graph-executable))
  (with-temp-buffer
    (let ((exit-code
           (call-process-region dot-string nil
                                grove-graph-executable
                                nil t nil
                                (format "-K%s" grove-graph-layout)
                                "-Tsvg")))
      (unless (zerop exit-code)
        (user-error "Graphviz failed (exit %d): %s" exit-code (buffer-string)))
      (buffer-string))))

;;;; Command

;;;###autoload
(defun grove-graph ()
  "Display a graph of notes and their links in the vault."
  (interactive)
  (grove--ensure-directory)
  (message "Building graph...")
  (let* ((adjacency (grove-graph--adjacency-list))
         (dot (grove-graph--generate-dot adjacency))
         (svg (grove-graph--render-svg dot))
         (buf (get-buffer-create "*grove-graph*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert svg)
        (image-mode)))
    (grove-graph--display buf)
    (message "Graph: %d notes, %d links"
             (length adjacency)
             (cl-reduce #'+ (mapcar (lambda (e) (length (cdr e))) adjacency)))))

(defun grove-graph--use-side-window-p ()
  "Return non-nil if the graph should display in a side window."
  (pcase grove-graph-display
    ('side t)
    ('buffer nil)
    ('auto (>= (frame-width) grove-graph-min-width))))

(defun grove-graph--display (buf)
  "Display graph buffer BUF according to `grove-graph-display'."
  (if (grove-graph--use-side-window-p)
      (display-buffer-in-side-window
       buf
       '((side . right)
         (slot . 0)
         (window-width . 0.4)
         (window-parameters
          . ((no-delete-other-windows . t)))))
    (switch-to-buffer buf)))

(provide 'grove-graph)
;;; grove-graph.el ends here
