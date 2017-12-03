;;; core-ui.el

;; ;; only type 'y' or 'n' instead of 'yes' or 'no'
;; (fset 'yes-or-no-p 'y-or-n-p)

;; ;; no menu bar
;; (menu-bar-mode -1)

;; ;; no toolbar
;; (when (functionp 'tool-bar-mode)
;;   (tool-bar-mode -1))  ;; no toolbar

;; ;; disable scroll bars
;; (if window-system
;;     (progn
;;       (scroll-bar-mode -1)
;;       ;;(set-frame-font "Inconsolata 15"))) ;; set font
;;       ))

;; (setq-default cursor-type 'bar)

;; ;; nice fonts in OS X
;; (setq mac-allow-anti-aliasing t)

;; ;; show line number in mode line
;; (line-number-mode 1)

;; ;; show column number in the mode line
;; (column-number-mode 1)

;; ;; highlight current line
;; (global-hl-line-mode +1)

;; (add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono for Powerline-13"))

;; ;; mode line modifications based on powerline
;; (defvar mode-line-height 30)
;; (defvar-local doom--env-version nil)
;; (defvar-local doom--env-command nil)

;; (eval-when-compile (require 'powerline))
;; (defvar mode-line-bar          (pl/percent-xpm mode-line-height 100 0 100 0 3 "#00B3EF" nil))
;; (defvar mode-line-eldoc-bar    (pl/percent-xpm mode-line-height 100 0 100 0 3 "#B3EF00" nil))
;; (defvar mode-line-inactive-bar (pl/percent-xpm mode-line-height 100 0 100 0 3 nil nil))

;; (defface mode-line-is-modified nil "Face for mode-line modified symbol")
;; (defface mode-line-buffer-path nil "Face for mode-line buffer file path")
;; (defface mode-line-highlight nil "")
;; (defface mode-line-2 nil "")

;; ;;
;; ;; Mode-line segments
;; ;;

;; (defun *buffer-path ()
;;   (when buffer-file-name
;;     (propertize
;;      (f-dirname
;;       (let ((buffer-path (file-relative-name buffer-file-name (doom/project-root)))
;;             (max-length (truncate (/ (window-body-width) 1.75))))
;;         (concat (projectile-project-name) "/"
;;                 (if (> (length buffer-path) max-length)
;;                     (let ((path (reverse (split-string buffer-path "/" t)))
;;                           (output ""))
;;                       (when (and path (equal "" (car path)))
;;                         (setq path (cdr path)))
;;                       (while (and path (<= (length output) (- max-length 4)))
;;                         (setq output (concat (car path) "/" output))
;;                         (setq path (cdr path)))
;;                       (when path
;;                         (setq output (concat "../" output)))
;;                       (when (string-suffix-p "/" output)
;;                         (setq output (substring output 0 -1)))
;;                       output)
;;                   buffer-path))))
;;      'face (if active 'mode-line-buffer-path))))

;; (defun *buffer-state ()
;;   (when buffer-file-name
;;     (propertize
;;      (concat (if (not (file-exists-p buffer-file-name))
;;                  "∄"
;;                (if (buffer-modified-p) "✱"))
;;              (if buffer-read-only ""))
;;      'face 'mode-line-is-modified)))

;; (defun *buffer-name ()
;;   "The buffer's name."
;;   (s-trim-left (format-mode-line "%b")))

;; (defun *buffer-pwd ()
;;   "Displays `default-directory'."
;;   (propertize
;;    (concat "[" (abbreviate-file-name default-directory) "]")
;;    'face 'mode-line-2))

;; (defun *major-mode ()
;;   "The major mode, including process, environment and text-scale info."
;;   (concat (format-mode-line mode-name)
;;           (if (stringp mode-line-process) mode-line-process)
;;           (if doom--env-version (concat " " doom--env-version))
;;           (and (featurep 'face-remap)
;;                (/= text-scale-mode-amount 0)
;;                (format " (%+d)" text-scale-mode-amount))))


(provide 'core-ui)

;;; core-ui.el ends here
