;;; core-editor.el

;; Warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; M-q
(setq fill-column 80)

;; no word wrap
(setq-default truncate-lines t)

(setq-default line-spacing 4)

;; no tabs
(setq-default indent-tabs-mode nil)

(setq ring-bell-function 'ignore)

;; show extra whitespace
(setq show-trailing-whitespace t)

;; ensure last line is a return
(setq require-final-newline t)

;; show file size
(size-indication-mode t)

;; make sure looking at most recent changes
(global-auto-revert-mode t)

(setq window-combination-resize t)

;;keep cursor at same position when scrolling
(setq scroll-preserve-screen-position t)

;; scroll one line at a time
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
(setq scroll-conservatively 10000)
(setq scroll-margin 3)

(provide 'core-editor)
;;; core-editor.el ends here
