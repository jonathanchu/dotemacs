;;; core-editor.el

;; ;; Warn when opening files bigger than 100MB
;; (setq large-file-warning-threshold 100000000)

;; ;; M-q
;; (setq fill-column 80)

;; ;; no word wrap
;; (setq-default truncate-lines t)

;; (setq-default line-spacing 4)

;; ;; no tabs
;; (setq-default indent-tabs-mode nil)

;; (setq ring-bell-function 'ignore)

;; ;; show extra whitespace
;; (setq show-trailing-whitespace t)

;; ;; ensure last line is a return
;; (setq require-final-newline t)

;; ;; show file size
;; (size-indication-mode t)

;; ;; make sure looking at most recent changes
;; (global-auto-revert-mode t)

;; (setq window-combination-resize t)

;; ;;keep cursor at same position when scrolling
;; (setq scroll-preserve-screen-position t)

;; ;; scroll one line at a time
;; (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
;; (setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
;; (setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
;; (setq scroll-step 1) ;; keyboard scroll one line at a time
;; (setq scroll-conservatively 10000)
;; (setq scroll-margin 3)

;; ;; open with in original frame, not new window
;; (setq ns-pop-up-frames nil)

;; ;; sentences end with single space
;; (setq sentence-end-double-space nil)

;; ;; useful for camelCase
;; (subword-mode t)

;; ;; delete selection, insert text
;; (delete-selection-mode t)

;; ;; prevent active process query on quit
;; (require 'cl)
;; (defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
;;   (cl-flet ((process-list ())) ad-do-it))

;; ;; instantly display current key sequence in mini buffer
;; (setq echo-keystrokes 0.02)

;; ;; desktop save mode
;; (desktop-save-mode t)
;; (setq desktop-restore-eager 5)
;; (setq desktop-save t)

;; (setq initial-major-mode 'emacs-lisp-mode)

;; ;; improve filename completion
;; (setq read-file-name-completion-ignore-case t)
;; (setq read-buffer-completion-ignore-case t)
;; (mapc (lambda (x)
;;         (add-to-list 'completion-ignored-extensions x))
;;       '(".gz" ".pyc" ".elc" ".exe"))

;; ;; Suppress warnings for functions redefined with defadvice
;; (setq ad-redefinition-action 'accept)

;; (setq tab-always-indent 'complete)

;; ;; try to improve handling of long lines
;; (setq bidi-display-reordering nil)

;; ;; delete trailing whitespace in all modes
;; (add-hook 'before-save-hook #'delete-trailing-whitespace)

(provide 'core-editor)
;;; core-editor.el ends here
