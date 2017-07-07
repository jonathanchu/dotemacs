;;; core.el --- Nothing much to see here yet.

(defvar core-dir (concat user-emacs-directory "core/")
  "Where the core files are stored.")

;; set encoding
(prefer-coding-system 'utf-8)

;; and tell emacs to play nice with encoding
(define-coding-system-alias 'UTF-8 'utf-8)
(define-coding-system-alias 'utf8 'utf-8)

;; debugging
;; (setq debug-on-error t)

;; save nothing
(setq auto-save-default nil
      create-lockfiles nil)

;; no splash screen
(setq inhibit-splash-screen t)

;; no message on startup
(setq initial-scratch-message nil)


;;;
;; Bootstrap
(require 'cl-lib)

;; Reduce the frequency of garbage collection by making it happen on
;; each 25MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 25000000)
(setq gc-cons-percentage 0.6)

(provide 'core)
;;; core.el ends here
