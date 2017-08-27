;;; core-ui.el

;; only type 'y' or 'n' instead of 'yes' or 'no'
(fset 'yes-or-no-p 'y-or-n-p)

;; no menu bar
(menu-bar-mode -1)

;; no toolbar
(when (functionp 'tool-bar-mode)
  (tool-bar-mode -1))  ;; no toolbar

;; disable scroll bars
(if window-system
    (progn
      (scroll-bar-mode -1)
      ;;(set-frame-font "Inconsolata 15"))) ;; set font
      ))

(setq-default cursor-type 'bar)

;; nice fonts in OS X
(setq mac-allow-anti-aliasing t)

;; show line number in mode line
(line-number-mode 1)

;; show column number in the mode line
(column-number-mode 1)

;; highlight current line
(global-hl-line-mode +1)

(provide 'core-ui)

;;; core-ui.el ends here
