;;; core-os.el

; set paths from shell
(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns))
  :config
  (exec-path-from-shell-initialize))

(provide 'core-os)
;;; core-os.el ends here
