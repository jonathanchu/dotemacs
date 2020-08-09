(defun reload-config()
  "Reload Emacs configuration"
  (interactive)
  (load-file "~/.emacs.d/init.el"))

(use-package cus-edit
  :defer t
  :config
  (setq custom-file "~/.emacs.d/custom.el")

  (unless (file-exists-p custom-file)
    (write-region "" nil custom-file))

  (load custom-file))
