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

(defconst emacs-start-time (current-time))

(setq user-full-name "Jonathan Chu"
      user-mail-address "me@jonathanchu.is")

(add-to-list 'exec-path "/usr/local/bin")

(defvar current-user
  (getenv
   (if (equal system-type 'windows-nt) "USERNAME" "USER")))

(message "Your Emacs is powering up... Be patient, Master %s!" current-user)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
