;;; core-packages.el
(require 'package)

(setq load-prefer-newer t
      package-enable-at-startup nil
      package-user-dir (concat user-emacs-directory "elpa")
      package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")))

(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; use-package
(setq use-package-enable-imenu-support t)
(eval-when-compile
  (require 'use-package))
(setq use-package-verbose t)
(require 'diminish)
(require 'bind-key)

(provide 'core-packages)
;;; core-packages.el ends here