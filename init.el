;; init.el --- My personal Emacs configuration.
;;
;; Copyright (c) 2015-2020
;;
;; Author: Jonathan Chu <me@jonathanchu.is>
;; URL: https://github.com/jonathanchu/dotemacs
;; Version: 1.0

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This is the whole #!

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

;;----------------------------------------------------------------------------
;; Bootstrapping
;;----------------------------------------------------------------------------

;; load literate config first
(org-babel-load-file (expand-file-name "~/.emacs.d/emacs-init.org"))

(require 'init-markdown)
(require 'init-paredit)
(require 'init-highlight)
(require 'init-smex)
(require 'init-yasnippet)
(require 'init-funcs)

(require 'elegant)

;;----------------------------------------------------------------------------
;; Global Config
;;----------------------------------------------------------------------------

;; FIXME
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#1B2229" "#ff6c6b" "#98be65" "#ECBE7B" "#51afef" "#c678dd" "#46D9FF" "#DFDFDF"])
 '(custom-safe-themes
   '("99ea831ca79a916f1bd789de366b639d09811501e8c092c85b2cb7d697777f93" "6b2636879127bf6124ce541b1b2824800afc49c6ccd65439d6eb987dbf200c36" default))
 '(fci-rule-color "#5B6268")
 '(jdee-db-active-breakpoint-face-colors (cons "#1B2229" "#51afef"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1B2229" "#98be65"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1B2229" "#3f444a"))
 '(notmuch-saved-searches
   '((:name "inbox" :query "tag:inbox" :key "i" :sort-order newest-first)
     (:name "unread" :query "tag:unread" :key "u" :sort-order newest-first)
     (:name "flagged" :query "tag:flagged" :key "f" :sort-order newest-first)
     (:name "sent" :query "tag:sent" :key "t" :sort-order newest-first)
     (:name "drafts" :query "tag:draft" :key "d" :sort-order newest-first)
     (:name "all mail" :query "*" :key "a" :sort-order newest-first)))
 '(package-selected-packages
   '(cus-edit notmuch nav-flash dired-narrow deadgrep ivy-posframe rg vterm org-ql uuidgen elpy forge org-present org-plus-contrib clj-refactor cider org org-bullets org-journal cask-mode centaur-tabs go-mode toml-mode helm-projectile projectile edit-indirect typo-mode indium olivetti ox-hugo-auto-export ox-hugo which-key counsel-projectile beginend jinja2-mode company-lsp lsp-javascript-flow lsp-ui lsp-javascript lsp-mode all-the-icons-dired yaml-mode web-mode volatile-highlights smex smartparens smart-comment scratch rjsx-mode restclient rainbow-mode rainbow-delimiters python-mode popwin paredit paradox paperless origami neotree move-text markdown-mode magit key-chord json-mode js2-refactor js2-mode ido-vertical-mode ibuffer-vc helm-ag helm gitignore-mode github-browse-file gitconfig-mode git-timemachine git-messenger fullframe flycheck-pos-tip flow-minor-mode fix-word fish-mode fireplace expand-region esup elm-mode easy-kill dumb-jump dired-single deft counsel company command-log-mode clojure-mode anzu aggressive-indent add-node-modules-path ace-window doom-themes use-package powerline popup nlinum git-gutter-fringe f exec-path-from-shell diminish all-the-icons))
 '(paradox-github-token t)
 '(smtpmail-smtp-server "smtp.fastmail.com")
 '(smtpmail-smtp-service 587)
 '(vc-annotate-background "#282c34")
 '(vc-annotate-color-map
   (list
    (cons 20 "#98be65")
    (cons 40 "#b4be6c")
    (cons 60 "#d0be73")
    (cons 80 "#ECBE7B")
    (cons 100 "#e6ab6a")
    (cons 120 "#e09859")
    (cons 140 "#da8548")
    (cons 160 "#d38079")
    (cons 180 "#cc7cab")
    (cons 200 "#c678dd")
    (cons 220 "#d974b7")
    (cons 240 "#ec7091")
    (cons 260 "#ff6c6b")
    (cons 280 "#cf6162")
    (cons 300 "#9f585a")
    (cons 320 "#6f4e52")
    (cons 340 "#5B6268")
    (cons 360 "#5B6268")))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(centaur-tabs-active-bar-face ((t (:inherit doom-modeline-bar))))
 '(org-ellipsis ((t (:foreground unspecified)))))

;;----------------------------------------------------------------------------
;; Libraries
;;----------------------------------------------------------------------------

(use-package f
  :ensure t
  :defer t)

;;----------------------------------------------------------------------------
;; Packages
;;----------------------------------------------------------------------------

(use-package command-log-mode
  :ensure t)

(use-package deft
  :ensure t
  :config
  (progn
    (setq deft-directory "~/Dropbox/org")
    (setq deft-extensions '("org" "txt"))
    (setq deft-default-extension "org")
    (setq deft-org-mode-title-prefix t)
    (setq deft-use-filename-as-title t)
    (setq deft-auto-save-interval 0)))

(use-package ibuffer
  :bind
  ("C-x C-b" . ibuffer))

(use-package ibuffer-vc
  :ensure t
  :defer t
  :init
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-vc-set-filter-groups-by-vc-root)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic)))))

(use-package ox-hugo
  :ensure t
  :after ox)

(use-package find-file-in-project
  :disabled
  :ensure t
  :bind ("s-t" . find-file-in-project))

(use-package ivy-posframe
  :ensure t
  :after ivy
  :diminish
  :config
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-top-center))
        ivy-posframe-height-alist '((t . 20))
        ivy-posframe-parameters '((internal-border-width . 1)
                                  (internal-border-color . "black")
                                  ))
  (setq ivy-posframe-width 70)
  (setq posframe-mouse-banish t)
  (setq ivy-posframe-border-width 1)
  (ivy-posframe-mode +1))

(setq-default mac-option-modifier 'meta)
(setq-default mac-command-modifier 'super)

(use-package deadgrep
  :ensure t
  :bind
  ("s-F" . deadgrep))

;; narrow dired to match filter
(use-package dired-narrow
  :ensure t
  :bind
  (:map dired-mode-map
        ("/" . dired-narrow)))

(use-package nav-flash
  :ensure t
  :config
  (nav-flash-show))

(use-package elpy
  :ensure t
  :defer t
  :init
  (elpy-enable))

(use-package vterm
  :ensure t)


(add-to-list 'default-frame-alist '(internal-border-width . 20))
(defun mode-line-align (left right)
  "Return a string with LEFT and RIGHT at the edges of the
current window."
  (format (format "%%s %%%ds" (- (window-total-width) (length left) 2))
          left right))

(setq-default mode-line-format
              '(:eval
                (mode-line-align
                 (format-mode-line
                  (list " " mode-line-buffer-identification
                        " " mode-line-modified
                        " " mode-name))
                 (format-mode-line
                  (list minor-mode-alist
                        " " mode-line-misc-info)))))

;;; When we set a face, we take care of removing any previous settings
;;; -------------------------------------------------------------------
(defun set-face (face style)
  "Reset a FACE and make it inherit STYLE."
  (set-face-attribute face nil
                      :foreground 'unspecified :background 'unspecified
                      :family     'unspecified :slant      'unspecified
                      :weight     'unspecified :height     'unspecified
                      :underline  'unspecified :overline   'unspecified
                      :box        'unspecified :inherit    style))
;;; -------------------------------------------------------------------


(defun set-modeline-faces ()
  "Mode line at top."
  (set-face 'header-line                                 'face-strong)
  (set-face-attribute 'header-line nil
                      :underline (face-foreground 'default))
  (set-face-attribute 'mode-line nil
                      :height 10
                      :underline (face-foreground 'default)
                      :overline nil
                      :box nil
                      :foreground (face-background 'default)
                      :background (face-background 'default))
  (set-face 'mode-line-inactive                            'mode-line)
  (set-face-attribute 'cursor nil
                      :background (face-foreground 'default))
  (set-face-attribute 'window-divider nil
                      :foreground (face-background 'mode-line))
  (set-face-attribute 'window-divider-first-pixel nil
                      :foreground (face-background 'default))
  (set-face-attribute 'window-divider-last-pixel nil
                      :foreground (face-background 'default)))


(set-modeline-faces)

(setq frame-resize-pixelwise t)

;;----------------------------------------------------------------------------
;; Finalization
;;----------------------------------------------------------------------------

(when window-system
  (let ((elapsed (float-time (time-subtract (current-time)
                                            emacs-start-time))))
    (message "Loading %s...done (%.3fs)" load-file-name elapsed))

  (add-hook 'after-init-hook
            `(lambda ()
               (let ((elapsed
                      (float-time
                       (time-subtract (current-time) emacs-start-time))))
                 (message "Loading %s...done (%.3fs) [after-init]"
                          ,load-file-name elapsed))) t))

;;; init.el ends here
