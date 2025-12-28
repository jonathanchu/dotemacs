;; init-email.el --- My custom email config.
;;
;; Copyright (c) 2019-2020
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
;; Email
;;----------------------------------------------------------------------------

(setq send-mail-function 'sendmail-send-it
      sendmail-program "/usr/local/bin/msmtp"
      mail-specify-envelope-from t
      message-sendmail-envelope-from 'header
      mail-envelope-from 'header)

(use-package notmuch
  :ensure t
  :defer t
  :bind
  ("C-c m" . notmuch))

(add-hook 'message-setup-hook
          (lambda ()
            (gnus-alias-determine-identity)
            (define-key message-mode-map (kbd "C-c f")
              (lambda ()
                (interactive)
                (message-remove-header "Fcc")
                (message-remove-header "Organization")
                (gnus-alias-select-identity)
                (notmuch-fcc-header-setup)))
            (flyspell-mode)))

(use-package gnus-alias
  :ensure t
  :defer t
  :config
  (setq gnus-alias-identity-alist
        '(("personal"
           nil ;; Does not refer to any other identity
           "Jonathan Chu <me@jonathanchu.is>"
           nil ;; No organization header
           nil ;; No extra headers
           nil ;; No extra body text
           nil)
          ("simplehealth"
           nil
           "Jonathan Chu <jchu@simplehealth.com>"
           nil ;; No organization header
           nil ;; No extra headers
           nil ;; No extra body text
           nil)
          ("me.com"
           nil
           "Jonathan Chu <jonathan.chu@me.com>"
           nil ;; No organization header
           nil ;; No extra headers
           nil ;; No extra body text
           nil)
          ("fastmail"
           nil
           "Jonathan Chu <jonathanchu@fastmail.com>"
           nil ;; No organization header
           nil ;; No extra headers
           nil ;; No extra body text
           nil)
          ("3atmospheres"
           nil
           "Jonathan Chu <jc@3atmospheres.com>"
           nil ;; No organization header
           nil ;; No extra headers
           nil ;; No extra body text
           nil)
          ))
  (setq gnus-alias-default-identity "personal")
  (setq gnus-alias-identity-rules
        '(("@simplehealth.com" ("any" "@simplehealth\\.com" both) "simplehealth")
          ("@jonathanchu.is" ("any" "@jonathanchu\\.is" both) "personal")
          ("@3atmospheres.com" ("any" "@3atmospheres\\.com" both) "3atmospheres")
          ("@fastmail.com" ("any" "@fastmail\\.com" both) "fastmail")
          ("@me.com" ("any" "@me\\.com" both) "me.com")
          ))
  )





(provide 'init-email)
;;; init-email.el ends here
