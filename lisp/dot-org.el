;;;_ , Org-mode

(eval-and-compile
  (require 'cl-lib)
  (require 'use-package)
  (setq use-package-verbose nil)
  (setq use-package-expand-minimally t)
  (load "org-settings"))

(eval-when-compile
  (require 'cl)
  (setplist 'string-to-multibyte
            (use-package-plist-delete
             (symbol-plist 'string-to-multibyte) 'byte-obsolete-info)))

(require 'org)
(require 'org-agenda)

(message "hello!")

(setq org-agenda-files '("~/Dropbox/org/inbox.org"
                         "~/Dropbox/org/todo.org"
                         "~/Dropbox/org/gtd.org"
                         "~/Dropbox/org/simplehealth.org"))

;; TODO Put this in org-settings.el
;; (setq org-todo-keywords
;;       '((sequence "TODO(t)" "STARTED(s)" "WAITING(w)" "PROJECT"
;;                   "DELEGATED" "DEFERRED" "SOMEDAY" "|" "DONE(d)" "CANCELED(c)")
;;         (sequence "⚑(T)" "🏴(S)" "❓(W)" "|" "✔(D)" "✘(C)")))
;;
;; (setq org-priority-faces '((?A . error)
;;                            (?B . warning)
;;                            (?C . success)))

(setq org-tags-column -80)

(provide 'dot-org)
;;; dot-org.el ends here
