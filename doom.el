(set-default-font "-*-DejaVu Sans Mono for Powerline-normal-normal-normal-*-13-*-*-*-m-0-iso10646-1")

(defvar doom-unicode-font
  (font-spec :family "DejaVu Sans Mono" :size 13)
  "Font to fall back to for unicode glyphs.")

(set-fontset-font t 'unicode doom-unicode-font)

(use-package f
  :ensure t)

(use-package git-gutter-fringe
  :ensure t)

(use-package git-gutter
  :ensure t
  :config
  (require 'git-gutter-fringe)
  (global-git-gutter-mode +1)
  (define-fringe-bitmap 'git-gutter-fr:added
    [224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224]
    nil nil 'center)
  (define-fringe-bitmap 'git-gutter-fr:modified
    [224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224]
    nil nil 'center)
  (define-fringe-bitmap 'git-gutter-fr:deleted
    [0 0 0 0 0 0 0 0 0 0 0 0 0 128 192 224 240 248]
    nil nil 'center)

  (add-hook 'focus-in-hook 'git-gutter:update-all-windows))

(use-package powerline)

(setq fringes-outside-margins t
      highlight-nonselected-windows nil)

(define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
  [0 0 0 0 0 4 12 28 60 124 252 124 60 28 12 4 0 0 0 0])

;; Custom line number stuff
(set-face-attribute 'fringe nil)
;;(set-face-foreground 'linum-highlight-face "#00B3EF")
;;(set-face-background 'linum-highlight-face "#1f252b")

(defun doom-fix-unicode (font &rest chars)
  "Display certain unicode characters in a specific font.
e.g. (doom-fix-unicode \"DejaVu Sans\" ?⚠ ?★ ?λ)"
  (declare (indent 1))
  (mapc (lambda (x) (set-fontset-font
                t (cons x x)
                (cond ((fontp font)
                       font)
                      ((listp font)
                       (font-spec :family (car font) :size (nth 1 font)))
                      ((stringp font)
                       (font-spec :family font))
                      (t (error "FONT is an invalid type: %s" font)))))
        chars))

(defun doom/project-root (&optional strict-p)
  "Get the path to the root of your project."
  (let (projectile-require-project-root strict-p)
    (projectile-project-root)))

(defun doom/-flycheck-count (state)
  "Return flycheck information for the given error type STATE."
  (when (flycheck-has-current-errors-p state)
    (if (eq 'running flycheck-last-status-change)
        "?"
      (cdr-safe (assq state (flycheck-count-errors flycheck-current-errors))))))

(defvar mode-line-height 30)
(defvar-local doom--env-version nil)
(defvar-local doom--env-command nil)

(eval-when-compile (require 'powerline))
(defvar mode-line-bar          (pl/percent-xpm mode-line-height 100 0 100 0 3 "#00B3EF" nil))
(defvar mode-line-eldoc-bar    (pl/percent-xpm mode-line-height 100 0 100 0 3 "#B3EF00" nil))
(defvar mode-line-inactive-bar (pl/percent-xpm mode-line-height 100 0 100 0 3 nil nil))

(defface mode-line-is-modified nil "Face for mode-line modified symbol")
(defface mode-line-buffer-path nil "Face for mode-line buffer file path")
(defface mode-line-highlight nil "")
(defface mode-line-2 nil "")

;; Make certain unicode glyphs bigger for UI purposes
(doom-fix-unicode '("DejaVu Sans Mono" 15) ?✱)
(let ((font "DejaVu Sans Mono for Powerline"))
  (doom-fix-unicode (list font 12) ?)
  (doom-fix-unicode (list font 16) ?∄)
  (doom-fix-unicode (list font 15) ?))

(defvar mode-line-selected-window nil)
(defun doom|set-selected-window (&rest _)
  (let ((window (frame-selected-window)))
    (unless (minibuffer-window-active-p window)
      (setq mode-line-selected-window window))))
(add-hook 'window-configuration-change-hook #'doom|set-selected-window)
(add-hook 'focus-in-hook #'doom|set-selected-window)

(advice-add 'select-window :after 'doom|set-selected-window)
(advice-add 'select-frame  :after 'doom|set-selected-window)


;;
;; Mode-line segments
;;

(defun *buffer-path ()
  (when buffer-file-name
    (propertize
     (f-dirname
      (let ((buffer-path (file-relative-name buffer-file-name (doom/project-root)))
            (max-length (truncate (/ (window-body-width) 1.75))))
        (concat (projectile-project-name) "/"
                (if (> (length buffer-path) max-length)
                    (let ((path (reverse (split-string buffer-path "/" t)))
                          (output ""))
                      (when (and path (equal "" (car path)))
                        (setq path (cdr path)))
                      (while (and path (<= (length output) (- max-length 4)))
                        (setq output (concat (car path) "/" output))
                        (setq path (cdr path)))
                      (when path
                        (setq output (concat "../" output)))
                      (when (string-suffix-p "/" output)
                        (setq output (substring output 0 -1)))
                      output)
                  buffer-path))))
     'face (if active 'mode-line-buffer-path))))

(defun *buffer-state ()
  (when buffer-file-name
    (propertize
     (concat (if (not (file-exists-p buffer-file-name))
                 "∄"
               (if (buffer-modified-p) "✱"))
             (if buffer-read-only ""))
     'face 'mode-line-is-modified)))

(defun *buffer-name ()
  "The buffer's name."
  (s-trim-left (format-mode-line "%b")))

(defun *buffer-pwd ()
  "Displays `default-directory'."
  (propertize
   (concat "[" (abbreviate-file-name default-directory) "]")
   'face 'mode-line-2))

(defun *major-mode ()
  "The major mode, including process, environment and text-scale info."
  (concat (format-mode-line mode-name)
          (if (stringp mode-line-process) mode-line-process)
          (if doom--env-version (concat " " doom--env-version))
          (and (featurep 'face-remap)
               (/= text-scale-mode-amount 0)
               (format " (%+d)" text-scale-mode-amount))))

(defun *buffer-encoding-abbrev ()
  "The line ending convention used in the buffer."
  (if (memq buffer-file-coding-system '(utf-8 utf-8-unix))
      ""
    (symbol-name buffer-file-coding-system)))

(defface mode-line-vcs-info nil '((t (:inherit warning))) :group 'epresent)
(defface mode-line-vcs-warning nil '((t (:inherit warning))) :group 'epresent)
(defun *vc ()
  "Displays the current branch, colored based on its state."
  (when vc-mode
    (let ((backend (concat " " (substring vc-mode (+ 2 (length (symbol-name (vc-backend buffer-file-name)))))))
          (face (let ((state (vc-state buffer-file-name)))
                  (cond ((memq state '(edited added))
                         'mode-line-vcs-info)
                        ((memq state '(removed needs-merge needs-update conflict removed unregistered))
                         'mode-line-vcs-warning)))))
      (if active
          (propertize backend 'face face)
        backend))))

(defface doom-flycheck-error '((t (:inherit error)))
  "Face for flycheck error feedback in the modeline.")
(defface doom-flycheck-warning '((t (:inherit warning)))
  "Face for flycheck warning feedback in the modeline.")
(defvar-local doom--flycheck-err-cache nil "")
(defvar-local doom--flycheck-cache nil "")
(defun *flycheck ()
  "Persistent and cached flycheck indicators in the mode-line."
  (when (and (featurep 'flycheck)
             flycheck-mode
             (or flycheck-current-errors
                 (eq 'running flycheck-last-status-change)))
    (or (and (or (eq doom--flycheck-err-cache doom--flycheck-cache)
                 (memq flycheck-last-status-change '(running not-checked)))
             doom--flycheck-cache)
        (and (setq doom--flycheck-err-cache flycheck-current-errors)
             (setq doom--flycheck-cache
                   (let ((fe (doom/-flycheck-count 'error))
                         (fw (doom/-flycheck-count 'warning)))
                     (concat
                      (if fe (propertize (format " •%d " fe)
                                         'face (if active
                                                   'doom-flycheck-error
                                                 'mode-line)))
                      (if fw (propertize (format " •%d " fw)
                                         'face (if active
                                                   'doom-flycheck-warning
                                                 'mode-line))))))))))

(defun *macro-recording ()
  "Show when recording macro."
  (when (and active defining-kbd-macro)
    (propertize
     (format " %s ▶ " (char-to-string evil-this-macro))
     'face 'mode-line-highlight)))

(defun *buffer-position ()
  "A more vim-like buffer position."
  (let ((start (window-start))
        (end (window-end))
        (pend (point-max)))
    (if (and (= start 1)
             (= end pend))
        ":All"
      (cond ((= start 1) ":Top")
            ((= end pend) ":Bot")
            (t (format ":%d%%%%" (/ end 0.01 pend)))))))

(defun *pyenv-name ()
  "The current python venv.  Works with `pyenv'."
  (when (and active
             (eq 'python-mode major-mode)
             (fboundp 'pyenv-mode-version)
             (pyenv-mode-version))
    (let ((name (pyenv-mode-version)))
      (propertize name
                  'help-echo "Virtual environment (via pyenv)"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun doom-mode-line (&optional id)
  `(:eval
    (let* ((active (eq (selected-window) mode-line-selected-window))
           (lhs (list (propertize " " 'display (if active mode-line-bar mode-line-inactive-bar))
                      (*flycheck)
                      (*macro-recording)
                      " "
                      (*buffer-path)
                      (*buffer-name)
                      " "
                      (*buffer-state)
                      ,(if (eq id 'scratch) '(*buffer-pwd))))
           (rhs (list (*vc)
                      "  " (*major-mode) "  "
                      "  " (*pyenv-name) "  "
                      (propertize
                       (concat "(%l,%c) " (*buffer-position))
                       'face (if active 'mode-line-2))))
           (middle (propertize
                    " " 'display `((space :align-to (- (+ right right-fringe right-margin)
                                                       ,(1+ (string-width (format-mode-line rhs)))))))))
      (list lhs middle rhs))))

(setq-default mode-line-format (doom-mode-line))
