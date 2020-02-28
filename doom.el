(defconst doom-fringe-size '3 "Default fringe width")

;;; Setting up the fringe
;; switches order of fringe and margin
(setq-default fringes-outside-margins t)

;; standardize fringe width
(fringe-mode doom-fringe-size)
(push `(left-fringe  . ,doom-fringe-size) default-frame-alist)
(push `(right-fringe . ,doom-fringe-size) default-frame-alist)

(defmacro add-hook! (hook &rest func-or-forms)
  "A convenience macro for `add-hook'.

HOOK can be one hook or a list of hooks. If the hook(s) are not quoted, -hook is
appended to them automatically. If they are quoted, they are used verbatim.

FUNC-OR-FORMS can be a quoted symbol, a list of quoted symbols, or forms. Forms will be
wrapped in a lambda. A list of symbols will expand into a series of add-hook calls.

Examples:
    (add-hook! 'some-mode-hook 'enable-something)
    (add-hook! some-mode '(enable-something and-another))
    (add-hook! '(one-mode-hook second-mode-hook) 'enable-something)
    (add-hook! (one-mode second-mode) 'enable-something)
    (add-hook! (one-mode second-mode) (setq v 5) (setq a 2))"
  (declare (indent defun) (debug t))
  (unless func-or-forms
    (error "add-hook!: FUNC-OR-FORMS is empty"))
  (let* ((val (car func-or-forms))
         (quoted (eq (car-safe hook) 'quote))
         (hook (if quoted (cadr hook) hook))
         (funcs (if (eq (car-safe val) 'quote)
                    (if (cdr-safe (cadr val))
                        (cadr val)
                      (list (cadr val)))
                  (list func-or-forms)))
         (forms '()))
    (mapc
     (lambda (f)
       (let ((func (cond ((symbolp f) `(quote ,f))
                         (t `(lambda (&rest _) ,@func-or-forms)))))
         (mapc
          (lambda (h)
            (push `(add-hook ',(if quoted h (intern (format "%s-hook" h))) ,func) forms))
          (-list hook)))) funcs)
    `(progn ,@forms)))

    (defmacro after! (feature &rest forms)
      "A smart wrapper around `with-eval-after-load', that supresses warnings
    during compilation."
      (declare (indent defun) (debug t))
      `(,(if (or (not (boundp 'byte-compile-current-file))
                 (not byte-compile-current-file)
                 (if (symbolp feature)
                     (require feature nil :no-error)
                   (load feature :no-message :no-error)))
             'progn
           (message "after: cannot find %s" feature)
           'with-no-warnings)
        (with-eval-after-load ',feature ,@forms)))

(defun doom/nlinum-toggle ()
  (interactive)
  (if (bound-and-true-p nlinum-mode)
      (doom|nlinum-disable)
    (doom|nlinum-enable)))

(defun doom|nlinum-enable (&rest _)
  (nlinum-mode +1)
  (add-hook 'post-command-hook 'doom|nlinum-hl-line nil t)
  (doom--nlinum-unhl-line))

;;;###autoload
(defun doom|nlinum-disable (&rest _)
  (nlinum-mode -1)
  (remove-hook 'post-command-hook 'doom|nlinum-hl-line t)
  (doom--nlinum-unhl-line))

(defun doom--nlinum-unhl-line ()
  "Unhighlight line number"
  (when doom--hl-nlinum-overlay
    (let* ((disp (get-text-property
                  0 'display (overlay-get doom--hl-nlinum-overlay 'before-string)))
           (str (nth 1 disp)))
      (put-text-property 0 (length str) 'face 'linum str)
      (setq doom--hl-nlinum-overlay nil)
      disp)))

(defun doom|nlinum-hl-line (&rest _)
  "Highlight line number"
  (let* ((pbol (line-beginning-position))
         (peol (1+ pbol))
         (max (point-max)))
    ;; Handle EOF case
    (when (>= peol max)
      (setq peol max))
    (jit-lock-fontify-now pbol peol)
    (let ((ov (--first (overlay-get it 'nlinum) (overlays-in pbol peol))))
      (doom--nlinum-unhl-line)
      (when ov
        (let ((str (nth 1 (get-text-property 0 'display (overlay-get ov 'before-string)))))
          (put-text-property 0 (length str) 'face 'doom-nlinum-highlight str)
          (setq doom--hl-nlinum-overlay ov))))))

(use-package hl-line
  :init (add-hook 'prog-mode-hook 'hl-line-mode)
  :config
  ;; Doesn't seem to play nice in emacs 25+
  (setq hl-line-sticky-flag nil
        global-hl-line-sticky-flag nil)

  (defvar-local doom--hl-line-mode nil)
  (defun doom|hl-line-on ()  (if doom--hl-line-mode (hl-line-mode +1)))
  (defun doom|hl-line-off () (if doom--hl-line-mode (hl-line-mode -1)))
  (add-hook! hl-line-mode (if hl-line-mode (setq doom--hl-line-mode t)))
  ;; Disable line highlight in visual mode
  (add-hook 'evil-visual-state-entry-hook 'doom|hl-line-off)
  (add-hook 'evil-visual-state-exit-hook  'doom|hl-line-on))


;; (set-default-font "-*-DejaVu Sans Mono for Powerline-normal-normal-normal-*-13-*-*-*-m-0-iso10646-1")

(defvar doom-unicode-font
  (font-spec :family "DejaVu Sans Mono" :size 13)
  "Font to fall back to for unicode glyphs.")

(set-fontset-font t 'unicode doom-unicode-font)

(use-package f
  :ensure t)

(use-package fringe-helper
  :ensure t)

(use-package git-gutter-fringe
  :ensure t
  :config
  (progn
    (setq-default fringes-outside-margins t)
    ;; thin fringe bitmaps
    (fringe-helper-define 'git-gutter-fr:added '(center repeated)
      "XXX.....")
    (fringe-helper-define 'git-gutter-fr:modified '(center repeated)
      "XXX.....")
    (fringe-helper-define 'git-gutter-fr:deleted 'bottom
      "X......."
      "XX......"
      "XXX....."
      "XXXX....")
    ))

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

(use-package nlinum
  :ensure t
  :commands nlinum-mode
  :preface
  (setq linum-format "%3d ")
  (defvar nlinum-format "%4d ")
  (defvar doom--hl-nlinum-overlay nil)
  (defvar doom--hl-nlinum-line nil)
  :init
  (add-hook!
    (markdown-mode prog-mode scss-mode web-mode conf-mode groovy-mode
                   nxml-mode snippet-mode php-mode python-mode)
    'nlinum-mode)
  ;; FIXME This only works if hl-line is active!
  (add-hook! nlinum-mode
    (if nlinum-mode-hook
        (add-hook 'post-command-hook 'doom|nlinum-hl-line nil t)
      (remove-hook 'post-command-hook 'doom|nlinum-hl-line t)))
  :config
  ;; Calculate line number column width
  (add-hook! nlinum-mode
    (setq nlinum--width (length (save-excursion (goto-char (point-max))
                                                (format-mode-line "%l")))))

  ;; Disable nlinum when making frames, otherwise we get linum face error
  ;; messages that prevent frame creation.
  (add-hook 'before-make-frame-hook 'doom|nlinum-disable)
  (add-hook 'after-make-frame-functions 'doom|nlinum-enable))

;; (use-package powerline)

(setq-default fringes-outside-margins t
              highlight-nonselected-windows nil)

(define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
  [0 0 0 0 0 4 12 28 60 124 252 124 60 28 12 4 0 0 0 0])

;; Custom line number stuff
;;(set-face-attribute 'fringe nil)
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

(defun *selection-info ()
  "Information about the current selection, such as how many characters and
lines are selected, or the NxM dimensions of a block selection."
  (when (and active (evil-visual-state-p))
    (propertize
     (let ((reg-beg (region-beginning))
           (reg-end (region-end))
           (evil (eq 'visual evil-state)))
       (let ((lines (count-lines reg-beg (min (1+ reg-end) (point-max))))
             (chars (- (1+ reg-end) reg-beg))
             (cols (1+ (abs (- (evil-column reg-end)
                               (evil-column reg-beg))))))
         (cond
          ;; rectangle selection
          ((or (bound-and-true-p rectangle-mark-mode)
               (and evil (eq 'block evil-visual-selection)))
           (format " %dx%dB " lines (if evil cols (1- cols))))
          ;; line selection
          ((or (> lines 1) (eq 'line evil-visual-selection))
           (if (and (eq evil-state 'visual) (eq evil-this-type 'line))
               (format " %dL " lines)
             (format " %dC %dL " chars lines)))
          (t (format " %dC " (if evil chars (1- chars)))))))
     'face 'mode-line-highlight)))

(defun *macro-recording ()
  "Show when recording macro."
  (when (and active defining-kbd-macro)
    (propertize
     (format " %s ▶ " (char-to-string evil-this-macro))
     'face 'mode-line-highlight)))

(make-variable-buffer-local 'anzu--state)
(defun *anzu ()
  "Show the current match number and the total number of matches. Requires anzu
to be enabled."
  (when (and (featurep 'evil-anzu) (evil-ex-hl-active-p 'evil-ex-search))
    (propertize
     (format " %s/%d%s "
             anzu--current-position anzu--total-matched
             (if anzu--overflow-p "+" ""))
     'face (if active 'mode-line-count-face))))

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
                      (*selection-info)
                      (*anzu)
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
