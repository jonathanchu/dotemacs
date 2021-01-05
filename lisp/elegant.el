;; elegant.el --- My custom company config.
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
;; Elegant
;;----------------------------------------------------------------------------

(message "ELEGANT BEGIN")

;;; elegant.el --- A very minimal but elegant and consistent theme
;;; Copyright (C) 2020 Nicolas P. Rougier and Nicolò Zorzetto
;;; -------------------------------------------------------------------
;;; Authors: Nicolas P. Rougier and Nicolò Zorzetto
;;; -------------------------------------------------------------------
;;; URL: https://github.com/rougier/elegant-emacs
;;; -------------------------------------------------------------------
;;; Version: 1
;;; Package-Requires: ((emacs "25.1"))
;;; -------------------------------------------------------------------
;;; This file is not part of GNU Emacs.
;;;
;;; This program is free software: you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program. If not, see <http://www.gnu.org/licenses/>
;;; -------------------------------------------------------------------
;;; Commentary:
;;; This theme offers an almost vanilla yet elegant Emacs experience
;;; -------------------------------------------------------------------
;;; Code:


;;; Font and frame size
;;; -------------------------------------------------------------------
(setq default-frame-alist
      (append (list '(width  . 72) '(height . 40)
                    '(vertical-scroll-bars . nil)
                    '(internal-border-width . 24)
                    '(font . "Roboto Mono Light 14"))))
(set-frame-parameter (selected-frame)
                     'internal-border-width 24)
;;; -------------------------------------------------------------------

;;; Line spacing, can be 0 for code and 1 or 2 for text
;;; -------------------------------------------------------------------
(setq-default line-spacing 0)
(setq x-underline-at-descent-line t)
(setq widget-image-enable nil)
;;; -------------------------------------------------------------------

;;; Line cursor and no blink
;;; -------------------------------------------------------------------
(set-default 'cursor-type  '(bar . 1))
(blink-cursor-mode 0)
;;; -------------------------------------------------------------------


;;; No sound
;;; -------------------------------------------------------------------
(setq visible-bell t)
(setq ring-bell-function 'ignore)
;;; -------------------------------------------------------------------


;;; No Tooltips
;;; -------------------------------------------------------------------
(tooltip-mode 0)
;;; -------------------------------------------------------------------


;;; Paren mode is part of the theme
;;; -------------------------------------------------------------------
(show-paren-mode t)
;;; -------------------------------------------------------------------


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


;;; A theme is fully defined by these six faces
;;; -------------------------------------------------------------------
(defgroup elegance nil
  "Faces for the elegance theme"
  :prefix "elegance-face-"
  :group 'faces)
;;; -------------------------------------------------------------------


;;; Custom faces definition
;;; -------------------------------------------------------------------
(defface face-critical nil
  "Critical face is for information that requires immediate action.
It should be of high constrast when compared to other faces. This
can be realized (for example) by setting an intense background
color, typically a shade of red. It must be used scarcely."
  :group 'elegance)

(defface face-popout nil
  "Popout face is used for information that needs attention.
To achieve such effect, the hue of the face has to be
sufficiently different from other faces such that it attracts
attention through the popout effect."
  :group 'elegance)

(defface face-strong nil
  "Strong face is used for information of a structural nature.
It has to be the same color as the default color and only the
weight differs by one level (e.g., light/regular or
regular/bold). IT is generally used for titles, keywords,
directory, etc."
  :group 'elegance)

(defface face-salient nil
  "Salient face is used for information that are important.
To suggest the information is of the same nature but important,
the face uses a different hue with approximately the same
intensity as the default face. This is typically used for links."

  :group 'elegance)

(defface face-faded nil
  "Faded face is for information that are less important.
It is made by using the same hue as the default but with a lesser
intensity than the default. It can be used for comments,
secondary information and also replace italic (which is generally
abused anyway)."
  :group 'elegance)

(defface face-subtle nil
  "Subtle face is used to suggest a physical area on the screen.
It is important to not disturb too strongly the reading of
information and this can be made by setting a very light
background color that is barely perceptible."
  :group 'elegance)
;;; -------------------------------------------------------------------


;;; Mode line rendering
;;; -------------------------------------------------------------------
;;; This line below makes things a bit faster
(set-fontset-font "fontset-default"  '(#x2600 . #x26ff) "Fira Code 16")

(define-key mode-line-major-mode-keymap [header-line]
  (lookup-key mode-line-major-mode-keymap [mode-line]))

(defun mode-line-render (left right)
  "Function to render the modeline LEFT to RIGHT."
  (let* ((available-width (- (window-width) (length left) )))
    (format (format "%%s %%%ds" available-width) left right)))
(setq-default mode-line-format
              '((:eval
                 (mode-line-render
                  (format-mode-line (list
                                     (propertize "☰" 'face `(:inherit mode-line-buffer-id)
                                                 'help-echo "Mode(s) menu"
                                                 'mouse-face 'mode-line-highlight
                                                 'local-map   mode-line-major-mode-keymap)
                                     " %b "
                                     (if (and buffer-file-name (buffer-modified-p))
                                         (propertize "(modified)" 'face `(:inherit face-faded)))))
                  (format-mode-line
                   (propertize "%4l:%2c" 'face `(:inherit face-faded)))))))
;;; -------------------------------------------------------------------


;;; Set modeline at the top
;;; -------------------------------------------------------------------
(setq-default header-line-format mode-line-format)
(setq-default mode-line-format'(""))
;;; -------------------------------------------------------------------


;;; Vertical window divider
;;; -------------------------------------------------------------------
(setq window-divider-default-right-width 3)
(setq window-divider-default-places 'right-only)
(window-divider-mode)
;;; -------------------------------------------------------------------


;;; Modeline
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
;;; -------------------------------------------------------------------


;;; Buttons
;;; -------------------------------------------------------------------
(defun set-button-faces ()
  "Set button faces."
  (set-face-attribute 'custom-button nil
                      :foreground (face-foreground 'face-faded)
                      :background (face-background 'face-subtle)
                      :box `(:line-width 1
                                         :color ,(face-foreground 'face-faded)
                                         :style nil))
  (set-face-attribute 'custom-button-mouse nil
                      :foreground (face-foreground 'default)
                      ;;; :background (face-foreground 'face-faded)
                      :inherit 'custom-button
                      :box `(:line-width 1
                                         :color ,(face-foreground 'face-subtle)
                                         :style nil))
  (set-face-attribute 'custom-button-pressed nil
                      :foreground (face-background 'default)
                      :background (face-foreground 'face-salient)
                      :inherit 'face-salient
                      :box `(:line-width 1
                                         :color ,(face-foreground 'face-salient)
                                         :style nil)
                      :inverse-video nil))
'(cus-edit (set-button-faces))
;;; -------------------------------------------------------------------




;; Structural
;; -------------------------------------------------------------------
(set-face 'bold                                          'face-strong)
(set-face 'italic                                         'face-faded)
(set-face 'bold-italic                                   'face-strong)
(set-face 'region                                        'face-subtle)
(set-face 'highlight                                     'face-subtle)
(set-face 'fixed-pitch                                       'default)
(set-face 'fixed-pitch-serif                                 'default)
(set-face 'variable-pitch                                    'default)
(set-face 'cursor                                            'default)
;;; -------------------------------------------------------------------


;; Semantic
;;; -------------------------------------------------------------------
(set-face 'shadow                                         'face-faded)
(set-face 'success                                      'face-salient)
(set-face 'warning                                       'face-popout)
(set-face 'error                                       'face-critical)
;;; -------------------------------------------------------------------


;; General
;;; -------------------------------------------------------------------
(set-face 'buffer-menu-buffer                            'face-strong)
(set-face 'minibuffer-prompt                             'face-strong)
(set-face 'link                                         'face-salient)
(set-face 'fringe                                         'face-faded)
(set-face 'isearch                                       'face-strong)
(set-face 'isearch-fail                                   'face-faded)
(set-face 'lazy-highlight                                'face-subtle)
(set-face 'trailing-whitespace                           'face-subtle)
(set-face 'show-paren-match                              'face-popout)
(set-face 'show-paren-mismatch                           'face-normal)
(set-face-attribute 'tooltip nil                         :height 0.85)
;;; -------------------------------------------------------------------


;; Programmation mode
;;; -------------------------------------------------------------------
(set-face 'font-lock-comment-face                         'face-faded)
(set-face 'font-lock-doc-face                             'face-faded)
(set-face 'font-lock-string-face                         'face-popout)
(set-face 'font-lock-constant-face                      'face-salient)
(set-face 'font-lock-warning-face                        'face-popout)
(set-face 'font-lock-function-name-face                  'face-strong)
(set-face 'font-lock-variable-name-face                  'face-strong)
(set-face 'font-lock-builtin-face                       'face-salient)
(set-face 'font-lock-type-face                          'face-salient)
(set-face 'font-lock-keyword-face                       'face-salient)
;;; -------------------------------------------------------------------


;; Documentation
;;; -------------------------------------------------------------------
''(set-face 'info-header-node                            'face-normal)
'(set-face 'Info-quoted                                  'face-faded)
'(set-face 'info-title-1                                'face-strong)
'(set-face 'info-title-2                                'face-strong)
'(set-face 'info-title-3                                'face-strong)
'(set-face 'info-title-4                               'face-strong)
;;; -------------------------------------------------------------------


;; Bookmarks
;;; -------------------------------------------------------------------
'(bookmark-menu-heading                       'face-strong)
'(bookmark-menu-bookmark                    'face-salient)
;;; -------------------------------------------------------------------


;; Message
;;; -------------------------------------------------------------------
'(message-cited-text                           'face-faded)
'(message-header-cc                               'default)
'(message-header-name                         'face-strong)
'(message-header-newsgroups                       'default)
'(message-header-other                            'default)
'(message-header-subject                     'face-salient)
'(message-header-to                          'face-salient)
'(message-header-xheader                          'default)
'(message-mml                                 'face-popout)
'(message-separator                           'face-faded)
;;; -------------------------------------------------------------------


;; Outline
;;; -------------------------------------------------------------------
'(outline-1                                   'face-strong)
'(outline-2                                   'face-strong)
'(outline-3                                   'face-strong)
'(outline-4                                   'face-strong)
'(outline-5                                   'face-strong)
'(outline-6                                   'face-strong)
'(outline-7                                   'face-strong)
'(outline-8                                  'face-strong)
;;; -------------------------------------------------------------------


;; Interface
;;; -------------------------------------------------------------------
'(widget-field                                'face-subtle)
'(widget-button                               'face-strong)
'(widget-single-line-field                    'face-subtle)
'(custom-group-subtitle                       'face-strong)
'(custom-group-tag                            'face-strong)
'(custom-group-tag-1                          'face-strong)
'(custom-comment                               'face-faded)
'(custom-comment-tag                           'face-faded)
'(custom-changed                             'face-salient)
'(custom-modified                            'face-salient)
'(custom-face-tag                             'face-strong)
'(custom-variable-tag                             'default)
'(custom-invalid                              'face-popout)
'(custom-visibility                          'face-salient)
'(custom-state                               'face-salient)
'(custom-link                               'face-salient)
;;; -------------------------------------------------------------------


;; Package
;;; -------------------------------------------------------------------
'(package-description                             'default)
'(package-help-section-name                       'default)
'(package-name                               'face-salient)
'(package-status-avail-obso                    'face-faded)
'(package-status-available                        'default)
'(package-status-built-in                    'face-salient)
'(package-status-dependency                  'face-salient)
'(package-status-disabled                      'face-faded)
'(package-status-external                         'default)
'(package-status-held                             'default)
'(package-status-incompat                      'face-faded)
'(package-status-installed                   'face-salient)
'(package-status-new                              'default)
'(package-status-unsigned                         'default)
;;; -------------------------------------------------------------------


;; Button function (hardcoded)
;;; -------------------------------------------------------------------
(defun package-make-button (text &rest properties)
  "Insert button labeled TEXT with button PROPERTIES at point.
PROPERTIES are passed to `insert-text-button', for which this
function is a convenience wrapper used by `describe-package-1'."
  (let ((button-text (if (display-graphic-p)
                         text (concat "[" text "]")))
        (button-face (if (display-graphic-p)
                         '(:box `(:line-width 1
                                              :color "#999999":style nil)
                                :foreground "#999999"
                                :background "#F0F0F0")
                       'link)))
    (apply #'insert-text-button button-text
           'face button-face 'follow-link t properties)))
;;; -------------------------------------------------------------------


;; Flyspell
;;; -------------------------------------------------------------------
'(flyspell-duplicate                         'face-popout)
'(flyspell-incorrect                         'face-popout)
;;; -------------------------------------------------------------------


;; Ido
;;; -------------------------------------------------------------------
'(ido-first-match                            'face-salient)
'(ido-only-match                               'face-faded)
'(ido-subdir                                 'face-strong)
;;; -------------------------------------------------------------------


;; Diff
;;; -------------------------------------------------------------------
'(diff-header                                  'face-faded)
'(diff-file-header                            'face-strong)
'(diff-context                                    'default)
'(diff-removed                                 'face-faded)
'(diff-changed                                'face-popout)
'(diff-added                                 'face-salient)
'(diff-refine-added            '(face-salient face-strong))
'(diff-refine-changed                         'face-popout)
'(diff-refine-removed                          'face-faded)
'(set-face-attribute     'diff-refine-removed nil :strike-through t)
;;; -------------------------------------------------------------------


;; Term
;;; -------------------------------------------------------------------
'(term-bold                                   'face-strong)
'(set-face-attribute 'term-color-black nil
                     :foreground (face-foreground 'default)
                     :background (face-foreground 'default))
'(set-face-attribute 'term-color-white nil
                     :foreground "white" :background "white")
'(set-face-attribute 'term-color-blue nil
                     :foreground "#42A5F5" :background "#BBDEFB")
'(set-face-attribute 'term-color-cyan nil
                     :foreground "#26C6DA" :background "#B2EBF2")
'(set-face-attribute 'term-color-green nil
                     :foreground "#66BB6A" :background "#C8E6C9")
'(set-face-attribute 'term-color-magenta nil
                     :foreground "#AB47BC" :background "#E1BEE7")
'(set-face-attribute 'term-color-red nil
                     :foreground "#EF5350" :background "#FFCDD2")
'(set-face-attribute 'term-color-yellow nil
                     :foreground "#FFEE58" :background "#FFF9C4")
;;; -------------------------------------------------------------------


;; org-agendas
;;; -------------------------------------------------------------------
'(org-agenda-calendar-event                    'default)
'(org-agenda-calendar-sexp                     'face-faded)
'(org-agenda-clocking                          'face-faded)
'(org-agenda-column-dateline                   'face-faded)
'(org-agenda-current-time                      'face-faded)
'(org-agenda-date                            'face-salient)
'(org-agenda-date-today        '(face-salient face-strong))
'(org-agenda-date-weekend                      'face-faded)
'(org-agenda-diary                             'face-faded)
'(org-agenda-dimmed-todo-face                  'face-faded)
'(org-agenda-done                              'face-faded)
'(org-agenda-filter-category                   'face-faded)
'(org-agenda-filter-effort                     'face-faded)
'(org-agenda-filter-regexp                     'face-faded)
'(org-agenda-filter-tags                       'face-faded)
'(org-agenda-restriction-lock                  'face-faded)
'(org-agenda-structure                        'face-faded)
;;; -------------------------------------------------------------------


;; org mode
;;; -------------------------------------------------------------------
'(org-archived                                 'face-faded)
'(org-block                                    'face-faded)
'(org-block-begin-line                         'face-faded)
'(org-block-end-line                           'face-faded)
'(org-checkbox                                 'face-faded)
'(org-checkbox-statistics-done                 'face-faded)
'(org-checkbox-statistics-todo                 'face-faded)
'(org-clock-overlay                            'face-faded)
'(org-code                                     'face-faded)
'(org-column                                   'face-faded)
'(org-column-title                             'face-faded)
'(org-date                                     'face-faded)
'(org-date-selected                            'face-faded)
'(org-default                                  'face-faded)
'(org-document-info                            'face-faded)
'(org-document-info-keyword                    'face-faded)
'(org-document-title                           'face-faded)
'(org-done                                        'default)
'(org-drawer                                   'face-faded)
'(org-ellipsis                                 'face-faded)
'(org-footnote                                 'face-faded)
'(org-formula                                  'face-faded)
'(org-headline-done                            'face-faded)
'(org-latex-and-related                        'face-faded)
'(org-level-1                                 'face-strong)
'(org-level-2                                 'face-strong)
'(org-level-3                                 'face-strong)
'(org-level-4                                 'face-strong)
'(org-level-5                                 'face-strong)
'(org-level-6                                 'face-strong)
'(org-level-7                                 'face-strong)
'(org-level-8                                 'face-strong)
'(org-link                                   'face-salient)
'(org-list-dt                                  'face-faded)
'(org-macro                                    'face-faded)
'(org-meta-line                                'face-faded)
'(org-mode-line-clock                          'face-faded)
'(org-mode-line-clock-overrun                  'face-faded)
'(org-priority                                 'face-faded)
'(org-property-value                           'face-faded)
'(org-quote                                    'face-faded)
'(org-scheduled                                'face-faded)
'(org-scheduled-previously                     'face-faded)
'(org-scheduled-today                          'face-faded)
'(org-sexp-date                                'face-faded)
'(org-special-keyword                          'face-faded)
'(org-table                                    'face-faded)
'(org-tag                                      'face-faded)
'(org-tag-group                                'face-faded)
'(org-target                                   'face-faded)
'(org-time-grid                                'face-faded)
'(org-todo                                    'face-popout)
'(org-upcoming-deadline                        'face-faded)
'(org-verbatim                                 'face-faded)
'(org-verse                                    'face-faded)
'(org-warning                                'face-popout)
(setq org-hide-emphasis-markers t)
;;; -------------------------------------------------------------------


;; Mu4e
;;; -------------------------------------------------------------------
'(mu4e-attach-number-face                     'face-strong)
'(mu4e-cited-1-face                            'face-faded)
'(mu4e-cited-2-face                            'face-faded)
'(mu4e-cited-3-face                            'face-faded)
'(mu4e-cited-4-face                            'face-faded)
'(mu4e-cited-5-face                            'face-faded)
'(mu4e-cited-6-face                            'face-faded)
'(mu4e-cited-7-face                            'face-faded)
'(mu4e-compose-header-face                     'face-faded)
'(mu4e-compose-separator-face                  'face-faded)
'(mu4e-contact-face                          'face-salient)
'(mu4e-context-face                            'face-faded)
'(mu4e-draft-face                              'face-faded)
'(mu4e-flagged-face                            'face-faded)
'(mu4e-footer-face                             'face-faded)
'(mu4e-forwarded-face                          'face-faded)
'(mu4e-header-face                                'default)
'(mu4e-header-highlight-face                  'face-subtle)
'(mu4e-header-key-face                        'face-strong)
'(mu4e-header-marks-face                       'face-faded)
'(mu4e-header-title-face                      'face-strong)
'(mu4e-header-value-face                          'default)
'(mu4e-highlight-face                         'face-popout)
'(mu4e-link-face                             'face-salient)
'(mu4e-modeline-face                           'face-faded)
'(mu4e-moved-face                              'face-faded)
'(mu4e-ok-face                                 'face-faded)
'(mu4e-region-code                             'face-faded)
'(mu4e-replied-face                          'face-salient)
'(mu4e-special-header-value-face                  'default)
'(mu4e-system-face                             'face-faded)
'(mu4e-title-face                             'face-strong)
'(mu4e-trashed-face                            'face-faded)
'(mu4e-unread-face                            'face-strong)
'(mu4e-url-number-face                         'face-faded)
'(mu4e-view-body-face                             'default)
'(mu4e-warning-face                            'face-faded)
;;; -------------------------------------------------------------------


;;;;###autoload
;;; -------------------------------------------------------------------
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))
;;; -------------------------------------------------------------------


;;; Provide commons for the elegant-emacs-themes
;;; -------------------------------------------------------------------
;; (provide 'elegant)
;;; -------------------------------------------------------------------

;;; elegant.el ends here

;;; Buttons

(message "ELEGANT ENDS")


(message "ELEGANCE BEGINS")

;; -------------------------------------------------------------------
;; A very minimal but elegant and consistent theme
;; Copyright 2020 Nicolas P. Rougier
;; -------------------------------------------------------------------
;; This file is not part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>
;; -------------------------------------------------------------------


;; Only necessary for the splash screen mockup
;; -------------------------------------------------------------------
(with-eval-after-load 'org
  (setq org-display-inline-images t)
  (setq org-redisplay-inline-images t)
  (setq org-startup-with-inline-images "inlineimages")
  (setq org-hide-emphasis-markers t)
  (setq org-confirm-elisp-link-function nil)
  (setq org-link-frame-setup '((file . find-file))))
;; -------------------------------------------------------------------


;; Font and frame size
(set-face-font 'default "Roboto Mono Light 14")
(setq default-frame-alist
      (append (list '(width  . 72) '(height . 40)
                    '(vertical-scroll-bars . nil)
                    '(internal-border-width . 24)
                    '(font . "Roboto Mono Light 14"))))
(set-frame-parameter (selected-frame)
                     'internal-border-width 24)

;; Line spacing, can be 0 for code and 1 or 2 for text
(setq-default line-spacing 0)

;; Underline line at descent position, not baseline position
(setq x-underline-at-descent-line t)

;; No ugly button for checkboxes
(setq widget-image-enable nil)

;; Line cursor and no blink
(set-default 'cursor-type  '(bar . 1))
(blink-cursor-mode 0)

;; No sound
(setq visible-bell t)
(setq ring-bell-function 'ignore)

;; No Tooltips
(tooltip-mode 0)

;; Paren mode is part of the theme
(show-paren-mode t)

;; No fringe but nice glyphs for truncated and wrapped lines
(fringe-mode '(0 . 0))
(defface fallback '((t :family "Fira Code Light"
                       :inherit 'face-faded)) "Fallback")
(set-display-table-slot standard-display-table 'truncation
                        (make-glyph-code ?… 'fallback))
(set-display-table-slot standard-display-table 'wrap
                        (make-glyph-code ?↩ 'fallback))
(set-display-table-slot standard-display-table 'selective-display
                        (string-to-vector " …"))


;; When we set a face, we take care of removing any previous settings
(defun set-face (face style)
  "Reset a face and make it inherit style."
  (set-face-attribute face nil
                      :foreground 'unspecified :background 'unspecified
                      :family     'unspecified :slant      'unspecified
                      :weight     'unspecified :height     'unspecified
                      :underline  'unspecified :overline   'unspecified
                      :box        'unspecified :inherit    style))

;; A theme is fully defined by these six faces
(defgroup elegance nil
  "Faces for the elegance theme"
  :prefix "face-")

;; Do not show prefix when displaying the elegance group
(setq custom-unlispify-remove-prefixes t)

(defface face-critical nil
  "Critical face is for information that requires immediate action.
It should be of high constrast when compared to other faces. This
can be realized (for example) by setting an intense background
color, typically a shade of red. It must be used scarcely."
  :group 'elegance)

(defface face-popout nil
  "Popout face is used for information that needs attention.
To achieve such effect, the hue of the face has to be
sufficiently different from other faces such that it attracts
attention through the popout effect."
  :group 'elegance)

(defface face-strong nil
  "Strong face is used for information of a structural nature.
It has to be the same color as the default color and only the
weight differs by one level (e.g., light/regular or
regular/bold). IT is generally used for titles, keywords,
directory, etc."
  :group 'elegance)

(defface face-salient nil
  "Salient face is used for information that are important.
To suggest the information is of the same nature but important,
the face uses a different hue with approximately the same
intensity as the default face. This is typically used for links."

  :group 'elegance)

(defface face-faded nil
  "Faded face is for information that are less important.
It is made by using the same hue as the default but with a lesser
intensity than the default. It can be used for comments,
secondary information and also replace italic (which is generally
abused anyway)."
  :group 'elegance)

(defface face-subtle nil
  "Subtle face is used to suggest a physical area on the screen.
It is important to not disturb too strongly the reading of
information and this can be made by setting a very light
background color that is barely perceptible."
  :group 'elegance)


;; Mode line (this might be slow because of the "☰" that requires substitution)
;; This line below makes things a bit faster
(set-fontset-font "fontset-default"  '(#x2600 . #x26ff) "Fira Code 16")

(define-key mode-line-major-mode-keymap [header-line]
  (lookup-key mode-line-major-mode-keymap [mode-line]))

(defun mode-line-render (left right)
  (let* ((available-width (- (window-width) (length left) )))
    (format (format "%%s %%%ds" available-width) left right)))
(setq-default mode-line-format
              '((:eval
                 (mode-line-render
                  (format-mode-line (list
                                     (propertize "☰" 'face `(:inherit mode-line-buffer-id)
                                                 'help-echo "Mode(s) menu"
                                                 'mouse-face 'mode-line-highlight
                                                 'local-map   mode-line-major-mode-keymap)
                                     " %b "
                                     (if (and buffer-file-name (buffer-modified-p))
                                         (propertize "(modified)" 'face `(:inherit face-faded)))))
                  (format-mode-line
                   (propertize "%4l:%2c  " 'face `(:inherit face-faded)))))))


;; Comment if you want to keep the modeline at the bottom
(setq-default header-line-format mode-line-format)
(setq-default mode-line-format'(""))


;; Vertical window divider
(setq window-divider-default-right-width 3)
(setq window-divider-default-places 'right-only)
(window-divider-mode)

;; Modeline
(defun set-modeline-faces ()

  ;; Mode line at top
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

  ;; Mode line at bottom
  ;; (set-face 'header-line                                 'face-strong)
  ;; (set-face-attribute 'mode-line nil
  ;;                     :height 1.0
  ;;                     :overline (face-background 'default)
  ;;                     :underline nil
  ;;                     :foreground (face-foreground 'default)
  ;;                     :background (face-background 'face-subtle)
  ;;                     :box `(:line-width 2
  ;;                            :color ,(face-background 'face-subtle)
  ;;                            :style nil))
  ;; (set-face 'mode-line-highlight '(face-popout mode-line))
  ;; (set-face 'mode-line-emphasis  'face-strong)
  ;; (set-face-attribute 'mode-line-buffer-id nil :weight 'regular)
  ;; (set-face-attribute 'mode-line-inactive nil
  ;;                     :height 1.0
  ;;                     :overline (face-background 'default)
  ;;                     :underline nil
  ;;                     :foreground (face-foreground 'face-faded)
  ;;                     :background (face-background 'face-subtle)
  ;;                     :box `(:line-width 2
  ;;                            :color ,(face-background 'face-subtle)
  ;;                            :style nil))


  (set-face-attribute 'cursor nil
                      :background (face-foreground 'default))
  (set-face-attribute 'window-divider nil
                      :foreground (face-background 'mode-line))
  (set-face-attribute 'window-divider-first-pixel nil
                      :foreground (face-background 'default))
  (set-face-attribute 'window-divider-last-pixel nil
                      :foreground (face-background 'default))
  )

;; Buttons
(defun set-button-faces ()
  (set-face-attribute 'custom-button nil
                      :foreground (face-foreground 'face-faded)
                      :background (face-background 'face-subtle)
                      :box `(:line-width 1
                                         :color ,(face-foreground 'face-faded)
                                         :style nil))
  (set-face-attribute 'custom-button-mouse nil
                      :foreground (face-foreground 'default)
                      ;; :background (face-foreground 'face-faded)
                      :inherit 'custom-button
                      :box `(:line-width 1
                                         :color ,(face-foreground 'face-subtle)
                                         :style nil))
  (set-face-attribute 'custom-button-pressed nil
                      :foreground (face-background 'default)
                      :background (face-foreground 'face-salient)
                      :inherit 'face-salient
                      :box `(:line-width 1
                                         :color ,(face-foreground 'face-salient)
                                         :style nil)
                      :inverse-video nil))

;; Light theme
(defun elegance-light ()
  (setq frame-background-mode 'light)
  (set-background-color "#ffffff")
  (set-foreground-color "#333333")
  (set-face-attribute 'default nil
                      :foreground (face-foreground 'default)
                      :background (face-background 'default))
  (set-face-attribute 'face-critical nil :foreground "#ffffff"
                      :background "#ff6347")
  (set-face-attribute 'face-popout nil :foreground "#ffa07a")
  (set-face-attribute 'face-strong nil :foreground "#333333"
                      :weight 'regular)
  (set-face-attribute 'face-salient nil :foreground "#00008b"
                      :weight 'light)
  (set-face-attribute 'face-faded nil :foreground "#999999"
                      :weight 'light)
  (set-face-attribute 'face-subtle nil :background "#f0f0f0")

  (set-modeline-faces)

  (with-eval-after-load 'cus-edit (set-button-faces)))

;; Dark theme
(defun elegance-dark ()
  (setq frame-background-mode 'dark)
  (set-background-color "#3f3f3f")
  (set-foreground-color "#dcdccc")
  (set-face-attribute 'default nil
                      :foreground (face-foreground 'default)
                      :background (face-background 'default))
  (set-face-attribute 'face-critical nil :foreground "#385f38"
                      :background "#f8f893")
  (set-face-attribute 'face-popout nil :foreground "#f0dfaf")
  (set-face-attribute 'face-strong nil :foreground "#dcdccc"
                      :weight 'regular)
  (set-face-attribute 'face-salient nil :foreground "#dca3a3"
                      :weight 'light)
  (set-face-attribute 'face-faded nil :foreground "#777767"
                      :weight 'light)
  (set-face-attribute 'face-subtle nil :background "#4f4f4f")
  (set-modeline-faces)
  (with-eval-after-load 'cus-edit (set-button-faces)))

;; Set theme
(elegance-dark)

;; Structural
(set-face 'bold                                          'face-strong)
(set-face 'italic                                         'face-faded)
(set-face 'bold-italic                                   'face-strong)
(set-face 'region                                        'face-subtle)
(set-face 'highlight                                     'face-subtle)
(set-face 'fixed-pitch                                       'default)
(set-face 'fixed-pitch-serif                                 'default)
(set-face 'variable-pitch                                    'default)
(set-face 'cursor                                            'default)

;; Semantic
(set-face 'shadow                                         'face-faded)
(set-face 'success                                      'face-salient)
(set-face 'warning                                       'face-popout)
(set-face 'error                                       'face-critical)

;; General
(set-face 'buffer-menu-buffer                            'face-strong)
(set-face 'minibuffer-prompt                             'face-strong)
(set-face 'link                                         'face-salient)
(set-face 'fringe                                         'face-faded)
(set-face 'isearch                                       'face-strong)
(set-face 'isearch-fail                                   'face-faded)
(set-face 'lazy-highlight                                'face-subtle)
(set-face 'trailing-whitespace                           'face-subtle)
(set-face 'show-paren-match                              'face-popout)
(set-face 'show-paren-mismatch                           'face-normal)
(set-face-attribute 'tooltip nil                         :height 0.85)

;; Programmation mode
(set-face 'font-lock-comment-face                         'face-faded)
(set-face 'font-lock-doc-face                             'face-faded)
(set-face 'font-lock-string-face                         'face-popout)
(set-face 'font-lock-constant-face                      'face-salient)
(set-face 'font-lock-warning-face                        'face-popout)
(set-face 'font-lock-function-name-face                  'face-strong)
(set-face 'font-lock-variable-name-face                  'face-strong)
(set-face 'font-lock-builtin-face                       'face-salient)
(set-face 'font-lock-type-face                          'face-salient)
(set-face 'font-lock-keyword-face                       'face-salient)

;; Documentation
(with-eval-after-load 'info
  (set-face 'info-menu-header                            'face-strong)
  (set-face 'info-header-node                            'face-normal)
  (set-face 'Info-quoted                                  'face-faded)
  (set-face 'info-title-1                                'face-strong)
  (set-face 'info-title-2                                'face-strong)
  (set-face 'info-title-3                                'face-strong)
  (set-face 'info-title-4                               'face-strong))

;; Bookmarks
(with-eval-after-load 'bookmark
  (set-face 'bookmark-menu-heading                       'face-strong)
  (set-face 'bookmark-menu-bookmark                    'face-salient))

;; Message
(with-eval-after-load 'message
  (set-face 'message-cited-text                           'face-faded)
  (set-face 'message-header-cc                               'default)
  (set-face 'message-header-name                         'face-strong)
  (set-face 'message-header-newsgroups                       'default)
  (set-face 'message-header-other                            'default)
  (set-face 'message-header-subject                     'face-salient)
  (set-face 'message-header-to                          'face-salient)
  (set-face 'message-header-xheader                          'default)
  (set-face 'message-mml                                 'face-popout)
  (set-face 'message-separator                           'face-faded))

;; Outline
(with-eval-after-load 'outline
  (set-face 'outline-1                                   'face-strong)
  (set-face 'outline-2                                   'face-strong)
  (set-face 'outline-3                                   'face-strong)
  (set-face 'outline-4                                   'face-strong)
  (set-face 'outline-5                                   'face-strong)
  (set-face 'outline-6                                   'face-strong)
  (set-face 'outline-7                                   'face-strong)
  (set-face 'outline-8                                  'face-strong))

;; Interface
(with-eval-after-load 'cus-edit
  (set-face 'widget-field                                'face-subtle)
  (set-face 'widget-button                               'face-strong)
  (set-face 'widget-single-line-field                    'face-subtle)
  (set-face 'custom-group-subtitle                       'face-strong)
  (set-face 'custom-group-tag                            'face-strong)
  (set-face 'custom-group-tag-1                          'face-strong)
  (set-face 'custom-comment                               'face-faded)
  (set-face 'custom-comment-tag                           'face-faded)
  (set-face 'custom-changed                             'face-salient)
  (set-face 'custom-modified                            'face-salient)
  (set-face 'custom-face-tag                             'face-strong)
  (set-face 'custom-variable-tag                             'default)
  (set-face 'custom-invalid                              'face-popout)
  (set-face 'custom-visibility                          'face-salient)
  (set-face 'custom-state                               'face-salient)
  (set-face 'custom-link                               'face-salient))

;; Package
(with-eval-after-load 'package
  (set-face 'package-description                             'default)
  (set-face 'package-help-section-name                       'default)
  (set-face 'package-name                               'face-salient)
  (set-face 'package-status-avail-obso                    'face-faded)
  (set-face 'package-status-available                        'default)
  (set-face 'package-status-built-in                    'face-salient)
  (set-face 'package-status-dependency                  'face-salient)
  (set-face 'package-status-disabled                      'face-faded)
  (set-face 'package-status-external                         'default)
  (set-face 'package-status-held                             'default)
  (set-face 'package-status-incompat                      'face-faded)
  (set-face 'package-status-installed                   'face-salient)
  (set-face 'package-status-new                              'default)
  (set-face 'package-status-unsigned                         'default)

  ;; Button face is hardcoded, we have to redefine the relevant
  ;; function
  (defun package-make-button (text &rest properties)
    "Insert button labeled TEXT with button PROPERTIES at point.
PROPERTIES are passed to `insert-text-button', for which this
function is a convenience wrapper used by `describe-package-1'."
    (let ((button-text (if (display-graphic-p)
                           text (concat "[" text "]")))
          (button-face (if (display-graphic-p)
                           '(:box `(:line-width 1
                                                :color "#999999":style nil)
                                  :foreground "#999999"
                                  :background "#F0F0F0")
                         'link)))
      (apply #'insert-text-button button-text
             'face button-face 'follow-link t properties)))
  )

;; Flyspell
(with-eval-after-load 'flyspell
  (set-face 'flyspell-duplicate                         'face-popout)
  (set-face 'flyspell-incorrect                         'face-popout))

;; Ido
(with-eval-after-load 'ido
  (set-face 'ido-first-match                            'face-salient)
  (set-face 'ido-only-match                               'face-faded)
  (set-face 'ido-subdir                                 'face-strong))

;; Diff
(with-eval-after-load 'diff-mode
  (set-face 'diff-header                                  'face-faded)
  (set-face 'diff-file-header                            'face-strong)
  (set-face 'diff-context                                    'default)
  (set-face 'diff-removed                                 'face-faded)
  (set-face 'diff-changed                                'face-popout)
  (set-face 'diff-added                                 'face-salient)
  (set-face 'diff-refine-added            '(face-salient face-strong))
  (set-face 'diff-refine-changed                         'face-popout)
  (set-face 'diff-refine-removed                          'face-faded)
  (set-face-attribute     'diff-refine-removed nil :strike-through t))

;; Term
(with-eval-after-load 'term
  ;; (setq eterm-256color-disable-bold nil)
  (set-face 'term-bold                                   'face-strong)
  (set-face-attribute 'term-color-black nil
                      :foreground (face-foreground 'default)
                      :background (face-foreground 'default))
  (set-face-attribute 'term-color-white nil
                      :foreground "white" :background "white")
  (set-face-attribute 'term-color-blue nil
                      :foreground "#42A5F5" :background "#BBDEFB")
  (set-face-attribute 'term-color-cyan nil
                      :foreground "#26C6DA" :background "#B2EBF2")
  (set-face-attribute 'term-color-green nil
                      :foreground "#66BB6A" :background "#C8E6C9")
  (set-face-attribute 'term-color-magenta nil
                      :foreground "#AB47BC" :background "#E1BEE7")
  (set-face-attribute 'term-color-red nil
                      :foreground "#EF5350" :background "#FFCDD2")
  (set-face-attribute 'term-color-yellow nil
                      :foreground "#FFEE58" :background "#FFF9C4"))

;; org-agende
(with-eval-after-load 'org-agenda
  (set-face 'org-agenda-calendar-event                    'default)
  (set-face 'org-agenda-calendar-sexp                     'face-faded)
  (set-face 'org-agenda-clocking                          'face-faded)
  (set-face 'org-agenda-column-dateline                   'face-faded)
  (set-face 'org-agenda-current-time                      'face-faded)
  (set-face 'org-agenda-date                            'face-salient)
  (set-face 'org-agenda-date-today        '(face-salient face-strong))
  (set-face 'org-agenda-date-weekend                      'face-faded)
  (set-face 'org-agenda-diary                             'face-faded)
  (set-face 'org-agenda-dimmed-todo-face                  'face-faded)
  (set-face 'org-agenda-done                              'face-faded)
  (set-face 'org-agenda-filter-category                   'face-faded)
  (set-face 'org-agenda-filter-effort                     'face-faded)
  (set-face 'org-agenda-filter-regexp                     'face-faded)
  (set-face 'org-agenda-filter-tags                       'face-faded)
  ;; fixes issue #18 (set-face 'org-agenda-property-face                     'face-faded)
  (set-face 'org-agenda-restriction-lock                  'face-faded)
  (set-face 'org-agenda-structure                        'face-faded))

;; org mode
(with-eval-after-load 'org
  (set-face 'org-archived                                 'face-faded)
  (set-face 'org-block                                    'face-faded)
  (set-face 'org-block-begin-line                         'face-faded)
  (set-face 'org-block-end-line                           'face-faded)
  (set-face 'org-checkbox                                 'face-faded)
  (set-face 'org-checkbox-statistics-done                 'face-faded)
  (set-face 'org-checkbox-statistics-todo                 'face-faded)
  (set-face 'org-clock-overlay                            'face-faded)
  (set-face 'org-code                                     'face-faded)
  (set-face 'org-column                                   'face-faded)
  (set-face 'org-column-title                             'face-faded)
  (set-face 'org-date                                     'face-faded)
  (set-face 'org-date-selected                            'face-faded)
  (set-face 'org-default                                  'face-faded)
  (set-face 'org-document-info                            'face-faded)
  (set-face 'org-document-info-keyword                    'face-faded)
  (set-face 'org-document-title                           'face-faded)
  (set-face 'org-done                                        'default)
  (set-face 'org-drawer                                   'face-faded)
  (set-face 'org-ellipsis                                 'face-faded)
  (set-face 'org-footnote                                 'face-faded)
  (set-face 'org-formula                                  'face-faded)
  (set-face 'org-headline-done                            'face-faded)
  ;;  (set-face 'org-hide                                     'face-faded)
  ;;  (set-face 'org-indent                                   'face-faded)
  (set-face 'org-latex-and-related                        'face-faded)
  (set-face 'org-level-1                                 'face-strong)
  (set-face 'org-level-2                                 'face-strong)
  (set-face 'org-level-3                                 'face-strong)
  (set-face 'org-level-4                                 'face-strong)
  (set-face 'org-level-5                                 'face-strong)
  (set-face 'org-level-6                                 'face-strong)
  (set-face 'org-level-7                                 'face-strong)
  (set-face 'org-level-8                                 'face-strong)
  (set-face 'org-link                                   'face-salient)
  (set-face 'org-list-dt                                  'face-faded)
  (set-face 'org-macro                                    'face-faded)
  (set-face 'org-meta-line                                'face-faded)
  (set-face 'org-mode-line-clock                          'face-faded)
  (set-face 'org-mode-line-clock-overrun                  'face-faded)
  (set-face 'org-priority                                 'face-faded)
  (set-face 'org-property-value                           'face-faded)
  (set-face 'org-quote                                    'face-faded)
  (set-face 'org-scheduled                                'face-faded)
  (set-face 'org-scheduled-previously                     'face-faded)
  (set-face 'org-scheduled-today                          'face-faded)
  (set-face 'org-sexp-date                                'face-faded)
  (set-face 'org-special-keyword                          'face-faded)
  (set-face 'org-table                                    'default)
  (set-face 'org-tag                                      'face-faded)
  (set-face 'org-tag-group                                'face-faded)
  (set-face 'org-target                                   'face-faded)
  (set-face 'org-time-grid                                'face-faded)
  (set-face 'org-todo                                    'face-popout)
  (set-face 'org-upcoming-deadline                        'face-faded)
  (set-face 'org-verbatim                                 'face-faded)
  (set-face 'org-verse                                    'face-faded)
  (set-face 'org-warning                                'face-popout))

;; Mu4e
(with-eval-after-load 'mu4e
  (set-face 'mu4e-attach-number-face                     'face-strong)
  (set-face 'mu4e-cited-1-face                            'face-faded)
  (set-face 'mu4e-cited-2-face                            'face-faded)
  (set-face 'mu4e-cited-3-face                            'face-faded)
  (set-face 'mu4e-cited-4-face                            'face-faded)
  (set-face 'mu4e-cited-5-face                            'face-faded)
  (set-face 'mu4e-cited-6-face                            'face-faded)
  (set-face 'mu4e-cited-7-face                            'face-faded)
  (set-face 'mu4e-compose-header-face                     'face-faded)
  (set-face 'mu4e-compose-separator-face                  'face-faded)
  (set-face 'mu4e-contact-face                          'face-salient)
  (set-face 'mu4e-context-face                            'face-faded)
  (set-face 'mu4e-draft-face                              'face-faded)
  (set-face 'mu4e-flagged-face                            'face-faded)
  (set-face 'mu4e-footer-face                             'face-faded)
  (set-face 'mu4e-forwarded-face                          'face-faded)
  (set-face 'mu4e-header-face                                'default)
  (set-face 'mu4e-header-highlight-face                  'face-subtle)
  (set-face 'mu4e-header-key-face                        'face-strong)
  (set-face 'mu4e-header-marks-face                       'face-faded)
  (set-face 'mu4e-header-title-face                      'face-strong)
  (set-face 'mu4e-header-value-face                          'default)
  (set-face 'mu4e-highlight-face                         'face-popout)
  (set-face 'mu4e-link-face                             'face-salient)
  (set-face 'mu4e-modeline-face                           'face-faded)
  (set-face 'mu4e-moved-face                              'face-faded)
  (set-face 'mu4e-ok-face                                 'face-faded)
  (set-face 'mu4e-region-code                             'face-faded)
  (set-face 'mu4e-replied-face                          'face-salient)
  (set-face 'mu4e-special-header-value-face                  'default)
  (set-face 'mu4e-system-face                             'face-faded)
  (set-face 'mu4e-title-face                             'face-strong)
  (set-face 'mu4e-trashed-face                            'face-faded)
  (set-face 'mu4e-unread-face                            'face-strong)
  (set-face 'mu4e-url-number-face                         'face-faded)
  (set-face 'mu4e-view-body-face                             'default)
  (set-face 'mu4e-warning-face                            'face-faded))


(message "ELEGANCE ENDS")



(provide 'elegant)
;;; elegant.el ends here
