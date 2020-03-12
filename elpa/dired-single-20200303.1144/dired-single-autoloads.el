;;; dired-single-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "dired-single" "dired-single.el" (0 0 0 0))
;;; Generated autoloads from dired-single.el

(autoload 'dired-single-buffer "dired-single" "\
Visit selected directory in current buffer.

Visits the selected directory in the current buffer, replacing the
   current contents with the contents of the new directory.  This doesn't
   prevent you from having more than one dired buffer.  The main difference
   is that a given dired buffer will not spawn off a new buffer every time
   a new directory is visited.

If the variable `dired-single-use-magic-buffer' is non-nil, and the current
   buffer's name is the same as that specified by the variable
`dired-single-magic-buffer-name', then the new directory's buffer will retain
   that same name (i.e. not only will dired only use a single buffer, but
its name will not change every time a new directory is entered).

Optional argument DEFAULT-DIRNAME specifies the directory to visit; if not
specified, the directory or file on the current line is used (assuming it's
a dired buffer).  If the current line represents a file, the file is visited
in another window.

\(fn &optional DEFAULT-DIRNAME)" t nil)

(autoload 'dired-single-buffer-mouse "dired-single" "\
Mouse-initiated version of `dired-single-buffer' (which see).

Argument CLICK is the mouse-click event.

\(fn CLICK)" t nil)

(autoload 'dired-single-magic-buffer "dired-single" "\
Switch to buffer whose name is the value of `dired-single-magic-buffer-name'.

If no such buffer exists, launch dired in a new buffer and rename that buffer
to the value of `dired-single-magic-buffer-name'.  If the current buffer is the
magic buffer, it will prompt for a new directory to visit.

Optional argument DEFAULT-DIRNAME specifies the directory to visit (defaults to
the currently displayed directory).

\(fn &optional DEFAULT-DIRNAME)" t nil)

(autoload 'dired-single-toggle-buffer-name "dired-single" "\
Toggle between the 'magic' buffer name and the 'real' dired buffer name.

Will also seek to uniquify the 'real' buffer name." t nil)

(autoload 'dired-single-up-directory "dired-single" "\
Like `dired-up-directory' but with `dired-single-buffer'.

\(fn &optional OTHER-WINDOW)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dired-single" '("dired-single-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; dired-single-autoloads.el ends here
