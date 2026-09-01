;;; grove-core.el --- Core definitions for grove -*- lexical-binding: t -*-

;; Copyright 2026 Jonathan Chu

;; Author: Jonathan Chu <me@jonathanchu.is>
;; URL: https://github.com/jonathanchu/grove

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Shared customization options, vault cache, and utility functions
;; used across all grove modules.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

;;;; Customization

(defgroup grove nil
  "Obsidian-like note-taking for Org and Markdown notes."
  :group 'org
  :prefix "grove-")

(defcustom grove-directory nil
  "Root directory for grove notes.
Every file in this directory and its subdirectories whose extension
is in `grove-file-extensions' is considered part of the vault.
When `grove-profiles' is non-nil, this is used only as a fallback
for configs that have not yet migrated to profiles."
  :type '(choice (const nil) directory)
  :group 'grove)

(defcustom grove-profiles nil
  "Alist of grove profiles, each of the form (NAME :directory DIR).
NAME is a symbol identifying the profile.  DIR is the root directory.
Example:
  (setq grove-profiles
    \\='((personal :directory \"~/notes\")
      (work     :directory \"~/work/notes\")))
Use `grove-switch-profile' to select the active profile."
  :type '(alist :key-type symbol
                :value-type (plist :key-type keyword :value-type sexp))
  :group 'grove)

(defcustom grove-file-extensions '("org" "md")
  "List of file extensions, without the leading dot, treated as notes.
Every file under the vault root with one of these extensions is
scanned, cached, searched, and shown in the tree sidebar.  Extensions
are compared case-insensitively, so \"org\" also covers NOTE.ORG.

Org and Markdown notes can share one vault; grove reads each in its own
format.  An extension grove has no format for is still indexed, but its
title and tags are read with the Org rules."
  :type '(repeat string)
  :group 'grove)

(defcustom grove-default-format 'org
  "Format used for notes grove creates.
Affects `grove-capture', `grove-daily', and the note offered when a
wikilink points at a title the vault does not have yet.  Notes that
already exist are always read in their own format, whatever this is
set to."
  :type '(choice (const :tag "Org" org)
                 (const :tag "Markdown" md))
  :group 'grove)

(defcustom grove-inbox-directory "inbox"
  "Subdirectory of `grove-directory' for captured notes.
Relative to `grove-directory'."
  :type 'string
  :group 'grove)

(defcustom grove-daily-directory "daily"
  "Subdirectory of `grove-directory' for daily notes.
Relative to `grove-directory'."
  :type 'string
  :group 'grove)

(defcustom grove-daily-format "%Y-%m-%d"
  "Format string for daily note filenames.
Passed to `format-time-string'."
  :type 'string
  :group 'grove)

;;;; Profile state

(defvar grove--active-profile nil
  "Symbol naming the currently active profile, or nil to use `grove-directory'.")

(defvar grove--profile-caches (make-hash-table :test #'eq)
  "Hash table mapping profile name symbols to their vault cache hash tables.")

(defun grove--profile-directory (profile)
  "Return the expanded root directory of PROFILE, or nil if it has none.
PROFILE is an entry of `grove-profiles'."
  (let ((dir (plist-get (cdr profile) :directory)))
    (when (stringp dir)
      (file-name-as-directory (expand-file-name dir)))))

(defun grove--active-directory ()
  "Return the root directory for the active profile or `grove-directory'.
Returns nil if neither is configured."
  (if grove--active-profile
      (let ((profile (assq grove--active-profile grove-profiles)))
        (unless profile
          (user-error "Unknown grove profile: %s" grove--active-profile))
        (or (grove--profile-directory profile)
            (user-error "Grove profile %s has no :directory"
                        grove--active-profile)))
    (when grove-directory
      (file-name-as-directory (expand-file-name grove-directory)))))

(defun grove--active-cache ()
  "Return the cache hash table for the active profile.
Creates a new empty cache lazily on first access per profile."
  (if grove--active-profile
      (or (gethash grove--active-profile grove--profile-caches)
          (let ((cache (make-hash-table :test #'equal)))
            (puthash grove--active-profile cache grove--profile-caches)
            cache))
    grove--cache))

(defun grove--profile-cache (name)
  "Return the note cache for profile NAME, scanning its vault if needed.
Unlike `grove--active-cache', this populates the cache on first use so
callers can inspect profiles the user has not activated this session.
Returns nil if NAME is unknown or its directory does not exist."
  (or (gethash name grove--profile-caches)
      (let* ((profile (assq name grove-profiles))
             (dir (and profile (grove--profile-directory profile))))
        (when (and dir (file-directory-p dir))
          (let ((grove--active-profile name))
            (grove--refresh-cache)
            (grove--active-cache))))))

(defun grove--profile-for-file (file)
  "Return the profile name symbol whose directory contains FILE, or nil.
Profiles with no `:directory' are skipped."
  (when grove-profiles
    (catch 'found
      (dolist (profile grove-profiles)
        (let ((dir (grove--profile-directory profile)))
          (when (and dir (string-prefix-p dir (expand-file-name file)))
            (throw 'found (car profile))))))))

;;;; Vault cache

(defvar grove--cache (make-hash-table :test #'equal)
  "Hash table mapping absolute file paths to note metadata plists.
Used when no profile is active.
Each value is a plist with keys :title :tags :links :mtime.")

(defconst grove--hashtag-regexp
  "\\(?:^\\|[^[:alnum:]_#]\\)#\\([[:alnum:]_][[:alnum:]_/-]*\\)\\b"
  "Regexp matching inline hashtags in note content.")

(defconst grove--org-link-protocol-regexp
  "\\`[[:alpha:]][[:alnum:]+.-]*:"
  "Regexp matching an Org-style link protocol prefix.")

(defun grove--org-link-target-p (target)
  "Return non-nil if TARGET looks like a standard Org link target.
This excludes Grove wikilinks such as note titles containing colons."
  (and (string-match-p grove--org-link-protocol-regexp target)
       (not (string-match-p "[[:space:]]" target))))

(defun grove--collect-inline-tags ()
  "Return inline hashtags from the current buffer.
Matches #tag-style markers and returns them without the leading hash.
Org keyword lines such as #+title: are ignored, as are Markdown ATX
headings: those put whitespace after the hash, which the regexp does
not accept."
  (let (tags)
    (goto-char (point-min))
    (while (re-search-forward grove--hashtag-regexp nil t)
      (push (match-string-no-properties 1) tags))
    (nreverse tags)))

(defun grove--merge-tags (&rest tag-lists)
  "Return unique tags from TAG-LISTS, preserving first-seen order."
  (let ((seen (make-hash-table :test #'equal))
        result)
    (dolist (tags tag-lists)
      (dolist (tag tags)
        (unless (or (null tag)
                    (string-empty-p tag)
                    (gethash tag seen))
          (puthash tag t seen)
          (push tag result))))
    (nreverse result)))

(defun grove--ensure-directory ()
  "Ensure the active grove directory is set and exists, or prompt the user."
  (when (and grove-profiles (not grove--active-profile))
    (setq grove--active-profile
          (intern (completing-read "Grove profile: "
                                   (mapcar (lambda (p) (symbol-name (car p)))
                                           grove-profiles)
                                   nil t))))
  (if grove--active-profile
      (let ((dir (grove--active-directory)))
        (unless (file-directory-p dir)
          (if (y-or-n-p (format "Directory %s does not exist.  Create it? " dir))
              (make-directory dir t)
            (user-error "Grove requires a vault directory"))))
    (unless grove-directory
      (setq grove-directory
            (read-directory-name "Grove vault directory: ")))
    (unless (file-directory-p grove-directory)
      (if (y-or-n-p (format "Directory %s does not exist.  Create it? "
                            grove-directory))
          (make-directory grove-directory t)
        (user-error "Grove requires a vault directory")))
    (setq grove-directory (file-name-as-directory
                           (expand-file-name grove-directory)))))

(defun grove--inbox-path ()
  "Return the absolute path to the inbox directory, creating it if needed."
  (grove--ensure-directory)
  (let ((path (expand-file-name grove-inbox-directory (grove--active-directory))))
    (unless (file-directory-p path)
      (make-directory path t))
    (file-name-as-directory path)))

(defun grove--daily-path ()
  "Return the absolute path to the daily notes directory, creating it if needed."
  (grove--ensure-directory)
  (let ((path (expand-file-name grove-daily-directory (grove--active-directory))))
    (unless (file-directory-p path)
      (make-directory path t))
    (file-name-as-directory path)))

(defun grove--unique-path (directory filename)
  "Return a unique file path in DIRECTORY for FILENAME.
Appends a numeric suffix if the file already exists."
  (let ((base (file-name-sans-extension filename))
        (ext (file-name-extension filename t))
        (path (expand-file-name filename directory)))
    (if (not (file-exists-p path))
        path
      (let ((n 1))
        (while (file-exists-p
                (setq path (expand-file-name
                            (concat base (format "-%d" n) ext)
                            directory)))
          (setq n (1+ n)))
        path))))

;;;; Note formats

(defconst grove--formats
  '((org :extension "org" :mode org-mode
         :metadata-fn grove--org-metadata :header-fn grove--org-header)
    (md  :extension "md"  :mode markdown-mode
         :metadata-fn grove--md-metadata :header-fn grove--md-header))
  "Alist of note formats grove understands.
Each entry is (FORMAT . PLIST).  `:extension' is the file extension
that selects the format, `:mode' the major mode for editing it,
`:metadata-fn' a function returning the note's (TITLE . TAGS) from the
current buffer, and `:header-fn' a function returning the header text
for a new note.")

(defun grove--format-property (format property)
  "Return PROPERTY of FORMAT.
Signals an error when FORMAT names no format grove knows."
  (let ((entry (assq format grove--formats)))
    (unless entry
      (user-error "Unknown grove format: %s (see `grove--formats')" format))
    (plist-get (cdr entry) property)))

(defun grove--format-for-file (file)
  "Return the format symbol for FILE, based on its extension.
Defaults to `org' for extensions no format claims, so an unrecognized
note is still indexed rather than dropped."
  (let ((extension (file-name-extension file)))
    (or (and extension
             (car (seq-find
                   (lambda (entry)
                     (string-equal-ignore-case
                      (plist-get (cdr entry) :extension) extension))
                   grove--formats)))
        'org)))

;;;; Metadata extraction

(defun grove--org-metadata ()
  "Return (TITLE . TAGS) from the Org keywords in the current buffer.
Reads #+title: and #+filetags:.  Either element is nil when absent."
  (let (title tags)
    (goto-char (point-min))
    (when (re-search-forward "^#\\+title:\\s-*\\(.+\\)" nil t)
      (setq title (string-trim (match-string-no-properties 1))))
    (goto-char (point-min))
    (when (re-search-forward "^#\\+filetags:\\s-*\\(.+\\)" nil t)
      (setq tags (split-string (match-string-no-properties 1) ":" t "\\s-*")))
    (cons title tags)))

(defun grove--md-unquote (string)
  "Return STRING with a matching pair of surrounding YAML quotes removed.
Escapes inside the quotes are resolved: backslash escapes for a
double-quoted scalar, doubled quotes for a single-quoted one."
  (let* ((trimmed (string-trim string))
         (size (length trimmed))
         (first (and (> size 1) (aref trimmed 0))))
    (cond
     ((and (eq first ?\") (eq (aref trimmed (1- size)) ?\"))
      (replace-regexp-in-string "\\\\\\(.\\)" "\\1"
                                (substring trimmed 1 -1) t))
     ((and (eq first ?') (eq (aref trimmed (1- size)) ?'))
      (replace-regexp-in-string "''" "'" (substring trimmed 1 -1) t))
     (t trimmed))))

(defun grove--md-quote (string)
  "Return STRING as a double-quoted YAML scalar.
Quoting unconditionally keeps a title containing a colon readable by
other tools, which would otherwise parse it as a nested key."
  (concat "\"" (replace-regexp-in-string "[\\\"]" "\\\\\\&" string) "\""))

(defun grove--md-normalize-tag (string)
  "Return the tag named by front matter entry STRING.
Front matter writes tags both as \"tag\" and as #tag; grove stores them
bare so they match the inline #hashtags of a note body."
  (string-remove-prefix "#" (grove--md-unquote string)))

(defun grove--md-frontmatter-end ()
  "Return the position where YAML front matter ends, or nil if there is none.
Point is left just after the opening delimiter.  Front matter counts
only when it opens on the very first line, as YAML requires."
  (goto-char (point-min))
  (when (looking-at "---[[:blank:]]*$")
    (forward-line 1)
    (when (re-search-forward "^\\(?:---\\|\\.\\.\\.\\)[[:blank:]]*$" nil t)
      (prog1 (match-beginning 0)
        (goto-char (point-min))
        (forward-line 1)))))

(defun grove--md-frontmatter-title (end)
  "Return the front matter title: value before END, or nil."
  (when (re-search-forward "^title:[[:blank:]]*\\(.+\\)$" end t)
    (let ((title (grove--md-unquote (match-string-no-properties 1))))
      (unless (string-empty-p title) title))))

(defun grove--md-frontmatter-tags (start end)
  "Return the front matter tags: value between START and END, or nil.
Accepts both YAML forms Obsidian writes: a flow sequence on the key's
own line (\"tags: [a, b]\" or \"tags: a, b\") and a block sequence of
\"- tag\" lines beneath it."
  (goto-char start)
  (when (re-search-forward "^tags:[[:blank:]]*\\(.*\\)$" end t)
    (let ((inline (string-trim (match-string-no-properties 1)))
          tags)
      (if (string-empty-p inline)
          (let ((scanning t))
            (while (and scanning
                        (zerop (forward-line 1))
                        (< (point) end)
                        (looking-at "[[:blank:]]*-[[:blank:]]*\\(.+\\)$"))
              (let ((tag (grove--md-normalize-tag
                          (match-string-no-properties 1))))
                (if (string-empty-p tag)
                    (setq scanning nil)
                  (push tag tags)))))
        (dolist (tag (split-string (string-trim inline "\\[" "\\]") "," t))
          (dolist (word (split-string tag nil t))
            (push (grove--md-normalize-tag word) tags))))
      (nreverse (seq-remove #'string-empty-p tags)))))

(defun grove--md-metadata ()
  "Return (TITLE . TAGS) for the Markdown note in the current buffer.
The title is the front matter title:, else the first level-one ATX
heading, else nil so the caller can fall back to the filename.  Tags
come from a front matter tags: key; inline #hashtags are collected
separately for every format."
  (let* ((end (grove--md-frontmatter-end))
         (body (if end (save-excursion (goto-char end) (forward-line 1) (point))
                 (point-min)))
         title tags)
    (when end
      (setq title (save-excursion (grove--md-frontmatter-title end)))
      (setq tags (grove--md-frontmatter-tags (point) end)))
    (unless title
      (goto-char body)
      (when (re-search-forward "^#[[:blank:]]+\\(.+?\\)[[:blank:]]*#*[[:blank:]]*$"
                               nil t)
        (setq title (string-trim (match-string-no-properties 1)))))
    (cons title tags)))

;;;; Note parsing

(defun grove--collect-wikilinks ()
  "Return the wikilink targets in the current buffer, in document order.
Standard Org link targets such as https: or file: are skipped."
  (let (links)
    (goto-char (point-min))
    (while (re-search-forward "\\[\\[\\([^]]+\\)\\]\\]" nil t)
      (let ((link (match-string-no-properties 1)))
        (unless (grove--org-link-target-p link)
          (push link links))))
    (nreverse links)))

(defun grove--existing-note-path (directory basename)
  "Return the path of an existing note named BASENAME in DIRECTORY, or nil.
Every extension in `grove-file-extensions' is tried, so a note stays
findable whatever `grove-default-format' is set to now."
  (seq-some (lambda (extension)
              (let ((path (expand-file-name (concat basename "." extension)
                                            directory)))
                (and (file-exists-p path) path)))
            grove-file-extensions))

(defun grove--parse-note (file)
  "Parse note FILE and return a metadata plist.
Returns (:title TITLE :tags TAGS :links LINKS :mtime MTIME).
Title and tags are read with the rules of FILE's format; wikilinks and
inline #hashtags are collected identically for every format, since both
syntaxes mean the same thing in Org and Markdown."
  (let ((mtime (file-attribute-modification-time (file-attributes file)))
        (metadata-fn (grove--format-property (grove--format-for-file file)
                                             :metadata-fn))
        title tags links)
    (with-temp-buffer
      (insert-file-contents file)
      (let ((metadata (funcall metadata-fn)))
        (setq title (car metadata)
              tags (cdr metadata)))
      (setq tags (grove--merge-tags tags (grove--collect-inline-tags)))
      (setq links (grove--collect-wikilinks)))
    (list :title (or title (file-name-sans-extension
                            (file-name-nondirectory file)))
          :tags (and tags (copy-sequence tags))
          :links links
          :mtime mtime)))

(defun grove--note-extension-regexp ()
  "Return an unanchored regexp matching a note file extension.
Built from `grove-file-extensions'.  Use this to pick note paths out
of a larger string, such as a line of ripgrep output."
  (concat "\\." (regexp-opt grove-file-extensions)))

(defun grove--note-regexp ()
  "Return a regexp matching note filenames by their extension.
Intended for `directory-files-recursively', which matches with
`case-fold-search' enabled, so this also picks up NOTE.ORG and NOTE.MD."
  (concat (grove--note-extension-regexp) "\\'"))

(defun grove--rg-glob-args ()
  "Return ripgrep --glob arguments restricting a search to note files."
  (mapcar (lambda (ext) (format "--glob=*.%s" ext)) grove-file-extensions))

(defun grove--note-file-p (file)
  "Return non-nil when FILE has an extension in `grove-file-extensions'.
Compares case-insensitively so callers agree with the vault scan in
`grove--refresh-cache', which folds case."
  (when-let ((ext (file-name-extension file)))
    (seq-contains-p grove-file-extensions ext #'string-equal-ignore-case)))

(defun grove--cacheable-note-p (file)
  "Return non-nil when FILE should be included in the note cache.
Only lock files need excluding here.  Callers scan with
`grove--note-regexp', which already leaves out autosave
(\"#note.org#\") and backup (\"note.org~\") files.  It does not leave
out the \".#note.org\" symlink Emacs creates while a buffer is
modified, and that symlink dangles once the note is renamed or deleted,
which breaks the whole refresh."
  (not (string-prefix-p ".#" (file-name-nondirectory file))))

(defun grove--refresh-cache ()
  "Refresh the vault cache by scanning the active grove directory.
Only re-parses files whose mtime has changed."
  (grove--ensure-directory)
  (let* ((dir (grove--active-directory))
         (cache (grove--active-cache))
         (files (seq-filter #'grove--cacheable-note-p
                            (directory-files-recursively
                             dir (grove--note-regexp))))
         (seen (make-hash-table :test #'equal)))
    ;; Update or add entries
    (dolist (file files)
      (puthash file t seen)
      (let* ((cached (gethash file cache))
             (current-mtime (file-attribute-modification-time
                             (file-attributes file)))
             (cached-mtime (plist-get cached :mtime)))
        (when (or (null cached)
                  (not (equal current-mtime cached-mtime)))
          (puthash file (grove--parse-note file) cache))))
    ;; Remove deleted files
    (maphash (lambda (key _val)
               (unless (gethash key seen)
                 (remhash key cache)))
             cache)))

(defun grove--note-titles ()
  "Return an alist of (TITLE . PATH) for all cached notes."
  (let (result)
    (maphash (lambda (path meta)
               (push (cons (plist-get meta :title) path) result))
             (grove--active-cache))
    (sort result (lambda (a b) (string< (car a) (car b))))))

(defun grove--sanitize-filename (title)
  "Convert TITLE into a safe filename.
Downcases, replaces spaces with hyphens, strips non-alphanumeric characters."
  (let ((name (downcase (string-trim title))))
    (setq name (replace-regexp-in-string "[^a-z0-9 -]" "" name))
    (setq name (replace-regexp-in-string "\\s-+" "-" name))
    (setq name (replace-regexp-in-string "^-+\\|-+$" "" name))
    (if (string-empty-p name) "untitled" name)))

;;;; New notes

(defun grove--org-header (title date)
  "Return the Org header for a new note titled TITLE.
DATE, when non-nil, is recorded as a #+date: keyword."
  (concat "#+title: " title "\n"
          (if date (concat "#+date: " date "\n") "")))

(defun grove--md-header (title date)
  "Return the Markdown header for a new note titled TITLE.
DATE, when non-nil, is recorded as a date: key.  Grove writes YAML
front matter rather than a bare heading so that titles surviving no
filename -- ones with punctuation, or a date alongside them -- round
trip, and reads all three Obsidian conventions back regardless."
  (concat "---\n"
          "title: " (grove--md-quote title) "\n"
          (if date (concat "date: " date "\n") "")
          "---\n"))

(defun grove--note-header (title &optional date)
  "Return the header text for a new note titled TITLE.
DATE is an optional ISO date string recorded alongside the title.
The syntax follows `grove-default-format'."
  (funcall (grove--format-property grove-default-format :header-fn)
           title date))

(defun grove--new-note-extension ()
  "Return the extension, leading dot included, for notes grove creates."
  (concat "." (grove--format-property grove-default-format :extension)))

(defun grove--new-note-filename (title)
  "Return a filename for a new note titled TITLE."
  (concat (grove--sanitize-filename title) (grove--new-note-extension)))

(defun grove--enable-format-mode ()
  "Enable the major mode for `grove-default-format' in the current buffer.
Falls back to `text-mode' when that mode is unavailable, which is the
case for Markdown unless the user has installed `markdown-mode'."
  (let ((mode (grove--format-property grove-default-format :mode)))
    (if (fboundp mode) (funcall mode) (text-mode))))

(defun grove-file-p (file)
  "Return non-nil if FILE is inside the active grove directory.
Returns nil rather than signaling when the active profile is
misconfigured: this runs from `after-change-major-mode-hook' and
`buffer-list-update-hook', where an error would break unrelated buffers."
  (when-let ((dir (ignore-errors (grove--active-directory))))
    (and file (string-prefix-p dir (expand-file-name file)))))

(provide 'grove-core)
;;; grove-core.el ends here
