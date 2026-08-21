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

;;;; Customization

(defgroup grove nil
  "Obsidian-like note-taking for org files."
  :group 'org
  :prefix "grove-")

(defcustom grove-directory nil
  "Root directory for grove notes.
All org files in this directory and its subdirectories are
considered part of the vault.
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
Matches #tag-style markers while ignoring Org keyword lines such as
#+title: and returns tags without the leading hash."
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

(defun grove--parse-note (file)
  "Parse an org FILE and return a metadata plist.
Returns (:title TITLE :tags TAGS :links LINKS :mtime MTIME)."
  (let ((mtime (file-attribute-modification-time (file-attributes file)))
        title tags links)
    (with-temp-buffer
      (insert-file-contents file)
      ;; Extract #+title:
      (goto-char (point-min))
      (when (re-search-forward "^#\\+title:\\s-*\\(.+\\)" nil t)
        (setq title (string-trim (match-string 1))))
      ;; Extract #+filetags: and inline #hashtags.
      (goto-char (point-min))
      (when (re-search-forward "^#\\+filetags:\\s-*\\(.+\\)" nil t)
        (setq tags (split-string (match-string 1) ":" t "\\s-*")))
      (setq tags (grove--merge-tags tags (grove--collect-inline-tags)))
      ;; Extract [[wikilinks]]
      (goto-char (point-min))
      (while (re-search-forward "\\[\\[\\([^]]+\\)\\]\\]" nil t)
        (let ((link (match-string 1)))
          ;; Skip standard org links like http: or file:
          (unless (grove--org-link-target-p link)
            (push link links)))))
    (list :title (or title (file-name-sans-extension
                            (file-name-nondirectory file)))
          :tags (and tags (copy-sequence tags))
          :links (nreverse links)
          :mtime mtime)))

(defun grove--refresh-cache ()
  "Refresh the vault cache by scanning the active grove directory.
Only re-parses files whose mtime has changed."
  (grove--ensure-directory)
  (let* ((dir (grove--active-directory))
         (cache (grove--active-cache))
         (files (directory-files-recursively dir "\\.org\\'"))
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

(defun grove-file-p (file)
  "Return non-nil if FILE is inside the active grove directory.
Returns nil rather than signaling when the active profile is
misconfigured: this runs from `after-change-major-mode-hook' and
`buffer-list-update-hook', where an error would break unrelated buffers."
  (when-let ((dir (ignore-errors (grove--active-directory))))
    (and file (string-prefix-p dir (expand-file-name file)))))

(provide 'grove-core)
;;; grove-core.el ends here
