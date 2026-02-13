;;; resnippets.el --- Regex-based snippets for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Morazotti

;; Author: Morazotti
;; Keywords: convenience, snippets, regex

;;; Commentary:

;; Resnippets allows defining snippets triggered by regular expressions
;; that are checked against the text immediately preceding the cursor.
;; This enables suffix-based expansions and more complex triggers than
;; fixed keywords.

;;; Code:

(defgroup resnippets nil
  "Regex-based snippets."
  :group 'convenience)

(defcustom resnippets-lookback-limit 200
  "Maximum number of characters to look back for a regex match."
  :type 'integer
  :group 'resnippets)

(defvar resnippets--snippets nil
  "Alist of registered snippets.
Each element is of the form (REGEX EXPANSION . PROPS).")

(defvar resnippets--current-project-root nil
  "When non-nil, `resnippets-add' auto-scopes snippets to this directory.
Bound dynamically during project file loading.")

(defun resnippets--project-label (dir)
  "Generate a label for project at DIR."
  (format "project:%s" (md5 dir)))

(defun resnippets-add (regex expansion &rest props)
  "Register a new snippet.

REGEX is a string containing the regular expression to match.
The match must end exactly at the current cursor position.

EXPANSION is a list of strings and integers, or a string.
Strings are inserted literally.
Integers N refer to the Nth capture group from REGEX.

PROPS is a property list of optional conditions:
:mode MODE-OR-LIST - Check if current `major-mode` matches or derives from MODE.
:condition FORM - Evaluate FORM; snippet active if non-nil.

When called during project file loading, snippets are automatically
scoped to buffers within the project directory.

Example:
  (resnippets-add \"\\\\([a-zA-Z]+\\\\)hat\" '(\"\\\\hat{\" 1 \"}\")
                  :mode 'org-mode
                  :condition '(org-inside-LaTeX-fragment-p))"
  ;; Auto-scope when loading from a project file
  (when resnippets--current-project-root
    (let* ((root resnippets--current-project-root)
           (label (resnippets--project-label root))
           (existing-condition (plist-get props :condition))
           (dir-condition `(string-prefix-p ,root default-directory))
           (combined-condition (if existing-condition
                                  `(and ,dir-condition ,existing-condition)
                                dir-condition)))
      (setq props (plist-put props :label label))
      (setq props (plist-put props :condition combined-condition))))
  (let ((explist (if (listp expansion) expansion (list expansion))))
    (push (append (list regex explist) props) resnippets--snippets)))

(defun resnippets-remove (regex)
  "Remove all snippets with the given REGEX key."
  (setq resnippets--snippets
        (cl-remove regex resnippets--snippets
                   :test #'string=
                   :key #'car)))

(defun resnippets-remove-by-label (label)
  "Remove all snippets with the given LABEL."
  (setq resnippets--snippets
        (cl-remove-if (lambda (entry)
                        (string= (plist-get (cddr entry) :label) label))
                      resnippets--snippets)))

(defun resnippets-clear ()
  "Remove all registered snippets."
  (setq resnippets--snippets nil))

(defun resnippets--check-condition (props)
  "Check if snippet with PROPS should be active."
  (let ((mode (plist-get props :mode))
        (condition (plist-get props :condition)))
    (and (or (null mode)
             (if (listp mode)
                 (apply #'derived-mode-p mode)
               (derived-mode-p mode)))
         (or (null condition)
             (eval condition)))))

(defmacro resnippets-define (label-or-props &rest args)
  "Define multiple snippets with shared PROPS.
LABEL-OR-PROPS can be an optional string label, followed by PROPS.
ARGS is PROPS (if label used) and then SNIPPETS.
SNIPPETS is a list of (REGEX EXPANSION [PROPS...]) where PROPS are
optional per-snippet properties that override the shared ones.

If LABEL is provided, any existing snippets with that label are removed first.

Example:
  (resnippets-define \"my-group\" '(:mode org-mode :priority 1)
    (\"alpha\" \"\\\\alpha\" :priority 10)  ;; Override priority
    (\"beta\" \"\\\\beta\"))                ;; Uses shared priority 1"
  (let ((label (if (stringp label-or-props) label-or-props nil))
        (props (if (stringp label-or-props) (car args) label-or-props))
        (snippets (if (stringp label-or-props) (cdr args) args)))
    (let ((props-code (if label
                          `(append (list :label ,label) ,props)
                        props)))
      `(progn
         ,(when label `(resnippets-remove-by-label ,label))
         ,@(mapcar (lambda (s)
                     (let ((regex (car s))
                           (expansion (cadr s))
                           (snippet-props (cddr s)))
                       (if snippet-props
                           ;; Merge: snippet props override shared props
                           `(apply #'resnippets-add ,regex ,expansion
                                   (append (list ,@snippet-props) ,props-code))
                         `(apply #'resnippets-add ,regex ,expansion ,props-code))))
                   snippets)))))

(defvar resnippets--last-match-data nil
  "Holds the match data of the snippet currently being expanded.")

(defvar resnippets--last-match-string nil
  "Holds the match string of the snippet currently being expanded.")

(defun resnippets-group (n)
  "Return the content of capture group N from the current snippet match."
  (let ((match-data resnippets--last-match-data)
        (match-string resnippets--last-match-string))
    (if (and match-data match-string)
        (let ((start (nth (* 2 n) match-data))
              (end (nth (1+ (* 2 n)) match-data)))
          (if (and start end)
              (substring match-string start end)
            ""))
      "")))

(defun resnippets--detect-case-pattern (str)
  "Detect the case pattern of STR.
Returns one of: \\='lower, \\='upper, \\='capitalized, \\='mixed."
  (cond
   ((string= str (downcase str)) 'lower)
   ((string= str (upcase str)) 'upper)
   ((string= str (capitalize str)) 'capitalized)
   (t 'mixed)))

(defun resnippets--apply-case-pattern (pattern str)
  "Apply PATTERN to STR.
PATTERN is one of: \\='lower, \\='upper, \\='capitalized, \\='mixed."
  (pcase pattern
    ('lower (downcase str))
    ('upper (upcase str))
    ('capitalized (capitalize str))
    (_ str)))

(defcustom resnippets-expand-env nil
  "Alist of variables to bind during snippet expansion.
Each element is a cons cell (VARIABLE . VALUE).
Example: '((smartparens-mode . nil) (cdlatex-mode . nil))"
  :type '(alist :key-type variable :value-type sexp)
  :group 'resnippets)

(defun resnippets--expand (match-string match-data expansion &optional match-case)
  "Expand the snippet.
MATCH-STRING is the full text that was matched (from buffer).
MATCH-DATA is the list of indices from `string-match` on MATCH-STRING.
EXPANSION is the definition list.
When MATCH-CASE is non-nil, apply the detected case pattern to strings."
  ;; The full match (group 0) spans the whole MATCH-STRING if the regex covered it,
  ;; but wait, we effectively matched (concat regex "\\'").
  ;; Group 0 is the entire match.
  ;; We will use MATCH-STRING to extract groups.
  (undo-boundary)  ;; Group all changes for atomic undo
  (delete-region (- (point) (length (substring match-string (nth 0 match-data) (nth 1 match-data))))
                 (point))
  (let* ((final-point nil)
         (resnippets--last-match-data match-data)
         (resnippets--last-match-string match-string)
         (matched-text (substring match-string (nth 0 match-data) (nth 1 match-data)))
         (case-pattern (when match-case (resnippets--detect-case-pattern matched-text))))
    (let ((vars (mapcar #'car resnippets-expand-env))
          (vals (mapcar #'cdr resnippets-expand-env)))
      (cl-progv vars vals
        (dolist (item expansion)
          (cond
           ((stringp item)
            (insert (if case-pattern
                        (resnippets--apply-case-pattern case-pattern item)
                      item)))
           ((integerp item)
            (insert (resnippets-group item)))
           ((or (equal item '(resnippet-cursor))
                (equal item '(resnippets-cursor)))
            (setq final-point (point-marker)))
           ((listp item)
            (let ((result
                   (condition-case err
                       (eval item)
                     (error (message "Resnippets error: %S" err) nil))))
              (when (or (stringp result) (numberp result))
                (insert (format "%s" result)))))
           (t "")))))
    (when final-point
      (goto-char final-point)
      (set-marker final-point nil))))

(defvar resnippets--chain-depth 0
  "Current depth of chained expansions.")

(defcustom resnippets-max-chain-depth 10
  "Maximum depth for chained snippet expansions to prevent infinite loops."
  :type 'integer
  :group 'resnippets)

(defun resnippets--check ()
  "Check if the text before point matches any registered snippet.
When multiple snippets match, the one with highest :priority wins (default 0).
When :word-boundary is t, the snippet only matches at word boundaries.
When :suffix is t, the snippet only expands after a non-alphabetic character is typed.
When :chain is t, after expansion, check for more snippet matches."
  (let* ((limit (max (point-min) (- (point) resnippets-lookback-limit)))
         (text-to-check (buffer-substring-no-properties limit (point)))
         (matches nil))
    ;; Collect all matching snippets
    (cl-loop for (regex expansion . props) in resnippets--snippets
             for match-case = (plist-get props :match-case)
             for word-boundary = (plist-get props :word-boundary)
             for suffix = (plist-get props :suffix)
             for case-fold-search = match-case
             for base-regex = (if word-boundary
                                  (concat "\\_<" regex)
                                regex)
             for effective-regex = (if suffix
                                      (concat base-regex "\\(?:[^a-zA-Z]\\)")
                                    base-regex)
             when (and (resnippets--check-condition props)
                       (string-match (concat effective-regex "\\'") text-to-check))
             do (push (list (match-data) expansion props) matches))
    ;; Sort by priority (highest first) and expand the winner
    (when matches
      (let* ((sorted (sort matches
                           (lambda (a b)
                             (> (or (plist-get (nth 2 a) :priority) 0)
                                (or (plist-get (nth 2 b) :priority) 0)))))
             (winner (car sorted))
             (data (nth 0 winner))
             (expansion (nth 1 winner))
             (props (nth 2 winner))
             (match-case (plist-get props :match-case))
             (suffix (plist-get props :suffix))
             (chain (plist-get props :chain)))
        (resnippets--expand text-to-check data expansion match-case)
        ;; Re-insert the suffix character that was consumed
        (when suffix
          ;; Suffix char is the last char of group 0 match
          (let* ((match-end (nth 1 data))
                 (suffix-char (substring text-to-check (1- match-end) match-end)))
            (insert suffix-char)))
        ;; Chain: try to match more snippets after expansion
        (when (and chain (< resnippets--chain-depth resnippets-max-chain-depth))
          (let ((resnippets--chain-depth (1+ resnippets--chain-depth)))
            (resnippets--check)))
        t))))

(defun resnippets--post-command-handler ()
  "Handler for `post-command-hook` (or `post-self-insert-hook`)."
  ;; Only check if the last command was a self-insert operation.
  ;; We use `this-command` or simply rely on hooking into `post-self-insert-hook`.
  ;; However, `post-self-insert-hook` is safer to avoid expanding on cursors moves.
  (resnippets--check))

;;;###autoload
(define-minor-mode resnippets-mode
  "Minor mode for regex-based snippets."
  :init-value nil
  :lighter " ReSnip"
  (if resnippets-mode
      (progn
        (add-hook 'post-self-insert-hook #'resnippets--post-command-handler nil t)
        (add-hook 'after-save-hook #'resnippets--after-save-handler)
        (resnippets--load-project-file))
    (remove-hook 'post-self-insert-hook #'resnippets--post-command-handler t)))

(defcustom resnippets-project-file ".resnippets.el"
  "Name of the per-project snippet file."
  :type 'string
  :group 'resnippets)

(defvar resnippets--loaded-project-files nil
  "List of project snippet files already loaded (absolute paths).")

(defun resnippets--find-project-file ()
  "Find the nearest `.resnippets.el' by walking up from `default-directory'."
  (let ((dir (locate-dominating-file default-directory resnippets-project-file)))
    (when dir
      (expand-file-name resnippets-project-file dir))))

(defun resnippets--load-project-file ()
  "Load the project snippet file if found and not already loaded."
  (let ((file (resnippets--find-project-file)))
    (when (and file (not (member file resnippets--loaded-project-files)))
      (let* ((dir (file-name-directory file))
             (resnippets--current-project-root dir))
        (load file nil t))
      (push file resnippets--loaded-project-files)
      (resnippets--setup-watch file)
      (message "Resnippets: loaded %s" file))))

(defun resnippets-reload-project ()
  "Reload the project snippet file, even if already loaded.
If the file has been deleted, removes the previously loaded snippets."
  (interactive)
  (let ((file (resnippets--find-project-file)))
    (if file
        (let ((dir (file-name-directory file)))
          ;; Remove old snippets from this project
          (resnippets-remove-by-label (resnippets--project-label dir))
          (setq resnippets--loaded-project-files
                (delete file resnippets--loaded-project-files))
          (resnippets--load-project-file))
      ;; File not found — clean up any previously loaded project snippets
      (let ((dir (file-name-as-directory default-directory)))
        (resnippets-remove-by-label (resnippets--project-label dir))
        (setq resnippets--loaded-project-files
              (cl-remove-if (lambda (f) (string-prefix-p dir f))
                            resnippets--loaded-project-files))
        (message "Resnippets: %s removed, snippets cleaned" resnippets-project-file)))))

;;; Hot Reload via file-notify

(defvar resnippets--watch-descriptors nil
  "Alist mapping project file paths to their `file-notify' watch descriptors.")

(defun resnippets--setup-watch (file)
  "Set up a file watcher for project snippet FILE.
If a watcher already exists for FILE, do nothing."
  (unless (assoc file resnippets--watch-descriptors)
    (when (fboundp 'file-notify-add-watch)
      (condition-case err
          (let ((desc (file-notify-add-watch
                       file '(change)
                       #'resnippets--file-watch-callback)))
            (push (cons file desc) resnippets--watch-descriptors))
        (error (message "Resnippets: could not watch %s: %S" file err))))))

(defun resnippets--file-watch-callback (event)
  "Callback for file-notify events on project snippet files.
EVENT is the file-notify event."
  (let ((descriptor (nth 0 event))
        (action (nth 1 event))
        (file (nth 2 event)))
    (pcase action
      ((or 'changed 'renamed)
       (when (file-exists-p file)
         (let ((dir (file-name-directory file)))
           (resnippets-remove-by-label (resnippets--project-label dir))
           (setq resnippets--loaded-project-files
                 (delete file resnippets--loaded-project-files))
           (let ((resnippets--current-project-root dir))
             (load file nil t))
           (push file resnippets--loaded-project-files)
           (message "Resnippets: hot-reloaded %s" file))))
      ('deleted
       (let ((dir (file-name-directory file)))
         (resnippets-remove-by-label (resnippets--project-label dir))
         (setq resnippets--loaded-project-files
               (delete file resnippets--loaded-project-files))
         (resnippets--remove-watch file)
         (message "Resnippets: %s deleted, snippets removed" file))))))

(defun resnippets--remove-watch (file)
  "Remove the file watcher for FILE."
  (let ((entry (assoc file resnippets--watch-descriptors)))
    (when entry
      (ignore-errors (file-notify-rm-watch (cdr entry)))
      (setq resnippets--watch-descriptors
            (assq-delete-all (car entry) resnippets--watch-descriptors)))))

(defun resnippets-stop-watching ()
  "Remove all file watchers for project snippet files."
  (interactive)
  (dolist (entry resnippets--watch-descriptors)
    (ignore-errors (file-notify-rm-watch (cdr entry))))
  (setq resnippets--watch-descriptors nil)
  (message "Resnippets: all file watchers removed"))

(defun resnippets--after-save-handler ()
  "Reload project snippets when a `.resnippets.el' file is saved.
This provides reliable hot reload without depending on `file-notify'."
  (when (and buffer-file-name
             (string= (file-name-nondirectory buffer-file-name)
                      resnippets-project-file))
    (let ((file buffer-file-name)
          (dir (file-name-directory buffer-file-name)))
      (resnippets-remove-by-label (resnippets--project-label dir))
      (setq resnippets--loaded-project-files
            (delete file resnippets--loaded-project-files))
      (let ((resnippets--current-project-root dir))
        (load file nil t))
      (push file resnippets--loaded-project-files)
      (message "Resnippets: reloaded %s" file))))


(defun resnippets-diagnose ()
  "Diagnose snippet activation.
Creates a buffer listing all snippets and their current status (Active/Inactive),
showing which condition failed.
Checks are performed in the context of the current buffer."
  (interactive)
  (let ((origin-buffer (current-buffer))
        (origin-mode major-mode)
        (buf (get-buffer-create "*Resnippets Diagnose*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert (format "Resnippets Diagnostics (Buffer: %s, Mode: %s)\n\n"
                      (buffer-name origin-buffer) origin-mode))
      (insert (format "%-10s | %-20s | %-30s | %s\n" "STATUS" "LABEL" "REGEX" "REASON"))
      (insert (make-string 80 ?-) "\n")
      (dolist (snippet (reverse resnippets--snippets))
        (let* ((regex (car snippet))
               (props (cddr snippet))
               (label (or (plist-get props :label) "-"))
               (mode (plist-get props :mode))
               (condition (plist-get props :condition))
               (evaluation-results
                (with-current-buffer origin-buffer
                  (let ((mode-ok (or (null mode)
                                     (if (listp mode)
                                         (apply #'derived-mode-p mode)
                                       (derived-mode-p mode))))
                        (cond-ok (or (null condition)
                                     (condition-case err
                                         (eval condition)
                                       (error (format "ERROR: %s" err))))))
                    (list mode-ok cond-ok))))
               (mode-ok (nth 0 evaluation-results))
               (cond-ok (nth 1 evaluation-results))
               (status (if (and mode-ok (not (stringp cond-ok)) cond-ok) "ACTIVE" "INACTIVE"))
               (reason (cond
                        ((not mode-ok) (format "Mode mismatch (Expeted: %s)" mode))
                        ((stringp cond-ok) cond-ok) ;; Error message
                        ((not cond-ok) (format "Condition failed: %S" condition))
                        (t "OK"))))
          (insert (format "%-10s | %-20s | %-30s | %s\n"
                          status
                          (truncate-string-to-width label 20 0 nil "...")
                          (truncate-string-to-width regex 30 0 nil "...")
                          reason)))))
    (pop-to-buffer buf)))


(defun resnippets--format-candidate (snippet)
  "Format a snippet entry for display in `completing-read`.
SNIPPET is specific format: (REGEX EXPANSION . PROPS)."
  (let* ((regex (car snippet))
         (expansion (cadr snippet))
         (props (cddr snippet))
         (label (plist-get props :label))
         (desc (if label
                   (format "%s (%s)" label regex)
                 regex))
         ;; Format expansion preview
         (preview (if (stringp expansion)
                      expansion
                    (format "%S" expansion)))
         ;; Truncate preview
         (preview (if (> (length preview) 50)
                      (concat (substring preview 0 47) "...")
                    preview)))
    (format "%-40s -> %s" desc preview)))

(defun resnippets--active-snippets ()
  "Return a list of snippets active in the current context."
  (cl-remove-if-not (lambda (snippet)
                      (resnippets--check-condition (cddr snippet)))
                    resnippets--snippets))

;;;###autoload
(defun resnippets-insert ()
  "Interactively insert a snippet trigger or content.
Lists only snippets active in the current buffer context.
If a snippet has an :insert property, inserts that string.
Otherwise, inserts the regex trigger."
  (interactive)
  (let* ((candidates (mapcar (lambda (s)
                               (cons (resnippets--format-candidate s) s))
                             (resnippets--active-snippets)))
         (selection (completing-read "Insert snippet: " candidates nil t))
         (snippet (cdr (assoc selection candidates)))
         (regex (car snippet))
         (props (cddr snippet))
         (insert-val (plist-get props :insert)))
    (if insert-val
        (insert insert-val)
      (insert regex))))

;;;###autoload
(define-globalized-minor-mode resnippets-global-mode resnippets-mode resnippets-mode)

;;;###autoload
(defun resnippets-export (file)
  "Export all registered snippets to FILE.
The file can later be loaded with `resnippets-load`."
  (interactive "FExport snippets to file: ")
  (with-temp-file file
    (insert ";;; Resnippets export file -*- lexical-binding: t; -*-\n")
    (insert ";; Generated by resnippets-export\n\n")
    (dolist (snippet (reverse resnippets--snippets))
      (let ((regex (car snippet))
            (expansion (cadr snippet))
            (props (cddr snippet)))
        ;; Check if expansion is a single-string list, convert back to string
        (let ((exp-output (if (and (listp expansion)
                                   (= (length expansion) 1)
                                   (stringp (car expansion)))
                              (car expansion)
                            expansion)))
          (insert (format "(resnippets-add %S " regex))
          (if (listp exp-output)
              (insert (format "'%S" exp-output))
            (insert (format "%S" exp-output)))
          (while props
            (let ((key (car props))
                  (val (cadr props)))
              (insert (format " %s " key))
              ;; Quote symbols and lists, but not strings, numbers, or t/nil
              (cond
               ((or (stringp val) (numberp val) (eq val t) (eq val nil))
                (insert (format "%S" val)))
               ((symbolp val)
                (insert (format "'%S" val)))
               ((listp val)
                (insert (format "'%S" val)))
               (t
                (insert (format "%S" val)))))
            (setq props (cddr props)))
          (insert ")\n"))))
    (insert "\n;;; End of export\n"))
  (message "Exported %d snippets to %s" (length resnippets--snippets) file))

;;;###autoload
(defun resnippets-load (file)
  "Load snippets from FILE exported by `resnippets-export`."
  (interactive "fLoad snippets from file: ")
  (load-file file)
  (message "Loaded snippets from %s" file))

(provide 'resnippets)
;;; resnippets.el ends here
