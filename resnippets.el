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

Example:
  (resnippets-add \"\\\\([a-zA-Z]+\\\\)hat\" '(\"\\\\hat{\" 1 \"}\")
                  :mode 'org-mode
                  :condition '(org-inside-LaTeX-fragment-p))"
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

;;; Field and Mirror System

(defvar-local resnippets--active-fields nil
  "List of active field overlays in current snippet expansion.
Each element is (FIELD-NUM . OVERLAY).")

(defvar-local resnippets--active-mirrors nil
  "List of active mirror overlays in current snippet expansion.
Each element is (FIELD-NUM . OVERLAY).")

(defvar-local resnippets--field-values nil
  "Alist of (FIELD-NUM . VALUE) for active fields.")

(defvar-local resnippets--in-field nil
  "Non-nil when actively editing a snippet field.")

(defface resnippets-field-face
  '((((class color) (background dark))
     :background "#44475a" :box (:line-width (-1 . -1) :color "#6272a4"))
    (((class color) (background light))
     :background "#e8e8fc" :box (:line-width (-1 . -1) :color "#6c71c4"))
    (t :inherit highlight))
  "Face for active snippet fields.
Fields are editable placeholders that can be tabbed between."
  :group 'resnippets)

(defface resnippets-mirror-face
  '((((class color) (background dark))
     :background "#3a3a4a" :slant italic)
    (((class color) (background light))
     :background "#f0f0f0" :slant italic)
    (t :inherit shadow))
  "Face for snippet mirrors.
Mirrors automatically reflect the content of their linked field."
  :group 'resnippets)

(defvar resnippets-field-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") #'resnippets-next-field)
    (define-key map (kbd "<tab>") #'resnippets-next-field)
    (define-key map (kbd "S-TAB") #'resnippets-prev-field)
    (define-key map (kbd "<backtab>") #'resnippets-prev-field)
    (define-key map (kbd "C-g") #'resnippets-abort-field)
    (define-key map (kbd "RET") #'resnippets-exit-field)
    map)
  "Keymap active during snippet field editing.")

(defun resnippets--make-field (n &optional default)
  "Create a field with number N and optional DEFAULT text.
If field N already exists, creates a mirror instead.
Returns the inserted text length."
  ;; Check if this field number already exists
  (if (assq n resnippets--field-values)
      ;; Field N already exists, create a mirror instead
      (resnippets--make-mirror n)
    ;; First occurrence of field N, create actual field
    (let* ((text (or default ""))
           (start (point))
           (end (progn (insert text) (point)))
           (ov (make-overlay start end nil nil t)))
      (overlay-put ov 'face 'resnippets-field-face)
      (overlay-put ov 'resnippets-field n)
      (overlay-put ov 'keymap resnippets-field-keymap)
      (overlay-put ov 'modification-hooks '(resnippets--field-modified))
      (overlay-put ov 'insert-in-front-hooks '(resnippets--field-modified))
      (overlay-put ov 'insert-behind-hooks '(resnippets--field-modified))
      (push (cons n ov) resnippets--active-fields)
      (push (cons n text) resnippets--field-values)
      (length text))))

(defun resnippets--make-mirror (n)
  "Create a mirror for field N.
Returns the inserted text length."
  (let* ((text (or (cdr (assq n resnippets--field-values)) ""))
         (start (point))
         (end (progn (insert text) (point)))
         (ov (make-overlay start end nil nil t)))
    (overlay-put ov 'face 'resnippets-mirror-face)
    (overlay-put ov 'resnippets-mirror n)
    (overlay-put ov 'evaporate t)
    (push (cons n ov) resnippets--active-mirrors)
    (length text)))

(defun resnippets--field-modified (ov after-p beg end &optional len)
  "Hook called when a field overlay is modified."
  (when (and after-p resnippets--in-field)
    (let* ((n (overlay-get ov 'resnippets-field))
           (new-text (buffer-substring-no-properties 
                      (overlay-start ov) (overlay-end ov))))
      ;; Update stored value
      (setf (cdr (assq n resnippets--field-values)) new-text)
      ;; Update all mirrors
      (resnippets--update-mirrors n new-text))))

(defun resnippets--update-mirrors (n text)
  "Update all mirrors for field N with TEXT."
  (dolist (mirror-entry resnippets--active-mirrors)
    (when (and (eq (car mirror-entry) n)
               (overlay-buffer (cdr mirror-entry)))
      (let* ((ov (cdr mirror-entry))
             (start (overlay-start ov))
             (end (overlay-end ov)))
        (save-excursion
          (let ((inhibit-modification-hooks t))
            (goto-char start)
            (delete-region start end)
            (insert text)
            (move-overlay ov start (point))))))))

(defun resnippets-next-field ()
  "Move to the next field in the snippet."
  (interactive)
  (let* ((current (resnippets--current-field-num))
         (sorted (sort (mapcar #'car resnippets--active-fields) #'<))
         (next (or (cl-find-if (lambda (n) (> n (or current -1))) sorted)
                   (car sorted))))
    (when next
      (resnippets--goto-field next))))

(defun resnippets-prev-field ()
  "Move to the previous field in the snippet."
  (interactive)
  (let* ((current (resnippets--current-field-num))
         (sorted (sort (mapcar #'car resnippets--active-fields) #'>))
         (prev (or (cl-find-if (lambda (n) (< n (or current 999))) sorted)
                   (car sorted))))
    (when prev
      (resnippets--goto-field prev))))

(defun resnippets--current-field-num ()
  "Return the field number at point, or nil."
  (let ((ovs (overlays-at (point))))
    (cl-loop for ov in ovs
             when (overlay-get ov 'resnippets-field)
             return (overlay-get ov 'resnippets-field))))

(defun resnippets--goto-field (n)
  "Go to field N and select its content."
  (let ((ov (cdr (assq n resnippets--active-fields))))
    (when (and ov (overlay-buffer ov))
      (let ((start (overlay-start ov))
            (end (overlay-end ov)))
        (goto-char end)
        (push-mark start t t)
        ;; Activate the region so delete-selection-mode works
        (setq deactivate-mark nil)
        (activate-mark)))))

(defun resnippets-exit-field ()
  "Exit field editing and finalize snippet."
  (interactive)
  (resnippets--cleanup-fields)
  (message "Snippet finalized."))

(defun resnippets-abort-field ()
  "Abort field editing."
  (interactive)
  (resnippets--cleanup-fields)
  (message "Snippet aborted."))

(defun resnippets--cleanup-fields ()
  "Remove all field overlays and cleanup state."
  (dolist (entry resnippets--active-fields)
    (when (overlay-buffer (cdr entry))
      (delete-overlay (cdr entry))))
  (dolist (entry resnippets--active-mirrors)
    (when (overlay-buffer (cdr entry))
      (delete-overlay (cdr entry))))
  (setq resnippets--active-fields nil
        resnippets--active-mirrors nil
        resnippets--field-values nil
        resnippets--in-field nil))

(defun resnippets--activate-fields ()
  "Activate field editing mode if there are fields."
  (when resnippets--active-fields
    (setq resnippets--in-field t)
    ;; Ensure transient-mark-mode is active for selection to work
    (unless transient-mark-mode
      (transient-mark-mode 1))
    ;; Go to first field
    (let ((first-field (car (sort (mapcar #'car resnippets--active-fields) #'<))))
      (resnippets--goto-field first-field))))

;;; Yasnippet Integration

(defvar resnippets-use-yasnippet nil
  "When non-nil and yasnippet is available, use it for field expansion.")

(defun resnippets-yasnippet (template)
  "Create a yasnippet expansion from TEMPLATE string.
TEMPLATE uses yasnippet syntax ($1, $2, ${1:default}, $0, etc).
Returns a form that will expand the template using yasnippet.

Example:
  (resnippets-add \"for\" (resnippets-yasnippet \"for ($1; $2; $3) {\\n$0\\n}\"))"
  `((resnippets--expand-yasnippet ,template)))

(defun resnippets--expand-yasnippet (template)
  "Expand TEMPLATE using yasnippet if available."
  (if (and (featurep 'yasnippet) (fboundp 'yas-expand-snippet))
      (progn
        (yas-expand-snippet template)
        "")  ;; Return empty string, yasnippet handles insertion
    ;; Fallback: just insert template with placeholders stripped
    (let ((simple (replace-regexp-in-string "\\${?[0-9]+\\(?::[^}]*\\)?}?" "" template)))
      (replace-regexp-in-string "\\$[0-9]+" "" simple))))

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
When MATCH-CASE is non-nil, apply the detected case pattern to strings.

Expansion items can include:
- Strings: inserted literally
- Integers: replaced with regex capture group
- (resnippets-cursor): sets cursor position after expansion
- (field N DEFAULT): creates editable field N with optional default
- (mirror N): creates mirror of field N"
  ;; The full match (group 0) spans the whole MATCH-STRING if the regex covered it,
  ;; but wait, we effectively matched (concat regex "\\\'").
  ;; Group 0 is the entire match.
  ;; We will use MATCH-STRING to extract groups.
  (resnippets--cleanup-fields)  ;; Clear any previous field state
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
           ;; Field: (field N) or (field N "default")
           ((and (listp item) (eq (car item) 'field))
            (let ((n (nth 1 item))
                  (default (nth 2 item)))
              (resnippets--make-field n default)))
           ;; Mirror: (mirror N)
           ((and (listp item) (eq (car item) 'mirror))
            (let ((n (nth 1 item)))
              (resnippets--make-mirror n)))
           ((listp item)
            (let ((result 
                   (condition-case err
                       (eval item)
                     (error (message "Resnippets error: %S" err) nil))))
              (when (or (stringp result) (numberp result))
                (insert (format "%s" result)))))
           (t "")))))
    ;; Activate field editing if there are fields
    (if resnippets--active-fields
        (resnippets--activate-fields)
      ;; Otherwise just go to cursor position
      (when final-point
        (goto-char final-point)
        (set-marker final-point nil)))))

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
When :chain is t, after expansion, check for more snippet matches."
  (let* ((limit (max (point-min) (- (point) resnippets-lookback-limit)))
         (text-to-check (buffer-substring-no-properties limit (point)))
         (matches nil))
    ;; Collect all matching snippets
    (cl-loop for (regex expansion . props) in resnippets--snippets
             for match-case = (plist-get props :match-case)
             for word-boundary = (plist-get props :word-boundary)
             for case-fold-search = match-case
             for effective-regex = (if word-boundary
                                       (concat "\\_<" regex)
                                     regex)
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
             (chain (plist-get props :chain)))
        (resnippets--expand text-to-check data expansion match-case)
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
      (add-hook 'post-self-insert-hook #'resnippets--post-command-handler nil t)
    (remove-hook 'post-self-insert-hook #'resnippets--post-command-handler t)))

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
