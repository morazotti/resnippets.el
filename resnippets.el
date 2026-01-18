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

(defun resnippets-clear ()
  "Remove all registered snippets."
  (interactive)
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
SNIPPETS is a list of (REGEX EXPANSION) lists.

Example:
  (resnippets-define \"my-group\" '(:mode org-mode)
    (\"alpha\" \"\\\\alpha\"))"
  (let ((label (if (stringp label-or-props) label-or-props nil))
        (props (if (stringp label-or-props) (car args) label-or-props))
        (snippets (if (stringp label-or-props) (cdr args) args)))
    ;; We need to combine props with label if label exists
    (let ((props-code (if label
                          `(append (list :label ,label) ,props)
                        props)))
      `(progn
         ,@(mapcar (lambda (s)
                     `(apply #'resnippets-add ,(car s) ,(cadr s) ,props-code))
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

(defun resnippets--expand (match-string match-data expansion)
  "Expand the snippet.
MATCH-STRING is the full text that was matched (from buffer).
MATCH-DATA is the list of indices from `string-match` on MATCH-STRING.
EXPANSION is the definition list."
  ;; The full match (group 0) spans the whole MATCH-STRING if the regex covered it,
  ;; but wait, we effectively matched (concat regex "\\'").
  ;; Group 0 is the entire match.
  ;; We will use MATCH-STRING to extract groups.
  (delete-region (- (point) (length (substring match-string (nth 0 match-data) (nth 1 match-data))))
                 (point))
  (let ((final-point nil)
        (resnippets--last-match-data match-data)
        (resnippets--last-match-string match-string))
    (dolist (item expansion)
      (cond
       ((stringp item)
        (insert item))
       ((integerp item)
        (insert (resnippets-group item)))
       ((or (equal item '(resnippet-cursor))
            (equal item '(resnippets-cursor)))
        (setq final-point (point-marker)))
       ((listp item)
        (let ((result (eval item)))
          (when (or (stringp result) (numberp result))
            (insert (format "%s" result)))))
       (t "")))
    (when final-point
      (goto-char final-point)
      (set-marker final-point nil))))

(defun resnippets--check ()
  "Check if the text before point matches any registered snippet."
  (let* ((limit (max (point-min) (- (point) resnippets-lookback-limit)))
         (text-to-check (buffer-substring-no-properties limit (point)))
         (case-fold-search nil))
    (cl-loop for (regex expansion . props) in resnippets--snippets
             if (and (resnippets--check-condition props)
                     (string-match (concat regex "\\'") text-to-check))
             return
             (let ((data (match-data)))
               (resnippets--expand text-to-check data expansion)
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

(provide 'resnippets)
;;; resnippets.el ends here
