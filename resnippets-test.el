;;; resnippets-test.el --- Tests for resnippets.el -*- lexical-binding: t; -*-

(require 'ert)
(require 'resnippets "/home/nicolas/repos/resnippets/resnippets.el")

(ert-deftest resnippets-test-simple-suffix ()
  (with-temp-buffer
    (resnippets-mode 1)
    (let ((resnippets--snippets nil))
      (resnippets-add "hat" '("hat-expanded"))
      (insert "foo hat")
      ;; Simulate typing "hat"
      (should (resnippets--check))
      (should (equal (buffer-string) "foo hat-expanded")))))

(ert-deftest resnippets-test-no-match ()
  (with-temp-buffer
    (resnippets-mode 1)
    (let ((resnippets--snippets nil))
      (resnippets-add "hat" '("expanded"))
      (insert "foo bar")
      (should-not (resnippets--check))
      (should (equal (buffer-string) "foo bar")))))

(ert-deftest resnippets-test-regex-group ()
  (with-temp-buffer
    (resnippets-mode 1)
    (let ((resnippets--snippets nil))
      ;; "word" followed by "hat" -> \hat{word}
      (resnippets-add "\\([a-z]+\\)hat" '("\\hat{" 1 "}"))
      (insert "barhat")
      (should (resnippets--check))
      (should (equal (buffer-string) "\\hat{bar}")))))

(ert-deftest resnippets-test-complex-regex ()
  (with-temp-buffer
    (resnippets-mode 1)
    (let ((resnippets--snippets nil))
      ;; Match something that might include spaces if regex allows
      ;; But here we test "testhat" -> hat{test}
      (resnippets-add "\\(test\\)hat" '("\\hat{" 1 "}"))
      (insert " my testhat")
      (should (resnippets--check))
      (should (equal (buffer-string) " my \\hat{test}")))))

(ert-deftest resnippets-test-multiple-groups ()
  (with-temp-buffer
    (resnippets-mode 1)
    (let ((resnippets--snippets nil))
      (resnippets-add "\\([a-z]+\\)-\\([0-9]+\\)" '("ID: " 2 " Name: " 1))
      (insert "user-123")
      (should (resnippets--check))
      (should (equal (buffer-string) "ID: 123 Name: user")))))

(ert-deftest resnippets-test-condition-mode ()
  (with-temp-buffer
    (resnippets-mode 1)
    (let ((resnippets--snippets nil))
      (resnippets-add "test" "expanded" :mode 'emacs-lisp-mode)
      (insert "test")
      ;; Should not expand in fundamental-mode (default for temp buffer)
      (should-not (resnippets--check))
      (emacs-lisp-mode)
      ;; Should expand now
      (should (resnippets--check))
      (should (equal (buffer-string) "expanded")))))

(ert-deftest resnippets-test-condition-predicate ()
  (with-temp-buffer
    (resnippets-mode 1)
    (let ((resnippets--snippets nil))
      (setq-local my-flag nil)
      (resnippets-add "test" "expanded" :condition 'my-flag)
      (insert "test")
      (should-not (resnippets--check))
      (setq my-flag t)
      (should (resnippets--check))
      (should (equal (buffer-string) "expanded")))))

(ert-deftest resnippets-test-define-macro ()
  (with-temp-buffer
    (resnippets-mode 1)
    (let ((resnippets--snippets nil))
      (resnippets-define '(:mode emacs-lisp-mode)
        ("alpha" "beta")
        ("gamma" "delta"))
      (insert "alpha")
      (should-not (resnippets--check)) ;; Wrong mode
      (emacs-lisp-mode)
      (should (resnippets--check))
      (should (equal (buffer-string) "beta"))
      (erase-buffer)
      (insert "gamma")
      (should (resnippets--check))
      (should (equal (buffer-string) "delta")))))

(ert-deftest resnippets-test-cursor-placement ()
  (with-temp-buffer
    (resnippets-mode 1)
    (let ((resnippets--snippets nil))
      ;; Expand "hat" to "hat{cursor}"
      (resnippets-add "\\([a-z]+\\)hat" '("\\hat{" 1 (resnippets-cursor) "}"))
      (insert "barhat")
      (should (resnippets--check))
      (should (equal (buffer-string) "\\hat{bar}"))
      ;; Point should be stuck between "bar" and "}"
      ;; \hat{bar}  <-- length is 5+3+1 = 9
      ;; \hat{ is 5 chars. bar is 3. } is 1.
      ;; Cursor should be at 5+3+1 = 9 (1-based)? No, point indices.
      ;; \hat{bar}
      ;; 123456789
      ;;      ^  ^
      ;;      |  |
      ;;      |  Point 10 (after })
      ;;      Point 9 (between r and })
      (should (equal (point) 9)))))

(ert-deftest resnippets-test-removal ()
  (with-temp-buffer
    (resnippets-mode 1)
    (let ((resnippets--snippets nil))
      (resnippets-add "foo" "FOO")
      (resnippets-add "bar" "BAR")
      (insert "foo")
      (should (resnippets--check))
      (should (equal (buffer-string) "FOO"))

      (erase-buffer)
      (resnippets-remove "foo")
      (insert "foo")
      (should-not (resnippets--check))
      (should (equal (buffer-string) "foo"))

      (erase-buffer)
      (insert "bar")
      (should (resnippets--check)) ;; bar still exists
      (should (equal (buffer-string) "BAR"))

      (erase-buffer)
      (resnippets-clear)
      (insert "bar")
      (should-not (resnippets--check)) ;; All gone
      (should (equal (buffer-string) "bar")))))

(ert-deftest resnippets-test-define-label-multimode ()
  (with-temp-buffer
    (resnippets-mode 1)
    (let ((resnippets--snippets nil))
      (resnippets-define "my-label"
       '(:mode (text-mode emacs-lisp-mode))
       ("foo" "FOO"))

      (text-mode)
      (insert "foo")
      (should (resnippets--check))
      (should (equal (buffer-string) "FOO"))

      (erase-buffer)
      (emacs-lisp-mode)
      (insert "foo")
      (should (resnippets--check))
      (should (equal (buffer-string) "FOO"))

      (erase-buffer)
      (prog-mode) ;; Not in list
      (insert "foo")
      (should-not (resnippets--check))
      (should (equal (buffer-string) "foo"))

      ;; Verify label exists in props
      (should (equal (plist-get (cddr (car resnippets--snippets)) :label) "my-label")))))

(ert-deftest resnippets-test-braket-corrected ()
  (with-temp-buffer
    (resnippets-mode 1)
    (let ((resnippets--snippets nil))
      ;; Corrected regex: add \\\\ to match \
      ;; String: "<\\([a-zA-Z0-9_^{}\\\\]+\\)|\\([a-zA-Z0-9_^{}\\\\]+\\)>"
      ;; \\\\ in string -> \\ in regex -> matches literal \
      (resnippets-add "<\\([a-zA-Z0-9_^{}\\\\]+\\)|\\([a-zA-Z0-9_^{}\\\\]+\\)>"
                      '("\\braket{" 1 "}{" 2 "}"))

      (insert "<\\alpha|\\beta>")
      (should (resnippets--check))
      (should (equal (buffer-string) "\\braket{\\alpha}{\\beta}")))))

(ert-deftest resnippets-test-funcall ()
  (with-temp-buffer
    (resnippets-mode 1)
    (let ((resnippets--snippets nil))
      ;; Expand uppercase of capture
      (resnippets-add "\\([a-z]+\\)up" '("UP: " (upcase (resnippets-group 1))))
      (insert "fooup")
      (should (resnippets--check))
      (should (equal (buffer-string) "UP: FOO")))))

(ert-deftest resnippets-test-label-overwrite ()
  (with-temp-buffer
    (resnippets-mode 1)
    (let ((resnippets--snippets nil))
      ;; Define first version
      (resnippets-define "my-group" nil
        ("foo" "FOO1")
        ("bar" "BAR1"))

      (should (equal (length resnippets--snippets) 2))

      ;; Define second version - should overwrite
      (resnippets-define "my-group" nil
        ("foo" "FOO2"))

      (should (equal (length resnippets--snippets) 1))

      (insert "foo")
      (should (resnippets--check))
      (should (equal (buffer-string) "FOO2")))))

(defvar resnippets-test-var nil)

(ert-deftest resnippets-test-expand-env ()
  (with-temp-buffer
    (resnippets-mode 1)
    (let ((resnippets--snippets nil)
          (resnippets-expand-env '((resnippets-test-var . t))))
      ;; Snippet that inserts value of resnippets-test-var
      (resnippets-add "check" '((if (bound-and-true-p resnippets-test-var) "yes" "no")))

      (setq resnippets-test-var nil)
      (insert "check")
      (should (resnippets--check))
      ;; Should expand to "yes" because of the env binding
      (should (equal (buffer-string) "yes")))))

(defun resnippets-test-double (x) (* x 2))

(ert-deftest resnippets-test-dynamic-dispatch ()
  (with-temp-buffer
    (resnippets-mode 1)
    (let ((resnippets--snippets nil))
      ;; Pattern: ;func=arg; -> calls func(arg)
      ;; Group 1: function name (string) -> intern -> symbol
      ;; Group 2: argument (string) -> string-to-number -> number
      (resnippets-add ";\\([a-z-]+\\)=\\([0-9.]+\\);"
                      '((number-to-string
                         (funcall (intern (resnippets-group 1))
                                  (string-to-number (resnippets-group 2))))))

      (insert ";resnippets-test-double=5;")
      (should (resnippets--check))
      (should (equal (buffer-string) "10"))

      (erase-buffer)
      (insert ";1+=5;")
      (should-not (resnippets--check)) ;; Fails because 1+ has digits/symbols

      (resnippets-clear)
      ;; Broader regex for function name: [^=]+ (any char except =)
      (resnippets-add ";\\([^=]+\\)=\\([0-9.]+\\);"
                      '((number-to-string
                         (funcall (intern (resnippets-group 1))
                                  (string-to-number (resnippets-group 2))))))
      (erase-buffer)
      (insert ";1+=5;")
      (should (resnippets--check))
      (should (equal (buffer-string) "6")))))

(ert-deftest resnippets-test-error-handling ()
  (with-temp-buffer
    (resnippets-mode 1)
    (let ((resnippets--snippets nil))
      ;; Snippet that causes an error (void-function)
      (resnippets-add "err" '((funcall 'non-existent-function)))
      (insert "err")

      ;; Should not crash check, but maybe insert nothing or error message?
      ;; For now, let's just ensure it checks without throwing elisp signal to top level
      ;; We will implement robust error catching next.
      (should (resnippets--check))
      ;; It inserts nothing, just prints message
      (should (equal (buffer-string) "")))))

;; Tests for :match-case feature

(ert-deftest resnippets-test-detect-case-pattern ()
  "Test case pattern detection."
  (should (eq (resnippets--detect-case-pattern "hello") 'lower))
  (should (eq (resnippets--detect-case-pattern "HELLO") 'upper))
  (should (eq (resnippets--detect-case-pattern "Hello") 'capitalized))
  (should (eq (resnippets--detect-case-pattern "hELLO") 'mixed))
  (should (eq (resnippets--detect-case-pattern "HeLLo") 'mixed)))

(ert-deftest resnippets-test-apply-case-pattern ()
  "Test case pattern application."
  (should (equal (resnippets--apply-case-pattern 'lower "HELLO") "hello"))
  (should (equal (resnippets--apply-case-pattern 'upper "hello") "HELLO"))
  (should (equal (resnippets--apply-case-pattern 'capitalized "hELLO") "Hello"))
  (should (equal (resnippets--apply-case-pattern 'mixed "hello") "hello")))

(ert-deftest resnippets-test-match-case-lower ()
  "Test :match-case with lowercase input."
  (with-temp-buffer
    (resnippets-mode 1)
    (let ((resnippets--snippets nil))
      (resnippets-add "integracao" "integração" :match-case t)
      (insert "integracao")
      (should (resnippets--check))
      (should (equal (buffer-string) "integração")))))

(ert-deftest resnippets-test-match-case-upper ()
  "Test :match-case with uppercase input."
  (with-temp-buffer
    (resnippets-mode 1)
    (let ((resnippets--snippets nil))
      (resnippets-add "integracao" "integração" :match-case t)
      (insert "INTEGRACAO")
      (should (resnippets--check))
      (should (equal (buffer-string) "INTEGRAÇÃO")))))

(ert-deftest resnippets-test-match-case-capitalized ()
  "Test :match-case with capitalized input."
  (with-temp-buffer
    (resnippets-mode 1)
    (let ((resnippets--snippets nil))
      (resnippets-add "integracao" "integração" :match-case t)
      (insert "Integracao")
      (should (resnippets--check))
      (should (equal (buffer-string) "Integração")))))

(ert-deftest resnippets-test-match-case-plural ()
  "Test :match-case with plural suffix replacement."
  (with-temp-buffer
    (resnippets-mode 1)
    (let ((resnippets--snippets nil))
      ;; integracoes -> integrações
      (resnippets-add "\\([a-zA-Z]+\\)coes" '(1 "ções") :match-case t)
      (insert "integracoes")
      (should (resnippets--check))
      (should (equal (buffer-string) "integrações")))))

(ert-deftest resnippets-test-match-case-disabled ()
  "Test that without :match-case, case must match exactly."
  (with-temp-buffer
    (resnippets-mode 1)
    (let ((resnippets--snippets nil))
      (resnippets-add "integracao" "integração")
      (insert "Integracao")  ;; Capitalized, but snippet is lowercase
      (should-not (resnippets--check))
      (should (equal (buffer-string) "Integracao")))))

;; Tests for :priority feature

(ert-deftest resnippets-test-priority-higher-wins ()
  "Test that higher priority snippet wins when multiple match."
  (with-temp-buffer
    (resnippets-mode 1)
    (let ((resnippets--snippets nil))
      ;; Both match "foohat", but regex version has higher priority
      (resnippets-add "foohat" "literal" :priority 5)
      (resnippets-add "\\([a-z]+\\)hat" '("\\hat{" 1 "}") :priority 10)
      (insert "foohat")
      (should (resnippets--check))
      (should (equal (buffer-string) "\\hat{foo}")))))

(ert-deftest resnippets-test-priority-lower-loses ()
  "Test that lower priority snippet loses."
  (with-temp-buffer
    (resnippets-mode 1)
    (let ((resnippets--snippets nil))
      ;; Literal has higher priority now
      (resnippets-add "foohat" "literal" :priority 10)
      (resnippets-add "\\([a-z]+\\)hat" '("\\hat{" 1 "}") :priority 5)
      (insert "foohat")
      (should (resnippets--check))
      (should (equal (buffer-string) "literal")))))

(ert-deftest resnippets-test-priority-default-zero ()
  "Test that snippets without :priority default to 0."
  (with-temp-buffer
    (resnippets-mode 1)
    (let ((resnippets--snippets nil))
      ;; No priority = 0, explicit priority 1 wins
      (resnippets-add "test" "no-priority")
      (resnippets-add "test" "has-priority" :priority 1)
      (insert "test")
      (should (resnippets--check))
      (should (equal (buffer-string) "has-priority")))))

(ert-deftest resnippets-test-priority-tie-first-registered-wins ()
  "Test that among equal priorities the first registered snippet wins."
  (with-temp-buffer
    (resnippets-mode 1)
    (let ((resnippets--snippets nil))
      (resnippets-add "test" "first")
      (resnippets-add "test" "second")
      (insert "test")
      (should (resnippets--check))
      (should (equal (buffer-string) "first")))))

(ert-deftest resnippets-test-priority-inactive-winner-falls-back ()
  "Test that a matching higher-priority snippet with a false condition
does not block a lower-priority active one."
  (with-temp-buffer
    (resnippets-mode 1)
    (let ((resnippets--snippets nil))
      (resnippets-add "test" "low")
      (resnippets-add "test" "high" :priority 10 :condition '(eq 1 2))
      (insert "test")
      (should (resnippets--check))
      (should (equal (buffer-string) "low")))))

(defvar resnippets-test--condition-calls 0
  "Number of times the counting test condition was evaluated.")

(ert-deftest resnippets-test-condition-evaluated-lazily ()
  "Test that conditions only run for matching regexes, once per distinct form."
  (with-temp-buffer
    (resnippets-mode 1)
    (let ((resnippets--snippets nil)
          (resnippets-test--condition-calls 0))
      (resnippets-define '(:condition (cl-incf resnippets-test--condition-calls))
        ("foo" "FOO")
        ("\\([a-z]*\\)oo" '("X" 1))
        ("o" "O"))
      (insert "bar")
      (should-not (resnippets--check))
      (should (= resnippets-test--condition-calls 0))
      (insert " foo")
      (should (resnippets--check))
      (should (= resnippets-test--condition-calls 1))
      (should (equal (buffer-string) "bar FOO")))))

(ert-deftest resnippets-test-define-per-snippet-props ()
  "Test that per-snippet props override shared props in resnippets-define."
  (with-temp-buffer
    (resnippets-mode 1)
    (let ((resnippets--snippets nil))
      (resnippets-define "test-group" '(:priority 1)
        ("foo" "FOO-LOW")              ;; priority 1
        ("foo" "FOO-HIGH" :priority 10)) ;; priority 10 overrides

      (insert "foo")
      (should (resnippets--check))
      ;; FOO-HIGH should win because it has higher priority
      (should (equal (buffer-string) "FOO-HIGH")))))

;; Tests for :word-boundary feature

(ert-deftest resnippets-test-word-boundary-matches ()
  "Test that :word-boundary matches at word start."
  (with-temp-buffer
    (resnippets-mode 1)
    (let ((resnippets--snippets nil))
      (resnippets-add "int" "\\int" :word-boundary t)
      (insert "int")
      (should (resnippets--check))
      (should (equal (buffer-string) "\\int")))))

(ert-deftest resnippets-test-word-boundary-no-match-mid-word ()
  "Test that :word-boundary does NOT match mid-word."
  (with-temp-buffer
    (resnippets-mode 1)
    (let ((resnippets--snippets nil))
      (resnippets-add "int" "\\int" :word-boundary t)
      (insert "print")  ;; "int" is at end but not at word boundary
      (should-not (resnippets--check))
      (should (equal (buffer-string) "print")))))

(ert-deftest resnippets-test-word-boundary-after-space ()
  "Test that :word-boundary matches after space."
  (with-temp-buffer
    (resnippets-mode 1)
    (let ((resnippets--snippets nil))
      (resnippets-add "int" "\\int" :word-boundary t)
      (insert "foo int")
      (should (resnippets--check))
      (should (equal (buffer-string) "foo \\int")))))

;; Tests for :suffix feature

(ert-deftest resnippets-test-suffix-expands-after-space ()
  "Test that :suffix expands when followed by a space."
  (with-temp-buffer
    (resnippets-mode 1)
    (let ((resnippets--snippets nil))
      (resnippets-add "cao" "cão" :suffix t)
      (insert "cao ")
      (should (resnippets--check))
      (should (equal (buffer-string) "cão ")))))

(ert-deftest resnippets-test-suffix-no-expand-mid-word ()
  "Test that :suffix does NOT expand when followed by a letter."
  (with-temp-buffer
    (resnippets-mode 1)
    (let ((resnippets--snippets nil))
      (resnippets-add "cao" "cão" :suffix t)
      (insert "caos")
      (should-not (resnippets--check))
      (should (equal (buffer-string) "caos")))))

(ert-deftest resnippets-test-suffix-combined-word-boundary ()
  "Test that :suffix works together with :word-boundary."
  (with-temp-buffer
    (resnippets-mode 1)
    (let ((resnippets--snippets nil))
      (resnippets-add "cao" "cão" :suffix t :word-boundary t)
      ;; Should not match mid-word
      (insert "macaos")
      (should-not (resnippets--check))
      (should (equal (buffer-string) "macaos"))
      ;; Should match at word boundary with suffix
      (erase-buffer)
      (insert "cao,")
      (should (resnippets--check))
      (should (equal (buffer-string) "cão,")))))

(ert-deftest resnippets-test-suffix-with-groups ()
  "Test that :suffix does not interfere with user-defined capture groups."
  (with-temp-buffer
    (resnippets-mode 1)
    (let ((resnippets--snippets nil))
      (resnippets-add "\\(.\\)cao" '(1 "ção") :suffix t)
      (insert "acao ")
      (should (resnippets--check))
      (should (equal (buffer-string) "ação ")))))

;; Tests for :chain feature



(ert-deftest resnippets-test-chain-basic ()
  "Test that :chain triggers another expansion."
  (with-temp-buffer
    (resnippets-mode 1)
    (let ((resnippets--snippets nil))
      ;; "int" -> "\int " which then chains to match "\int " -> "INTEGRAL"
      (resnippets-add "int" "\\int " :chain t)
      (resnippets-add "\\\\int " "INTEGRAL")
      (insert "int")
      (should (resnippets--check))
      (should (equal (buffer-string) "INTEGRAL")))))

(ert-deftest resnippets-test-chain-no-match ()
  "Test that :chain doesn't break when no further match exists."
  (with-temp-buffer
    (resnippets-mode 1)
    (let ((resnippets--snippets nil))
      (resnippets-add "foo" "bar" :chain t)
      (insert "foo")
      (should (resnippets--check))
      (should (equal (buffer-string) "bar")))))

(ert-deftest resnippets-test-chain-depth-limit ()
  "Test that chain depth limit prevents infinite loops."
  (with-temp-buffer
    (resnippets-mode 1)
    (let ((resnippets--snippets nil)
          (resnippets-max-chain-depth 3))
      ;; Infinite loop: "a" -> "a" with chain would loop forever
      (resnippets-add "a" "a" :chain t)
      (insert "a")
      ;; Should expand 3 times max and stop, result is still "a"
      (should (resnippets--check))
      (should (equal (buffer-string) "a")))))

;; Tests for export/load feature

(ert-deftest resnippets-test-export-load ()
  "Test that export and load work correctly."
  (let ((resnippets--snippets nil)
        (temp-file (make-temp-file "resnippets-test" nil ".el")))
    (unwind-protect
        (progn
          ;; Add some snippets
          (resnippets-add "foo" "bar" :priority 5)
          (resnippets-add "baz" '("qux" 1) :mode 'text-mode)
          (should (equal (length resnippets--snippets) 2))

          ;; Export
          (resnippets-export temp-file)

          ;; Clear and verify empty
          (resnippets-clear)
          (should (equal (length resnippets--snippets) 0))

          ;; Load
          (resnippets-load temp-file)
          (should (equal (length resnippets--snippets) 2))

          ;; Verify first snippet works
          (with-temp-buffer
            (resnippets-mode 1)
            (insert "foo")
            (should (resnippets--check))
            (should (equal (buffer-string) "bar"))))
      ;; Cleanup
      (delete-file temp-file))))

;; Tests for per-project snippet loading

(ert-deftest resnippets-test-project-file-find ()
  "Test that project file is found by walking up directories."
  (let* ((dir (make-temp-file "resnippets-proj-" t))
         (subdir (expand-file-name "sub/" dir))
         (file (expand-file-name ".resnippets.el" dir)))
    (unwind-protect
        (progn
          (make-directory subdir t)
          (with-temp-file file
            (insert "(resnippets-add \"projtest\" \"found\")\n"))
          (let ((default-directory subdir)
                (resnippets-project-file ".resnippets.el"))
            (should (equal (resnippets--find-project-file) file))))
      (delete-directory dir t))))

(ert-deftest resnippets-test-project-file-load ()
  "Test that project file loads and auto-scopes snippets."
  (let* ((dir (make-temp-file "resnippets-proj-" t))
         (file (expand-file-name ".resnippets.el" dir))
         (resnippets--snippets nil)
         (resnippets--loaded-project-files nil))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "(resnippets-add \"projfoo\" \"projbar\")\n"))
          (let ((default-directory (file-name-as-directory dir))
                (resnippets-project-file ".resnippets.el"))
            (resnippets--load-project-file)
            ;; Should have loaded the snippet
            (should (= (length resnippets--snippets) 1))
            ;; Verify auto-scoping properties were applied
            (let* ((snippet (car resnippets--snippets))
                   (props (cddr snippet)))
              (should (string-prefix-p "project:" (plist-get props :project-label)))
              (should (equal (plist-get props :condition)
                             `(string-prefix-p ,(file-name-directory file) default-directory))))
            ;; Should be tracked as loaded
            (should (member file resnippets--loaded-project-files))
            ;; Second call should NOT reload
            (resnippets--load-project-file)
            (should (= (length resnippets--snippets) 1))))
      (delete-directory dir t))))

(ert-deftest resnippets-test-project-file-scoped ()
  "Test that project snippets only match in project directories."
  (let* ((dir (make-temp-file "resnippets-proj-" t))
         (other-dir (make-temp-file "resnippets-other-" t))
         (file (expand-file-name ".resnippets.el" dir))
         (resnippets--snippets nil)
         (resnippets--loaded-project-files nil))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "(resnippets-add \"scoped\" \"yes\")\n"))
          (let ((default-directory (file-name-as-directory dir))
                (resnippets-project-file ".resnippets.el"))
            (resnippets--load-project-file))
          ;; Should match in project directory
          (with-temp-buffer
            (let ((default-directory (file-name-as-directory dir)))
              (resnippets-mode 1)
              (insert "scoped")
              (should (resnippets--check))
              (should (equal (buffer-string) "yes"))))
          ;; Should NOT match outside project
          (with-temp-buffer
            (let ((default-directory (file-name-as-directory other-dir)))
              (resnippets-mode 1)
              (insert "scoped")
              (should-not (resnippets--check))
              (should (equal (buffer-string) "scoped")))))
      (delete-directory dir t)
      (delete-directory other-dir t))))

(ert-deftest resnippets-test-project-file-reload ()
  "Test that reload cleans old snippets and loads new ones."
  (let* ((dir (make-temp-file "resnippets-proj-" t))
         (file (expand-file-name ".resnippets.el" dir))
         (resnippets--snippets nil)
         (resnippets--loaded-project-files nil))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "(resnippets-add \"reloadtest\" \"v1\")\n"))
          (let ((default-directory (file-name-as-directory dir))
                (resnippets-project-file ".resnippets.el"))
            (resnippets--load-project-file)
            (should (= (length resnippets--snippets) 1))
            ;; Update file and reload — old snippets are removed first
            (with-temp-file file
              (insert "(resnippets-add \"reloadtest2\" \"v2\")\n"))
            (resnippets-reload-project)
            ;; Should have only the new snippet (old was cleaned)
            (should (= (length resnippets--snippets) 1))))
      (delete-directory dir t))))

;; Tests for resnippets-insert command

(ert-deftest resnippets-test-insert-command ()
  "Test interactive insertion command logic."
  (with-temp-buffer
    (resnippets-mode 1)
    (let ((resnippets--snippets nil))
      (resnippets-add "foo" "FOO")
      (resnippets-add "bar" "BAR" :mode 'emacs-lisp-mode)
      (resnippets-add "baz" "BAZ" :insert "my-baz")

      ;; Verify filtering (bar should not be active in fundamental-mode)
      (should (equal (length (resnippets--active-snippets)) 2))
      (should (cl-find "foo" (resnippets--active-snippets) :key #'car :test #'string=))
      (should (cl-find "baz" (resnippets--active-snippets) :key #'car :test #'string=))

      ;; Verify formatting
      (let ((formatted (resnippets--format-candidate (car resnippets--snippets))))
        (should (stringp formatted)))

      ;; Verify insertion logic (mocking completing-read)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt candidates _pred _require)
                   (car (car candidates))))) ;; Select first candidate
        ;; Insert default regex
        (let ((resnippets--snippets (list '("foo" "FOO"))))
          (resnippets-insert)
          (should (equal (buffer-string) "foo")))

        (erase-buffer)
        ;; Insert custom :insert property
        (let ((resnippets--snippets (list '("baz" "BAZ" :insert "my-baz"))))
          (resnippets-insert)
          (should (equal (buffer-string) "my-baz")))))))


(ert-deftest resnippets-test-diagnose ()
  "Test that diagnose buffer is created and populated."
  (let ((resnippets--snippets nil))
    (resnippets-add "active" "A" :mode 'emacs-lisp-mode)
    (resnippets-add "inactive-mode" "I" :mode 'text-mode)
    (resnippets-add "inactive-cond" "C" :condition '(eq 1 2))

    (with-temp-buffer
      (emacs-lisp-mode)
      (resnippets-diagnose)
      (let ((buf (get-buffer "*Resnippets Diagnose*")))
        (unwind-protect
            (with-current-buffer buf
              (let ((content (buffer-string)))
                ;; Columns are STATUS | LABEL | REGEX
                (should (string-match "ACTIVE.*active" content))
                (should (string-match "INACTIVE.*inactive-mode" content))
                (should (string-match "INACTIVE.*inactive-cond" content))
                (should (string-match "Condition failed" content))))
          (if buf (kill-buffer buf)))))))

;; Tests for hot reload

(ert-deftest resnippets-test-hot-reload-changed ()
  "Test that the file-watch callback reloads snippets on change."
  (let* ((dir (make-temp-file "resnippets-proj-" t))
         (file (expand-file-name ".resnippets.el" dir))
         (resnippets--snippets nil)
         (resnippets--loaded-project-files nil)
         (resnippets--watch-descriptors nil))
    (unwind-protect
        (progn
          ;; Create initial file and load it
          (with-temp-file file
            (insert "(resnippets-add \"hr1\" \"version1\")\n"))
          (let ((default-directory (file-name-as-directory dir))
                (resnippets-project-file ".resnippets.el"))
            (resnippets--load-project-file))
          ;; Should have 1 snippet
          (should (= (length resnippets--snippets) 1))

          ;; Update the file
          (with-temp-file file
            (insert "(resnippets-add \"hr2\" \"version2\")\n"))

          ;; Simulate the file-notify callback with a 'changed event
          (resnippets--file-watch-callback (list nil 'changed file))

          ;; Should have replaced old snippet with new one
          (should (= (length resnippets--snippets) 1))
          (should (string= (car (car resnippets--snippets)) "hr2"))

          ;; Verify expansion works
          (with-temp-buffer
            (let ((default-directory (file-name-as-directory dir)))
              (resnippets-mode 1)
              (insert "hr2")
              (should (resnippets--check))
              (should (equal (buffer-string) "version2")))))
      (delete-directory dir t))))

(ert-deftest resnippets-test-hot-reload-deleted ()
  "Test that the file-watch callback cleans up on delete."
  (let* ((dir (make-temp-file "resnippets-proj-" t))
         (file (expand-file-name ".resnippets.el" dir))
         (resnippets--snippets nil)
         (resnippets--loaded-project-files nil)
         (resnippets--watch-descriptors nil))
    (unwind-protect
        (progn
          ;; Create and load
          (with-temp-file file
            (insert "(resnippets-add \"del1\" \"gone\")\n"))
          (let ((default-directory (file-name-as-directory dir))
                (resnippets-project-file ".resnippets.el"))
            (resnippets--load-project-file))
          (should (= (length resnippets--snippets) 1))

          ;; Delete the file
          (delete-file file)

          ;; Simulate the file-notify callback with a 'deleted event
          (resnippets--file-watch-callback (list nil 'deleted file))

          ;; Snippets should be removed
          (should (= (length resnippets--snippets) 0))
          (should (null (member file resnippets--loaded-project-files))))
      (ignore-errors (delete-directory dir t)))))

;; Tests for resnippets-define-interactive

(ert-deftest resnippets-test-define-interactive ()
  "Test appending snippets to project file via interactive definition command."
  (let* ((dir (make-temp-file "resnippets-define-" t))
         (file (expand-file-name ".resnippets.el" dir)))
    (unwind-protect
        (progn
          ;; 1. Add standalone snippet
          (resnippets-define-interactive "r1" "e1" ":priority 10" "" file)
          (let ((content (with-temp-buffer
                           (insert-file-contents file)
                           (buffer-string))))
            (should (string-match "(resnippets-add \"r1\" \"e1\" :priority 10)" content)))

          ;; 2. Add snippet to new group
          (resnippets-define-interactive "r2" "e2" "" "G1" file)
          (let ((content (with-temp-buffer
                           (insert-file-contents file)
                           (buffer-string))))
            (should (string-match-p "(resnippets-define \"local:G1\" nil" content))
            (should (string-match-p "(\"r2\" \"e2\")" content)))

          ;; 3. Add snippet to existing group
          (resnippets-define-interactive "r3" "e3" ":suffix t" "G1" file)
          (let ((content (with-temp-buffer
                           (insert-file-contents file)
                           (buffer-string))))
            (should (string-match-p "(resnippets-define \"local:G1\" nil\n *(\"r2\" \"e2\")\n *(\"r3\" \"e3\" :suffix t))" content))))
      (ignore-errors (delete-directory dir t)))))

;; Tests for resnippets-new-snippet

(ert-deftest resnippets-test-new-snippet-buffer-creation ()
  "Test that the new-snippet form buffer is created with all elements."
  (unwind-protect
      (progn
        (resnippets-new-snippet)
        (with-current-buffer "*Resnippets New Snippet*"
          (let ((content (buffer-string)))
            ;; Check header
            (should (string-match "Resnippets: New Snippet" content))
            ;; Check fields
            (should (string-match "Trigger (regex):" content))
            (should (string-match "Expansion:" content))
            (should (string-match "Label (group):" content))
            (should (string-match "File:" content))
            ;; Check options
            (should (string-match ":word-boundary" content))
            (should (string-match ":suffix" content))
            (should (string-match ":match-case" content))
            (should (string-match ":chain" content))
            (should (string-match "Priority:" content))
            (should (string-match ":mode" content))
            (should (string-match ":condition" content))
            ;; Check footer
            (should (string-match "C-c C-c.*Save" content))
            (should (string-match "C-c C-k.*Cancel" content))
            ;; Check widgets were created
            (should resnippets-new-snippet--widgets)
            (should (assq 'trigger resnippets-new-snippet--widgets))
            (should (assq 'expansion resnippets-new-snippet--widgets))
            (should (assq 'suffix resnippets-new-snippet--widgets))
            (should (assq 'word-boundary resnippets-new-snippet--widgets)))))
    (when (get-buffer "*Resnippets New Snippet*")
      (kill-buffer "*Resnippets New Snippet*"))))

(ert-deftest resnippets-test-new-snippet-cancel ()
  "Test that cancel closes the form buffer."
  (resnippets-new-snippet)
  (should (get-buffer "*Resnippets New Snippet*"))
  (with-current-buffer "*Resnippets New Snippet*"
    (resnippets-new-snippet--cancel))
  (should-not (get-buffer "*Resnippets New Snippet*")))

(ert-deftest resnippets-test-new-snippet-save-basic ()
  "Test that saving creates the snippet in file and session."
  (let* ((dir (make-temp-file "resnippets-new-" t))
         (file (expand-file-name "test-snippets" dir))
         (resnippets--snippets nil)
         (resnippets-snippet-file file))
    (unwind-protect
        (progn
          (resnippets-new-snippet)
          (with-current-buffer "*Resnippets New Snippet*"
            ;; Set widget values
            (widget-value-set
             (cdr (assq 'trigger resnippets-new-snippet--widgets)) "hello")
            (widget-value-set
             (cdr (assq 'expansion resnippets-new-snippet--widgets)) "world")
            (widget-value-set
             (cdr (assq 'file resnippets-new-snippet--widgets)) file)
            ;; Save
            (resnippets-new-snippet--save))
          ;; Buffer should be closed
          (should-not (get-buffer "*Resnippets New Snippet*"))
          ;; Verify file was created with correct content
          (let ((content (with-temp-buffer
                           (insert-file-contents file)
                           (buffer-string))))
            (should (string-match-p "resnippets-add.*\"hello\".*\"world\"" content)))
          ;; Verify snippet was registered in session
          (should (cl-find "hello" resnippets--snippets
                           :key #'car :test #'string=)))
      (ignore-errors (delete-directory dir t))
      (when-let ((buf (find-buffer-visiting file)))
        (kill-buffer buf))
      (when (get-buffer "*Resnippets New Snippet*")
        (kill-buffer "*Resnippets New Snippet*")))))

(ert-deftest resnippets-test-new-snippet-save-with-options ()
  "Test that saving with options creates correct props."
  (let* ((dir (make-temp-file "resnippets-new-" t))
         (file (expand-file-name "test-snippets-opts" dir))
         (resnippets--snippets nil)
         (resnippets-snippet-file file))
    (unwind-protect
        (progn
          (resnippets-new-snippet)
          (with-current-buffer "*Resnippets New Snippet*"
            ;; Set required fields
            (widget-value-set
             (cdr (assq 'trigger resnippets-new-snippet--widgets)) "cao")
            (widget-value-set
             (cdr (assq 'expansion resnippets-new-snippet--widgets)) "cão")
            (widget-value-set
             (cdr (assq 'file resnippets-new-snippet--widgets)) file)
            ;; Enable options
            (widget-value-set
             (cdr (assq 'suffix resnippets-new-snippet--widgets)) t)
            (widget-value-set
             (cdr (assq 'word-boundary resnippets-new-snippet--widgets)) t)
            (widget-value-set
             (cdr (assq 'match-case resnippets-new-snippet--widgets)) t)
            ;; Save
            (resnippets-new-snippet--save))
          ;; Verify file content has the options
          (let ((content (with-temp-buffer
                           (insert-file-contents file)
                           (buffer-string))))
            (should (string-match-p ":word-boundary t" content))
            (should (string-match-p ":suffix t" content))
            (should (string-match-p ":match-case t" content)))
          ;; Verify session snippet has options
          (let* ((snippet (cl-find "cao" resnippets--snippets
                                   :key #'car :test #'string=))
                 (props (cddr snippet)))
            (should (plist-get props :suffix))
            (should (plist-get props :word-boundary))
            (should (plist-get props :match-case))))
      (ignore-errors (delete-directory dir t))
      (when-let ((buf (find-buffer-visiting file)))
        (kill-buffer buf))
      (when (get-buffer "*Resnippets New Snippet*")
        (kill-buffer "*Resnippets New Snippet*")))))

(ert-deftest resnippets-test-new-snippet-validation ()
  "Test that empty trigger raises an error."
  (let ((resnippets-snippet-file "/tmp/test-validation"))
    (unwind-protect
        (progn
          (resnippets-new-snippet)
          (with-current-buffer "*Resnippets New Snippet*"
            ;; Try to save with empty trigger
            (should-error (resnippets-new-snippet--save) :type 'user-error)))
      (when (get-buffer "*Resnippets New Snippet*")
        (kill-buffer "*Resnippets New Snippet*")))))

(ert-deftest resnippets-test-new-snippet-save-with-group ()
  "Test saving a snippet with a group label."
  (let* ((dir (make-temp-file "resnippets-new-" t))
         (file (expand-file-name "test-snippets-grp" dir))
         (resnippets--snippets nil)
         (resnippets-snippet-file file))
    (unwind-protect
        (progn
          (resnippets-new-snippet)
          (with-current-buffer "*Resnippets New Snippet*"
            (widget-value-set
             (cdr (assq 'trigger resnippets-new-snippet--widgets)) "grp1")
            (widget-value-set
             (cdr (assq 'expansion resnippets-new-snippet--widgets)) "GROUP1")
            (widget-value-set
             (cdr (assq 'label resnippets-new-snippet--widgets)) "my-group")
            (widget-value-set
             (cdr (assq 'file resnippets-new-snippet--widgets)) file)
            (resnippets-new-snippet--save))
          ;; Verify file has resnippets-define with local: prefix
          (let ((content (with-temp-buffer
                           (insert-file-contents file)
                           (buffer-string))))
            (should (string-match-p "resnippets-define.*\"local:my-group\"" content))
            (should (string-match-p "(\"grp1\" \"GROUP1\")" content)))
          ;; Verify session registration with label
          (let* ((snippet (cl-find "grp1" resnippets--snippets
                                   :key #'car :test #'string=))
                 (props (cddr snippet)))
            (should (equal (plist-get props :label) "local:my-group"))))
      (ignore-errors (delete-directory dir t))
      (when-let ((buf (find-buffer-visiting file)))
        (kill-buffer buf))
      (when (get-buffer "*Resnippets New Snippet*")
        (kill-buffer "*Resnippets New Snippet*")))))

(ert-deftest resnippets-test-new-snippet-build-props-string ()
  "Test building props string from form values."
  ;; No options
  (should (equal (resnippets-new-snippet--build-props-string
                  nil nil nil nil 0 nil "" nil "")
                 ""))
  ;; All boolean options
  (should (equal (resnippets-new-snippet--build-props-string
                  t t t t 0 nil "" nil "")
                 ":word-boundary t :suffix t :match-case t :chain t"))
  ;; Priority
  (should (equal (resnippets-new-snippet--build-props-string
                  nil nil nil nil 10 nil "" nil "")
                 ":priority 10"))
  ;; Mode
  (should (equal (resnippets-new-snippet--build-props-string
                  nil nil nil nil 0 t "'org-mode" nil "")
                 ":mode 'org-mode"))
  ;; Combined
  (should (equal (resnippets-new-snippet--build-props-string
                  t nil nil nil 5 t "'org-mode" nil "")
                 ":word-boundary t :priority 5 :mode 'org-mode")))

(ert-deftest resnippets-test-new-snippet-build-add-args ()
  "Test building resnippets-add argument list."
  ;; Basic
  (should (equal (resnippets-new-snippet--build-add-args
                  "foo" "bar" nil nil nil nil 0 nil "" nil "" "")
                 '("foo" "bar")))
  ;; With options
  (let ((args (resnippets-new-snippet--build-add-args
               "foo" "bar" t t nil nil 5 nil "" nil "" "my-label")))
    (should (equal (nth 0 args) "foo"))
    (should (equal (nth 1 args) "bar"))
    (should (plist-get (cddr args) :word-boundary))
    (should (plist-get (cddr args) :suffix))
    (should (equal (plist-get (cddr args) :priority) 5))
    (should (equal (plist-get (cddr args) :label) "my-label"))))
