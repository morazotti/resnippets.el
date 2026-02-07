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
