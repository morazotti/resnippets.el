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
