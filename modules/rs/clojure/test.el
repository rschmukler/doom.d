;;; rs/clojure/test.el -*- lexical-binding: t; -*-
;;;
;;; Run with: M-x ert RET t RET  (runs all tests)
;;; or:       M-x ert RET rs/ RET (runs just rs/ tests)

(require 'ert)

;;
;;; Test helpers

(defmacro rs/with-clojure-ts-buffer (content cursor-search &rest body)
  "Run BODY in a temp clojure-ts-mode + lispyville-mode buffer.
CONTENT is inserted, then point is moved to the beginning of CURSOR-SEARCH.
forward-sexp-function is left as clojure-ts-mode sets it (treesit) so
tests exercise the same conditions as a real buffer."
  (declare (indent 2))
  `(with-temp-buffer
     (clojure-ts-mode)
     (lispyville-mode)
     (insert ,content)
     (goto-char (point-min))
     (search-forward ,cursor-search)
     (goto-char (match-beginning 0))
     ,@body))

(defun rs/buf-before-p (str1 str2)
  "Return t if STR1 appears before STR2 in the current buffer."
  (let ((p1 (save-excursion (goto-char (point-min))
                            (search-forward str1 nil t)
                            (match-beginning 0)))
        (p2 (save-excursion (goto-char (point-min))
                            (search-forward str2 nil t)
                            (match-beginning 0))))
    (and p1 p2 (< p1 p2))))

;;
;;; Drag forward — cursor on opening (

(ert-deftest rs/drag-fwd-list-on-open-paren ()
  "Drag list forward with cursor on its opening (."
  (rs/with-clojure-ts-buffer
      "(do\n  (foo 1)\n  (bar 2))"
      "(foo"
    (lispyville-drag-forward 1)
    (should (rs/buf-before-p "(bar 2)" "(foo 1)"))))

(ert-deftest rs/drag-fwd-list-cursor-stays-on-open-paren ()
  "After dragging forward, cursor is on the ( of the moved form."
  (rs/with-clojure-ts-buffer
      "(do\n  (foo 1)\n  (bar 2))"
      "(foo"
    (lispyville-drag-forward 1)
    (should (looking-at "(foo"))))

;;
;;; Drag backward — cursor on opening (

(ert-deftest rs/drag-bwd-list-on-open-paren ()
  "Drag list backward with cursor on its opening (."
  (rs/with-clojure-ts-buffer
      "(do\n  (foo 1)\n  (bar 2))"
      "(bar"
    (lispyville-drag-backward 1)
    (should (rs/buf-before-p "(bar 2)" "(foo 1)"))))

(ert-deftest rs/drag-bwd-list-cursor-stays-on-open-paren ()
  "After dragging backward, cursor is on the ( of the moved form."
  (rs/with-clojure-ts-buffer
      "(do\n  (foo 1)\n  (bar 2))"
      "(bar"
    (lispyville-drag-backward 1)
    (should (looking-at "(bar"))))

;;
;;; Drag forward — cursor on function name (inside the form)

(ert-deftest rs/drag-fwd-list-cursor-inside-on-fn-name ()
  "Drag forward with cursor on a fn name swaps it past its next sibling arg."
  (rs/with-clojure-ts-buffer
      "(do\n  (foo 1)\n  (bar 2))"
      "foo"
    (lispyville-drag-forward 1)
    ;; foo has sibling 1 within the form — drag swaps them: (foo 1) -> (1 foo)
    (should (rs/buf-before-p "1" "foo"))))

;;
;;; Drag backward — cursor inside a form on an arg

(ert-deftest rs/drag-bwd-list-cursor-inside-on-arg ()
  "Drag backward with cursor on an arg inside the form."
  (rs/with-clojure-ts-buffer
      "(do\n  (foo 1)\n  (bar 2))"
      "bar"
    (lispyville-drag-backward 1)
    (should (rs/buf-before-p "(bar 2)" "(foo 1)"))))

;;
;;; Symbol drag

(ert-deftest rs/drag-fwd-symbol ()
  "Drag a bare symbol forward."
  (rs/with-clojure-ts-buffer
      "(let [a b c d] nil)"
      " b"
    (forward-char 1) ;; on b
    (lispyville-drag-forward 1)
    (should (rs/buf-before-p "c" "b"))))

(ert-deftest rs/drag-bwd-symbol ()
  "Drag a bare symbol backward."
  (rs/with-clojure-ts-buffer
      "(let [a b c d] nil)"
      " c"
    (forward-char 1) ;; on c
    (lispyville-drag-backward 1)
    (should (rs/buf-before-p "c" "b"))))

;;
;;; Keyword drag

(ert-deftest rs/drag-fwd-keyword ()
  "Drag a keyword forward."
  (rs/with-clojure-ts-buffer
      "(or :dev :prod)"
      ":dev"
    (lispyville-drag-forward 1)
    (should (rs/buf-before-p ":prod" ":dev"))))

(ert-deftest rs/drag-bwd-keyword ()
  "Drag a keyword backward."
  (rs/with-clojure-ts-buffer
      "(or :dev :prod)"
      ":prod"
    (lispyville-drag-backward 1)
    (should (rs/buf-before-p ":prod" ":dev"))))

;;
;;; Drag backward past a string (the defsys regression)

(ert-deftest rs/drag-bwd-list-past-string ()
  "Drag a list form backward past a string sibling — must not corrupt buffer."
  (rs/with-clojure-ts-buffer
      "(defsys *env*\n  \"The docstring.\"\n  (or :dev :prod))"
      "(or"
    (lispyville-drag-backward 1)
    ;; Both forms must still be present and intact
    (should (string-match-p "(or :dev :prod)" (buffer-string)))
    (should (string-match-p "\"The docstring\\.\"" (buffer-string)))
    ;; (or ...) must now appear before the docstring
    (should (rs/buf-before-p "(or" "docstring"))))

;;
;;; Multiline form drag

(ert-deftest rs/drag-fwd-multiline-form ()
  "Drag a multiline list form forward past a one-liner."
  (rs/with-clojure-ts-buffer
      "(do\n  (multi\n    :line\n    :form)\n  (simple))"
      "(multi"
    (lispyville-drag-forward 1)
    (should (rs/buf-before-p "(simple)" "(multi"))))

(ert-deftest rs/drag-bwd-multiline-form ()
  "Drag a multiline list form backward past a one-liner."
  (rs/with-clojure-ts-buffer
      "(do\n  (simple)\n  (multi\n    :line\n    :form))"
      "(multi"
    (lispyville-drag-backward 1)
    (should (rs/buf-before-p "(multi" "(simple)"))))
