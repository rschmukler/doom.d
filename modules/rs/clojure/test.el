;;; rs/clojure/test.el -*- lexical-binding: t; -*-
;;;
;;; Run interactively with M-x ert, or evaluate this buffer and run a
;;; specific test with M-x ert RET <test-name> RET.
;;;
;;; All tests spin up a temp clojure-ts-mode + lispyville-mode buffer so
;;; they exercise the real stack without touching your open files.

(require 'ert)

(defmacro rs/with-clojure-ts-buffer (content cursor-search &rest body)
  "Run BODY in a temp clojure-ts-mode+lispyville buffer.
CONTENT is inserted as buffer text.  CURSOR-SEARCH is a string passed to
`search-forward' to position point before testing; point is moved to the
start of the match."
  (declare (indent 2))
  `(with-temp-buffer
     (clojure-ts-mode)
     (lispyville-mode)
     (setq-local forward-sexp-function nil)  ;; mirror real config
     (insert ,content)
     (goto-char (point-min))
     (search-forward ,cursor-search)
     (goto-char (match-beginning 0))
     ,@body))

;;
;;; Drag tests

(ert-deftest rs/clojure-ts-drag-list-forward ()
  "Dragging a list form forward past its sibling swaps them."
  (rs/with-clojure-ts-buffer
      "(if true\n  (foo x)\n  (bar y))"
      "(foo"
    (lispyville-drag-forward 1)
    (should (string-match-p "(bar y)\\(?:.\\|\n\\)*(foo x)"
                            (buffer-string)))))

(ert-deftest rs/clojure-ts-drag-list-backward ()
  "Dragging a list form backward past its sibling swaps them."
  (rs/with-clojure-ts-buffer
      "(if true\n  (foo x)\n  (bar y))"
      "(bar"
    (lispyville-drag-backward 1)
    (should (string-match-p "(bar y)\\(?:.\\|\n\\)*(foo x)"
                            (buffer-string)))))

(ert-deftest rs/clojure-ts-drag-list-forward-cursor ()
  "After dragging forward, cursor stays on the opening ( of the moved form."
  (rs/with-clojure-ts-buffer
      "(if true\n  (foo x)\n  (bar y))"
      "(foo"
    (lispyville-drag-forward 1)
    (should (looking-at "(foo"))))

(ert-deftest rs/clojure-ts-drag-list-backward-cursor ()
  "After dragging backward, cursor stays on the opening ( of the moved form."
  (rs/with-clojure-ts-buffer
      "(if true\n  (foo x)\n  (bar y))"
      "(bar"
    (lispyville-drag-backward 1)
    (should (looking-at "(bar"))))

(ert-deftest rs/clojure-ts-drag-symbol-forward ()
  "Dragging a symbol forward still works (regression: t-branch must be unaffected)."
  (rs/with-clojure-ts-buffer
      "(let [a 1 b 2] (+ a b))"
      "a 1"
    (lispyville-drag-forward 1)
    (should (string-match-p "b.*a" (buffer-string)))))

(ert-deftest rs/clojure-ts-drag-vector-forward ()
  "Dragging a vector form forward works ([ is also lispy-left)."
  (rs/with-clojure-ts-buffer
      "(do\n  [1 2 3]\n  [4 5 6])"
      "[1 2"
    (lispyville-drag-forward 1)
    (should (string-match-p "\\[4 5 6\\]\\(?:.\\|\n\\)*\\[1 2 3\\]"
                            (buffer-string)))))
