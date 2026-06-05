;;; rs/lisp-editing/config.el -*- lexical-binding: t; -*-
;;;
;;; Structural-editing shared across lisp-family languages: rainbow
;;; delimiters, aggressive indentation, and lispyville.

(use-package! rainbow-delimiters
  :hook
  (clojure-mode . rainbow-delimiters-mode)
  (clojure-ts-mode . rainbow-delimiters-mode)
  (elixir-mode . rainbow-delimiters-mode)
  (haskell-mode . rainbow-delimiters-mode)
  (racket-mode . rainbow-delimiters-mode))

(use-package! aggressive-indent
  :hook
  ;; (clojure-mode . aggressive-indent-mode)
  (hy-mode . aggressive-indent-mode)
  (lisp-mode . aggressive-indent-mode))

(use-package! lispyville
  :hook ((emacs-lisp-mode clojure-mode clojure-ts-mode hy-mode json-mode) . lispyville-mode)
  :config
  (lispyville-set-key-theme
   '(operators
     c-w
     prettify
     text-objects
     atom-movement
     commentary
     wrap
     slurp/barf-lispy
     additional
     additional-movement
     additional-insert
     escape))

  ;; lispyville--drag's middle branch (for `(` / `[` forms) is broken in
  ;; tree-sitter modes: it calls lispy-move-down from *inside* the form,
  ;; which goes through lispy--out-backward -> lispy--move-down-region.
  ;; lispy--move-down-region relies on `bnd0` (the enclosing-form bounds),
  ;; which comes back nil/wrong in clojure-ts-mode because
  ;; bounds-of-thing-at-point returns nil at the position after `up-list`.
  ;; The resulting error is swallowed silently and drag does nothing.
  ;;
  ;; Fix: bypass lispy internals entirely for list/vector forms. Find the
  ;; adjacent sibling directly and swap the two regions ourselves using a
  ;; marker to track the moved form's new position.

  (defun rs/lispy--find-next-sexp (pos)
    "Return bounds of the first sexp at or after POS, nil if at a closing delimiter."
    (save-excursion
      (goto-char pos)
      (skip-chars-forward " \t\n")
      (and (not (eobp))
           (not (looking-at "[])}]"))
           (ignore-errors (bounds-of-thing-at-point 'sexp)))))

  (defun rs/lispy--find-prev-sexp (pos)
    "Return bounds of the sexp ending just before POS
    nil if at an opening delimiter."
    (save-excursion
      (goto-char pos)
      (skip-chars-backward " \t\n")
      (and (not (bobp))
           (not (looking-back "[[({]" (1- (point))))
           (ignore-errors
             (backward-sexp)
             (bounds-of-thing-at-point 'sexp)))))

  (defun rs/lispy--swap-regions-goto-bnd1 (bnd1 bnd2)
    "Swap text in BND1 and BND2, then move point to ( of BND1 in its new position.
     Always replaces the later region first so earlier positions stay valid.
     A marker placed at (car BND2) tracks BND1's new start through both edits."
    (let* ((earlier (if (< (car bnd1) (car bnd2)) bnd1 bnd2))
           (later (if (< (car bnd1) (car bnd2)) bnd2 bnd1))
           (str-e (buffer-substring-no-properties (car earlier) (cdr earlier)))
           (str-l (buffer-substring-no-properties (car later) (cdr later)))
           ;; BND1's content lands at (car bnd2). Track it through the edits.
           (marker (set-marker (make-marker) (car bnd2))))
      (save-excursion
        (goto-char (car later))
        (delete-region (car later) (cdr later))
        (insert str-e))
      (save-excursion
        (goto-char (car earlier))
        (delete-region (car earlier) (cdr earlier))
        (insert str-l))
      (goto-char (marker-position marker))
      (set-marker marker nil)))

  (defun rs/lispyville--drag-fix (orig-fn func count)
    (if (and (not (region-active-p))
             (looking-at lispy-left))
        (let* ((bnd1 (lispy--bounds-dwim))
               (bnd2 (if (eq func #'lispy-move-down)
                         (rs/lispy--find-next-sexp (cdr bnd1))
                       (rs/lispy--find-prev-sexp  (car bnd1)))))
          (if bnd2
              (rs/lispy--swap-regions-goto-bnd1 bnd1 bnd2)
            ;; No sibling found — no-op, fall through to original
            (funcall orig-fn func count)))
      (funcall orig-fn func count)))

  (advice-add 'lispyville--drag :around #'rs/lispyville--drag-fix))
