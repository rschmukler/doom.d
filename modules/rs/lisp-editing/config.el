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
  ;;
  ;; Comments are a second broken case. `bounds-of-thing-at-point' returns nil
  ;; on a `;' line and lispy's own comment handling (lispy--bounds-comment ->
  ;; comment-beginning) either errors (`wrong-type-argument stringp nil') or
  ;; recurses until `max-lisp-eval-depth' — the latter manifests as a frozen
  ;; Emacs when dragging a form across a comment block. We avoid lispy entirely
  ;; by treating a contiguous run of whole-line `;' comments as one draggable
  ;; unit and swapping it like any other sexp.

  (defun rs/lispy--comment-line-p ()
    "Non-nil if the current line is a whole-line `;' comment."
    (save-excursion
      (beginning-of-line)
      (skip-chars-forward " \t")
      (and (eq (char-after) ?\;)
           (not (eolp)))))

  (defun rs/lispy--comment-block-bounds (pos)
    "Return (START . END) of the contiguous comment block at POS, or nil.
A block is a run of adjacent whole-line `;' comments. START is the first
`;' of the block's first line; END is the end of the block's last line
\(trailing newline excluded), so the bounds behave like a sexp region."
    (save-excursion
      (goto-char pos)
      (when (rs/lispy--comment-line-p)
        ;; Walk up to the first comment line of the block.
        (while (and (zerop (forward-line -1))
                    (rs/lispy--comment-line-p)))
        (unless (rs/lispy--comment-line-p)
          (forward-line 1))
        (skip-chars-forward " \t")
        (let ((start (point)))
          ;; Walk down to the last comment line of the block.
          (goto-char pos)
          (while (and (zerop (forward-line 1))
                      (rs/lispy--comment-line-p)))
          (forward-line -1)
          (end-of-line)
          (cons start (point))))))

  (defun rs/lispy--find-next-sexp (pos)
    "Return bounds of the first sexp at or after POS, nil if at a closing delimiter.
A contiguous comment block counts as a sexp. Binds `forward-sexp-function'
to nil so treesit cannot return wrong bounds (e.g. navigating past a string
literal to a different sibling)."
    (save-excursion
      (goto-char pos)
      (skip-chars-forward " \t\n")
      (cond
       ((eobp) nil)
       ((rs/lispy--comment-line-p) (rs/lispy--comment-block-bounds (point)))
       ((looking-at "[])}]") nil)
       (t (let ((forward-sexp-function nil))
            (ignore-errors (bounds-of-thing-at-point 'sexp)))))))

  (defun rs/lispy--find-prev-sexp (pos)
    "Return bounds of the sexp ending just before POS, or nil at an open delimiter.
A contiguous comment block counts as a sexp. Binds `forward-sexp-function'
to nil so treesit's backward-sexp cannot navigate to the wrong sibling
\(e.g. jumping past a string to a symbol)."
    (save-excursion
      (goto-char pos)
      (skip-chars-backward " \t\n")
      (cond
       ((bobp) nil)
       ((looking-back "[[({]" (1- (point))) nil)
       ((rs/lispy--comment-line-p) (rs/lispy--comment-block-bounds (point)))
       (t (let ((forward-sexp-function nil))
            (ignore-errors
              (backward-sexp)
              (bounds-of-thing-at-point 'sexp)))))))

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
      (set-marker marker nil)
      ;; Buffer modifications (delete-region + insert) displace doom's
      ;; window-local hl-line overlay markers, leaving them on the wrong line.
      ;; Force hl-line to reposition its overlays to the current line.
      (when (fboundp 'hl-line-highlight)
        (hl-line-highlight))))

  (defun rs/lispyville--containing-form-bounds ()
    "Return bounds of the innermost containing list, or nil."
    (let ((forward-sexp-function nil))
      (save-excursion
        (ignore-errors
          (up-list)
          (backward-list)
          (bounds-of-thing-at-point 'sexp)))))

  (defun rs/lispyville--drag-fix (orig-fn func count)
    ;; For region-active (visual-state drag) fall through — that path works.
    (if (region-active-p)
        (funcall orig-fn func count)
      ;; Use syntax-table sexp navigation (forward-sexp-function nil) to find
      ;; the sexp at point and its sibling, bypassing broken lispy internals.
      (let* (;; A cursor anywhere on a comment line must drag the whole comment
             ;; block — never an inner token. Check this FIRST: inside a comment
             ;; `bounds-of-thing-at-point' happily returns the word at point
             ;; (e.g. "Foooooooo"), which is nested inside the block and would
             ;; make the swap overlap and corrupt the buffer. It also dodges
             ;; lispy's comment handling, which freezes/errors here.
             (on-comment (rs/lispy--comment-line-p))
             (bnd1 (if on-comment
                       (rs/lispy--comment-block-bounds (point))
                     (let ((forward-sexp-function nil))
                       (ignore-errors (bounds-of-thing-at-point 'sexp)))))
             ;; Validate point is on/inside the sexp, not floating in whitespace
             ;; (comment-block bounds already contain point, so skip the check).
             (bnd1 (when (and bnd1
                              (or on-comment
                                  (and (<= (car bnd1) (point))
                                       (<  (point) (cdr bnd1)))))
                     bnd1))
             (bnd2 (when bnd1
                     (if (eq func #'lispy-move-down)
                         (rs/lispy--find-next-sexp (cdr bnd1))
                       (rs/lispy--find-prev-sexp  (car bnd1))))))
        (cond
         ;; Found a sibling — swap directly
         (bnd2
          (rs/lispy--swap-regions-goto-bnd1 bnd1 bnd2))
         ;; A comment block with no sibling to swap with — nothing to do.
         ;; Never fall through to lispy here: it freezes on comments.
         (on-comment nil)
         ;; No sibling for the atom at point (e.g. cursor on first/last element
         ;; of a form) — escalate to the containing form and drag that instead
         (bnd1
          (let* ((container (rs/lispyville--containing-form-bounds))
                 (container-bnd2
                  (when container
                    (if (eq func #'lispy-move-down)
                        (rs/lispy--find-next-sexp (cdr container))
                      (rs/lispy--find-prev-sexp  (car container))))))
            (if container-bnd2
                (rs/lispy--swap-regions-goto-bnd1 container container-bnd2)
              (funcall orig-fn func count))))
         ;; No sexp at point at all
         (t
          (funcall orig-fn func count))))))

  (advice-add 'lispyville--drag :around #'rs/lispyville--drag-fix))
