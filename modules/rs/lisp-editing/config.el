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
     escape)))
