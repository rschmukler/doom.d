;; -*- no-byte-compile: t; -*-
;;; rs/clojure/packages.el

;; Packages specific to this module. The core clojure/cider/clj-refactor
;; packages are provided by Doom's built-in `:lang clojure' module; this
;; module only layers on the extras we use.

;; NOTE: clojure-mode is still required even with clojure-ts-mode because
;; CIDER and clj-refactor depend on it for certain APIs.
(package! clojure-ts-mode)
(package! flycheck-clojure)
(package! flycheck-clj-kondo)
(package! helm-cider)
(package! ivy-cider
  :recipe (:host github :repo "rschmukler/ivy-cider"))
