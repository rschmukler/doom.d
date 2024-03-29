;; -*- no-byte-compile: t; -*-
;;; private/rschmukler/packages.el

(package! flycheck)
(package! flycheck-mix)
(package! flycheck-credo)
(package! flycheck-clojure)
(package! erlang)
(package! racket-mode)
(package! geiser)
(package! graphql-mode)
(package! rust-mode)
(package! racer)
(package! flycheck-rust)
(package! intero)
;; (package! lsp-mode)
;; (package! lsp-ui)
;; (package! lsp-haskell)
(package! haskell-mode)
(package! flycheck-haskell)
(package! yapfify)
(package! company-lsp)
(package! dockerfile-mode)
(package! aggressive-indent)
(package! lispyville)
(package! reason-mode)
(package! flycheck-clj-kondo)
(package! helm-cider)
(package! dhall-mode)
(package! ivy-cider
  :recipe (:host github :repo "rschmukler/ivy-cider"))
(package! gif-screencast
  :recipe (:host gitlab :repo "ambrevar/emacs-gif-screencast"))
(package! fennel-mode)
(package! sqlformat)
(package! code-review)
(package! git-link)
(package! vulpea :recipe (:host github :repo "d12frosted/vulpea"))
(package! s)
(package! magit-todos
  :recipe (:host github :repo "rschmukler/magit-todos" :branch "rs/enhancements"))
(package! fabb
  :recipe (:host github :repo "teknql/fabb" :build (:not compile)))
