;; -*- no-byte-compile: t; -*-
;;; private/rschmukler/packages.el

(package! flycheck)
(package! flycheck-mix)
(package! flycheck-credo)
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
(package! reason-mode)
(package! dhall-mode)
(package! gif-screencast
  :recipe (:host gitlab :repo "ambrevar/emacs-gif-screencast"))
(package! fennel-mode)
(package! sqlformat)
(package! s)
(package! fabb
  :recipe (:host github :repo "teknql/fabb" :build (:not compile)))
(package! copilot
  :recipe (:host github :repo "copilot-emacs/copilot.el" :files ("*.el")))
(package! eca
  ;; :recipe (:host github :repo "editor-code-assistant/eca-emacs" :files ("*.el"))
  :recipe (:local-repo "~/dev/rschmukler/eca-emacs" :files ("*.el"))
  )
(package! direnv)
(package! ultra-scroll
  :recipe (:host github :repo "jdtsmith/ultra-scroll"))
