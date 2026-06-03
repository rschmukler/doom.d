;; -*- no-byte-compile: t; -*-
;;; private/rschmukler/packages.el

(package! flycheck)
(package! erlang)
(package! racket-mode)
(package! geiser)
(package! graphql-mode)

(package! yapfify)
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
(package! direnv)
(package! ultra-scroll
  :recipe (:host github :repo "jdtsmith/ultra-scroll"))
