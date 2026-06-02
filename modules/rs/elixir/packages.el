;; -*- no-byte-compile: t; -*-
;;; rs/elixir/packages.el

;; elixir-mode + alchemist come from Doom's `:lang elixir' module; this module
;; layers on our flycheck integrations.

(package! flycheck-mix)
(package! flycheck-credo)
