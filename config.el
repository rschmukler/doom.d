;;; private/rschmukler/config.el -*- lexical-binding: t; -*-

(load! +functions)
(load! +theming)

(when (featurep 'evil)
  (load! +bindings)
  (load! +commands))

(after! neotree
  (setq neo-theme 'icons))

(after! company
  (setq company-idle-delay 0))

(after! doom-themes
  (setq doom-neotree-file-icons t))

(add-hook! elixir-mode
  (flycheck-mode))

(add-hook! elm-mode
  (flycheck-mode))

(add-hook! rust-mode
  (flycheck-mode))

(def-package! flycheck-mix
  :after elixir-mode
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-mix-setup))

(def-package! flycheck-credo
  :after elixir-mode
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-credo-setup))
