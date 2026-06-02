;;; rs/rust/config.el -*- lexical-binding: t; -*-
;;;
;;; Self-contained rust module: rust-mode, rustic, format-on-save, and
;;; keybindings. LSP wiring lives in the `:rs lsp' module.

(use-package! rust-mode
  :mode "\\.rs$"
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
  (setq rust-format-on-save t)
  (flycheck-mode))

(after! rustic
  :mode "\\.rs$"
  (setq rustic-format-on-save t))

;; (add-hook! flycheck-rust
;;   :after rust-mode
;;   :config
;;   (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;; (use-package! racer
;;   :after rust-mode
;;   :config
;;   (setq racer-rust-src-path
;;         (concat
;;          (replace-regexp-in-string "\n$" "" (shell-command-to-string "rustc --print sysroot"))
;;          "/lib/rustlib/src/rust/src"))
;;   (company-mode)
;;   (eldoc-mode))

;;
;;; Keybindings

(map!
 (:after rustic
   (:map rustic-mode-map
      :leader
     :desc "Find documentation" :n "d" #'+lookup/documentation
     :desc "Find definition" :n "l" #'+lookup/definition
     :localleader
     :desc "Peek implementations" :n "i" #'lsp-ui-peek-find-implementation
     :desc "Apply LSP Code Action" :n "a" #'lsp-ui-sideline-apply-code-actions)))
