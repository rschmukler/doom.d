;;; rs/lsp/config.el -*- lexical-binding: t; -*-
;;;
;;; Self-contained lsp module: lsp-mode core, the per-language hooks, the
;;; reason-language-server client, lsp-ui, and company-lsp.

(use-package! lsp-mode
  :hook
  (haskell-mode . lsp)
  ;; (python-mode . lsp)
  (rustic-mode . lsp)
  (rust-mode . lsp)
  (reason-mode . lsp)
  :config
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "reason-language-server")
                    :major-modes '(reason-mode)
                    :notification-handlers (ht ("client/registerCapability" 'ignore))
                    :priority 1
                    :server-id 'reason-ls))
  :commands
  lsp)

(use-package! lsp-ui
  :commands
  lsp-ui-mode)

(use-package! company-lsp
  :commands company-lsp)
