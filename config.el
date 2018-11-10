;;; private/rschmukler/config.el -*- lexical-binding: t; -*-

 (def-package! org
  :config
  (setq org-agenda-files (file-expand-wildcards "~/data/org/*.org"))
  (setq org-directory (expand-file-name "~/data/org"))
  (setq org-cycle-separator-lines 1)
  (defvar +org-dir (expand-file-name "~/data/org"))
  (setq org-capture-templates
        '(("c" "Code Task" entry (file+headline "~/data/org/main.org" "Coding Tasks")
           "* TODO %?\n  Entered on: %U - %a\n")
          ("t" "Task" entry (file+headline "~/data/org/main.org" "Tasks")
           "* TODO %?\n  Entered on: %U")
          ("n" "Note" entry (file+datetree "~/data/org/main.org")
           "* %?\n\n"))))

(after! neotree
  (setq neo-theme 'icons))

(after! company
  (setq company-idle-delay 0))

(after! doom-themes
  (setq doom-neotree-file-icons t))

(after! ivy
  (setq ivy-re-builders-alist
      '((t . ivy--regex-ignore-order))))

(after! projectile
  (projectile-mode)
  (projectile-load-known-projects))


(add-hook! elixir-mode
  (flycheck-mode)
  (rainbow-delimiters-mode))

(add-hook! elm-mode
  (flycheck-mode))

(def-package! rust-mode
  :mode "\\.rs$"
  :config
  (flycheck-mode))

(def-package! lsp-rust
  :after (lsp-mode lsp-ui rust-mode)
  :config
  (setq lsp-rust-rls-command '("rustup" "run" "stable" "rls"))
  :hook
  (rust-mode . lsp-rust-enable))


(def-package! dockerfile-mode
  :mode "Dockerfile$")


;; (add-hook! flycheck-rust
;;   :after rust-mode
;;   :config
;;   (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;; (def-package! racer
;;   :after rust-mode
;;   :config
;;   (setq racer-rust-src-path
;;         (concat
;;          (replace-regexp-in-string "\n$" "" (shell-command-to-string "rustc --print sysroot"))
;;          "/lib/rustlib/src/rust/src"))
;;   (company-mode)
;;   (eldoc-mode))

(def-package! flycheck-mix
  :after elixir-mode
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-mix-setup))

(def-package! flycheck-credo
  :after elixir-mode
  :config
  (setq flycheck-elixir-credo-strict t)
  (add-hook 'flycheck-mode-hook #'flycheck-credo-setup))

(def-package! lux-mode
  :mode "\\.lux$")

(def-package! erlang
  :mode "\\.erl$"
  :config
  (erlang-mode))

(def-package! racket-mode
  :mode "\\.rkt$"
  :config
  (company-mode)
  (flycheck-mode)
  (rainbow-delimiters-mode)
  )

(after! clojure-mode
  (define-clojure-indent
    (PUT 2)
    (POST 2)
    (GET 2)
    (PATCH 2)
    (DELETE 2)
    (context 2)
    (checking 3))
  (setq cider-cljs-lein-repl
	"(do (require 'figwheel-sidecar.repl-api)
         (figwheel-sidecar.repl-api/start-figwheel!)
         (figwheel-sidecar.repl-api/cljs-repl))"))


(def-package! graphql-mode
  :mode "\\.gql$")

;; (def-package! intero
;;   :after haskell-mode
;;   :config
;;   (intero-global-mode 1)
;;   (eldoc-mode)
;;   (flycheck-add-next-checker 'intero 'haskell-hlint))

(def-package! lsp-mode
  :after (:any haskell-mode rust-mode python-mode)
  :config
  (lsp-mode))

(def-package! lsp-ui
  :after lsp-mode
  :config
  (setq lsp-ui-flycheck-enable t)
  (setq imenu-auto-rescan t)
  :hook
  (lsp-mode . lsp-ui-mode)
  (lsp-mode . lsp-ui-peek-mode)
  (lsp-ui-mode . flycheck-mode))

(def-package! company-lsp
  :after (lsp-mode lsp-ui)
  :config
  (setq company-backends '(company-lsp))
  (setq company-lsp-async t))

(def-package! lsp-haskell
  :after
  (lsp-mode lsp-ui haskell-mode)
  :hook
  (haskell-mode . lsp-haskell-enable))

;; (def-package! lsp-python
;;   :hook
;;   (python-mode . lsp-python-enable))

(def-package! yapfify
  :hook
  (python-mode . yapf-mode)
  (before-save . yapify-buffer))



(after! haskell-mode
  (rainbow-delimiters-mode)
  (flycheck-mode)
  (setq lsp-haskell-process-path-hie "hie-wrapper")
  ;; (setq haskell-font-lock-symbols t)
  (add-to-list 'haskell-font-lock-symbols-alist '("<>" . "⊕"))
  (setq haskell-font-lock-symbols-alist
        (-reject
         (lambda (elem)
           (or))
            ;; (string-equal "::" (car elem))))
         haskell-font-lock-symbols-alist)))


(def-package! paxedit
  :config
  (map!
   (:map paxedit-mode-map
     :n ">>" #'evil-shift-right
     :n ">e" #'paxedit-transpose-forward
     :n ">)" #'sp-forward-slurp-sexp
     :n ">(" #'sp-backward-barf-sexp
     :n ">I" #'grfn/insert-at-sexp-end
     :n ">a" #'grfn/insert-at-form-end
     :n "<<" #'evil-shift-left
     :n "<e" #'paxedit-transpose-backward
     :n "<)" #'sp-forward-barf-sexp
     :n "<(" #'sp-backward-slurp-sexp
     :n "<I" #'grfn/insert-at-sexp-start
     :n "<a" #'grfn/insert-at-form-start))
  :hook
  (clojure-mode . smartparens-mode)
  (clojure-mode . paxedit-mode)
  (emacs-lisp-mode . paxedit-mode))

(def-package! alchemist
  :after elixir-mode
  :config
  (defun rm/alchemist-project-toggle-file-and-tests ()
    "Toggle between a file and its tests in the current window."
    (interactive)
    (if (alchemist-utils-test-file-p)
        (alchemist-project-open-file-for-current-tests 'find-file)
      (rm/alchemist-project-open-tests-for-current-file 'find-file)))

  (defun rm/alchemist-project-open-tests-for-current-file (opener)
    "Visit the test file for the current buffer with OPENER."
    (let* ((filename (file-relative-name (buffer-file-name) (alchemist-project-root)))
           (filename (replace-regexp-in-string "^lib/" "test/" filename))
           (filename (replace-regexp-in-string "^web/" "test/" filename))
           (filename (replace-regexp-in-string "^apps/\\(.*\\)/lib/" "apps/\\1/test/" filename))
           (filename (replace-regexp-in-string "\.ex$" "_test\.exs" filename))
           (filename (format "%s/%s" (alchemist-project-root) filename)))
      (if (file-exists-p filename)
          (funcall opener filename)
        (if (y-or-n-p "No test file found; create one now?")
            (alchemist-project--create-test-for-current-file
             filename (current-buffer))
          (message "No test file found."))))))

(load! "+functions")
(load! "+theming")
(load! "+bindings")
(load! "+commands")

