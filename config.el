;;; private/rschmukler/config.el -*- lexical-binding: t; -*-

(load! +functions)
(load! +theming)

(when (featurep 'evil)
  (load! +bindings)
  (load! +commands))

 (def-package! org
  :config
  (setq org-agenda-files (file-expand-wildcards "~/data/org/*.org"))
  (setq org-directory (expand-file-name "~/data/org"))
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
  (setq projectile-switch-project-action #'counsel-projectile-find-file)
  (projectile-mode)
  (projectile-load-known-projects))


(add-hook! elixir-mode
  (flycheck-mode)
  (rainbow-delimiters-mode)
  )

(add-hook! elm-mode
  (flycheck-mode))

(def-package! rust-mode
  :mode "\\.rs$"
  :config
  (flycheck-mode))


(add-hook! flycheck-rust
  :after rust-mode
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(def-package! racer
  :after rust-mode
  :config
  (setq racer-rust-src-path
        (concat
         (replace-regexp-in-string "\n$" "" (shell-command-to-string "rustc --print sysroot"))
         "/lib/rustlib/src/rust/src"))
  (company-mode)
  (eldoc-mode))

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

(def-package! clojure-mode
  :mode "\\.cljs?$"
  :config
  (company-mode)
  (flycheck-mode)
  (rainbow-delimiters-mode)
  (setq cider-prompt-for-symbol nil))

(def-package! cider
  :after clojure-mode)

(def-package! flycheck-clojure
  :after clojure-mode
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-clojure-setup))

(def-package! graphql-mode
  :mode "\\.gql$")

(def-package! intero
  :after haskell-mode
  :config
  (intero-global-mode 1)
  (eldoc-mode)
  (flycheck-add-next-checker 'intero 'haskell-hlint))


;; (add-hook
;;  'before-save-hook
;;  (lambda ()
;;    (when (eq major-mode 'haskell-mode)
;;      (urbint/format-haskell-source))))

;; (def-package! parinfer
;;   :init
;;   (progn (setq parinfer-extensions '(defaults pretty-parens evil paredit)))
;;   (add-hook 'lux-mode-hook #'parinfer-mode)
;;   (add-hook 'clojure-mode-hook #'parinfer-mode)
;;   (add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
;;   (add-hook 'lux-mode-hook #'parinfer-mode))

(def-package! all-the-icons-ivy
  :after ivy
  :config
  (all-the-icons-ivy-setup))

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
