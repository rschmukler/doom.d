;;; private/rschmukler/config.el -*- lexical-binding: t; -*-

(setq epg-pinentry-mode 'loopback)

(setq whitespace-line-column 100 whitespace-style '(face trailing lines-tail))
(setq whitespace-global-modes '(not org-mode whitespace-mode magit-mode))
(global-whitespace-mode 1)

(after! org
  (setq org-agenda-files (file-expand-wildcards "~/docs/org/*.org"))
  (setq org-cycle-separator-lines 1)
  (setq org-archive-location "::* Archived Tasks")
  (setq org-agenda-dim-blocked-tasks 't)
  (defvar +org-dir (expand-file-name "~/docs/org"))
  (setq org-todo-keywords
        '((sequence "[ ](t)" "[.](s)" "[!](h)" "[?](q)" "[@](b)" "|" "[x](d)" "[#](k)")))
  (advice-add 'org-agenda-quit :after #'org-save-all-org-buffers)
  (setq org-todo-keyword-faces
        '(("[.]" . +org-todo-active)
          ("[?]" . +org-todo-onhold)
          ("[@]" . +org-todo-onhold)
          ("[!]" . +org-todo-onhold)
          ("[#]" . +org-todo-cancel))))

(after! org-roam
  (setq org-roam-directory (expand-file-name (concat org-directory "/" "roam"))))

(use-package! vulpea
  :after org
  :init
  ;; vulpea v2 maintains its own database, independent of org-roam.
  (setq vulpea-db-location
        (expand-file-name "vulpea.db" doom-cache-dir))
  ;; Index the same notes org-roam used to track.
  (setq vulpea-db-sync-directories
        (list (expand-file-name "roam" org-directory)))
  ;; Use org-roam's alias property so existing :ROAM_ALIASES: keep working.
  (setq vulpea-buffer-alias-property "ROAM_ALIASES")
  ;; Performance / behavior tuning.
  (setq vulpea-db-parse-method 'single-temp-buffer)
  (setq vulpea-db-sync-external-method 'auto)
  ;; Build/refresh the DB asynchronously when autosync is enabled so
  ;; startup stays fast. The DB persists between sessions on disk.
  (setq vulpea-db-sync-scan-on-enable 'async)
  :config
  ;; Start file-watcher based auto-sync (replaces the old
  ;; `vulpea-db-autosync-enable' / org-roam-db-autosync coupling).
  (vulpea-db-autosync-mode +1)
  (defun vulpea-agenda-category (&optional len)
    "Get category of item at point for agenda.

Category is defined by one of the following items:

- CATEGORY property
- TITLE keyword
- TITLE property
- filename without directory and extension

When LEN is a number, resulting string is padded right with
spaces and then truncated with ... on the right if result is
longer than LEN.

Usage example:

  (setq org-agenda-prefix-format
        '((agenda . \" %(vulpea-agenda-category) %?-12t %12s\")))

Refer to `org-agenda-prefix-format' for more information."
    (let* ((file-name (when buffer-file-name
                        (file-name-sans-extension
                         (file-name-nondirectory buffer-file-name))))
           (title (vulpea-buffer-prop-get "title"))
           (category (org-get-category))
           (result
            (or (if (and
                     title
                     (string-equal category file-name))
                    title
                  category)
                "")))
      (if (numberp len)
          (s-truncate len (s-pad-right len " " result))
        result)))
  (setq org-agenda-prefix-format
        '((agenda . " %i %(vulpea-agenda-category 20)%?-12t% s")
          (todo . " %i %(vulpea-agenda-category 20) ")
          (tags . " %i %(vulpea-agenda-category 20) ")
          (search . " %i %(vulpea-agenda-category 20) "))))

(after! neotree
  (setq doom-themes-neotree-file-icons 'icons)
  (setq doom-themes-neotree-enable-file-icons 'icons)
  (setq neo-theme 'icons))

(after! company
  (setq company-idle-delay 0))

(after! doom-themes
  (setq doom-neotree-file-icons t))

(after! ivy
  (setq ivy-re-builders-alist
        '((t . ivy--regex-ignore-order)))

  (setq ivy-initial-inputs-alist
        '((counsel-minor . "")
          (counsel-package . "")
          (counsel-org-capture . "")
          (counsel-M-x . "")
          (counsel-describe-function . "")
          (counsel-describe-variable . ""))))

(after! projectile
  (setq projectile-create-missing-test-files t)
  (projectile-register-project-type 'haskell-stack '("stack.yaml")
                              :compile "stack build"
                              :test "stack build --test"
                              :test-suffix "Test")
  (projectile-mode)
  (projectile-load-known-projects))


(use-package! rainbow-delimiters
  :hook
  (clojure-mode . rainbow-delimiters-mode)
  (elixir-mode . rainbow-delimiters-mode)
  (haskell-mode . rainbow-delimiters-mode)
  (racket-mode . rainbow-delimiters-mode))

(add-hook! elixir-mode
  (flycheck-mode))

(add-hook! elm-mode
  (flycheck-mode))

(use-package! rust-mode
  :mode "\\.rs$"
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
  (setq rust-format-on-save t)
  (flycheck-mode))

(after! rustic
  :mode "\\.rs$"
  (setq rustic-format-on-save t))

(use-package! dockerfile-mode
  :mode "Dockerfile$")


(after! ranger
  :config
  (setq ranger-show-literal nil))


(use-package! reason-mode
  :mode "\\.re$"
  :hook
  (before-save . (lambda ()
                   (when (equal major-mode 'reason-mode)
                     (refmt)))))


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

(use-package! flycheck-mix
  :after elixir-mode
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-mix-setup))

(use-package! flycheck-credo
  :after elixir-mode
  :config
  (setq flycheck-elixir-credo-strict t)
  (add-hook 'flycheck-mode-hook #'flycheck-credo-setup))

(use-package! erlang
  :mode "\\.erl$"
  :config
  (erlang-mode))

(use-package! racket-mode
  :mode "\\.rkt$"
  :config
  (company-mode)
  (flycheck-mode))

(use-package! aggressive-indent
  :hook
  ;; (clojure-mode . aggressive-indent-mode)
  (hy-mode . aggressive-indent-mode)
  (lisp-mode . aggressive-indent-mode))

(after! typescript-mode
  (setq typescript-indent-level 2))

(after! typescript-tsx-mode
  (setq typescript-indent-level 2))

;; (after! company-box
;;   (add-function
;;    :after
;;    (symbol-function 'company-box-doc--show)
;;    (lambda (_ frame)
;;      (when (frame-visible-p (frame-parameter frame 'company-box-doc-frame))
;;        (when (not (frame-visible-p (company-box--get-frame)))
;;          (make-frame-visible (company-box--get-frame)))))))

(use-package! graphql-mode
  :mode "\\.gql$")

(use-package! fennel-mode
  :mode "\\.fnl$")

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

(use-package! lsp-haskell
  :after haskell-mode
  :config
  (setq lsp-haskell-process-path-hie "hie-wrapper"))

;; (use-package! yapfify
;;   :hook
;;   (python-mode . yapf-mode)
;;   (before-save . (lambda ()
;;                    (when (eq major-mode 'python-mode)
;;                     (yapify-buffer)))))

(after! org-babel
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append))


(use-package! haskell-mode
  :mode "\\.hs$"
  :config
  ;; (setq haskell-font-lock-symbols t)
  ;; (add-to-list ("<>" . "⊕"))
  (setq haskell-font-lock-symbols-alist
        (-reject
         (lambda (elem)
           (or))
         ;; (string-equal "::" (car elem))))
         haskell-font-lock-symbols-alist)))

;; (use-package! flycheck-haskell
;;   :hook (haskell-mode . flycheck-haskell-setup))

(use-package! dhall-mode
  :mode "\\.dhall$")

(use-package! lispyville
  :hook ((emacs-lisp-mode clojure-mode hy-mode json-mode) . lispyville-mode)
  :config
  (lispyville-set-key-theme
   '(operators
     c-w
     prettify
     text-objects
     atom-movement
     commentary
     wrap
     slurp/barf-lispy
     additional
     additional-movement
     additional-insert
     escape)))

(use-package! sqlformat
  :hook
  (sql-mode . sqlformat-on-save-mode)
  :config
  (setq sqlformat-command 'pgformatter))

(use-package! alchemist
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

(use-package! fabb
  :config
  (map!
   (:leader
    :desc "Fabb Status" :n "t" #'fabb-status
    :desc "Fabb Ivy" :n "T" #'fabb-invoke-ivy)
   (:map fabb-mode-map
    :n "q" #'kill-this-buffer
    :n "/" #'fabb-invoke-ivy
    :n "?" #'fabb-dispatch)))

(use-package! eca
  :config
  (setq eca-buttons-allow-mouse t)
  (add-to-list 'whitespace-global-modes 'eca-chat-mode t)
  (map!
   (:map doom-leader-map
    :leader
    :desc "Eca" :prefix "e"
    :desc "Start session" :n "e" #'eca
    :desc "Start session" :n "r" #'eca-restart
    :desc "Rewrite" :n "w" #'eca-rewrite
    :desc "Talk" :n "t" #'eca-chat-talk)
   :after eca-chat
   :map eca-chat-mode-map
   :desc "Toggle section" :n "<tab>" #'eca-chat-toggle-expandable-block
   :desc "Toggle section" :n "TAB" #'eca-chat-toggle-expandable-block
   :n "<return>" #'eca-chat--key-pressed-return
   :n "RET" #'eca-chat--key-pressed-return
   :i "<return>" #'eca-chat--key-pressed-return
   :i "RET" #'eca-chat--key-pressed-return
   :localleader
   :desc "Clear the chat" :n "c" #'eca-chat-clear
   :desc "New chat" :n "n" #'eca-chat-new
   :desc "Delete chat (server)" :n "d" #'eca-chat-delete
   :desc "Rename chat" :n "r" #'eca-chat-rename
   :desc "Reset chat" :n "R" #'eca-chat-reset
   :desc "Accept all chat tool calls" :n "a" #'eca-chat-tool-call-accept-all
   :desc "Accept all chat tool calls (and remember)" :n "A" #'eca-chat-tool-call-accept-all-and-remember
   :desc "Select the model" :n "m" #'eca-chat-select-model
   :desc "Select the agent" :n "b" #'eca-chat-select-agent
   :desc "Select the variant" :n "v" #'eca-chat-select-variant
   :desc "Cycle eca tab" :n "<tab>" #'tab-line-switch-to-next-tab
   :desc "View MCPs" :n "M" #'eca-mcp-details))

(load! "+functions")
(load! "+theming")
(load! "+bindings")
(load! "+commands")
(load! "+org")
