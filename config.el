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
  :hook
  ((org-roam-db-autosync-mode . vulpea-db-autosync-enable))
  :config
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

(use-package! magit-todos
  :after magit-mode
  :config
  (magit-add-section-hook 'magit-status-sections-hook 'magit-todos--insert-todos nil)
  (setq magit-todos-branch-list-merge-base-ref "origin/main"))

(after! forge
  (magit-add-section-hook 'magit-status-sections-hook
                          'forge-insert-authored-pullreqs
                          'forge-insert-pullreqs nil)
  (magit-add-section-hook 'magit-status-sections-hook
                          'forge-insert-requested-reviews
                          'forge-insert-pullreqs nil)

  (transient-append-suffix 'forge-dispatch '(0 -1)
    ["Misc"
     ("y" "yank" forge-copy-url-at-point-as-kill)])
  (transient-append-suffix 'forge-dispatch '(0 -1)
    ["Edit"
     ("e a" "assignees" forge-edit-topic-assignees)
     ("e l" "labels" forge-edit-topic-labels)
     ("e r" "review requests" forge-edit-topic-review-requests)]))

(use-package! code-review
  :config
  (setq code-review-new-buffer-window-strategy #'switch-to-buffer))

(use-package! git-link
  :config
  (setq git-link-use-commit t))

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


(add-hook! elixir-mode
  (flycheck-mode)
  (rainbow-delimiters-mode))

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


(after! magit
  :config
  (setq +workspaces-switch-project-function #'magit-status)
  (setq magit-prefer-remote-upstream t)
  (when (eq system-type 'darwin)
    (setq magit-delete-by-moving-to-trash nil)
    (setq delete-by-moving-to-trash nil)))


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
  (flycheck-mode)
  (rainbow-delimiters-mode))

(use-package! aggressive-indent
  :hook
  ;; (clojure-mode . aggressive-indent-mode)
  (hy-mode . aggressive-indent-mode)
  (lisp-mode . aggressive-indent-mode))

(use-package! ivy-cider
  :after cider-mode)

(use-package! flycheck-clj-kondo
  :after clojure-mode
  :config
  ;; (dolist (checkers '((clj-kondo-clj . clojure-joker)
  ;;                   (clj-kondo-cljs . clojurescript-joker)
  ;;                   (clj-kondo-cljc . clojure-joker)
  ;;                   (clj-kondo-edn . edn-joker)))
  ;; (flycheck-add-next-checker (car checkers) (cons 'error (cdr checkers)))))
  )

(after! typescript-mode
  (setq typescript-indent-level 2))

(after! typescript-tsx-mode
  (setq typescript-indent-level 2))

(after! clojure-mode
  (define-clojure-indent
    (PUT 2)
    (POST 2)
    (GET 2)
    (PATCH 2)
    (DELETE 2)
    (context 2)
    (for-all 2)
    (checking 3)
    (>defn :defn)
    (>defn- :defn)
    (defnc :defn)
    (defnc- :defn)
    (defresolver :defn)
    (fnc :defn)
    (match 1)
    (cond 0)
    (case 1)
    (describe 1)
    (it 2)
    (letfn 1)
    (fn-traced :defn)
    (defn-traced :defn)
    (assert-match 1)
    (assert-exception 1)
    (at-media 1)
    (fiber-loop 1)
    (agent-loop 1)
    (js-await 1)
    (fnk 1)
    (fw 1))
  (add-to-list 'clojure-align-binding-forms "let-flow")
  (setq clojure-indent-style 'align-arguments)
  (setq cider-default-cljs-repl 'shadow)
  (put '>defn 'clojure-doc-string-elt 2)
  (put '>defn- 'clojure-doc-string-elt 2)
  (put 'defsys 'clojure-doc-string-elt 2)
  (put 'defevent-db 'clojure-doc-string-elt 2)
  (put 'defevent-fx 'clojure-doc-string-elt 2)
  (put 'defsub 'clojure-doc-string-elt 2)
  (put 'defhandler 'clojure-doc-string-elt 2)
  (put 'defstream 'clojure-doc-string-elt 2)
  (put 'defn-traced 'clojure-doc-string-elt 2)
  (put 'defui 'clojure-doc-string-elt 2)
  (put 'defui- 'clojure-doc-string-elt 2)
  (put 'defnc 'clojure-doc-string-elt 2)
  (put 'defnc- 'clojure-doc-string-elt 2)
  (put 'defresolver 'clojure-doc-string-elt 2)


  (defun rs/ig/restart ()
    "Calls Integrant halt followed by integrant go"
    (interactive)
    (cider-interactive-eval "(do (integrant.repl/halt) (integrant.repl/go))"))

  (defun rs/systemic/restart ()
    "Calls Integrant halt followed by integrant go"
    (interactive)
    (cider-interactive-eval "(systemic.core/restart!)"))

  (defun rs/systemic/start ()
    "Calls Integrant halt followed by integrant go"
    (interactive)
    (cider-interactive-eval "(systemic.core/start!)"))

  (defun rs/systemic/stop ()
    "Calls Integrant halt followed by integrant go"
    (interactive)
    (cider-interactive-eval "(systemic.core/stop!)"))

  (defun rs/systemic/start-last-sexp ()
    "Starts the systemic system at the last sexp"
    (interactive)
    (cider-interactive-eval (concat "(systemic.core/start! `" (cider-sexp-at-point) ")")))

  (defun rs/systemic/stop-last-sexp ()
    "Starts the systemic system at the last sexp"
    (interactive)
    (cider-interactive-eval (concat "(systemic.core/stop! `" (cider-sexp-at-point) ")")))

  (defun rs/systemic/restart-last-sexp ()
    "Restart the systemic system at the last sexp"
    (interactive)
    (cider-interactive-eval (concat "(systemic.core/restart! `" (cider-sexp-at-point) ")")))

  (defvar rs/wing/alias nil)
  (defvar rs/malli/wrap-cider-enabled nil)
  (defun rs/wing/sync-libs ()
    "Calls Integrant reset"
    (interactive)
    (if rs/wing/alias
        (cider-interactive-eval (format "(wing.repl/sync-libs! :%s)" rs/wing/alias))
        (cider-interactive-eval "(wing.repl/sync-libs!)")))

  (defun rs/malli/try-start-dev ()
    (interactive)
    (cider-interactive-eval
     "(let [last *1]
        (try
          (with-out-str
            (let [start! (requiring-resolve 'malli.dev/start!)
                  fail!  (requiring-resolve 'malli.core/-fail!)]
              (start! {:report fail!})))
          last
          (catch java.io.FileNotFoundException _
            last)
          (catch Exception e
            (throw e))))"))

  (defun rs/malli/wrap-cider (origin-fn &rest args)
    (let ((res (apply origin-fn args)))
      (nth 2 (buffer-list))
      (when (and (eq (cider-repl-type-for-buffer) 'clj) rs/malli/wrap-cider-enabled)
        (rs/malli/try-start-dev))
      res))

  (advice-add 'cider-eval-buffer :around #'rs/malli/wrap-cider)
  (advice-add 'cider-eval-last-sexp :around #'rs/malli/wrap-cider)
  (advice-add 'cider-eval-defun-at-point :around #'rs/malli/wrap-cider)
  (advice-add 'cider-eval-sexp-at-point :around #'rs/malli/wrap-cider)

  (defun rs/portal/clear ()
    (interactive)
    (with-current-buffer (first (cider-repl-buffers 'clj 't))
      (cider-nrepl-sync-request:eval "(portal.api/clear)")))

  ; (advice-remove 'cider-eval-buffer #'rs/malli/wrap-cider)
  ; (advice-remove 'cider-eval-last-sexp #'rs/malli/wrap-cider)
  ; (advice-remove 'cider-eval-defun-at-point #'rs/malli/wrap-cider)
  ; (advice-remove 'cider-eval-sexp-at-point #'rs/malli/wrap-cider)

  (setq clojure-align-forms-automatically t)
  (setq cider-cljs-lein-repl
        "(do (require 'figwheel-sidecar.repl-api)
         (figwheel-sidecar.repl-api/start-figwheel!)
         (figwheel-sidecar.repl-api/cljs-repl))")

  (after! projectile
    (projectile-register-project-type
     'clojure-cli
     '("deps.edn")
     :project-file "deps.edn"
     :src-dir "src/"
     :test-dir "test/"
     :test-suffix "_test"))
  (setq cljr-magic-require-namespaces
        '(("io" . "clojure.java.io")
          ("sh" . "clojure.java.shell")
          ("jdbc" . "clojure.java.jdbc")
          ("set" . "clojure.set")
          ("time" . "java-time")
          ("str" . "cuerdas.core")
          ("path" . "pathetic.core")
          ("walk" . "clojure.walk")
          ("zip" . "clojure.zip")
          ("async" . "clojure.core.async")
          ("component" . "com.stuartsierra.component")
          ("http" . "hato.client")
          ("url" . "cemerick.url")
          ("sql" . "honey.sql")
          ("csv" . "clojure.data.csv")
          ("json" . "jsonista.core")
          ("s" . "manifold.stream")
          ("d" . "manifold.deferred")
          ("fs" . "me.raynes.fs")
          ("ig" . "integrant.core")
          ("cp" . "com.climate.claypoole")
          ("re-frame" . "re-frame.core")
          ("rf" . "re-frame.core")
          ("rf.db" . "re-frame.db")
          ("re" . "reagent.core")
          ("reagent" . "reagent.core")
          ("w" . "wing.core")
          ("gen" . "clojure.spec.gen.alpha")
          ("m" . "malli.core")
          ("mg" . "malli.generator")
          ("mt" . "malli.transform")
          ("t" . "tick.alpha.api"))))

(add-hook! clojure-mode
  (rainbow-delimiters-mode))

;; (after! company-box
;;   (add-function
;;    :after
;;    (symbol-function 'company-box-doc--show)
;;    (lambda (_ frame)
;;      (when (frame-visible-p (frame-parameter frame 'company-box-doc-frame))
;;        (when (not (frame-visible-p (company-box--get-frame)))
;;          (make-frame-visible (company-box--get-frame)))))))

(after! cider
  (add-hook 'company-completion-started-hook 'ans/set-company-maps)
  (add-hook 'company-completion-finished-hook 'ans/unset-company-maps)
  (add-hook 'company-completion-cancelled-hook 'ans/unset-company-maps)
  (setq cider-show-error-buffer nil)
  (set-popup-rule!
    '(("^\\*cider-error" :select t :size 0.5)))


  ;; Overwrite this cider function for custom focus modes
  (defun cider--anchored-search-suppressed-forms (limit)
  "Matcher for finding unused reader conditional expressions.
An unused reader conditional expression is an expression for a platform
that does not match the CIDER connection for the buffer.  Search is done
with the given LIMIT."
  (let ((repl-types (cond
                     ((eq major-mode 'clojure-mode)
                      '("clj"))
                     ((eq major-mode 'clojurescript-mode)
                      '("cljs"))
                     ('t
                      (seq-uniq (seq-map
                                 (lambda (repl)
                                   (symbol-name (cider-repl-type repl)))
                                 (cider-repls))))))
        (result 'retry))
    (while (and (eq result 'retry) (<= (point) limit))
      (condition-case condition
          (setq result
                (cider--anchored-search-suppressed-forms-internal
                 repl-types limit))
        (invalid-read-syntax
         (setq result 'retry))
        (wrong-type-argument
         (setq result 'retry))
        (scan-error
         (setq result 'retry))
        (end-of-file
         (setq result nil))
        (error
         (setq result nil)
         (message
          "Error during fontification while searching for forms: %S"
          condition))))
    (if (eq result 'retry) (setq result nil))
    result))


  (defun ans/unset-company-maps (&rest unused)
    "Set default mappings (outside of company).
    Arguments (UNUSED) are ignored."
    (general-def
      :states 'insert
      :keymaps 'override
      "C-j" nil
      "C-k" nil
      "C-n" nil
      "C-p" nil
      "RET" nil
      "TAB" nil
      "C-h" nil))

  (defun cider-eval-last-sexp-and-yank ()
    "Evaluate the expression preceding point and yank the result"
    (interactive)
    (cider-eval-last-sexp)
    (cider-kill-last-result))


  (defun ans/set-company-maps (&rest unused)
    "Set maps for when you're inside company completion.
    Arguments (UNUSED) are ignored."
    (general-def
      :states 'insert
      :keymaps 'override
      "C-j" #'company-select-next
      "C-k" #'company-select-previous
      "C-n" #'company-select-next
      "C-p" #'company-select-previous
      "RET" #'company-complete-selection
      "TAB" #'company-complete-common-or-cylce
      "C-h" #'company-show-doc-buffer)))

(after! clj-refactor
  (setq cljr-clojure-test-declaration "[clojure.test :refer [deftest testing is]]"))

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
  (rainbow-delimiters-mode)
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

(load! "+functions")
(load! "+theming")
(load! "+bindings")
(load! "+commands")
(load! "+org")
