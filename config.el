;;; private/rschmukler/config.el -*- lexical-binding: t; -*-

(setq epg-pinentry-mode 'loopback)

(setq whitespace-line-column 100 whitespace-style '(face trailing lines-tail))
(setq whitespace-global-modes '(not org-mode whitespace-mode magit-mode))
(global-whitespace-mode 1)

(after! flycheck
  ;; Stop the Emacs Lisp checker from littering `flycheck_*.el' copies next to
  ;; the file being checked.  That checker is the only elisp one that uses
  ;; `source-inplace' (so the byte-compiler could resolve relative requires);
  ;; `emacs-lisp-checkdoc' already uses `source'.  Since Doom sets
  ;; `flycheck-emacs-lisp-load-path' to `inherit', requires resolve via the
  ;; running Emacs's `load-path' regardless of where the temp copy lives, so
  ;; checking a copy in the *system* temp dir (`source') works identically --
  ;; without dropping files in the repo.
  ;;
  ;; Wrapped in `eval' (as Doom does elsewhere) because the `setf' gv-expander
  ;; for `flycheck-checker-get' isn't available until `flycheck' has loaded, but
  ;; this form is macroexpanded when the file is loaded/compiled.
  (eval '(setf (flycheck-checker-get 'emacs-lisp 'command)
               (cl-substitute 'source 'source-inplace
                              (flycheck-checker-get 'emacs-lisp 'command)))
        t))

(after! projectile
  (setq projectile-create-missing-test-files t)
  (projectile-register-project-type 'haskell-stack '("stack.yaml")
                              :compile "stack build"
                              :test "stack build --test"
                              :test-suffix "Test")
  (defun rs/projectile-source-dir-locals (&rest _)
    "Source the current project's `.dir-locals.el' when entering a project.
Applies the directory-local variables (including any `eval' forms) for
the project root to the current buffer.  Runs from
`projectile-after-switch-project-hook', so it is independent of and does
not interfere with `+workspaces-switch-project-function' (magit-status)."
    (when-let ((root (ignore-errors (projectile-project-root))))
      (let ((default-directory root)
            (enable-local-variables :all)
            (enable-local-eval t))
        (hack-dir-local-variables-non-file-buffer))))
  (add-hook 'projectile-after-switch-project-hook #'rs/projectile-source-dir-locals)
  (projectile-mode)
  (projectile-load-known-projects))


;; Enable envrc earlier (Doom waits for `doom-first-file') so non-file buffers like magit get sourced.
(add-hook 'doom-first-buffer-hook #'envrc-global-mode)

(add-hook! elm-mode
  (flycheck-mode))

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


(use-package! erlang
  :mode "\\.erl$"
  :config
  (erlang-mode))

(use-package! racket-mode
  :mode "\\.rkt$"
  :config
  (company-mode)
  (flycheck-mode))

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

;; (use-package! yapfify
;;   :hook
;;   (python-mode . yapf-mode)
;;   (before-save . (lambda ()
;;                    (when (eq major-mode 'python-mode)
;;                     (yapify-buffer)))))

(use-package! dhall-mode
  :mode "\\.dhall$")

(use-package! sqlformat
  :hook
  (sql-mode . sqlformat-on-save-mode)
  :config
  (setq sqlformat-command 'pgformatter))

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
(load! "+bindings")
(load! "+commands")
