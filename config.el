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

(after! ivy
  (setq ivy-re-builders-alist
      '((t . ivy--regex-ignore-order))))

(add-hook! elixir-mode
  (flycheck-mode)
  (rainbow-delimiters-mode)
  )

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
