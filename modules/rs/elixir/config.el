;;; rs/elixir/config.el -*- lexical-binding: t; -*-
;;;
;;; Self-contained elixir module: flycheck (mix + credo), alchemist
;;; test navigation, and keybindings.

(add-hook! elixir-mode
  (flycheck-mode))

(use-package! flycheck-mix
  :after elixir-mode
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-mix-setup))

(use-package! flycheck-credo
  :after elixir-mode
  :config
  (setq flycheck-elixir-credo-strict t)
  (add-hook 'flycheck-mode-hook #'flycheck-credo-setup))

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

;;
;;; Keybindings

(map!
 (:after elixir-mode
   (:leader
     :desc "Toggle between file and tests"   :n "t" (λ! (alchemist-project-toggle-file-and-tests))
     :desc "Jump to definition at point"     :n "l" #'alchemist-goto-definition-at-point)))
