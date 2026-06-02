;;; rs/haskell/config.el -*- lexical-binding: t; -*-
;;;
;;; Self-contained haskell module: haskell-mode, lsp-haskell, source
;;; formatting / test navigation helpers, and keybindings.

;;
;;; Helper commands

(defun urbint/format-haskell-source ()
  (interactive)
  (let ((output-buffer (generate-new-buffer "brittany-out"))
        (config-file-path
         (concat (string-trim
                  (shell-command-to-string "stack path --project-root"))
                 "/brittany.yaml")))
    (when (= 0 (call-process-region
                (point-min) (point-max)
                "stack"
                nil output-buffer nil
                "exec" "--" "brittany" "--config-file" config-file-path))
      (let ((pt (point))
            (wst (window-start))
            (formatted-source (with-current-buffer output-buffer
                                (buffer-string))))
        (erase-buffer)
        (insert formatted-source)
        (goto-char pt)
        (set-window-start nil wst)))))

(defun empire/haskell/module->test ()
  "Jump from a module to a test."
  (let ((filename (->> buffer-file-name
                       (s-replace "/src/" "/test/")
                       (s-replace ".hs" "Test.hs")
                       find-file)))
    (make-directory (f-dirname filename) t)
    (find-file filename)))

(defun empire/haskell/test->module ()
  "Jump from a test to a module."
  (let ((filename (->> buffer-file-name
                       (s-replace "/test/" "/src/")
                       (s-replace "Test.hs" ".hs")
                       )))
    (make-directory (f-dirname filename) t)
    (find-file filename)))

(defun empire/haskell/test<->module ()
  "Toggle between test and module in Haskell."
  (interactive)
  (if (s-contains? "/src/" buffer-file-name)
      (empire/haskell/module->test)
    (empire/haskell/test->module)))

;;
;;; Packages

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

(use-package! lsp-haskell
  :after haskell-mode
  :config
  (setq lsp-haskell-process-path-hie "hie-wrapper"))

;;
;;; Keybindings

(map!
 (:after haskell-mode
   :map haskell-mode-map
   :localleader
   :desc "Show IMenu Nodes"             "m" #'+lsp-ui-imenu
   :desc "Apply LSP Action"             "a" #'lsp-ui-sideline-apply-code-actions))
