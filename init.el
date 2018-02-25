;;; private/rschmukler/init.el -*- lexical-binding: t; -*-


;; Basic Config
(setq backup-directory-alist `(("." . "~/.emacs-tmp/")))
(setq auto-save-file-name-transforms `((".*" "~/.emacs-tmp/" t)))

;; Spaces over tabs
(setq c-basic-indent 2)
(setq c-default-style "linux")
(setq tab-width 2)
(setq-default indent-tabs-mode nil)

(setq exec-path
      (list "/usr/local/bin/"
            "/usr/bin/"
            "/bin/"
            "/usr/sbin/"
            "/sbin/"
            "/Applications/Emacs.app/Contents/MacOS/bin-x86_64-10_9/"
            "/Applications/Emacs.app/Contents/MacOS/libexec-x86_64-10_9/"
            "/Applications/Emacs.app/Contents/MacOS/libexec/"
            (concat (getenv "HOME") "/.cargo/bin")
            (concat (getenv "HOME") "/.local/bin")))

(setenv "PATH" (string-join exec-path ":"))





;; Auto revert-mode. Look ma, no hands...
(global-auto-revert-mode t)

(setq
 whitespace-line-column 100
 whitespace-style
 '(face trailing lines-tail))
(global-whitespace-mode)

;; Turn off line wrapping
(setq-default truncate-lines 1)

(add-hook 'before-save-hook 'whitespace-cleanup)
