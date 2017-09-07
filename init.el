;;; private/rschmukler/init.el -*- lexical-binding: t; -*-


;; Basic Config
(setq backup-directory-alist `(("." . "~/.emacs-tmp/")))
(setq auto-save-file-name-transforms `((".*" "~/.emacs-tmp/" t)))

;; Spaces over tabs
(setq c-basic-indent 2)
(setq c-default-style "linux")
(setq tab-width 2)
(setq-default indent-tabs-mode nil)

;; Auto revert-mode. Look ma, no hands...
(global-auto-revert-mode t)

;; Turn off line wrapping
(setq-default truncate-lines 1)

(add-hook 'before-save-hook 'whitespace-cleanup)
