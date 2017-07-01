;;; private/rschmukler/+commands.el -*- lexical-binding: t; -*-

(defalias 'ex! 'evil-ex-define-cmd)

;; File operations
(ex! "cp"          #'+evil:copy-this-file)
(ex! "mv"          #'+evil:move-this-file)
(ex! "rm"          #'+evil:delete-this-file)
