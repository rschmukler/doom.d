;;; private/rschmukler/theming.el -*- lexical-binding: t; -*-

(setq neo-theme 'icons)

(when (and window-system (eq system-type 'darwin))
  (set-default-font "Source Code Pro for Powerline")
  (set-frame-parameter (selected-frame) 'alpha '(90))
  (add-to-list 'default-frame-alist '(alpha . (90))))

