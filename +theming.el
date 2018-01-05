;;; private/rschmukler/theming.el -*- lexical-binding: t; -*-

;; (set-frame-parameter (selected-frame) 'alpha '(100 . 100))
;; (add-to-list 'default-frame-alist '(alpha . (100. 100)))

(when (window-system)
  (set-face-attribute 'default nil
                      :family "Source Code Pro"
                      :height 110
                      :weight 'normal
                      :width 'normal))

(setq neo-theme 'icons)

(when (and window-system (eq system-type 'darwin))
  (set-default-font "Source Code Pro for Powerline")
  (set-face-attribute 'default nil
                      :family "Source Code Pro for Powerline"
                      :height 110
                      :weight 'normal
                      :width 'normal)
  (set-frame-parameter (selected-frame) 'alpha '(90))
  (add-to-list 'default-frame-alist '(alpha . (90))))

