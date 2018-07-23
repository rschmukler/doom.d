;;; private/rschmukler/theming.el -*- lexical-binding: t; -*-

;; (set-frame-parameter (selected-frame) 'alpha '(100 . 100))
;; (add-to-list 'default-frame-alist '(alpha . (100. 100)))

(when (window-system)
  (add-to-list 'default-frame-alist '(alpha . (90)))
  (set-frame-parameter (selected-frame) 'alpha '(90))
  (set-face-attribute 'default nil
                      :family "InputMono"
                      :height 120
                      :weight 'normal
                      :width 'normal))

(setq neo-theme 'icons)
