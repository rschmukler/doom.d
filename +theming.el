;;; private/rschmukler/theming.el -*- lexical-binding: t; -*-

(when (window-system)
  (add-to-list 'default-frame-alist '(alpha . (90)))
  (set-frame-parameter (selected-frame) 'alpha '(90))
  (when (eq system-type 'darwin)
    (ns-auto-titlebar-mode))
  (set-face-attribute 'default nil
                      :family "mononoki"
                      :height 140
                      :weight 'normal
                      :width 'normal))

;; (setq neo-theme 'icons)
(doom-themes-neotree-config)
(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
      doom-themes-enable-italic t) ; if nil, italics is universally disabled
(doom-themes-org-config)

(load-theme 'doom-dream-gradient t)

(setq +ivy-buffer-icons t)
