;;; private/rschmukler/theming.el -*- lexical-binding: t; -*-

;; (setq neo-theme 'icons)
(doom-themes-neotree-config)
(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
      doom-themes-enable-italic t) ; if nil, italics is universally disabled
(doom-themes-org-config)

(defun rs/reload-theme-in-all-frames ()
  "Function to force the reloading of the active theme in all frames.
   To be called from emacsclient after modifying the theme file"
  (dolist (frame (visible-frame-list))
    (with-selected-frame frame (doom/reload-theme))))

(defun rs/initialize-theming ()
  (when (eq system-type 'darwin)
    (ns-auto-titlebar-mode)
    (set-face-attribute 'default nil
                        :family "JetBrains Mono"
                        :height 140
                        :weight 'normal
                        :width 'normal)
    (load-theme 'doom-dream-gradient t))
  (when (eq system-type 'gnu/linux)
    (set-face-attribute 'default nil
                        :family "JetBrainsMono"
                        :height 120
                        :weight 'normal
                        :width 'normal)
    (load-theme 'doom-one)))


(cond
 ((window-system) (rs/initialize-theming))
 ((daemonp) (add-hook 'after-make-frame-functions
                      '(lambda (frame)
                         (with-selected-frame frame
                           (rs/initialize-theming)
                           )))))

(setq +ivy-buffer-icons t)
(add-to-list 'default-frame-alist '(alpha . (95)))
(add-to-list 'default-frame-alist '(menu-bar-lines . 0))
(let ((alist '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
                 (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)") ;;
                 (36 . ".\\(?:>\\)")
                 (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
                 (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
                 (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)") ;;
                 (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
                 (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
                 (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)") ;;
                 (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
                 (48 . ".\\(?:x[a-zA-Z]\\)")
                 (58 . ".\\(?:::\\|[:=]\\)")
                 (59 . ".\\(?:;;\\|;\\)")
                 (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
                 (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
                 (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
                 (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
                 (91 . ".\\(?:]\\)")
                 (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
                 (94 . ".\\(?:=\\)")
                 (119 . ".\\(?:ww\\)")
                 (123 . ".\\(?:-\\)")
                 (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
                 (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)"))))
    (dolist (char-regexp alist)
      (set-char-table-range composition-function-table (car char-regexp)
                            `([,(cdr char-regexp) 0 font-shape-gstring]))))
(menu-bar-mode -1)

;; (require 'color)
;; (set-face-attribute 'org-block nil :background
;; (color-darken-name
;; (face-attribute 'default :background) 1))
;; (set-face-attribute 'org-block-begin-line nil :background (face-attribute 'default :background))
;; (set-face-attribute 'org-block-end-line nil :background (face-attribute 'default :background))
(after! org-mode
  (set-face-attribute 'org-level-1 nil :background
                      (doom-darken (face-attribute 'default :background) 0.15)))
