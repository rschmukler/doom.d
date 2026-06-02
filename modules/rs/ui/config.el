;;; rs/ui/config.el -*- lexical-binding: t; -*-
;;;
;;; Self-contained ui module: neotree, doom-themes, theme/font/ligature
;;; initialization, and neotree keybindings.

;;
;;; neotree

(after! neotree
  (setq doom-themes-neotree-file-icons 'icons)
  (setq doom-themes-neotree-enable-file-icons 'icons)
  (setq neo-theme 'icons))

(after! doom-themes
  (setq doom-neotree-file-icons t))

;;
;;; Theme / fonts / ligatures

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
  (load-theme 'doom-moonlight 't)
  (if (eq system-type 'darwin)
    (add-to-list 'default-frame-alist '(alpha . (85)))
    (ns-auto-titlebar-mode))
  (if (eq system-type 'darwin)
    (set-face-attribute 'default nil
                          :family "JetBrainsMono Nerd Font"
                          :height 140
                          :weight 'normal
                          :width 'normal)
    (set-face-attribute 'default nil
                        :family "Victor Mono"
                        :height 140
                        :weight 'normal
                        :width 'normal))
  (set-face-attribute #'font-lock-comment-face nil :slant 'italic)
  (set-face-attribute #'font-lock-doc-face nil :slant 'italic))


(cond
 ((window-system) (rs/initialize-theming))
 ((daemonp)
  (add-hook #'after-make-frame-functions
            #'(lambda (frame)
                (with-selected-frame frame
                  (rs/initialize-theming))))))

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

;;
;;; Keybindings

(map!
 (:after neotree
   :map neotree-mode-map
   :n "g"         nil
   :n [tab]       #'neotree-quick-look
   :n "RET"       #'neotree-enter
   :n [backspace] #'evil-window-prev
   :n "c"         #'neotree-create-node
   :n "j"         #'neotree-next-line
   :n "k"         #'neotree-previous-line
   :n "n"         #'neotree-next-line
   :n "p"         #'neotree-previous-line
   :n "h"         #'+neotree/collapse-or-up
   :n "l"         #'+neotree/expand-or-open
   :n "J"         #'neotree-select-next-sibling-node
   :n "K"         #'neotree-select-previous-sibling-node
   :n "H"         #'neotree-select-up-node
   :n "L"         #'neotree-select-down-node
   :n "G"         #'evil-goto-line
   :n "gg"        #'evil-goto-first-line
   :n "v"         #'neotree-enter-vertical-split
   :n "s"         #'neotree-enter-horizontal-split
   :n "q"         #'neotree-hide
   :n "R"         #'neotree-refresh))
