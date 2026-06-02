;;; rs/completion/config.el -*- lexical-binding: t; -*-
;;;
;;; Self-contained completion module: ivy/counsel + company tuning and
;;; their in-buffer / minibuffer keybindings.

;;
;;; ivy / company

(after! company
  (setq company-idle-delay 0))

(after! ivy
  (setq ivy-re-builders-alist
        '((t . ivy--regex-ignore-order)))

  (setq ivy-initial-inputs-alist
        '((counsel-minor . "")
          (counsel-package . "")
          (counsel-org-capture . "")
          (counsel-M-x . "")
          (counsel-describe-function . "")
          (counsel-describe-variable . ""))))

;;
;;; Keybindings

(map!
 (:after ivy
   :map ivy-minibuffer-map
   [escape] #'keyboard-escape-quit
   "M-v" #'yank
   "M-z" #'undo
   "C-r" #'evil-paste-from-register
   "C-k" #'ivy-previous-line
   "C-j" #'ivy-next-line
   "C-l" #'ivy-alt-done
   "C-w" #'ivy-backward-kill-word
   "C-u" #'ivy-kill-line
   "C-b" #'backward-word
   "C-f" #'forward-word)

 (:after company
   :map company-active-map
   "C-j"         #'company-select-next
   "C-n"         #'company-select-next
   "C-k"         #'company-select-previous
   "C-p"         #'company-select-previous
   "C-d"         #'company-show-doc-buffer
   )

 (:after counsel
   (:map ivy-mode-map
     "C-o"      #'ivy-dispatching-done)
   (:map counsel-ag-map
     [backtab]  #'+ivy/wgrep-occur           ; search/replace on results
     "C-SPC"    #'counsel-git-grep-recenter))) ; preview
