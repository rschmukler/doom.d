;;; private/rschmukler/+bindings.el -*- lexical-binding: t; -*-


(when (eq system-type 'gnu/linux)
  (setq x-hyper-keysym 'meta)
  (setq x-super-keysym 'meta))

(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta))

(map!

 ;; Ensure there are no conflicts
 :nmvo doom-leader-key nil
 :nmvo doom-localleader-key nil

 ;; --- Global keybindings ---------------------------
 ;; Make M-x available everywhere
 :gnvime "M-x" #'execute-extended-command
 :gnvime "A-x" #'execute-extended-command

 ;; A little sandbox to run code in
 :gnvime "M-;" #'eval-expression
 :gnvime "M-:" #'doom/open-scratch-buffer
 ;; Window Movements
 "C-h"    #'evil-window-left
 "C-j"    #'evil-window-down
 "C-k"    #'evil-window-up
 "C-l"    #'evil-window-right
 "A-q"    #'delete-window
 "C-`"      #'+popup/toggle
 "<C-tab>"  #'+popup/other

 :n "-" #'ranger


 :nmvo "C-p"    #'counsel-projectile-find-file

 ;; Text-scaling
 "M-+"    (λ! (text-scale-set 0))
 "M-="    #'text-scale-increase
 "M--"    #'text-scale-decrease

 "M-<return>" #'doom/toggle-fullscreen


 ;; Text Editing
 :nv  "gc"   #'evil-commentary

 (:map evil-window-map                  ; prefix "C-w"
   ;; Navigation
   "C-h"     #'evil-window-left
   "C-j"     #'evil-window-down
   "C-k"     #'evil-window-up
   "C-l"     #'evil-window-right
   "C-w"     #'ace-window
   ;; Swapping windows
   "H"       #'+evil/window-move-left
   "J"       #'+evil/window-move-down
   "K"       #'+evil/window-move-up
   "L"       #'+evil/window-move-right
   "C-S-w"   #'ace-swap-window
   ;; Window undo/redo
   "u"       #'winner-undo
   "C-u"     #'winner-undo
   "C-r"     #'winner-redo
   "o"       #'doom/window-enlargen
   ;; Delete window
   "c"       #'+workspace/close-window-or-workspace
   "C-C"     #'ace-delete-window)


 ;; Leader Configs
 (:leader
   :desc "M-x"                    :nv ":"   #'execute-extended-command
   :desc "Neotree toggle"         :n  "n"   #'neotree-projectile-action

   :desc "Horizonal Split"        :n  "s"   #'split-window-below
   :desc "Vertical Split"         :n  "v"   #'split-window-right

   :desc "Next Error"             :n  "]"   #'flycheck-next-error
   :desc "Previous Error"         :n  "["   #'flycheck-previous-error
   :desc "Show flycheck errors"   :n  "!"   #'flycheck-list-errors

   :desc "Find file content"      :n  "f"   #'counsel-projectile-rg
   :desc "Find project"           :n  "p"   #'projectile-switch-project

   :desc "Switch to last buffer"  :n  "SPC" #'wc/switch-to-mru-buffer
   :desc "Save buffer"            :n  "RET" #'save-buffer

   :desc "Delete the window"      :n  "q"   #'delete-window
   :desc "Ivy open buffers"       :n  "b"   #'ivy-switch-buffer

   :desc "Swiper Search"          :n  "/"   #'swiper
   (:desc "edit" :prefix "e"
    :desc "Doom.d" :n "d" #'doom/open-private-config)

   :desc "Toggle between file and tests"  :n "t" #'projectile-toggle-between-implementation-and-test

   (:desc "help" :prefix "h"
     :n "h" help-map
     :desc "Apropos"               :n "a" #'apropos
     :desc "Reload theme"          :n "R" #'doom/reload-theme
     :desc "Find library"          :n "l" #'find-library
     :desc "Toggle Emacs log"      :n "m" #'doom/popup-toggle-messages
     :desc "Command log"           :n "L" #'global-command-log-mode
     :desc "Describe function"     :n "f" #'describe-function
     :desc "Describe key"          :n "k" #'describe-key
     :desc "Describe char"         :n "c" #'describe-char
     :desc "Describe mode"         :n "M" #'describe-mode
     :desc "Show messages"         :n "m" #'view-echo-area-messages
     :desc "Describe variable"     :n "v" #'describe-variable
     :desc "Describe face"         :n "F" #'describe-face
     :desc "Describe DOOM setting" :n "s" #'doom/describe-setting
     :desc "Describe DOOM module"  :n "d" #'doom/describe-module
     :desc "Find definition"       :n "." #'+jump/definition
     :desc "Find references"       :n "/" #'+jump/references
     :desc "Find documentation"    :n "h" #'+jump/documentation
     :desc "What face"             :n "'" #'doom/what-face
     :desc "What minor modes"      :n ";" #'doom/what-minor-mode
     :desc "Info"                  :n "i" #'info
     :desc "Toggle profiler"       :n "p" #'doom/toggle-profiler)


   (:desc "insert" :prefix "i"
     :desc "From kill-ring" :nv "p" #'counsel-yank-pop
     :desc "From snippet"   :nv "s" #'yas-insert-snippet)

   (:desc "workspace" :prefix "TAB"
     :desc "Display tab bar"          :n "TAB" #'+workspace/display
     :desc "New workspace"            :n "n"   #'+workspace/new
     :desc "Load workspace from file" :n "l"   #'+workspace/load
     :desc "Load last session"        :n "L"   (λ! (+workspace/load-session))
     :desc "Save workspace to file"   :n "s"   #'+workspace/save
     :desc "Autosave current session" :n "S"   #'+workspace/save-session
     :desc "Switch workspace"         :n "."   #'+workspace/switch-to
     :desc "Kill all buffers"         :n "x"   #'doom/kill-all-buffers
     :desc "Delete session"           :n "X"   #'+workspace/kill-session
     :desc "Delete this workspace"    :n "d"   #'+workspace/kill
     :desc "Load session"             :n "L"   #'+workspace/load-session
     :desc "Next workspace"           :n "]"   #'+workspace/switch-right
     :desc "Previous workspace"       :n "["   #'+workspace/switch-left
     :desc "Rename workspace"         :n "r"   #'+workspace:rename
     :desc "Switch to 1st workspace"  :n "0"   (λ! (+workspace/switch-to 0))
     :desc "Switch to 2nd workspace"  :n "1"   (λ! (+workspace/switch-to 1))
     :desc "Switch to 3rd workspace"  :n "2"   (λ! (+workspace/switch-to 2))
     :desc "Switch to 4th workspace"  :n "3"   (λ! (+workspace/switch-to 3))
     :desc "Switch to 5th workspace"  :n "4"   (λ! (+workspace/switch-to 4))
     :desc "Switch to 6th workspace"  :n "5"   (λ! (+workspace/switch-to 5))
     :desc "Switch to 7th workspace"  :n "6"   (λ! (+workspace/switch-to 6))
     :desc "Switch to 8th workspace"  :n "7"   (λ! (+workspace/switch-to 7))
     :desc "Switch to 9th workspace"  :n "8"   (λ! (+workspace/switch-to 8))))

 ;; Racket Mode
 (:after racket-mode
   (:leader
     :desc "Describe symbol at point"     :n "d" #'racket-describe))

 (:after elisp-mode
   (:leader
     :desc "Describe symbol at point"     :n "d" #'+lookup/documentation
     :desc "Jump to definition at point"  :n "l" #'+lookup/definition)
   (:localleader
     :n "p" #'eval-last-sexp
     :n "b" #'eval-buffer))

 (:after sly-mode
   (:map sly-mode-map
     :localleader
     :n  "f"  #'sly-eval-defun))

 (:after julia-mode
   (:map julia-mode-map
     :localleader
     :n  "b"  #'ess-eval-buffer
     :n  "p"  #'ess-eval-function-or-paragraph
     :n  "s"  #'org-schedule))
 )


;; This section is dedicated to "fixing" certain keys so that they behave
;; properly, more like vim. Ripped directly from hlissner
(map! (:map input-decode-map
        [?\C-i] [C-i]
        [S-iso-lefttab] [backtab]
        (:unless window-system "TAB" [tab])) ; Fix TAB in terminal

      ;; I want C-a and C-e to be a little smarter. C-a will jump to
      ;; indentation. Pressing it again will send you to the true bol. Same goes
      ;; for C-e, except it will ignore comments and trailing whitespace before
      ;; jumping to eol.
      :i "C-a" #'doom/backward-to-bol-or-indent
      :i "C-e" #'doom/forward-to-last-non-comment-or-eol
      :i "C-u" #'doom/backward-kill-to-bol-and-indent

      ;; textmate-esque newline insertion
      :i [M-return] #'evil-open-below
      :i [S-M-return] #'evil-open-above
      ;; textmate-esque deletion
      [M-backspace] #'doom/backward-kill-to-bol-and-indent
      :i [backspace] #'delete-backward-char
      :i [M-backspace] #'doom/backward-kill-to-bol-and-indent
      ;; Emacsien motions for insert mode
      :i "C-b" #'backward-word
      :i "C-f" #'forward-word

      ;; Highjacks space/backspace to:
      ;;   a) balance spaces inside brackets/parentheses ( | ) -> (|)
      ;;   b) delete space-indented blocks intelligently
      ;;   c) do none of this when inside a string
      :i [remap newline] #'newline-and-indent

      ;; Make ESC quit all the things
      (:map (minibuffer-local-map
             minibuffer-local-ns-map
             minibuffer-local-completion-map
             minibuffer-local-must-match-map
             minibuffer-local-isearch-map)
        [escape] #'abort-recursive-edit
        "C-r" #'evil-paste-from-register)

      (:map messages-buffer-mode-map
        "M-;" #'eval-expression
        "A-;" #'eval-expression)

      (:map tabulated-list-mode-map
        [remap evil-record-macro] #'doom/popup-close-maybe)

      (:map (evil-ex-completion-map evil-ex-search-keymap read-expression-map)
        "C-a" #'move-beginning-of-line
        "C-w" #'doom/minibuffer-kill-word
        "C-u" #'doom/minibuffer-kill-line
        "C-b" #'backward-word
        "C-f" #'forward-word
        "M-z" #'doom/minibuffer-undo)

      (:after view
        (:map view-mode-map "<escape>" #'View-quit-all)))
