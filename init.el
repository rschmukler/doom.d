;;; private/rschmukler/init.el -*- lexical-binding: t; -*-
(when (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (setq comp-deferred-compilation 't))

(setq org-directory (expand-file-name "~/docs/org"))

(when (eq system-type 'darwin)
  (setq exec-path (cons "/opt/homebrew/bin" exec-path))
  (setenv "PATH" (concat "/opt/homebrew/bin:" (getenv "PATH"))))

(doom! :feature
      ;debugger          ; stepping through code, to help you add bugs

       :completion
       (company          ; the ultimate code completion backend
        +auto            ; as-you-type code completion
        +childframe)     ; a nicer company UI. Emacs +26 only!
      ;helm              ; the *other* search engine for love and life
      ;ido               ; the other *other* search engine...
       (ivy +childframe
            +icons)      ; a search engine for love and life

       :emacs
       (dired +ranger
              +icons)    ; making dired pretty [functional]
       electric          ; smarter, keyword-based electric-indent
      ;eshell            ; a consistent, cross-platform shell (WIP)
      ;imenu             ; an imenu sidebar and searchable code index
       vc                ; remember, remember that commit in November

       :editor
       (evil +everywhere); come to the dark side, we have cookies
       file-templates    ; auto-snippets for empty files
       rotate-text       ; cycle region at point between text candidates
       snippets          ; my elves. They type so I don't have to

       :ui
       doom              ; what makes DOOM look the way it does
       doom-dashboard    ; a nifty splash screen for Emacs
       modeline          ; a snazzy Atom-inspired mode-line
       doom-quit         ; DOOM quit-message prompts when you quit Emacs
       ophints           ; display visual hints when editing in evil
       hl-todo           ; highlight todo etc tags
       nav-flash         ; blink the current line after jumping
       neotree           ; a project drawer, like NERDTree for vim
      ;posframe          ; use child frames where possible (Emacs 26+ only)
       (popup            ; tame sudden yet inevitable temporary windows
        +all             ; catch all popups that start with an asterix
        +defaults)       ; default popup rules
      ;tabbar            ; an (incomplete) tab bar for Emacs
      ;unicode           ; extended unicode support for various languages
       vi-tilde-fringe   ; fringe tildes to mark beyond EOB
       vc-gutter         ; commit status in the sidelines
       window-select     ; visually switch windows
       workspaces        ; tab emulation, persistence & separate workspaces

       :tools
       (docker +lsp)     ; Docker
       eval              ; run code, run (also, repls)
      ;ein               ; tame Jupyter notebooks with emacs
       gist              ; interacting with github gists
       (lookup           ; helps you navigate your code and documentation
        +devdocs         ; ...on devdocs.io online
        +docsets)        ; ...or in Dash docsets locally
       make              ; run make tasks from Emacs
       (magit +forge)    ;
       pass              ; password manager for nerds
       pdf               ; pdf enhancements
       direnv            ; Use direnv
       lsp
      ;rgb               ; creating color strings
       tmux              ; an API for interacting with tmux
       upload            ; map local to remote projects via ssh/ftp

       :checkers
       syntax
       spell

       :os
       (:if IS-MAC
        macos)           ; MacOS-specific commands

       :lang
      ;assembly          ; assembly for fun or debugging
       cc                ; C/C++/Obj-C madness
       crystal           ; ruby at the speed of c
       common-lisp       ; Common lisp
       clojure           ; java with a lisp
       csharp            ; unity, .NET, and mono shenanigans
       data              ; config/data formats
       erlang            ; an elegant language for a more civilized age
       elixir            ; erlang done right
       elm               ; care for a cup of TEA?
       emacs-lisp        ; drown in parentheses
      ;ess               ; emacs speaks statistics
       go                ; the hipster dialect
      ;haskell           ; a language that's lazier than I am
       hy                ; readability of scheme w/ speed of python
       (java +meghanada) ; the poster child for carpal tunnel syndrome
       javascript        ; all(hope(abandon(ye(who(enter(here))))))
       julia             ; a better, faster MATLAB
      ;latex             ; writing papers in Emacs has never been so fun
       ledger            ; an accounting system in Emacs
       lua               ; one-based indices? one-based indices
       markdown          ; writing docs for people to ignore
       nix               ; I hereby declare "nix geht mehr!"
      ;ocaml             ; an objective camel
       (org              ; organize your plain life in plain text
        +attach          ; custom attachment system
        +babel           ; running code in org
        +capture         ; org-capture in and outside of Emacs
        +export          ; Exporting org to whatever you want
        +present         ; Emacs for presentations
        +publish         ; Emacs+Org as a static site generator
        +roam2)          ; Org Roam!
       raku              ; perl
       php               ; perl's insecure younger brother
       plantuml          ; diagrams for confusing people more
       purescript        ; javascript, but functional
       (python +pyenv +lsp +pyright)   ; beautiful is better than ugly
       rest              ; Emacs as a REST client
       ruby              ; 1.step do {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
       rust              ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
       scala             ; java, but good
       sh                ; she sells (ba|z)sh shells on the C xor
       swift             ; who asked for emoji variables?
       web               ; the tubes
       yaml

       :term
       term              ; terminals in Emacs

       ;; Applications are complex and opinionated modules that transform Emacs
       ;; toward a specific purpose. They may have additional dependencies and
       ;; should be loaded late.
       :app
      ;(email +gmail)    ; emacs as an email client
      ;irc               ; how neckbeards socialize
      ;(rss +org)        ; emacs as an RSS reader
      ;twitter           ; twitter client https://twitter.com/vnought
      ;(write            ; emacs as a word processor (latex + org + markdown)
      ; +wordnut         ; wordnet (wn) search
      ; +langtool)       ; a proofreader (grammar/style check) for Emacs

       :config
       ;; The default module set reasonable defaults for Emacs. It also provides
       ;; a Spacemacs-inspired keybinding scheme, a custom yasnippet library,
       ;; and additional ex commands for evil-mode. Use it as a reference for
       ;; your own modules.
       ;; (default +bindings +snippets +evil-commands)
       )


;; Basic Config
(setq backup-directory-alist `(("." . "~/.emacs-tmp/")))
(setq auto-save-file-name-transforms `((".*" "~/.emacs-tmp/" t)))

;; Spaces over tabs
(setq c-basic-indent 2)
(setq c-default-style "linux")
(setq tab-width 2)
(setq-default indent-tabs-mode nil)


(setq gcmh-high-cons-threshold 16777216)

;; Auto revert-mode. Look ma, no hands...
(global-auto-revert-mode t)

;; Turn off line wrapping
(setq-default truncate-lines 1)

(add-hook 'before-save-hook 'whitespace-cleanup)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   '((setq cider-clojure-cli-global-options "-Adev")
     (setq cider-clojure-cli-global-options "-A:default")
     (eval setenv "LD_PRELOAD" "libcurl.so.3"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
