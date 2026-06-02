;;; private/rschmukler/config.el -*- lexical-binding: t; -*-

(setq epg-pinentry-mode 'loopback)

(setq whitespace-line-column 100 whitespace-style '(face trailing lines-tail))
(setq whitespace-global-modes '(not org-mode whitespace-mode magit-mode))
(global-whitespace-mode 1)

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

(after! projectile
  (setq projectile-create-missing-test-files t)
  (projectile-register-project-type 'haskell-stack '("stack.yaml")
                              :compile "stack build"
                              :test "stack build --test"
                              :test-suffix "Test")
  (projectile-mode)
  (projectile-load-known-projects))


(add-hook! elm-mode
  (flycheck-mode))

(use-package! dockerfile-mode
  :mode "Dockerfile$")


(after! ranger
  :config
  (setq ranger-show-literal nil))


(use-package! reason-mode
  :mode "\\.re$"
  :hook
  (before-save . (lambda ()
                   (when (equal major-mode 'reason-mode)
                     (refmt)))))


(use-package! erlang
  :mode "\\.erl$"
  :config
  (erlang-mode))

(use-package! racket-mode
  :mode "\\.rkt$"
  :config
  (company-mode)
  (flycheck-mode))

(after! typescript-mode
  (setq typescript-indent-level 2))

(after! typescript-tsx-mode
  (setq typescript-indent-level 2))

;; (after! company-box
;;   (add-function
;;    :after
;;    (symbol-function 'company-box-doc--show)
;;    (lambda (_ frame)
;;      (when (frame-visible-p (frame-parameter frame 'company-box-doc-frame))
;;        (when (not (frame-visible-p (company-box--get-frame)))
;;          (make-frame-visible (company-box--get-frame)))))))

(use-package! graphql-mode
  :mode "\\.gql$")

(use-package! fennel-mode
  :mode "\\.fnl$")

;; (use-package! yapfify
;;   :hook
;;   (python-mode . yapf-mode)
;;   (before-save . (lambda ()
;;                    (when (eq major-mode 'python-mode)
;;                     (yapify-buffer)))))

(use-package! dhall-mode
  :mode "\\.dhall$")

(use-package! sqlformat
  :hook
  (sql-mode . sqlformat-on-save-mode)
  :config
  (setq sqlformat-command 'pgformatter))

(use-package! fabb
  :config
  (map!
   (:leader
    :desc "Fabb Status" :n "t" #'fabb-status
    :desc "Fabb Ivy" :n "T" #'fabb-invoke-ivy)
   (:map fabb-mode-map
    :n "q" #'kill-this-buffer
    :n "/" #'fabb-invoke-ivy
    :n "?" #'fabb-dispatch)))

(use-package! eca
  :config
  (setq eca-buttons-allow-mouse t)
  (add-to-list 'whitespace-global-modes 'eca-chat-mode t)
  (map!
   (:map doom-leader-map
    :leader
    :desc "Eca" :prefix "e"
    :desc "Start session" :n "e" #'eca
    :desc "Start session" :n "r" #'eca-restart
    :desc "Rewrite" :n "w" #'eca-rewrite
    :desc "Talk" :n "t" #'eca-chat-talk)
   :after eca-chat
   :map eca-chat-mode-map
   :desc "Toggle section" :n "<tab>" #'eca-chat-toggle-expandable-block
   :desc "Toggle section" :n "TAB" #'eca-chat-toggle-expandable-block
   :n "<return>" #'eca-chat--key-pressed-return
   :n "RET" #'eca-chat--key-pressed-return
   :i "<return>" #'eca-chat--key-pressed-return
   :i "RET" #'eca-chat--key-pressed-return
   :localleader
   :desc "Clear the chat" :n "c" #'eca-chat-clear
   :desc "New chat" :n "n" #'eca-chat-new
   :desc "Delete chat (server)" :n "d" #'eca-chat-delete
   :desc "Rename chat" :n "r" #'eca-chat-rename
   :desc "Reset chat" :n "R" #'eca-chat-reset
   :desc "Accept all chat tool calls" :n "a" #'eca-chat-tool-call-accept-all
   :desc "Accept all chat tool calls (and remember)" :n "A" #'eca-chat-tool-call-accept-all-and-remember
   :desc "Select the model" :n "m" #'eca-chat-select-model
   :desc "Select the agent" :n "b" #'eca-chat-select-agent
   :desc "Select the variant" :n "v" #'eca-chat-select-variant
   :desc "Cycle eca tab" :n "<tab>" #'tab-line-switch-to-next-tab
   :desc "View MCPs" :n "M" #'eca-mcp-details))

(load! "+functions")
(load! "+bindings")
(load! "+commands")
