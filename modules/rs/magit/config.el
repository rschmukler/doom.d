;;; rs/magit/config.el -*- lexical-binding: t; -*-
;;;
;;; Self-contained magit/VCS module: magit tweaks, forge, code-review,
;;; git-link, difftastic, magit-todos, and all the related keybindings.

;;
;;; magit

(after! magit
  :config
  (setq +workspaces-switch-project-function #'magit-status)
  (setq magit-prefer-remote-upstream t)
  (when (eq system-type 'darwin)
    (setq magit-delete-by-moving-to-trash nil)
    (setq delete-by-moving-to-trash nil)))

(use-package! magit-todos
  :after magit-mode
  :config
  (magit-add-section-hook 'magit-status-sections-hook 'magit-todos--insert-todos nil)
  (setq magit-todos-branch-list-merge-base-ref "origin/main"))

(after! forge
  (transient-append-suffix 'forge-dispatch '(0 -1)
    ["Misc"
     ("y" "yank" forge-copy-url-at-point-as-kill)])
  (transient-append-suffix 'forge-dispatch '(0 -1)
    ["Edit"
     ("e a" "assignees" forge-topic-set-assignees)
     ("e l" "labels" forge-topic-set-labels)
     ("e r" "review requests" forge-topic-set-review-requests)]))

(use-package! code-review
  :config
  (setq code-review-new-buffer-window-strategy #'switch-to-buffer))

(use-package! git-link
  :config
  (setq git-link-use-commit t))

;;
;;; difftastic

(use-package! difftastic
  :defer t
  ;; Sensible defaults already ship with the package:
  ;;   - `difftastic-executable' defaults to "difft" (matches our install)
  ;;   - output reuses the theme's `magit-diff-*' faces for a consistent look
  ;;   - window width / display buffer are auto-computed and reflow on rerun
  :init
  ;; Wire difftastic into magit's transients and relevant keymaps via
  ;; `difftastic-bindings-mode'. By default this binds:
  ;;   D / S  -> difftastic-magit-diff / -show     (in `magit-diff' transient)
  ;;   M-RET  -> difftastic-magit-show             (in `magit-blame')
  ;;   M-d    -> difftastic-magit-diff-buffer-file (in `magit-file-dispatch')
  ;;   M-=    -> difftastic-dired-diff             (in `dired-mode-map')
  (after! magit
    (require 'difftastic-bindings)
    (difftastic-bindings-mode))
  :config
  ;; `difftastic-mode' derives from `fundamental-mode', so under evil its
  ;; default single-key bindings (n/p/g/TAB/q) get shadowed in normal state.
  ;; Re-expose them with vim-friendly bindings.
  (map! :map difftastic-mode-map
        :n "gj"  #'difftastic-next-chunk
        :n "gk"  #'difftastic-previous-chunk
        :n "]f"  #'difftastic-next-file
        :n "[f"  #'difftastic-previous-file
        :n "gr"  #'difftastic-rerun
        :n "TAB" #'difftastic-toggle-chunk
        :n "RET" #'difftastic-diff-visit-file
        :n "q"   #'difftastic-quit
        :n "Q"   #'difftastic-quit-all))

(use-package! difftastic-status
  :after magit
  :config
  (difftastic-status-mode +1)
  (map! :map magit-mode-map
        :n "~" #'difftastic-status-toggle-file-rendering))

;;
;;; Keybindings

(map!
 (:leader
   (:desc "git" :prefix "g"
     :desc "Git status"        :n  "s" #'magit-status
     :desc "Git blame"         :n  "b" #'magit-blame
     :desc "Difftastic diff"   :n  "d" #'difftastic-magit-diff
     :desc "Difftastic show"   :n  "D" #'difftastic-magit-show
     :desc "Git timemachine branch" :n  "B" #'git-timemachine-switch-branch
     :desc "Git time machine"  :n  "t" #'git-timemachine-toggle
     :desc "Git revert hunk"   :n  "r" #'git-gutter:revert-hunk
     :desc "List gists"        :n  "g" #'+gist:list
     :desc "Link"              :v  "l" #'git-link
     :desc "Next hunk"         :nv "]" #'git-gutter:next-hunk
     :desc "Previous hunk"     :nv "[" #'git-gutter:previous-hunk))

 (:after magit
   :map magit-blame-mode-map
   :n "q" #'magit-blame-quit)

 (:after code-review
  :map forge-pullreq-section-map
  (:desc "Visit pull request" :g "RET" #'code-review-forge-pr-at-point)
  (:desc "Browse pull request" :g "S-<return>" #'forge-browse-dwim)
  :map forge-pullreq-list-mode-map
  (:desc "Start a code review" :n "r" #'code-review-forge-pr-at-point)
  (:desc "Start a code review" :n "RET" #'code-review-forge-pr-at-point)
  :map code-review-mode-map
  (:desc "Open code review transit panel" :n "r" #'code-review-transient-api))

 (:after git-timemachine
  :map git-timemachine-mode-map
  :n "C-j" #'git-timemachine-show-next-revision
  :n "C-k" #'git-timemachine-show-previous-revision
  :n "C-c" #'git-timemachine-show-commit
  :n "C-b" #'git-timemachine-blame
  :n "C-y" #'git-timemachine-kill-revision))
