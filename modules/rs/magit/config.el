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
;;; ediff
;;
;; ediff's defaults are hostile, and worse with a dedicated side window open
;; (e.g. an ECA chat).  Two problems:
;;
;;   1. On a GUI frame `ediff-window-setup-function' defaults to a *separate
;;      floating control frame' and shuffles your windows around without
;;      restoring them.
;;   2. ediff wants to `delete-other-windows' and split the frame, but a
;;      `window-side' / `no-delete-other-windows' window (how ECA displays its
;;      chat) cannot be deleted -- so the resulting layout is corrupted.
;;
;; Lives in this module because magit (conflict resolution / `magit-ediff-dwim')
;; is the dominant entry point, alongside the other diff tooling here.  It is an
;; `after! ediff' form, so it still activates whenever ediff loads on its own
;; (e.g. `ediff-current-file') regardless of whether magit was ever opened.
;;
;; The fixes below: keep the control panel in the current frame, split
;; side-by-side, snapshot the window layout before a session, and temporarily
;; neutralise side-window protections so ediff has a clean, splittable frame --
;; restoring the original layout (side window and all) when the session ends.
;; The protection relaxation is generic (any side window), so this also covers
;; magit/`ediff-buffers'/etc., not just ECA-initiated diffs.
(after! ediff
  (setq ediff-window-setup-function #'ediff-setup-windows-plain
        ediff-split-window-function #'split-window-horizontally
        ediff-merge-split-window-function #'split-window-horizontally
        ;; Hide the common-ancestor pane in 3-way merges for a clean A | B over
        ;; result layout (no fourth window).
        ediff-show-ancestor nil)

  (defvar rs/ediff--saved-window-config nil
    "Window configuration saved before an ediff session begins.
Restored verbatim on quit/suspend, which also brings back any side
window (and its parameters) that we relaxed for the session.")

  (defun rs/ediff--before-setup-h ()
    "Snapshot the layout and clear side-window protections for ediff.
ediff cannot manage a frame containing a protected side window (such as
an ECA chat); neutralising those parameters lets it split the frame
cleanly.  The originals return via `rs/ediff--after-quit-h', which
restores the whole window configuration captured here."
    (setq rs/ediff--saved-window-config (current-window-configuration))
    (dolist (w (window-list nil 'nomini))
      (when (or (window-parameter w 'window-side)
                (window-parameter w 'no-delete-other-windows))
        (set-window-parameter w 'window-side nil)
        (set-window-parameter w 'no-delete-other-windows nil))))

  (defun rs/ediff--after-quit-h ()
    "Restore the layout saved by `rs/ediff--before-setup-h'."
    (when rs/ediff--saved-window-config
      (ignore-errors
        (set-window-configuration rs/ediff--saved-window-config))
      (setq rs/ediff--saved-window-config nil)))

  (add-hook 'ediff-before-setup-hook #'rs/ediff--before-setup-h)
  ;; `append' so any ediff-internal quit handling runs first, then we restore.
  (add-hook 'ediff-quit-hook #'rs/ediff--after-quit-h 'append)
  (add-hook 'ediff-suspend-hook #'rs/ediff--after-quit-h 'append))

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

(use-package! magit-difftastic
  :after magit
  :config
  (magit-difftastic-mode +1)
  (map! :map magit-mode-map
        :n "~" #'magit-difftastic-toggle-file-rendering))

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
