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
;;; clone

(defvar rs/clone-root "~/dev"
  "Directory under which `rs/clone' places cloned repositories.")

(defvar rs/clone-default-host "github.com"
  "Host assumed by `rs/clone' for bare OWNER/REPO shorthand input.")

(defvar rs/clone-url-history nil
  "Minibuffer history of URLs passed to `rs/clone'.")

(defun rs/clone--normalize (input)
  "Resolve INPUT into a cons of (CLONE-URL . OWNER/REPO-PATH), or nil.

INPUT may be a full clone URL or a bare shorthand:

  - scheme URL,  e.g. https://github.com/owner/repo[.git]
  - scp-style,   e.g. git@github.com:owner/repo.git
  - shorthand,   e.g. owner/repo  -> https://HOST/owner/repo
                 where HOST is `rs/clone-default-host'

The returned PATH (sans scheme, host and .git, trailing slashes
trimmed) is used for the destination under `rs/clone-root'; nested
paths such as GitLab subgroups are preserved.  CLONE-URL is what is
handed to git."
  (let* ((input (string-trim input))
         (url input)
         (path (cond
                ;; scheme://host[:port]/owner/repo
                ((string-match "\\`[a-zA-Z][a-zA-Z0-9+.-]*://[^/]+/\\(.+\\)\\'" input)
                 (match-string 1 input))
                ;; scp-style: [user@]host:owner/repo (never matches a scheme URL)
                ((and (not (string-match-p "://" input))
                      (string-match "\\`\\(?:[^@/]+@\\)?[^/:]+:\\(.+\\)\\'" input))
                 (match-string 1 input))
                ;; bare shorthand: owner/repo (no scheme, no ':') -> default host
                ((string-match "\\`[^:[:space:]]+/[^:[:space:]]+\\'" input)
                 (setq url (format "https://%s/%s" rs/clone-default-host input))
                 input))))
    (when path
      (setq path (replace-regexp-in-string "/+\\'" "" path))
      (setq path (replace-regexp-in-string "\\.git\\'" "" path))
      ;; Only accept a clean relative OWNER/REPO[/...] path: non-empty, not
      ;; absolute, and with no empty/`.'/`..' segments that could escape
      ;; `rs/clone-root'.
      (when (and (not (string-empty-p path))
                 (not (string-prefix-p "/" path))
                 (seq-every-p (lambda (seg)
                                (not (member seg '("" "." ".."))))
                              (split-string path "/")))
        (cons url path)))))

(defun rs/clone--missing-ancestors (dest)
  "Return DEST's not-yet-existing ancestor directories, deepest-first.
Used to know exactly which directories `rs/clone' creates, so only
those can be pruned on a failed clone (never pre-existing ones)."
  (let ((dir (directory-file-name (file-name-directory (directory-file-name dest))))
        missing)
    (while (and dir
                (not (file-directory-p dir))
                (not (member dir '("/" ""))))
      (push dir missing)            ; ends up shallowest-first...
      (setq dir (directory-file-name (file-name-directory dir))))
    (nreverse missing)))            ; ...so reverse to deepest-first

(defun rs/clone--cleanup (dest created-dirs)
  "Remove DEST and any now-empty ancestor dirs in CREATED-DIRS.
Called after a failed clone so no orphan directories are left behind.
Only empty directories are removed, and only ones we created."
  (when (file-directory-p dest)
    (ignore-errors (delete-directory dest t)))
  (dolist (dir created-dirs)        ; deepest-first
    (when (and (file-directory-p dir)
               (null (directory-files dir nil directory-files-no-dot-files-regexp)))
      (ignore-errors (delete-directory dir)))))

(defun rs/clone--open-workspace (dir name)
  "Open a Doom workspace NAME for the freshly cloned project at DIR.
Mirrors `rs/projectile-switch-project-workspace': reuse the workspace if
it exists, otherwise create it and trigger the project switch action."
  (require 'projectile)
  (projectile-add-known-project (file-name-as-directory dir))
  (if (+workspace-exists-p name)
      (+workspace-switch name)
    (+workspace-switch name t)
    (counsel-projectile-switch-project-action (file-name-as-directory dir)))
  (when (fboundp '+tmux/run)
    (+tmux/run (concat "tt " name))))

(defun rs/clone (input)
  "Clone INPUT into `rs/clone-root' and open a new Doom workspace.

Like `magit-clone', but the destination is derived from INPUT: the
host path (github, gitlab, etc.) is mapped under `rs/clone-root',
creating intermediate directories as needed.  For example,

  https://github.com/plumce/plumcp

clones into ~/dev/plumce/plumcp and opens a workspace named \"plumcp\".

INPUT may also be a bare OWNER/REPO shorthand (e.g. plumce/plumcp),
which is resolved against `rs/clone-default-host' (GitHub by default).

Prompts for the repository interactively."
  (interactive
   (list (read-string "Clone repository: " nil 'rs/clone-url-history)))
  (let ((parsed (rs/clone--normalize input)))
    (unless parsed
      (user-error "Could not parse a repository from: %s" input))
    (let* ((url (car parsed))
           (path (cdr parsed))
           (dest (expand-file-name path rs/clone-root))
           (name (file-name-nondirectory (directory-file-name dest))))
      (when (file-exists-p dest)
        (user-error "Destination already exists: %s" dest))
      (let ((created (rs/clone--missing-ancestors dest))
            (buf (get-buffer-create "*rs/clone*")))
        (with-current-buffer buf (erase-buffer))
        (condition-case err
            (make-directory (file-name-directory dest) t)
          (error
           (rs/clone--cleanup dest created)
           (user-error "Could not create destination %s: %s"
                       (abbreviate-file-name dest) (error-message-string err))))
        (message "Cloning %s into %s..." url (abbreviate-file-name dest))
        (let ((proc (start-process "rs/clone" buf
                                   magit-git-executable "clone" url dest)))
          (set-process-sentinel
           proc
           (lambda (proc _event)
             (when (eq (process-status proc) 'exit)
               (if (zerop (process-exit-status proc))
                   (progn
                     (message "Cloned %s" (abbreviate-file-name dest))
                     (rs/clone--open-workspace dest name))
                 ;; Failed clone: prune the orphan dirs we created and warn,
                 ;; surfacing git's own output so the cause is visible.
                 (rs/clone--cleanup dest created)
                 (display-buffer buf)
                 (display-warning
                  'rs/clone
                  (format "git clone of %s failed (exit %d); cleaned up %s. See *rs/clone* for details."
                          url (process-exit-status proc) (abbreviate-file-name dest))
                  :error))))))))))

;;
;;; github fork

(defvar rs/github-fork-owner "rschmukler"
  "Default GitHub owner proposed by `rs/github-fork' as the fork target.")

(defvar rs/github--user-cache nil
  "Cached login of the authenticated `gh' user.")

(defun rs/github--current-user ()
  "Return the authenticated GitHub login (cached), or nil if unknown."
  (or rs/github--user-cache
      (setq rs/github--user-cache
            (let ((login (string-trim
                          (shell-command-to-string "gh api user -q .login 2>/dev/null"))))
              (and (not (string-empty-p login)) login)))))

(defun rs/github--repo-root ()
  "Return the toplevel directory of the current git repository.
Signals a `user-error' when not inside one."
  (let ((root (or (and (fboundp 'magit-toplevel) (magit-toplevel))
                  (and (fboundp 'projectile-project-root)
                       (ignore-errors (projectile-project-root)))
                  (locate-dominating-file default-directory ".git"))))
    (unless root
      (user-error "Not inside a git repository"))
    (file-name-as-directory (expand-file-name root))))

(defun rs/github--repo-name (root)
  "Return the repository name for the git repo at ROOT.
Derived from origin's URL when available, else from ROOT's directory name."
  (let* ((url (ignore-errors
                (string-trim
                 (shell-command-to-string
                  (format "git -C %s remote get-url origin 2>/dev/null"
                          (shell-quote-argument (directory-file-name root)))))))
         (parsed (and url (not (string-empty-p url)) (rs/clone--normalize url))))
    (if parsed
        (file-name-nondirectory (cdr parsed))
      (file-name-nondirectory (directory-file-name root)))))

(defun rs/github--run (args buf success-msg)
  "Run `gh' with ARGS asynchronously in BUF, warning on failure.
On success, echo SUCCESS-MSG."
  (with-current-buffer (get-buffer-create buf) (erase-buffer))
  (let ((proc (apply #'start-process "rs/github" buf "gh" args)))
    (set-process-sentinel
     proc
     (lambda (proc _event)
       (when (eq (process-status proc) 'exit)
         (if (zerop (process-exit-status proc))
             (message "%s" success-msg)
           (display-buffer buf)
           (display-warning
            'rs/github
            (format "gh %s failed (exit %d). See %s for details."
                    (string-join args " ") (process-exit-status proc) buf)
            :error)))))))

(defun rs/github-fork ()
  "Fork the current repository on GitHub and add a remote for the fork.

Proposes `rs/github-fork-owner'/<repo> as the fork target (editable in
the minibuffer), names the new remote after the fork owner (e.g.
\"rschmukler\"), and asks whether the fork should be public or private.

- Public uses a real GitHub fork (`gh repo fork'), leaving `origin'
  untouched and adding the fork under the owner-named remote.
- Private creates a new private repository from the local repo
  (`gh repo create --source'), since GitHub cannot make a private fork
  of a public repository."
  (interactive)
  (unless (executable-find "gh")
    (user-error "The GitHub CLI (gh) is not installed"))
  (let* ((root (rs/github--repo-root))
         (name (rs/github--repo-name root))
         (target (string-trim
                  (read-string "Fork to: "
                               (format "%s/%s" rs/github-fork-owner name)))))
    (unless (string-match "\\`\\([^/[:space:]]+\\)/\\([^/[:space:]]+\\)\\'" target)
      (user-error "Invalid fork target, expected OWNER/REPO: %s" target))
    (let* ((owner (match-string 1 target))
           (repo (match-string 2 target))
           (remote owner)
           (private (string= "private"
                             (completing-read "Visibility: " '("public" "private")
                                              nil t nil nil "public")))
           (default-directory root)
           (buf "*rs/github-fork*"))
      (if private
          (rs/github--run
           (list "repo" "create" target "--private"
                 "--source" "." "--remote" remote "--push")
           buf
           (format "Created private fork %s (remote %S)" target remote))
        (let ((user (rs/github--current-user)))
          (rs/github--run
           (append (list "repo" "fork" "--remote" "--remote-name" remote)
                   ;; Forking into an org (owner differs from the gh user).
                   (when (and user (not (equal owner user)))
                     (list "--org" owner))
                   ;; Renaming the fork's repo (target name differs from source).
                   (unless (equal repo name)
                     (list "--fork-name" repo)))
           buf
           (format "Forked to %s (remote %S)" target remote)))))))

;;
;;; Keybindings

(map!
 (:leader
   (:desc "git" :prefix "g"
     :desc "Git status"        :n  "s" #'magit-status
     :desc "Git clone"         :n  "c" #'rs/clone
     :desc "Github fork"       :n  "f" #'rs/github-fork
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
