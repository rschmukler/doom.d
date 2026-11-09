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
    (setq delete-by-moving-to-trash nil))
  ;; Summarise every worktree (branch/HEAD/path) at the end of the status
  ;; buffer.  Renders nothing when the repo has only a single worktree.
  (magit-add-section-hook 'magit-status-sections-hook
                          #'magit-insert-worktrees
                          nil
                          'append))

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

(after! git-commit
  ;; Doom defaults to 50 (the classic Chris Beams convention), which is tight
  ;; once Conventional Commits prefixes (e.g. "feat(scope): ") are accounted
  ;; for. 72 keeps a real hard limit while giving room to breathe.
  (setq git-commit-summary-max-length 72))

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
;;; worktrees
;;
;; Each git worktree is its own projectile project (a linked worktree has a
;; `.git' *file*, which counts as a root marker), so it gets its own doom
;; workspace and its own ECA session.  The only thing missing is a naming
;; convention that keeps the owning repo visible: a bare directory name like
;; `fix-cache-lock' says nothing about which repo it belongs to, and collides
;; when two repos have a worktree for the same branch.
;;
;; `rs/worktree-workspace-name' is the single source of truth for that name and
;; is used by *both* workspace creation here and ECA's session -> workspace
;; lookup (see rs/eca/config.el).  They must agree, or the ECA status coloring
;; and the workspace-kill hook stop finding their session.

(defun rs/worktree--main-root (dir)
  "Return the main worktree root for DIR, or nil when DIR is not in a repo.
All worktrees of a repository share a `git-common-dir', which lives inside
the main worktree; its parent is that worktree's root."
  (let ((default-directory (file-name-as-directory (expand-file-name dir))))
    (when-let* ((out (ignore-errors
                       (string-trim
                        (shell-command-to-string
                         "git rev-parse --git-common-dir 2>/dev/null"))))
                ((not (string-empty-p out)))
                ;; Git < 2.5 echoes the flag back instead of failing.
                ((not (string= out "--git-common-dir"))))
      (file-name-directory (directory-file-name (expand-file-name out))))))

(defun rs/worktree-workspace-name (dir)
  "Return the doom workspace name for the project or worktree at DIR.

For a repository's main worktree this is just its directory name; for a
linked worktree it is \"REPO:WORKTREE\", so the owning repo stays visible
in the workspace switcher and two repos can both have a worktree named
e.g. `fix-auth' without colliding.  Non-git directories fall back to
their own directory name.

Worktrees in a shared pool are already named `REPO_BRANCH' (see
`rs/worktree--default-directory'), so that prefix is stripped rather than
doubled up into `hylo:hylo_rs-HYL-125'."
  (if-let* ((linked (rs/worktree--linked dir)))
      (format "%s:%s" (file-name-nondirectory (car linked)) (cdr linked))
    (file-name-nondirectory (directory-file-name (expand-file-name dir)))))

(defvar rs/worktree--linked-cache (make-hash-table :test #'equal)
  "Memoized results of `rs/worktree--linked', keyed by directory.")

(defun rs/worktree--linked (dir)
  "Return (MAIN . NAME) when DIR is the root of a linked worktree, else nil.
MAIN is the main worktree's root (without trailing slash) and NAME is
DIR's directory name with any `REPO_' pool prefix stripped.  Only a
directory whose `.git' is a file can be a linked worktree, so other
directories are answered without running git.  Remote directories are
never treated as linked worktrees."
  (let ((dir (directory-file-name (expand-file-name dir))))
    (when (and (not (file-remote-p dir))
               (file-regular-p (expand-file-name ".git" dir)))
      (let ((cached (gethash dir rs/worktree--linked-cache 'none)))
        (if (not (eq cached 'none))
            cached
          (puthash
           dir
           (when-let* ((main (rs/worktree--main-root dir))
                       (main (directory-file-name main))
                       ((not (string= main dir)))
                       ;; A submodule's common dir is `.git/modules/NAME'
                       ;; in its parent, which is not a worktree root.
                       ((file-directory-p (expand-file-name ".git" main))))
             (let ((repo (file-name-nondirectory main))
                   (own (file-name-nondirectory dir)))
               (cons main
                     (if (string-prefix-p (concat repo "_") own)
                         (substring own (1+ (length repo)))
                       own))))
           rs/worktree--linked-cache))))))

(defun rs/worktree--projectile-name (root)
  "Return the projectile project name for ROOT.
A linked worktree is named like its workspace (see
`rs/worktree-workspace-name'); anything else gets projectile's default."
  (if (rs/worktree--linked root)
      (rs/worktree-workspace-name root)
    (projectile-default-project-name root)))

(defun rs/worktree--project-display (candidate)
  "Return the name column for project path CANDIDATE in the project switcher.
A linked worktree is shown as its main worktree's path followed by its
own name in green, instead of its pool directory."
  (if-let* ((linked (rs/worktree--linked candidate)))
      (concat (propertize (abbreviate-file-name (car linked)) 'face 'ivy-subdir)
              "  "
              (propertize (cdr linked) 'face 'nerd-icons-green))
    (nerd-icons-ivy-rich-project-name candidate)))

(defun rs/worktree--project-icon (candidate)
  "Return the icon column for project path CANDIDATE in the project switcher.
Linked worktrees get a branch icon and other git repos a repo icon;
anything else keeps the default directory icon."
  (cond
   ((file-remote-p candidate)
    (nerd-icons-ivy-rich-file-icon candidate))
   ((rs/worktree--linked candidate)
    (nerd-icons-ivy-rich-icon
     (nerd-icons-octicon "nf-oct-git_branch" :face 'nerd-icons-purple)))
   ((file-directory-p (expand-file-name ".git" candidate))
    (nerd-icons-ivy-rich-icon
     (nerd-icons-octicon "nf-oct-repo" :face 'nerd-icons-blue)))
   (t (nerd-icons-ivy-rich-file-icon candidate))))

;; Doom names a project's workspace after `doom-project-name', which goes
;; through `projectile-project-name-function'; naming worktrees there means
;; every project switcher creates the repo-qualified workspace directly.
(after! projectile
  (setq projectile-project-name-function #'rs/worktree--projectile-name))

;; The project switcher is drawn by nerd-icons-ivy-rich's columns; swap in
;; our icon and name columns, keeping the rest of its layout.  This runs
;; when the package loads, before Doom enables the mode, so the reload only
;; matters when re-evaluating in a live session.
(after! nerd-icons-ivy-rich
  (let* ((swaps '((nerd-icons-ivy-rich-file-icon . rs/worktree--project-icon)
                  (nerd-icons-ivy-rich-project-name . rs/worktree--project-display)))
         (spec (plist-get nerd-icons-ivy-rich-display-transformers-list
                          'counsel-projectile-switch-project))
         (columns (mapcar (lambda (column)
                            (if-let* ((ours (alist-get (car column) swaps)))
                                (cons ours (cdr column))
                              column))
                          (plist-get spec :columns))))
    (setq nerd-icons-ivy-rich-display-transformers-list
          (plist-put (copy-sequence nerd-icons-ivy-rich-display-transformers-list)
                     'counsel-projectile-switch-project
                     (plist-put (copy-sequence spec) :columns columns))))
  (when (bound-and-true-p nerd-icons-ivy-rich-mode)
    (nerd-icons-ivy-rich-reload)
    (ivy-rich-reload)))

(defun rs/open-project-workspace (dir)
  "Open (or switch to) the doom workspace for the project at DIR.
Registers DIR with projectile, then reuses an existing workspace when one
matches `rs/worktree-workspace-name', otherwise creates it and runs the
usual project-switch action.

The project-switch action names the workspace after `doom-project-name',
which already matches for worktrees; the rename afterwards is a fallback
for when Doom had to uniquify that name."
  (require 'projectile)
  (let* ((dir (file-name-as-directory (expand-file-name dir)))
         (name (rs/worktree-workspace-name dir)))
    (projectile-add-known-project dir)
    (if (+workspace-exists-p name)
        (+workspace-switch name)
      (counsel-projectile-switch-project-action dir)
      ;; Doom has now created/renamed a workspace for this project; give it
      ;; our repo-qualified name unless something already claimed it.
      (let ((current (+workspace-current-name)))
        (unless (equal current name)
          (if (+workspace-exists-p name)
              (+workspace-switch name)
            (with-demoted-errors "Workspace rename error: %s"
              (+workspace-rename current name))))))
    name))

(defun rs/worktree--candidates ()
  "Return an alist of (LABEL . PATH) for the current repo's worktrees.
LABEL shows the path with its checked-out branch, so worktrees of the same
repo are easy to tell apart."
  (require 'magit)
  (mapcar (lambda (wt)
            (let ((path (car wt))
                  (branch (nth 2 wt)))
              (cons (format "%s  [%s]"
                            (abbreviate-file-name path)
                            (or branch "detached"))
                    path)))
          (magit-list-worktrees)))

(defun rs/worktree--read ()
  "Read one of the current repo's worktree paths in the minibuffer."
  (let ((candidates (rs/worktree--candidates)))
    (unless candidates
      (user-error "Not inside a git repository"))
    (cdr (assoc (completing-read "Worktree: " candidates nil t)
                candidates))))

(defun rs/worktree--at-point-or-read ()
  "Return the worktree path at point, or read one in the minibuffer."
  (require 'magit)
  (or (magit-section-value-if 'worktree)
      (rs/worktree--read)))

(defun rs/worktree-switch (path)
  "Switch to the doom workspace for one of this repo's worktrees.
Opens the workspace only; start a chat there with `SPC e e' / `SPC e n'."
  (interactive (list (rs/worktree--read)))
  (rs/open-project-workspace path))

(defun rs/worktree-visit (path)
  "Show the status of the worktree at PATH in the current workspace.
The current workspace is renamed to match the worktree.  When a
workspace with that name already exists, it is switched to instead."
  (interactive (list (rs/worktree--at-point-or-read)))
  (when (bound-and-true-p persp-mode)
    (let ((name (rs/worktree-workspace-name path))
          (current (+workspace-current-name)))
      (cond ((equal name current))
            ((+workspace-exists-p name) (+workspace-switch name))
            (t (with-demoted-errors "Workspace rename error: %s"
                 (+workspace-rename current name))))))
  (magit-worktree-status path))

(defun rs/worktree-visit-in-new-workspace (path)
  "Open the worktree at PATH in its own doom workspace.
Reuses the worktree's workspace when one is already open."
  (interactive (list (rs/worktree--at-point-or-read)))
  (rs/open-project-workspace path))

(defvar rs/worktree-directory "~/dev/.worktrees"
  "Where `rs/worktree-create' places new worktrees.
A relative path is resolved inside the main worktree (so worktrees are
nested under the repo); an absolute path, or one starting with `~', is
used as-is.  A nested directory must be ignored by git -- see
`rs/worktree--ensure-excluded'.")

(defun rs/worktree--ensure-excluded (main)
  "Ensure `rs/worktree-directory' is ignored in the repo at MAIN.
Only applies when the directory is nested inside the repo, where git
would otherwise report it as untracked.  Written to `.git/info/exclude'
rather than `.gitignore' so the convention stays local and never lands
in a commit."
  (unless (file-name-absolute-p rs/worktree-directory)
    (let* ((exclude (expand-file-name ".git/info/exclude" main))
           (entry (file-name-as-directory rs/worktree-directory))
           (current (when (file-readable-p exclude)
                      (with-temp-buffer
                        (insert-file-contents exclude)
                        (buffer-string)))))
      (unless (and current
                   (string-match-p (concat "^" (regexp-quote entry) "$") current))
        (make-directory (file-name-directory exclude) t)
        (with-temp-buffer
          (when current (insert current))
          (goto-char (point-max))
          (unless (bolp) (insert "\n"))
          (insert entry "\n")
          (write-region (point-min) (point-max) exclude nil 'silent))
        (message "Added %s to %s" entry (abbreviate-file-name exclude))))))

(defvar rs/worktree-copy-files '(".envrc" ".dir-locals.el")
  "Gitignored files copied into a newly created worktree.
Copies (rather than links) are independent, so a worktree can diverge --
and `direnv' wants to hash and allow its own `.envrc' per directory.")

(defvar rs/worktree-link-files '(".eca")
  "Gitignored files symlinked into a newly created worktree.
Links keep a single source of truth in the main worktree: editing the
target from any worktree changes it for all of them.  Suitable for
shared config (ECA's MCP servers, rules, ...) that should not drift.")

(defun rs/worktree--link-missing (main worktree entry)
  "Symlink ENTRY from MAIN into WORKTREE, returning the linked relative paths.
When ENTRY is already a real directory in WORKTREE (because some of its
files are tracked), its missing children are linked individually instead,
recursively."
  (let ((src (expand-file-name entry main))
        (dst (expand-file-name entry worktree)))
    (cond
     ((not (file-exists-p src)) nil)
     ((not (or (file-exists-p dst) (file-symlink-p dst)))
      (make-symbolic-link src dst)
      (list entry))
     ((and (file-directory-p src)
           (file-directory-p dst)
           (not (file-symlink-p dst)))
      (mapcan (lambda (child)
                (rs/worktree--link-missing main worktree
                                           (concat (file-name-as-directory entry)
                                                   child)))
              (directory-files src nil directory-files-no-dot-files-regexp))))))

(defun rs/worktree--seed (main worktree)
  "Seed WORKTREE with the gitignored config files from MAIN.

`git worktree add' only materialises *tracked* files, so anything
gitignored (`.envrc', `.eca', ...) is absent from a fresh worktree even
though the repo does not work without it.  `rs/worktree-copy-files' are
copied and `rs/worktree-link-files' are symlinked (into a partially
tracked directory, only its missing children are linked); existing
entries in WORKTREE are never overwritten.  Caches such as `.direnv' are
deliberately not seeded -- direnv rebuilds them on `direnv allow'."
  (let (seeded)
    (dolist (entry rs/worktree-copy-files)
      (let ((src (expand-file-name entry main))
            (dst (expand-file-name entry worktree)))
        (when (and (file-exists-p src) (not (file-exists-p dst)))
          (if (file-directory-p src)
              (copy-directory src dst nil t t)
            (copy-file src dst nil t))
          (push entry seeded))))
    (dolist (entry rs/worktree-link-files)
      (dolist (linked (rs/worktree--link-missing main worktree entry))
        (push (concat linked " -> main") seeded)))
    (when seeded
      (message "Seeded worktree with %s" (string-join (nreverse seeded) ", ")))))

(defun rs/worktree--direnv-allow (worktree)
  "Run `direnv allow' in WORKTREE when it has an `.envrc'.
A copied `.envrc' is untrusted in its new location, so direnv refuses to
load it until allowed."
  (when (and (executable-find "direnv")
             (file-exists-p (expand-file-name ".envrc" worktree)))
    (let ((default-directory (file-name-as-directory worktree)))
      (if (zerop (call-process "direnv" nil nil nil "allow"))
          (message "direnv allow: %s" (abbreviate-file-name worktree))
        (message "direnv allow failed in %s" (abbreviate-file-name worktree))))))

(defun rs/worktree--default-directory (branch)
  "Return the default path for a new worktree checking out BRANCH.
Resolved under `rs/worktree-directory'.  When that is a shared pool
outside the repo (an absolute path), the directory name is prefixed with
the repo -- `hylo' + `rs/HYL-123' -> `hylo_rs-HYL-123' -- so worktrees
from different repos stay distinguishable.  Nested under the repo the
prefix is redundant and omitted.  Slashes in BRANCH are flattened so
`feat/foo' does not create intermediate directories."
  (let* ((main (directory-file-name (or (rs/worktree--main-root default-directory)
                                        (user-error "Not inside a git repository"))))
         (repo (file-name-nondirectory main))
         (absolute (file-name-absolute-p rs/worktree-directory))
         (base (if absolute
                   (expand-file-name rs/worktree-directory)
                 (expand-file-name rs/worktree-directory main)))
         (leaf (replace-regexp-in-string "/" "-" branch)))
    (expand-file-name (if absolute (format "%s_%s" repo leaf) leaf) base)))

(defun rs/worktree--add-and-open (name add-fn)
  "Add a worktree via ADD-FN, seed it, and switch to its workspace.
NAME (a branch or commit) determines the default location offered in the
minibuffer.  ADD-FN is called with the chosen directory and must run
`git worktree add' there."
  (let ((main (directory-file-name
               (or (rs/worktree--main-root default-directory)
                   (user-error "Not inside a git repository"))))
        (path (read-directory-name
               "Worktree location: "
               (file-name-as-directory (rs/worktree--default-directory name)))))
    (when (file-exists-p (directory-file-name path))
      (user-error "Destination already exists: %s" path))
    (rs/worktree--ensure-excluded main)
    ;; The magit worktree commands only proceed when git exits 0, so a failed
    ;; `git worktree add' must not fall through to opening a workspace for a
    ;; directory that was never created.
    (funcall add-fn path)
    (if (file-directory-p (directory-file-name path))
        (progn
          ;; Gitignored config (.envrc/.eca/...) is not part of the tree git
          ;; checks out, so seed it before anything tries to use the worktree.
          (rs/worktree--seed main path)
          (rs/worktree--direnv-allow path)
          (rs/open-project-workspace path))
      (user-error "Worktree was not created at %s (see the git output above)"
                  (abbreviate-file-name path)))))

(defun rs/worktree-create (branch start-point)
  "Create a new BRANCH from START-POINT in a worktree and switch to it.
The worktree is placed under `rs/worktree-directory' (by default
`.worktrees/' inside the repo); the location is editable in the
minibuffer.  A nested worktree directory is added to `.git/info/exclude'
so git does not report it as untracked."
  (interactive
   ;; `magit-branch-read-args' returns (BRANCH START-POINT) and resolves the
   ;; start-point the same way `magit-worktree-branch' does.  Reading it
   ;; ourselves (e.g. `magit-get-current-branch') breaks on a detached HEAD and
   ;; on repos whose default branch is not the checked-out one.
   (magit-branch-read-args "In new worktree; checkout new branch"))
  (rs/worktree--add-and-open
   branch
   (lambda (path) (magit-worktree-branch path branch start-point))))

(defun rs/worktree-checkout (commit)
  "Check out an existing COMMIT or branch in a new worktree and switch to it.
Like `rs/worktree-create', but no new branch is created."
  (interactive
   (list (magit-read-branch-or-commit
          "In new worktree; checkout" nil
          (mapcar #'caddr (magit-list-worktrees)))))
  (rs/worktree--add-and-open
   commit
   (lambda (path) (magit-worktree-checkout path commit))))

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

(defun rs/clone--open-workspace (dir _name)
  "Open a Doom workspace for the freshly cloned project at DIR."
  (rs/open-project-workspace dir))

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
     (:desc "worktree" :prefix "w"
       :desc "Switch worktree"   :n "w" #'rs/worktree-switch
       :desc "Create worktree"   :n "c" #'rs/worktree-create
       :desc "Checkout worktree" :n "C" #'rs/worktree-checkout
       :desc "Delete worktree"   :n "k" #'magit-worktree-delete
       :desc "Worktree actions"  :n "Z" #'magit-worktree)
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

 ;; RET on a worktree opens it in the current workspace (renamed to match);
 ;; S-RET opens it in its own workspace.  Section keymaps are text
 ;; properties, so they take precedence over evil's state maps.
 (:after magit
  :map magit-worktree-section-map
  [remap magit-visit-thing] #'rs/worktree-visit
  "S-<return>"              #'rs/worktree-visit-in-new-workspace)

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
