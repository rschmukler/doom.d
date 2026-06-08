;;; rs/eca/config.el -*- lexical-binding: t; -*-
;;;
;;; ECA (Editor Code Assistant) config + keybindings, plus helpers to jump to
;;; the next chat that needs attention (within a project or across projects).

;; Make eca's definitions available *at compile time* only.  With
;; `native-comp-jit-compilation' enabled, Emacs async-compiles this file in a
;; worker that does not have eca loaded.  Without the `eca--session'
;; `cl-defstruct' present, `setf' on a struct slot (e.g.
;; `(setf (eca--session-last-chat-buffer s) v)') cannot find the generated
;; setf-expander and instead bakes in a call to the nonexistent function
;; `(setf eca--session-last-chat-buffer)' -- which then errors with
;; "Symbol's function definition is void" when the cached .eln is loaded next
;; session.  Requiring the libs here (compile-time only) fixes the expansion
;; without forcing eca to load eagerly at startup; the interactive commands
;; still `require' what they need at runtime.
(eval-when-compile
  (require 'cl-lib)
  (require 'eca-util)
  (require 'eca-diff)
  (require 'eca-chat))

;;; --- "Jump to chat needing attention" -----------------------------------
;;
;; A chat "needs attention" when it is waiting on *you*: it has a tool call
;; pending approval (🚧) or an unanswered question.  A still-loading chat (⏳)
;; is busy working, not waiting, so it is intentionally ignored.
;;
;; Two entry points:
;; - `rs/eca-jump-to-attention-in-project' cycles the current project's chat
;;   tabs only.
;; - `rs/eca-jump-to-attention' exhausts the current project's tabs first, then
;;   moves on to the next project (ECA session), eventually visiting them all.

(defun rs/eca--needs-attention-p (buffer)
  "Non-nil when chat BUFFER is waiting on the user.
A chat needs attention when it has a pending tool-call approval or an
unanswered question.  A still-loading chat (⏳) is busy, not waiting."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (and (derived-mode-p 'eca-chat-mode)
              (or (eca-chat--has-pending-approvals-p)
                  eca-chat--pending-question)))))

(defun rs/eca--idle-p (buffer)
  "Non-nil when chat BUFFER is idle: open but not loading and not needing attention."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (and (derived-mode-p 'eca-chat-mode)
              (not (buffer-local-value 'eca-chat--chat-loading buffer))
              (not (rs/eca--needs-attention-p buffer))))))

(defun rs/eca--session-chats (session)
  "Return SESSION's chat buffers in tab order (oldest-first)."
  (reverse (eca-vals (eca--session-chats session))))

(defun rs/eca--sessions-ordered ()
  "Return all ECA sessions ordered by creation id (stable across calls)."
  (when (boundp 'eca--sessions)
    (sort (copy-sequence (eca-vals eca--sessions))
          (lambda (a b) (< (eca--session-id a) (eca--session-id b))))))

(defun rs/eca--session-workspace (session buffer)
  "Return the doom workspace name for SESSION's BUFFER, or nil.
Prefer a workspace named after the session's project (the convention
used by `rs/projectile-switch-project-workspace'); fall back to the
workspace that actually contains BUFFER."
  (when (and (bound-and-true-p persp-mode)
             (fboundp '+workspace-list-names))
    (let ((names (+workspace-list-names)))
      (or (and (fboundp 'eca--session-project-name)
               (car (member (eca--session-project-name session) names)))
          (and (fboundp '+workspace-buffer-list)
               (seq-find (lambda (name)
                           (memq buffer (ignore-errors (+workspace-buffer-list name))))
                         names))))))

(defun rs/eca--show-chat (session buffer)
  "Make BUFFER the active chat for SESSION and display it.
Switches to the buffer's doom workspace first when needed, so the chat
lands in the matching project tab."
  (when-let ((ws (rs/eca--session-workspace session buffer)))
    (when (and (fboundp '+workspace-current-name)
               (fboundp '+workspace-switch)
               (not (equal ws (+workspace-current-name))))
      (+workspace-switch ws)))
  (setf (eca--session-last-chat-buffer session) buffer)
  (eca-chat-open session)
  (when-let ((win (get-buffer-window buffer t)))
    (select-window win)))

(defun rs/eca-jump-to-attention-in-project ()
  "Jump to the next chat tab in the current project that needs attention.
Cycles through the current ECA session's chat tabs, wrapping around, so
repeated invocations move through every tab that is waiting on you."
  (interactive)
  (require 'eca-chat)
  (let ((session (eca-session)))
    (eca-assert-session-running session)
    (let* ((chats (rs/eca--session-chats session))
           (cur (current-buffer))
           (pos (cl-position cur chats))
           ;; Start the search *after* the current tab (wrapping) so each call
           ;; advances to the next one rather than re-selecting the current.
           (ordered (if pos
                        (append (nthcdr (1+ pos) chats)
                                (cl-subseq chats 0 (1+ pos)))
                      chats))
           (target (seq-find #'rs/eca--needs-attention-p ordered)))
      (if target
          (rs/eca--show-chat session target)
        (eca-info "No ECA chat needs attention in this project")))))

(defun rs/eca-jump-to-attention ()
  "Jump to the next ECA chat needing attention, cycling across projects.
First exhausts the current project's chat tabs, then moves on to the next
project (ECA session), eventually visiting them all and wrapping around."
  (interactive)
  (require 'eca-chat)
  (let ((sessions (rs/eca--sessions-ordered)))
    (unless sessions
      (user-error "ECA must be running, no session found, start with `eca` command"))
    (let* ((entries (cl-loop for session in sessions
                             append (cl-loop for buf in (rs/eca--session-chats session)
                                             collect (cons session buf))))
           (attention (cl-remove-if-not
                       (lambda (e) (rs/eca--needs-attention-p (cdr e)))
                       entries))
           (cur (current-buffer))
           ;; Anchor the search at the current chat tab; if we are not on a chat
           ;; buffer, anchor just before the current project's tabs so it is
           ;; still searched first.
           (pos (or (cl-position cur entries :key #'cdr)
                    (when-let* ((cs (ignore-errors (eca-session)))
                                (idx (cl-position cs entries :key #'car)))
                      (1- idx))))
           (after (when pos
                    (seq-find (lambda (e) (rs/eca--needs-attention-p (cdr e)))
                              (nthcdr (1+ pos) entries))))
           (target (or after (car attention))))
      (if target
          (rs/eca--show-chat (car target) (cdr target))
        (eca-info "No ECA chat needs attention in any project")))))

;;; --- "Jump to idle chat" ------------------------------------------------
;;
;; An "idle" chat is one that is open but not currently loading (⏳) and not
;; waiting on the user (🚧).  These are the chats you can send a new message
;; to immediately.
;;
;; Two entry points mirror the attention variants:
;; - `rs/eca-jump-to-idle-in-project' cycles the current project's chat tabs.
;; - `rs/eca-jump-to-idle' exhausts the current project's tabs first, then
;;   moves on to the next project, eventually wrapping around.

(defun rs/eca-jump-to-idle-in-project ()
  "Jump to the next idle chat tab in the current project.
An idle chat is open, not loading, and not waiting for your input.
Cycles through the current ECA session's chat tabs, wrapping around."
  (interactive)
  (require 'eca-chat)
  (let ((session (eca-session)))
    (eca-assert-session-running session)
    (let* ((chats (rs/eca--session-chats session))
           (cur (current-buffer))
           (pos (cl-position cur chats))
           (ordered (if pos
                        (append (nthcdr (1+ pos) chats)
                                (cl-subseq chats 0 (1+ pos)))
                      chats))
           (target (seq-find #'rs/eca--idle-p ordered)))
      (if target
          (rs/eca--show-chat session target)
        (eca-info "No idle ECA chat in this project")))))

(defun rs/eca-jump-to-idle ()
  "Jump to the next idle ECA chat, cycling across all projects.
First exhausts the current project's chat tabs, then moves on to the next
project (ECA session), eventually visiting them all and wrapping around."
  (interactive)
  (require 'eca-chat)
  (let ((sessions (rs/eca--sessions-ordered)))
    (unless sessions
      (user-error "ECA must be running, no session found, start with `eca` command"))
    (let* ((entries (cl-loop for session in sessions
                             append (cl-loop for buf in (rs/eca--session-chats session)
                                            collect (cons session buf))))
           (idle (cl-remove-if-not
                  (lambda (e) (rs/eca--idle-p (cdr e)))
                  entries))
           (cur (current-buffer))
           (pos (or (cl-position cur entries :key #'cdr)
                    (when-let* ((cs (ignore-errors (eca-session)))
                                (idx (cl-position cs entries :key #'car)))
                      (1- idx))))
           (after (when pos
                    (seq-find (lambda (e) (rs/eca--idle-p (cdr e)))
                              (nthcdr (1+ pos) entries))))
           (target (or after (car idle))))
      (if target
          (rs/eca--show-chat (car target) (cdr target))
        (eca-info "No idle ECA chat in any project")))))

;;; --- Whitespace ---------------------------------------------------------
;;
;; Keep `global-whitespace-mode' out of ECA chat buffers.  This lives at the
;; module top level (not inside `use-package!'s `:config') on purpose:
;;
;;   - It must not be able to abort the package's `:config' body.  Previously a
;;     bare `(add-to-list 'whitespace-global-modes 'eca-chat-mode)' ran there
;;     and errored with "listp, t" (the variable still holds its default `t'
;;     when this module loads), which aborted `:config' *before* the `map!'
;;     keybindings -- so RET in the chat silently did nothing.
;;   - It must run *after* the user's top-level `config.el' (which sets
;;     `whitespace-global-modes' to a `(not ...)' list) has loaded, else that
;;     `setq' would clobber our entry.  This file is evaluated during init, so
;;     registering on `after-init-hook' (which fires once init, including
;;     `config.el', finishes) is reliable -- unlike a deferred `:config', which
;;     can run *after* `after-init-hook' has already fired.
(add-hook! 'after-init-hook
  (defun rs/eca--exclude-from-whitespace-h ()
    "Keep whitespace highlighting out of `eca-chat-mode' buffers."
    (cond
     ((eq whitespace-global-modes t)
      (setq whitespace-global-modes '(not eca-chat-mode)))
     ((eq (car-safe whitespace-global-modes) 'not)
      (cl-pushnew 'eca-chat-mode (cdr whitespace-global-modes)))
     ((listp whitespace-global-modes)
      (setq whitespace-global-modes
            (delq 'eca-chat-mode whitespace-global-modes))))))

;;; --- Colorize the workspace pop-up strip by ECA status ------------------
;;
;; Doom's workspace strip (the `[1] foo  [2] bar' line shown in the echo area
;; on `+workspace/display' / a workspace switch) is built by
;; `+workspace--tabline'.  We override it so each workspace name is tinted by
;; the state of its ECA chats:
;;   - orange (`warning') when a chat needs attention (approval/question), and
;;   - green  (`success') when a chat is busy working (loading).
;; Attention wins over working.  Because the strip is rebuilt on every display,
;; status is simply recomputed on demand -- no caching or refresh hooks needed.
;;
;; We deliberately reuse the stock `warning'/`success' faces (set inline in
;; `rs/eca--workspace-tabline') rather than declaring our own with `defface':
;;   - `defface' is idempotent -- once a face exists, re-evaluating its form on
;;     `doom/reload' (SPC h r r) does NOT update it, so face tweaks would only
;;     take effect after a full Emacs restart.  Inlining keeps everything in the
;;     `defun' (which reloads cleanly), and grabs only a foreground from
;;     `warning'/`success' -- no background -- so the selection highlight still
;;     wins the background on a selected+active tab.

(defun rs/eca--session-workspace-name (session)
  "Return the doom workspace name SESSION maps to, or nil.
Prefer a workspace named after the session's project; fall back to a
workspace that contains one of the session's chat buffers."
  (when (and (bound-and-true-p persp-mode)
             (fboundp '+workspace-list-names))
    (let ((names (+workspace-list-names)))
      (or (and (fboundp 'eca--session-project-name)
               (car (member (eca--session-project-name session) names)))
          (and (fboundp '+workspace-buffer-list)
               (seq-find (lambda (name)
                           (seq-some (lambda (buf)
                                       (memq buf (ignore-errors (+workspace-buffer-list name))))
                                     (rs/eca--session-chats session)))
                         names))))))

(defun rs/eca--workspace-status (name)
  "Return aggregated ECA status for workspace NAME.
Either `attention' (a chat awaits you), `working' (a chat is loading),
or nil.  `attention' takes precedence over `working'."
  (let (working)
    (catch 'done
      (dolist (session (rs/eca--sessions-ordered))
        (when (equal name (rs/eca--session-workspace-name session))
          (dolist (buf (rs/eca--session-chats session))
            (when (buffer-live-p buf)
              (cond
               ((rs/eca--needs-attention-p buf) (throw 'done 'attention))
               ((buffer-local-value 'eca-chat--chat-loading buf) (setq working t)))))))
      (and working 'working))))

(defun rs/eca--workspace-tabline (&optional names)
  "Like `+workspace--tabline', but tint each workspace by ECA status.
Drop-in `:override' for Doom's `+workspace--tabline' that keeps the
selected-workspace highlight while layering an ECA status color on top."
  (let ((names (or names (+workspace-list-names)))
        (current-name (+workspace-current-name)))
    (mapconcat
     #'identity
     (cl-loop for name in names
              for i to (length names)
              collect
              (let* ((status (ignore-errors (rs/eca--workspace-status name)))
                     (selected (equal current-name name))
                     ;; Stock faces, used only for their foreground (no
                     ;; background) so the selection highlight survives below.
                     (status-face (pcase status
                                    ('attention 'warning)
                                    ('working 'success)))
                     (base-face (if selected
                                    '+workspace-tab-selected-face
                                  '+workspace-tab-face))
                     ;; Only the `[N]' index is tinted by ECA status; the name
                     ;; keeps the regular face.  When selected, layer the status
                     ;; color over the selection highlight (background).
                     (num-face (cond
                                ((and status-face selected)
                                 `(:inherit (,status-face +workspace-tab-selected-face)))
                                (status-face status-face)
                                (t base-face))))
                (concat (propertize (format " [%d] " (1+ i)) 'face num-face)
                        (propertize (format "%s " name) 'face base-face))))
     " ")))

(advice-add '+workspace--tabline :override #'rs/eca--workspace-tabline)

;;; --- Stop a workspace's ECA server when its tab is closed ---------------
;;
;; `eca-stop' acts on `(eca-session)', which resolves from the current buffer
;; via the buffer-local `eca--session-id-cache'.  Seed that cache in a throwaway
;; buffer to aim `eca-stop' at the session belonging to the closing workspace.

(defun rs/eca--stop-session-for-workspace-h (persp &rest _)
  "Stop the ECA session bound to the workspace PERSP being killed."
  (when (and persp (fboundp 'persp-name))
    (let ((name (persp-name persp)))
      (dolist (session (rs/eca--sessions-ordered))
        (when (equal name (rs/eca--session-workspace-name session))
          (condition-case err
              (progn
                (require 'eca)
                (with-temp-buffer
                  (setq-local eca--session-id-cache (eca--session-id session))
                  (eca-stop)))
            (error
             (message "rs/eca: failed to stop ECA server for %s: %S" name err))))))))

(add-hook 'persp-before-kill-functions #'rs/eca--stop-session-for-workspace-h)

;;; --- Inline difftastic rendering for file-change (edit_file) diffs ------
;;
;; ECA hands the client a plain *unified diff* string for every `fileChange'
;; tool call and renders it inline as a ```diff code fence, colored only by
;; markdown's `diff' fontification.  We swap that fence for a difftastic
;; ("difft") rendering, inline, in the tool-call block itself -- no popup.
;;
;; How it works: `eca-diff-parse-unified-diff' already reconstructs the diff's
;; old/new sides (the same data the ediff/smerge popups use).  We write those
;; two sides to temp files (named after the real file so difft picks the right
;; language) and run `difft' synchronously, then turn its ANSI output into
;; propertized text via `difftastic--ansi-color-apply'.  The result is tagged
;; with `eca-no-fontify' so ECA's custom `font-lock-fontify-region-function'
;; (`eca-chat--fontify-region') skips it and markdown can't clobber difft's
;; colors.
;;
;; This is a local, additive `:around' advice (we never patch upstream): it
;; keeps whatever header (the clickable file-path button) the original built and
;; replaces only the code fence, and it falls back to the stock ```diff block
;; whenever difft is missing or rendering fails.
;;
;; Caveats:
;;   - ECA's parser keeps only the diff's *hunk* lines, so -- exactly like the
;;     existing ediff/smerge views -- difft sees only the changed hunks, not the
;;     whole file (no surrounding context).
;;   - `difft' is run synchronously; results are memoized per (path . diff) in
;;     `rs/eca--difftastic-cache' so re-renders during streaming don't re-shell.

(defvar rs/eca-difftastic-display "side-by-side"
  "Value passed to difft's `--display' when rendering inline ECA diffs.
One of \"inline\", \"side-by-side\" or \"side-by-side-show-both\".")

(defvar rs/eca--difftastic-cache (make-hash-table :test 'equal)
  "Memoizes rendered difftastic strings keyed by PATH + unified-diff text.")

(defun rs/eca--difftastic-render-now (path diff)
  "Render unified DIFF for PATH with difft; return a propertized string or nil.
Returns nil (so callers fall back to the stock ```diff fence) when difft is
unavailable or anything goes wrong."
  (when (and (require 'difftastic nil t)
             (boundp 'difftastic-executable)
             (executable-find difftastic-executable))
    (ignore-errors
      (let* ((parsed (eca-diff-parse-unified-diff diff))
             (orig   (or (plist-get parsed :original) ""))
             (new    (or (plist-get parsed :new) ""))
             ;; Name the temp files after the real file so difft infers the
             ;; language (and shows a sensible heading) from the extension.
             (base   (file-name-nondirectory (or path "file")))
             (dir-a  (make-temp-file "eca-difft-a-" t))
             (dir-b  (make-temp-file "eca-difft-b-" t))
             (file-a (expand-file-name base dir-a))
             (file-b (expand-file-name base dir-b))
             (win    (get-buffer-window (current-buffer) t))
             (width  (max 40 (- (if win (window-body-width win) 80) 2))))
        (unwind-protect
            (progn
              (with-temp-file file-a (insert orig))
              (with-temp-file file-b (insert new))
              (let* ((args (difftastic--add-standard-args
                            (list "--display" rs/eca-difftastic-display)
                            width))
                     (raw (with-temp-buffer
                            (let ((process-environment
                                   (difftastic--build-process-environment)))
                              ;; difft exits non-zero when files differ; ignore
                              ;; the status and just take its output.
                              (apply #'process-file difftastic-executable nil t nil
                                     (append args (list file-a file-b))))
                            (buffer-string)))
                     (rendered (string-trim-right
                                (difftastic--ansi-color-apply raw))))
                (unless (string-empty-p rendered)
                  (propertize rendered 'eca-no-fontify t))))
          (delete-directory dir-a t)
          (delete-directory dir-b t))))))

(defun rs/eca--difftastic-render (path diff)
  "Memoized `rs/eca--difftastic-render-now' for PATH and unified DIFF."
  (when (and (stringp diff) (not (string-empty-p diff)))
    (let ((key (concat (or path "") "\0" diff)))
      (or (gethash key rs/eca--difftastic-cache)
          (when-let ((s (rs/eca--difftastic-render-now path diff)))
            (puthash key s rs/eca--difftastic-cache))))))

(defun rs/eca--file-change-diff-difftastic-a (orig-fn path diff roots)
  "Render the ECA file-change DIFF inline with difftastic.
`:around' advice for `eca-chat--file-change-diff': keeps the header (file-path
button) the original produced and replaces only its ```diff fence with a
difftastic rendering.  Falls back to ORIG-FN's output verbatim when difft is
unavailable or rendering fails."
  (let ((orig (funcall orig-fn path diff roots)))
    (or (when-let* ((rendered (rs/eca--difftastic-render path diff))
                    (idx (string-match "```diff\n" orig)))
          ;; `substring' preserves the header's text properties (the clickable
          ;; file-path button); append difft's rendering in place of the fence.
          (concat (substring orig 0 idx) rendered))
        orig)))

;;; --- Let this module own the chat keys ----------------------------------

(when (boundp '+evil-collection-disabled-list)
  (add-to-list '+evil-collection-disabled-list 'eca))

;;; --- Package ------------------------------------------------------------

(use-package! eca
  :config
  (setq eca-buttons-allow-mouse t)

  ;; Render file-change (edit_file) diffs inline with difftastic (see the
  ;; "Inline difftastic rendering" section above).  Additive `:around' advice;
  ;; falls back to the stock ```diff fence when difft is unavailable.
  (advice-add 'eca-chat--file-change-diff
              :around #'rs/eca--file-change-diff-difftastic-a)

  (add-hook! 'eca-chat-mode-hook
    (defun rs/eca--mark-as-real-buffer-h ()
      "Tell Doom that ECA chat buffers are real buffers."
      (setq-local doom-real-buffer-p t)))

  (add-hook! 'eca-chat-mode-hook
    (defun rs/eca--disable-evil-markdown-h ()
      "Disable `evil-markdown-mode' so its keymap doesn't shadow chat keys."
      (when (bound-and-true-p evil-markdown-mode)
        (evil-markdown-mode -1))))

  (map!
   (:map doom-leader-map
    :leader
    :desc "Eca" :prefix "e"
    :desc "Start session" :n "e" #'eca
    :desc "Start session" :n "r" #'eca-restart
    :desc "Rewrite" :n "w" #'eca-rewrite
    :desc "Talk" :n "t" #'eca-chat-talk
    :desc "Jump to attention (project)" :n "j" #'rs/eca-jump-to-attention-in-project
    :desc "Jump to attention (all projects)" :n "J" #'rs/eca-jump-to-attention
    :desc "Jump to idle chat (project)" :n "k" #'rs/eca-jump-to-idle-in-project
    :desc "Jump to idle chat (all projects)" :n "K" #'rs/eca-jump-to-idle)
   :after eca-chat
   :map eca-chat-mode-map
   :desc "Toggle section" :n "<tab>" #'eca-chat--key-pressed-tab
   :desc "Toggle section" :n "TAB" #'eca-chat--key-pressed-tab
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
