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

;;; --- Package ------------------------------------------------------------

(use-package! eca
  :config
  (setq eca-buttons-allow-mouse t)
  (map!
   (:map doom-leader-map
    :leader
    :desc "Eca" :prefix "e"
    :desc "Start session" :n "e" #'eca
    :desc "Start session" :n "r" #'eca-restart
    :desc "Rewrite" :n "w" #'eca-rewrite
    :desc "Talk" :n "t" #'eca-chat-talk
    :desc "Jump to attention (project)" :n "j" #'rs/eca-jump-to-attention-in-project
    :desc "Jump to attention (all projects)" :n "J" #'rs/eca-jump-to-attention)
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
