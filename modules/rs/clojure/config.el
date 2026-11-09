;;; rs/clojure/config.el -*- lexical-binding: t; -*-
;;;
;;; Self-contained Clojure/CIDER module: packages (see packages.el),
;;; configuration, helper commands, and keybindings all live here.

(require 'seq)

;;
;;; Helper commands

(defun rs/cider-cycle-buffer-type ()
  "Cycles between clojure, clojurescript, and clojurec repl types"
  (interactive)
  (if (string= "cljc" (file-name-extension (buffer-file-name)))
      (let ((repl-open? (get-buffer-window (cider-current-repl-buffer)))
            (window (selected-window)))
        (setq clojure-verify-major-mode nil)
        (cond
         ((memq major-mode '(clojurec-mode clojure-ts-clojurec-mode)) (clojure-ts-mode))
         ((memq major-mode '(clojure-mode clojure-ts-mode)) (clojure-ts-clojurescript-mode))
         ((memq major-mode '(clojurescript-mode clojure-ts-clojurescript-mode)) (clojure-ts-clojurec-mode)))
        (setq-local flycheck-clj-kondo-lang "cljc")
        (when (and repl-open? (not (memq major-mode '(clojurec-mode clojure-ts-clojurec-mode))))
          (cider-switch-to-repl-buffer)
          (select-window window)))
    (message (concat "Cycle repl type called from non .cljc file" (buffer-file-name)))))

(defun rs/cider-cycle-repl (&optional force-open?)
  "Cycles between visible cider repls. No op if we aren't open, unless force-open? is truthy."
  (interactive)
  (let ((open-repl-buffer (car (seq-filter 'get-buffer-window (cider-repls))))
        (window (selected-window)))
    (cond
     (open-repl-buffer
      (dolist (buff (cider-repls))
        (when (not (eq open-repl-buffer buff))
          (cider--switch-to-repl-buffer buff)
          (select-window window))))
     (force-open? (cider-switch-to-repl-buffer)))))

(defun rs/cider-clear-all-buffers ()
  "Clear all cider buffers"
  (interactive)
  (let ((inhibit-read-only 't))
    (dolist (repl (cider-repls))
      (with-current-buffer repl
        (cider-repl--clear-region (point-min) cider-repl-prompt-start-mark)
        (cider-repl--clear-region cider-repl-output-start cider-repl-output-end)
        (when (< (point) cider-repl-input-start-mark)
          (goto-char cider-repl-input-start-mark))))))

(defvar rs-cider-debug-sexp
  nil
  "The sexp currently used for debugging")

(defvar rs-cider-debug-sexp-b-local
  nil
  "The sexp used locally in the buffer for debugging")

(defvar rs-cider-debug-use-global
  't
  "Whether to use a global (or buffer local) variable for debug the sexp")

(defun rs/cider-debug/set-sexp ()
  "Sets the last sexp under the cursor to to the debug sexp"
  (interactive)
  (let ((last-sexp (evil-collection-cider-last-sexp #'cider-last-sexp)))
    (if rs-cider-debug-use-global
      (setq rs-cider-debug-sexp last-sexp)
      (setq-local rs-cider-debug-sexp-b-local last-sexp))
    (message (format "Debug sexp set to: %s" last-sexp))))

(defun rs/cider-debug/eval-sexp (&optional output-to-current-buffer)
  "Evaluates the debug function"
  (interactive "P")
  (let ((sexp (if rs-cider-debug-use-global
                rs-cider-debug-sexp
                rs-cider-debug-sexp-b-local)))
    (cider-interactive-eval
     sexp
     (when output-to-current-buffer (cider-eval-print-handler))
     (cider-last-sexp 'bounds)
     (cider--nrepl-pr-request-map))))

(defun rs/cider-debug/toggle-use-global ()
  "Toggles whether we use a global variable for storing the debug expression"
  (setq rs-cider-debug-use-global (not rs-cider-debug-use-global))
  (message (format "Global debug is %s." (if rs-cider-debug-use-global "enabled" "disabled"))))

(defun rs/cider/eval-last-sexp-up-to-point (&optional output-to-current-buffer)
  "Similar to cider eval sexp up to point but doesn't require you to be on trailing white space"
  (interactive "P")
  (let* ((cursor-text (buffer-substring-no-properties (point) (1+ (point))))
         (end-delim? (member cursor-text '("}" ")" "]")))
         (beg-of-sexp (save-excursion (when end-delim? (backward-up-list)) (up-list) (backward-list) (point)))
         (beg-delimiter (save-excursion (when end-delim? (backward-up-list)) (up-list) (backward-list) (char-after)))
         (beg-set? (save-excursion (when end-delim? (backward-up-list)) (up-list) (backward-list) (char-before)))
         (code (buffer-substring-no-properties beg-of-sexp (1+ (point))))
         (code (if (= beg-set? ?#) (concat (list beg-set?) code) code))
         (code (concat code (list (cider--matching-delimiter beg-delimiter)))))
    (cider-interactive-eval code
                            (when output-to-current-buffer
                              (cider-eval-print-handler))
                            (list beg-of-sexp (1+ (point)))
                            (cider--nrepl-pr-request-map))))

;;
;;; Packages

(use-package! ivy-cider
  :after cider-mode)

(use-package! flycheck-clj-kondo
  :after clojure-mode
  :config
  ;; (dolist (checkers '((clj-kondo-clj . clojure-joker)
  ;;                   (clj-kondo-cljs . clojurescript-joker)
  ;;                   (clj-kondo-cljc . clojure-joker)
  ;;                   (clj-kondo-edn . edn-joker)))
  ;; (flycheck-add-next-checker (car checkers) (cons 'error (cdr checkers)))))
  )

;;
;;; clojure-mode (kept for CIDER/clj-refactor compatibility)

(after! clojure-mode
  (setq cider-default-cljs-repl 'shadow)
  (setq cider-cljs-lein-repl
        "(do (require 'figwheel-sidecar.repl-api)
         (figwheel-sidecar.repl-api/start-figwheel!)
         (figwheel-sidecar.repl-api/cljs-repl))")
  (after! projectile
    (projectile-register-project-type
     'clojure-cli
     '("deps.edn")
     :project-file "deps.edn"
     :src-dir "src/"
     :test-dir "test/"
     :test-suffix "_test"))
  (setq cljr-magic-require-namespaces
        '(("io" . "clojure.java.io")
          ("sh" . "clojure.java.shell")
          ("jdbc" . "clojure.java.jdbc")
          ("set" . "clojure.set")
          ("time" . "java-time")
          ("str" . "cuerdas.core")
          ("path" . "pathetic.core")
          ("walk" . "clojure.walk")
          ("zip" . "clojure.zip")
          ("async" . "clojure.core.async")
          ("component" . "com.stuartsierra.component")
          ("http" . "hato.client")
          ("url" . "cemerick.url")
          ("sql" . "honey.sql")
          ("csv" . "clojure.data.csv")
          ("json" . "jsonista.core")
          ("s" . "manifold.stream")
          ("d" . "manifold.deferred")
          ("fs" . "me.raynes.fs")
          ("ig" . "integrant.core")
          ("cp" . "com.climate.claypoole")
          ("re-frame" . "re-frame.core")
          ("rf" . "re-frame.core")
          ("rf.db" . "re-frame.db")
          ("re" . "reagent.core")
          ("reagent" . "reagent.core")
          ("w" . "wing.core")
          ("gen" . "clojure.spec.gen.alpha")
          ("m" . "malli.core")
          ("mg" . "malli.generator")
          ("mt" . "malli.transform")
          ("t" . "tick.alpha.api"))))

;;
;;; Helper functions (REPL / system management)

(defun rs/ig/restart ()
  "Calls Integrant halt followed by integrant go"
  (interactive)
  (cider-interactive-eval "(do (integrant.repl/halt) (integrant.repl/go))"))

(defun rs/systemic/restart ()
  "Calls systemic restart"
  (interactive)
  (cider-interactive-eval "(systemic.core/restart!)"))

(defun rs/systemic/start ()
  "Calls systemic start"
  (interactive)
  (cider-interactive-eval "(systemic.core/start!)"))

(defun rs/systemic/stop ()
  "Calls systemic stop"
  (interactive)
  (cider-interactive-eval "(systemic.core/stop!)"))

(defun rs/systemic/start-last-sexp ()
  "Starts the systemic system at the last sexp"
  (interactive)
  (cider-interactive-eval (concat "(systemic.core/start! `" (cider-sexp-at-point) ")")))

(defun rs/systemic/stop-last-sexp ()
  "Stops the systemic system at the last sexp"
  (interactive)
  (cider-interactive-eval (concat "(systemic.core/stop! `" (cider-sexp-at-point) ")")))

(defun rs/systemic/restart-last-sexp ()
  "Restarts the systemic system at the last sexp"
  (interactive)
  (cider-interactive-eval (concat "(systemic.core/restart! `" (cider-sexp-at-point) ")")))

(defvar rs/wing/alias nil)
(defvar rs/malli/wrap-cider-enabled nil)

(defun rs/wing/sync-libs ()
  "Calls wing sync-libs"
  (interactive)
  (if rs/wing/alias
      (cider-interactive-eval (format "(wing.repl/sync-libs! :%s)" rs/wing/alias))
    (cider-interactive-eval "(wing.repl/sync-libs!)")))

(defun rs/malli/try-start-dev ()
  (interactive)
  (cider-interactive-eval
   "(let [last *1]
      (try
        (with-out-str
          (let [start! (requiring-resolve 'malli.dev/start!)
                fail!  (requiring-resolve 'malli.core/-fail!)]
            (start! {:report fail!})))
        last
        (catch java.io.FileNotFoundException _
          last)
        (catch Exception e
          (throw e))))"))

(defun rs/malli/wrap-cider (origin-fn &rest args)
  (let ((res (apply origin-fn args)))
    (when (and (eq (cider-repl-type-for-buffer) 'clj) rs/malli/wrap-cider-enabled)
      (rs/malli/try-start-dev))
    res))

(defun rs/portal/clear ()
  (interactive)
  (with-current-buffer (first (cider-repl-buffers 'clj 't))
    (cider-nrepl-sync-request:eval "(portal.api/clear)")))

;;
;;; clojure-ts-mode

;; Remap clojure-mode variants to their clojure-ts-mode counterparts.
;; We use `major-mode-remap-alist' rather than `auto-mode-alist' because
;; Doom's built-in `:lang clojure' module explicitly strips clojure-ts-mode
;; entries from `auto-mode-alist' (to guard against missing grammars) and
;; relies on this remap mechanism instead.
(add-to-list 'major-mode-remap-alist '(clojure-mode . clojure-ts-mode))
(add-to-list 'major-mode-remap-alist '(clojurescript-mode . clojure-ts-clojurescript-mode))
(add-to-list 'major-mode-remap-alist '(clojurec-mode . clojure-ts-clojurec-mode))

(after! clojure-ts-mode
  ;; Indentation rules - translated from the legacy define-clojure-indent block.
  ;; Format: ("symbol" (rule-type n) ...) using :block and :inner per cljfmt.
  ;;
  ;; :block N  => N args precede the body (like defn has 1 name arg, then body)
  ;; :inner N  => body forms at depth N are indented by 2 spaces (defn-like)
  ;;
  ;; NOTE: `letfn` '(0 ((:defn)) :form) cannot be fully expressed in
  ;; clojure-ts-mode's two-rule system — the nested :defn backtracking for
  ;; the binding vector fns is handled by clojure-ts-mode's built-in letfn
  ;; rule already. The outer (:block 0) (:inner 1) approximation is close.
  ;;
  ;; NOTE: `defc` '(1 :defn (:defn)) similarly can't express the third
  ;; positional rule; (:block 1) (:inner 0) covers the common case.
  (setopt clojure-ts-semantic-indent-rules
          '(("PUT"            (:block 2))
            ("POST"           (:block 2))
            ("GET"            (:block 2))
            ("PATCH"          (:block 2))
            ("DELETE"         (:block 2))
            ("context"        (:block 2))
            ("for-all"        (:block 2))
            ("checking"       (:block 3))
            (">defn"          (:inner 0))
            (">defn-"         (:inner 0))
            ("defnc"          (:inner 0))
            ("defnc-"         (:inner 0))
            ("defresolver"    (:inner 0))
            ("fnc"            (:inner 0))
            ("match"          (:block 1))
            ("cond"           (:block 0))
            ("case"           (:block 1))
            ("describe"       (:block 1))
            ("it"             (:block 2))
            ("letfn"          (:block 0) (:inner 1))  ;; see note above
            ("fn-traced"      (:inner 0))
            ("defn-traced"    (:inner 0))
            ("assert-match"   (:block 1))
            ("assert-exception" (:block 1))
            ("at-media"       (:block 1))
            ("fiber-loop"     (:block 1))
            ("agent-loop"     (:block 1))
            ("js-await"       (:block 1))
            ("fnk"            (:block 1))
            ("fw"             (:block 1))
            ("defc"           (:block 1) (:inner 0))   ;; see note above
            ("defsys"         (:inner 0))))

  ;; Alignment
  (add-to-list 'clojure-ts-align-binding-forms "let-flow")
  (setopt clojure-ts-align-forms-automatically t)

  ;; Highlight these defn-like forms with their name + docstring.
  ;; Must use setopt (not add-to-list) so the :set function fires and
  ;; propagates the new font-lock queries to all open clojure-ts-mode buffers.
  (setopt clojure-ts-extra-def-forms
          '(">defn" ">defn-" "defsys" "defevent-db" "defevent-fx"
            "defsub" "defhandler" "defstream" "defn-traced"
            "defui" "defui-" "defnc" "defnc-" "defresolver" "defc")))

;;
;;; cider

(after! cider
  (add-hook 'company-completion-started-hook 'ans/set-company-maps)
  (add-hook 'company-completion-finished-hook 'ans/unset-company-maps)
  (add-hook 'company-completion-cancelled-hook 'ans/unset-company-maps)
  (setq cider-show-error-buffer nil)
  (set-popup-rule!
    '(("^\\*cider-error" :select t :size 0.5)))
  (advice-add 'cider-eval-buffer :around #'rs/malli/wrap-cider)
  (advice-add 'cider-eval-last-sexp :around #'rs/malli/wrap-cider)
  (advice-add 'cider-eval-defun-at-point :around #'rs/malli/wrap-cider)
  (advice-add 'cider-eval-sexp-at-point :around #'rs/malli/wrap-cider)
  ; (advice-remove 'cider-eval-buffer #'rs/malli/wrap-cider)
  ; (advice-remove 'cider-eval-last-sexp #'rs/malli/wrap-cider)
  ; (advice-remove 'cider-eval-defun-at-point #'rs/malli/wrap-cider)
  ; (advice-remove 'cider-eval-sexp-at-point #'rs/malli/wrap-cider)


  ;; Overwrite this cider function for custom focus modes
  (defun cider--anchored-search-suppressed-forms (limit)
  "Matcher for finding unused reader conditional expressions.
An unused reader conditional expression is an expression for a platform
that does not match the CIDER connection for the buffer.  Search is done
with the given LIMIT."
  (let ((repl-types (cond
                     ((memq major-mode '(clojure-mode clojure-ts-mode))
                      '("clj"))
                     ((memq major-mode '(clojurescript-mode clojure-ts-clojurescript-mode))
                      '("cljs"))
                     ('t
                      (seq-uniq (seq-map
                                 (lambda (repl)
                                   (symbol-name (cider-repl-type repl)))
                                 (cider-repls))))))
        (result 'retry))
    (while (and (eq result 'retry) (<= (point) limit))
      (condition-case condition
          (setq result
                (cider--anchored-search-suppressed-forms-internal
                 repl-types limit))
        (invalid-read-syntax
         (setq result 'retry))
        (wrong-type-argument
         (setq result 'retry))
        (scan-error
         (setq result 'retry))
        (end-of-file
         (setq result nil))
        (error
         (setq result nil)
         (message
          "Error during fontification while searching for forms: %S"
          condition))))
    (if (eq result 'retry) (setq result nil))
    result))


  (defun ans/unset-company-maps (&rest unused)
    "Set default mappings (outside of company).
    Arguments (UNUSED) are ignored."
    (general-def
      :states 'insert
      :keymaps 'override
      "C-j" nil
      "C-k" nil
      "C-n" nil
      "C-p" nil
      "RET" nil
      "TAB" nil
      "C-h" nil))

  (defun cider-eval-last-sexp-and-yank ()
    "Evaluate the expression preceding point and yank the result"
    (interactive)
    (cider-eval-last-sexp)
    (cider-kill-last-result))


  (defun ans/set-company-maps (&rest unused)
    "Set maps for when you're inside company completion.
    Arguments (UNUSED) are ignored."
    (general-def
      :states 'insert
      :keymaps 'override
      "C-j" #'company-select-next
      "C-k" #'company-select-previous
      "C-n" #'company-select-next
      "C-p" #'company-select-previous
      "RET" #'company-complete-selection
      "TAB" #'company-complete-common-or-cylce
      "C-h" #'company-show-doc-buffer)))

(after! clj-refactor
  (setq cljr-clojure-test-declaration "[clojure.test :refer [deftest testing is]]"))

;;
;;; Quit a workspace's CIDER sessions when its tab is closed

(defun rs/cider--session-workspace-name (session)
  "Return the doom workspace name SESSION maps to, or nil.
Prefer the workspace named after a REPL's project directory (the naming
convention used by `rs/open-project-workspace'); fall back to a workspace
that actually contains one of the session's REPL buffers."
  (when (and (bound-and-true-p persp-mode)
             (fboundp '+workspace-list-names))
    (let ((repls (seq-filter #'buffer-live-p (cdr session))))
      (or (and (fboundp 'rs/worktree-workspace-name)
               (seq-some (lambda (repl)
                           (when-let* ((dir (buffer-local-value 'nrepl-project-dir repl)))
                             (car (member (rs/worktree-workspace-name dir)
                                          (+workspace-list-names)))))
                         repls))
          (and (fboundp '+workspace-buffer-list)
               (seq-find (lambda (name)
                           (seq-some (lambda (buf)
                                       (memq buf (ignore-errors (+workspace-buffer-list name))))
                                     repls))
                         (+workspace-list-names)))))))

(defun rs/cider--quit-sessions-for-workspace-h (persp &rest _)
  "Quit, after confirmation, CIDER sessions bound to the workspace PERSP."
  (when (and persp (fboundp 'persp-name) (fboundp 'cider-sessions))
    (let* ((name (persp-name persp))
           (sessions (seq-filter (lambda (session)
                                   (equal name (rs/cider--session-workspace-name session)))
                                 (cider-sessions))))
      (when (and sessions
                 (y-or-n-p (format "Quit CIDER session%s %s? "
                                   (if (cdr sessions) "s" "")
                                   (string-join (mapcar #'car sessions) ", "))))
        (dolist (session sessions)
          (condition-case err
              (with-temp-buffer
                (setq-local sesman-system 'CIDER)
                (sesman-quit session))
            (error
             (message "rs/clojure: failed to quit CIDER session %s: %S"
                      (car session) err))))))))

(add-hook 'persp-before-kill-functions #'rs/cider--quit-sessions-for-workspace-h)

;;
;;; Keybindings

(map!
 (:after clojure-ts-mode
   (:map (clojure-ts-mode-map clojure-ts-clojurescript-mode-map clojure-ts-clojurec-mode-map)
     :leader
     :n "\\" #'ivy-cider-apropos
     :n "DEL" #'ivy-cider-browse-ns
     (:desc "reload" :prefix "r"
       :desc "Refresh user libraries" :n "l" #'rs/wing/sync-libs
       :desc "Restart systemic" :n "r" #'rs/systemic/restart
       :desc "Restart system at point" :n "R" #'rs/systemic/restart-last-sexp
       :desc "Start systemic" :n "s" #'rs/systemic/start
       :desc "Stop systemic" :n "S" #'rs/systemic/stop
       :desc "Start system at point" :n "d" #'rs/systemic/start-last-sexp
       :desc "Stop system at point" :n "D" #'rs/systemic/stop-last-sexp
       :desc "Cycle clojure buffer type" :n "t" #'rs/cider-cycle-buffer-type
       :desc "Cycle between cider repl buffers" :n "T" (λ!  (rs/cider-cycle-repl 't)))
     :localleader
     :n "'" #'cider-jack-in
     :n "\"" #'cider-jack-in-clojurescript))
 ;; cider-mode
 (:after cider-mode
   (:map cider-mode-map
     :n "C-x C-y" #'cider-eval-last-sexp-and-yank
     :n "C-x C-t" #'cider-tap-last-sexp
     :n "C-x C-d" #'rs/cider-debug/set-sexp
     :n "C-x C-r" #'rs/cider-debug/eval-sexp
     :n "C-x C-p" #'rs/cider/eval-last-sexp-up-to-point
     :leader
     :desc "Lookup documentation at point" :n "d" #'cider-doc
     :desc "Jump to definition at point" :n "l" #'cider-find-var
     :desc "Show cider errors" :n "@"
     (λ! (with-current-buffer cider-error-buffer (+popup/buffer)))
     :localleader
     :n "b" #'cider-eval-buffer
     :n "B" (λ!  (rs/cider-cycle-repl 't))
     :n "n" #'cider-repl-set-ns
     :n "j" #'cider-find-var
     :n "s" #'cider-browse-spec
     :n "S" #'helm-cider-spec-ns
     :n "l" #'cljr-move-to-let
     :n "L" #'cljr-introduce-let
     (:desc "docs" :prefix "d"
       :desc "Browse Namespace" :n "n" #'cider-browse-ns
       :desc "Browse Spec" :n "s" #'cider-browse-spec
       :desc "Load ClojureDoc" :n "d" #'cider-clojuredocs)
     :n "c" #'rs/cider-clear-all-buffers
     :n "C" #'rs/portal/clear
     :n "i" #'cider-inspect-last-result
     :n "e c" #'cider-eval-last-sexp-in-context
     :n "e R" #'cider-eval-last-sexp-and-replace
     :n "p" #'cider-eval-sexp-at-point
     :n "f" #'cider-eval-defun-at-point
     :n "t" #'cider-test-run-ns-tests
     :n "T" #'cider-test-run-test)
   (:after cider-browse-ns-mode
     (:map cider-browse-ns-mode-map
      :n "RET"       #'cider-browse-ns-operate-at-point))
   (:map cider-repl-mode-map
    :localleader
    :n "B" (λ!  (rs/cider-cycle-repl 't)))))
