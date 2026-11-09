;; -*- no-byte-compile: t; -*-
;;; rs/eca/packages.el

;; ECA (Editor Code Assistant) - https://github.com/editor-code-assistant/eca-emacs

(package! eca
  ;; :recipe (:host github :repo "editor-code-assistant/eca-emacs" :files ("*.el"))
  :recipe (:local-repo "~/dev/editor-code-assistant/eca-emacs" :files ("*.el")))

;; Used to render `eca-chat' file-change (edit_file) diffs inline with
;; difftastic instead of a plain unified-diff code fence (see config.el).
(package! difftastic)

;; Integrations between magit and eca (e.g. auto-refresh magit buffers when eca
;; creates/modifies a file). Enabled via `magit-eca-mode' in config.el.
(package! magit-eca
  ;; :recipe (:host github :repo "rschmukler/magit-eca" :files ("*.el")))
  :recipe (:local-repo "~/dev/rschmukler/magit-eca" :files ("*.el")))
