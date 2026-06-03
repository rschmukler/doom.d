;; -*- no-byte-compile: t; -*-
;;; rs/eca/packages.el

;; ECA (Editor Code Assistant) - https://github.com/editor-code-assistant/eca-emacs

(package! eca
  ;; :recipe (:host github :repo "editor-code-assistant/eca-emacs" :files ("*.el"))
  :recipe (:local-repo "~/dev/rschmukler/eca-emacs" :files ("*.el")))
