;; -*- no-byte-compile: t; -*-
;;; rs/magit/packages.el

;; Core magit comes from Doom's `:tools (magit +forge)' module; this module
;; layers on the extras we use around it.

(package! code-review)
(package! git-link)
(package! magit-todos
  :recipe (:host github :repo "rschmukler/magit-todos" :branch "rs/enhancements"))
(package! forge
  :recipe (:host github :repo "magit/forge"))
(package! difftastic
  :recipe (:host github :repo "pkryger/difftastic.el" :files ("*.el")))
(package! magit-difftastic
  ;; :recipe (:host github :repo "rschmukler/magit-difftastic"))
  :recipe (:local-repo "/Users/ryan/dev/rschmukler/magit-difftastic" :type nil)
  )
