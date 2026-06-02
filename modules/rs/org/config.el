;;; rs/org/config.el -*- lexical-binding: t; -*-
;;;
;;; Self-contained org module: org/org-roam settings, vulpea database +
;;; agenda integration, and org keybindings.

;;
;;; org

(after! org
  (setq org-agenda-files (file-expand-wildcards "~/docs/org/*.org"))
  (setq org-cycle-separator-lines 1)
  (setq org-archive-location "::* Archived Tasks")
  (setq org-agenda-dim-blocked-tasks 't)
  (defvar +org-dir (expand-file-name "~/docs/org"))
  (setq org-todo-keywords
        '((sequence "[ ](t)" "[.](s)" "[!](h)" "[?](q)" "[@](b)" "|" "[x](d)" "[#](k)")))
  (advice-add 'org-agenda-quit :after #'org-save-all-org-buffers)
  (setq org-todo-keyword-faces
        '(("[.]" . +org-todo-active)
          ("[?]" . +org-todo-onhold)
          ("[@]" . +org-todo-onhold)
          ("[!]" . +org-todo-onhold)
          ("[#]" . +org-todo-cancel))))

(after! org-roam
  (setq org-roam-directory (expand-file-name (concat org-directory "/" "roam"))))

(after! org-babel
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append))

;;
;;; vulpea

(use-package! vulpea
  :after org
  :init
  ;; vulpea v2 maintains its own database, independent of org-roam.
  (setq vulpea-db-location
        (expand-file-name "vulpea.db" doom-cache-dir))
  ;; Index the same notes org-roam used to track.
  (setq vulpea-db-sync-directories
        (list (expand-file-name "roam" org-directory)))
  ;; Use org-roam's alias property so existing :ROAM_ALIASES: keep working.
  (setq vulpea-buffer-alias-property "ROAM_ALIASES")
  ;; Performance / behavior tuning.
  (setq vulpea-db-parse-method 'single-temp-buffer)
  (setq vulpea-db-sync-external-method 'auto)
  ;; Build/refresh the DB asynchronously when autosync is enabled so
  ;; startup stays fast. The DB persists between sessions on disk.
  (setq vulpea-db-sync-scan-on-enable 'async)
  :config
  ;; Start file-watcher based auto-sync (replaces the old
  ;; `vulpea-db-autosync-enable' / org-roam-db-autosync coupling).
  (vulpea-db-autosync-mode +1)
  (defun vulpea-agenda-category (&optional len)
    "Get category of item at point for agenda.

Category is defined by one of the following items:

- CATEGORY property
- TITLE keyword
- TITLE property
- filename without directory and extension

When LEN is a number, resulting string is padded right with
spaces and then truncated with ... on the right if result is
longer than LEN.

Usage example:

  (setq org-agenda-prefix-format
        '((agenda . \" %(vulpea-agenda-category) %?-12t %12s\")))

Refer to `org-agenda-prefix-format' for more information."
    (let* ((file-name (when buffer-file-name
                        (file-name-sans-extension
                         (file-name-nondirectory buffer-file-name))))
           (title (vulpea-buffer-prop-get "title"))
           (category (org-get-category))
           (result
            (or (if (and
                     title
                     (string-equal category file-name))
                    title
                  category)
                "")))
      (if (numberp len)
          (s-truncate len (s-pad-right len " " result))
        result)))
  (setq org-agenda-prefix-format
        '((agenda . " %i %(vulpea-agenda-category 20)%?-12t% s")
          (todo . " %i %(vulpea-agenda-category 20) ")
          (tags . " %i %(vulpea-agenda-category 20) ")
          (search . " %i %(vulpea-agenda-category 20) "))))

(defun org/agenda-files ()
  "Accessor function to get relevant files from the vulpea database."
  (seq-uniq (delq nil (mapcar #'vulpea-note-path (vulpea-db-query)))))

(defun org/set-agenda-files (&rest _)
  "Set agendas files to a relevant set of roam files."
  (setq org-agenda-files (org/agenda-files)))

(advice-add 'org-agenda :before #'org/set-agenda-files)

;;
;;; Keybindings

(map!
 (:leader
   (:desc "org" :prefix "o"
     :desc "Org Agenda"          :n  "a" #'org-agenda
     :desc "Org Capture"         :n  "c" #'org-roam-capture
     :desc "Find node"           :n  "o" #'vulpea-find
     :desc "Show todos"          :n  "t" #'org-todo-list))

 (:after org-agenda
  :map org-agenda-mode-map
  :desc "Open Link" :n "C-<return>" #'org-agenda-open-link)

 (:after org-mode
   (:map org-mode-map
     :localleader
     :n  "t"  #'org-todo
     :n  "d"  #'org-deadline
     :n  "s"  #'org-schedule)))

(map!
 (:after org-mode
   (:map org-mode-map
     :i [remap doom/inflate-space-maybe] #'org-self-insert-command)))
