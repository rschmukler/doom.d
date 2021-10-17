;;; ../dev/rschmukler/doom.d/+org.el -*- lexical-binding: t; -*-
;;;
(defun org/agenda-files ()
  "Accessor function to get relevant files from org-roam"
  (mapcar #'car
          (org-roam-db-query
           [:select [nodes:file]
            :from nodes])))

(defun org/set-agenda-files (&rest _)
  "Set agendas files to a relevant set of roam files."
  (setq org-agenda-files (org/agenda-files)))

(advice-add 'org-agenda :before #'org/set-agenda-files)
