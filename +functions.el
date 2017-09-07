;;; private/rschmukler/functions.el -*- lexical-binding: t; -*-

(defun rschmukler/neotree-project-root-dir-or-current-dir ()
  "Open NeoTree using the project root, using projectile, or the
current buffer directory."
  (interactive)
  (let ((project-dir (ignore-errors (projectile-project-root)))
        (file-name (buffer-file-name))
        (neo-smart-open t))
    (if (neo-global--window-exists-p)
        (neotree-hide)
      (progn
        (neotree-show)
        (if project-dir
            (neotree-dir project-dir))
        (if file-name
            (neotree-find file-name))))))

(defun wc/switch-to-mru-buffer ()
  "Switches to the most recently used buffer, including visible buffers."
  (interactive)
  (setq current-buffer-name (buffer-name (current-buffer)))
  (setq buffer-candidates (remove-if #'(lambda (buffer) (string-match-p current-buffer-name (buffer-name buffer))) (buffer-list)))
  (wc/do-switch-to-mru-buffer buffer-candidates))


(defun wc/do-switch-to-mru-buffer (buffer-candidates)
  (setq buffer-candidate (car buffer-candidates))
  (setq rest (cdr buffer-candidates))
  (if (string-match-p current-buffer-name (buffer-name buffer-candidate))
      (wc/do-switch-to--buffer rest)
    (if (eq 0 (list-length buffer-candidates))
        (message "No more buffer candidates.")
      (if (wc/file-buffer-p buffer-candidate)
          (switch-to-buffer buffer-candidate)
        (wc/do-switch-to-mru-buffer rest)))))


(defun wc/file-buffer-p (buffer-candidate)
  "Returns t if the buffer argument is backed by a file and is therefore presumably a code buffer."
  (interactive)
  (let ((buff-name (buffer-name buffer-candidate))
        (buff-mode (wc/buffer-major-mode buffer-candidate)))
    (not (or (string-match-p "*" buff-name)
             (member buff-mode '(neotree-mode dired-mode))))))

(defun wc/buffer-major-mode (buffer-handle)
  "Returns a buffer's active major-mode."
  (with-current-buffer buffer-handle major-mode))
