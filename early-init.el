(setenv "PATH" (concat (getenv "PATH")
		       (concat ":" (concat (getenv "HOME") "/.local/bin"))))
(when (eq system-type 'darwin)
  (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
  (setq exec-path (append exec-path '("/usr/local/bin"))))

(defun load-if-exists (file-name)
  "load file if exists"
  (when (file-exists-p file-name) (load file-name)))

;; sensitive keys
(load-if-exists (expand-file-name "env.el" user-emacs-directory))
