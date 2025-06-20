(when (eq system-type 'darwin)
  (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
  (setq exec-path (append exec-path '("/usr/local/bin"))))

(defun load-if-exists (file-name)
  "load file if exists"
  (if (file-exists-p file-name) (load file-name)))

(setq gc-cons-threshold (* 32 1024 1024))

;; sensitive keys
(load-if-exists (expand-file-name "env.el" user-emacs-directory))
