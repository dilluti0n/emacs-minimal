(setq inhibit-startup-screen t
      vc-follow-symlinks t
      backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
      default-input-method "korean-hangul"
      ring-bell-function 'ignore)

(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 1)
(ido-mode 1)
(column-number-mode t)

;; font
(set-frame-font "Monaco-14" nil t)
(setq tab-width 8)

;; default hooks
(add-hook 'prog-mode-hook
	  (lambda () (progn (display-line-numbers-mode t)
			    (local-set-key (kbd "C-x c c") 'compile))))

;; tree-sitter
(setq major-mode-remap-alist '((c-mode . c-ts-mode)
			       (c++-mode . c++-ts-mode)
			       (c-or-c++-mode . c-or-c++-ts-mode)
			       (shell-script-mode . bash-ts-mode)
			       (python-mode . python-ts-mode)))

;; cc-mode c-ts-mode c++-ts-mode
(setq c-basic-offset tab-width
      c-default-sytle '((awk-mode . "awk")
			(other . "linux"))
      c-ts-mode-indent-style "linux"
      c-ts-mode-indent-offset c-basic-offset)

(add-hook 'c-mode-hook
	  (lambda () (subword-mode 1)))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(require 'magit)
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(setq company-minimum-prefix-length 2
      company-idle-delay (lambda () (if (company-in-string-or-comment) nil 0)))

(require 'eglot)
(add-hook 'cc-mode-hook 'eglot-ensure)
(add-hook 'c-ts-mode-hook 'eglot-ensure)
(add-hook 'c++-ts-mode-hook 'eglot-ensure)
(add-hook 'python-base-mode-hook 'eglot-ensure)
(add-to-list 'eglot-server-programs
	     '((c-ts-mode c++-ts-mode cc-mode)
	       . ("clangd"
		  "-j=4"
		  "--log=error"
		  "--malloc-trim"
		  "--background-index"
		  "--clang-tidy"
		  "--compiletion=style=detailed"
		  "--pch-storage=memory"
		  "--header-insertion=never")))

(require 'eat)
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; custom.el
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file) (load custom-file))
