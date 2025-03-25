(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(setq inhibit-startup-screen t
      vc-follow-symlinks t
      backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
      default-input-method "korean-hangul"
      ring-bell-function 'ignore)

(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(ido-mode 1)
(column-number-mode t)
(recentf-mode 1)
(save-place-mode 1)
(setq history-length 25)
(savehist-mode 1)
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)

;; tab is 8 spaces !!!!
(setq tab-width 8)

;; default hooks
(add-hook 'prog-mode-hook
	  (lambda () (progn (display-line-numbers-mode t)
			    (local-set-key (kbd "C-x c c") 'compile)
			    (local-set-key (kbd "C-x c r") 'recompile))))

;; tree-sitter
(setq major-mode-remap-alist '((c-mode . c-ts-mode)
			       (c++-mode . c++-ts-mode)
			       (c-or-c++-mode . c-or-c++-ts-mode)
			       (shell-script-mode . bash-ts-mode)
			       (python-mode . python-ts-mode)))

;; cc-mode c-ts-mode c++-ts-mode
(indent-tabs-mode t)
(setq c-basic-offset tab-width
      c-default-sytle '((awk-mode . "awk")
			(other . "linux"))
      c-ts-mode-indent-style 'linux
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
(setq company-minimum-prefix-length 1
      company-idle-delay (lambda () (if (company-in-string-or-comment) nil 0)))

(require 'eglot)
(add-hook 'cc-mode-hook 'eglot-ensure)
(add-hook 'c-ts-mode-hook 'eglot-ensure)
(add-hook 'c++-ts-mode-hook 'eglot-ensure)
(add-hook 'python-base-mode-hook 'eglot-ensure)
(define-key eglot-mode-map (kbd "C-c r") 'eglot-rename)
(define-key eglot-mode-map (kbd "C-c o") 'eglot-code-action-organize-imports)
(define-key eglot-mode-map (kbd "C-c h") 'eldoc)
(define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
(define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error)

(require 'vterm)
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(require 'undo-tree)
(global-undo-tree-mode)
(setq undo-tree-history-directory-alist `(("." . ,(concat user-emacs-directory "undo"))))

;; tex-mode
(defun tex-render ()
  (interactive)
    (progn
      (tex-buffer)
      (print tex-directory)
      (print tex-zap-file)
      (let* ((tex-render-output-name (expand-file-name (concat tex-zap-file ".dvi") tex-directory))
	     (tex-render-output-buffer (get-file-buffer tex-render-output-name)))
	(progn
	  (print tex-render-output-name)
	  (if tex-render-output-buffer
	      (kill-buffer tex-render-output-buffer))
	  (find-file-other-window tex-render-output-name)))))

(require 'avy)
(avy-setup-default)
(global-set-key (kbd "C-c C-j") 'avy-resume)
(setq avy-timeout-seconds 0.3)
(global-set-key (kbd "C-;") 'avy-goto-char-timer)
(global-set-key (kbd "C-'") 'avy-goto-line)

(require 'which-key)
(which-key-mode)

(require 'gptel)
(setq gptel-api-key (getenv "OPENAI_API_KEY"))

;; custom functions
;; alpha
(setq-default m/default-alpha 90)
(add-to-list 'default-frame-alist '(alpha-background . 100))

(defun alpha-set (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nvalue: ")
  (set-frame-parameter nil 'alpha-background value))

(defun alpha-toggle ()
  "Toggles the transparency between opaque and current value"
  (interactive)
  (if (= (frame-parameter nil 'alpha-background) 100)
      (alpha-set m/default-alpha)
    (progn
      (setq m/default-alpha (frame-parameter nil 'alpha-background))
      (alpha-set 100))))

(global-set-key "\C-x\C-a" 'alpha-toggle)

;; terminal
(defun open-st-in-workdir ()
  (interactive)
  (call-process-shell-command
   (concat "setsid st -e tmux new-session -c " (expand-file-name default-directory)) nil 0))
(global-set-key (kbd "C-x c t") 'open-st-in-workdir)

;; copy pwd to kill ring
(defun copy-pwd-to-kill-ring ()
  "Copy the current buffer's default directory (PWD) to the kill ring."
  (interactive)
  (let ((pwd (expand-file-name default-directory)))
    (kill-new pwd)
    (message "Copied PWD to kill ring: %s" pwd)))
(global-set-key (kbd "C-x c p") 'copy-pwd-to-kill-ring)

;; custom.el
(load-if-exists custom-file)
