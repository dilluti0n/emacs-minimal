(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(setq inhibit-startup-screen t
      vc-follow-symlinks t
      backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
      default-input-method "korean-hangul"
      ring-bell-function 'ignore)

(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(column-number-mode t)
(recentf-mode 1)
(save-place-mode 1)
(setq history-length 25)
(savehist-mode 1)
(add-to-list 'default-frame-alist '(font . "Cascadia Code-13"))


;; tab is 8 spaces !!!!
(setq tab-width 8)

;; default hooks
(add-hook 'prog-mode-hook
	  (lambda () (progn (display-line-numbers-mode t))))

(global-set-key (kbd "C-x c c") 'compile)
(global-set-key (kbd "C-x c r") 'recompile)

;; tree-sitter
(setq major-mode-remap-alist '((c-mode . c-ts-mode)
			       (c++-mode . c++-ts-mode)
			       (c-or-c++-mode . c-or-c++-ts-mode)
			       (shell-script-mode . bash-ts-mode)
			       (python-mode . python-ts-mode)))

;; cc-mode c-ts-mode c++-ts-mode
(indent-tabs-mode t)
(setq c-basic-offset tab-width
      c-default-style '((awk-mode . "awk")
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
      company-idle-delay (if (company-in-string-or-comment) nil 0)
      company-show-numbers nil
      company-tooltip-align-annotations nil
      company-require-match 'never)

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
(define-key flymake-mode-map (kbd "C-x c b") 'flymake-show-diagnostics-buffer)
(setq eldoc-echo-area-use-multiline-p nil)

(require 'vterm)
(require 'vertico)
(vertico-mode)
;; (setq enable-recursive-minibuffers t
;;       read-extended-command-predicate #'command-completion-default-include-p
;;       minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))
(require 'orderless)
(setq completion-styles '(orderless basic)
      completion-category-defaults nil
      completion-category-overrides '((file (styles partial-completion))))
(vertico-flat-mode)
;; todo
(define-key vertico-flat-map (kbd "C-d") 'dired-at-point)
(define-key vertico-flat-map (kbd "C-f") 'find-file-at-point)

(require 'fussy)
(fussy-setup)
(fussy-company-setup)
(fussy-eglot-setup)
(add-to-list 'load-path (expand-file-name "elpa/fzf-native" user-emacs-directory))
(require 'fzf-native)
(setq fussy-score-fn 'fussy-fzf-native-score)
(fzf-native-load-dyn)

;; Prompt indicator for `completing-read-multiple'.
(when (< emacs-major-version 31)
  (advice-add #'completing-read-multiple :filter-args
              (lambda (args)
                (cons (format "[CRM%s] %s"
                              (string-replace "[ \t]*" "" crm-separator)
                              (car args))
                      (cdr args)))))

(require 'undo-tree)
(global-undo-tree-mode)
(setq undo-tree-history-directory-alist `(("." . ,(concat user-emacs-directory "undo"))))

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

(require 'crux)

(require 'rg)
(rg-enable-default-bindings)

(require 'fzf)
(setq fzf/args "-x --color bw --print-query"
      fzf/executable "fzf"
      fzf/git-grep-args "-i --line-number %s"
      fzf/grep-command "rg --no-heading -nH"
      fzf/position-bottom t
      fzf/window-height 15)
(global-set-key (kbd "C-s") 'fzf-find-in-buffer)

;; (require 'swiper)
;; (global-set-key (kbd "C-s") 'swiper)

;; end of package

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

(defun yank-file-contents-to-kill-ring (filename)
  "Read contents of FILENAME and add to kill-ring."
  (interactive "fFile to yank: ")
  (with-temp-buffer
    (insert-file-contents filename)
    (kill-new (buffer-string))
    (message "File contents added to kill-ring.")))

;; end of custom functions

;; global keymaps
(global-set-key (kbd "C-x c v") 'vterm-other-window)

;; custom.el
(load-if-exists custom-file)
