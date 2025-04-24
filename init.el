(profiler-start 'cpu)
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(setq inhibit-startup-screen t
      vc-follow-symlinks t
      backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
      default-input-method "korean-hangul"
      ring-bell-function 'ignore)

(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(tab-bar-mode 0)
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
(require 'treesit)
(defun m/setup-install-grammars ()
  (dolist (grammar
           '((css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
             (bash "https://github.com/tree-sitter/tree-sitter-bash")
             (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
             (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.21.2" "src"))
             (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
             (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
             (go "https://github.com/tree-sitter/tree-sitter-go" "v0.20.0")
             (markdown "https://github.com/ikatyang/tree-sitter-markdown")
             (make "https://github.com/alemuller/tree-sitter-make")
             (elisp "https://github.com/Wilfred/tree-sitter-elisp")
             (cmake "https://github.com/uyha/tree-sitter-cmake")
             (c "https://github.com/tree-sitter/tree-sitter-c")
             (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
             (toml "https://github.com/tree-sitter/tree-sitter-toml")
             (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
             (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
             (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))
             (prisma "https://github.com/victorhqc/tree-sitter-prisma")))
    (add-to-list 'treesit-language-source-alist grammar)
    ;; Only install `grammar' if we don't already have it
    ;; installed. However, if you want to *update* a grammar then
    ;; this obviously prevents that from happening.
    (unless (treesit-language-available-p (car grammar))
      (treesit-install-language-grammar (car grammar)))))
(m/setup-install-grammars)

(dolist (mapping
	 '(("\\.tsx\\'" . tsx-ts-mode)
	   ("\\.jsx\\'" . tsx-ts-mode)
	   ("\\.ts\\'" . typescript-ts-mode)
	   ("\\.js\\'" . typescript-ts-mode)
	   ("\\.mjs\\'" . typescript-ts-mode)
	   ("\\.mts\\'" . typescript-ts-mode)
	   ("\\.cjs\\'" . typescript-ts-mode)
	   ("\\.json\\'" . json-ts-mode)))
  (add-to-list 'auto-mode-alist mapping))

(dolist (mapping
         '((python-mode . python-ts-mode)
           (css-mode . css-ts-mode)
           (typescript-mode . typescript-ts-mode)
           (js-mode . typescript-ts-mode)
           (js2-mode . typescript-ts-mode)
           (c-mode . c-ts-mode)
           (c++-mode . c++-ts-mode)
           (c-or-c++-mode . c-or-c++-ts-mode)
           (bash-mode . bash-ts-mode)
           (css-mode . css-ts-mode)
           (json-mode . json-ts-mode)
           (js-json-mode . json-ts-mode)
           (sh-mode . bash-ts-mode)
           (sh-base-mode . bash-ts-mode)))
  (add-to-list 'major-mode-remap-alist mapping))

;; cc-mode c-ts-mode c++-ts-mode
(setq-default indent-tabs-mode t)
(setq-default c-basic-offset tab-width
	      c-default-style '((awk-mode . "awk")
				(other . "linux"))
	      c-ts-mode-indent-style 'linux)
(setq-default c-ts-mode-indent-offset c-basic-offset)

(dolist (hook '(c-ts-mode-hook c++-ts-mode-hook))
  (add-hook hook #'subword-mode))

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
      company-idle-delay 0
      company-show-numbers nil
      company-tooltip-align-annotations nil
      company-require-match 'never)

(require 'eglot)
(dolist (hook '(cc-mode-hook
		 c-ts-mode-hook
		 c++-ts-mode-hook
		 typescript-ts-mode-hook))
  (add-hook hook 'eglot-ensure))
(add-hook 'python-base-mode-hook 'eglot-ensure)
(define-key eglot-mode-map (kbd "C-c r") 'eglot-rename)
(define-key eglot-mode-map (kbd "C-c o") 'eglot-code-action-organize-imports)
(define-key eglot-mode-map (kbd "C-c h") 'eldoc)
(define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
(define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error)
(define-key flymake-mode-map (kbd "C-x c b") 'flymake-show-buffer-diagnostics)
(setq eglot-events-buffer-config '(:size 0 :format full))
(setq eldoc-echo-area-use-multiline-p nil)

;; (require 'lsp-mode)
;; (add-hook 'cc-mode-hook 'lsp)
;; (add-hook 'c-ts-mode-hook 'lsp)
;; (add-hook 'c++-ts-mode-hook 'lsp)
;; (add-hook 'python-base-mode-hook 'lsp)
;; (require 'flycheck)
;; (add-hook 'lsp-mode-hook (lambda () 'flycheck-mode))
;; (define-key flycheck-mode-map (kbd "C-x c b") 'list-flycheck-errors)
;; (define-key flycheck-mode-map (kbd "M-n") 'flycheck-next-error)
;; (define-key flycheck-mode-map (kbd "M-p") 'flycheck-previous-error)

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
(setq minibuffer-default-prompt-format " [%s]")
;; todo
;; (define-key vertico-flat-map (kbd "C-d") 'dired-at-point)
;; (define-key vertico-flat-map (kbd "C-f") 'find-file-at-point)

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
;; (global-set-key (kbd "C-s") 'fzf-find-in-buffer) 

;; (require 'swiper)
;; (global-set-key (kbd "C-s") 'swiper)

(require 'keycast)
(keycast-mode-line-mode)

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

;; native compile init.el

;; global keymaps
(global-set-key (kbd "C-x c v") 'vterm-other-window)

;; custom.el
(load-if-exists custom-file)

(profiler-stop)
