;;; -*- lexical-binding: t -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(gruvbox))
 '(custom-safe-themes
   '("d5fd482fcb0fe42e849caba275a01d4925e422963d1cd165565b31d3f4189c87"
     default))
 '(package-selected-packages
   '(async avy bash-completion cape corfu crux eglot-booster forge fussy
           fzf fzf-native gptel gruvbox-theme kdl-mode keycast
           orderless ox-hugo rg undo-tree vertico))
 '(package-vc-selected-packages
   '((fzf-native :vc-backend Git :url
                 "https://github.com/dangduc/fzf-native.git")
     (eglot-booster :vc-backend Git :url
                    "https://github.com/jdtsmith/eglot-booster.git")))
 '(safe-local-variable-values
   '((eval defun hugo/run-server nil (interactive)
           (async-shell-command "hugo server -D" "*hugo-server*"))))
 '(user-full-name "Hee-Suk Kim")
 '(user-mail-address "hskimse1@gmail.com"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
