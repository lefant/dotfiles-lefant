;; aptitude install emacs21-nox emacs-goodies-el w3m w3m-img w3m-elw3-el-e21 tramp emacs-color-themes 
;; aptitude install darcs screen zsh lftp rsync

(load "~/.emacs.d/emacs.el")
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(haskell-program-name "ghci")
)
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

(put 'downcase-region 'disabled nil)

(put 'upcase-region 'disabled nil)
