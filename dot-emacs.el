;; aptitude install emacs21-nox emacs-goodies-el w3m w3m-img w3m-elw3-el-e21 tramp emacs-color-themes 
;; aptitude install darcs screen zsh lftp rsync

(load "~/.emacs.d/emacs.el")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("d26509deaecf7ab9e4a6256056852eb136e6ecbd402969ca8cf0a417aa872a87" "e80932ca56b0f109f8545576531d3fc79487ca35a9a9693b62bf30d6d08c9aaf" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(haskell-program-name "ghci")
 '(haskell-stylish-on-save t)
 '(haskell-tags-on-save t)
 '(purescript-mode-hook (quote (turn-on-eldoc-mode turn-on-purescript-indentation))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(put 'downcase-region 'disabled nil)

(put 'upcase-region 'disabled nil)
