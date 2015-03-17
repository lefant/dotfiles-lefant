;; cabal install ghc-mod

;; ;; mac path
;; (add-to-list 'load-path
;;   "/Users/fabian/Library/Haskell/ghc-7.6.3/lib/ghc-mod-4.1.1/share")
;; ;; linux path
;; (add-to-list 'load-path
;;   "/usr/local/share/ghc-mod-1.10.18")

;; (autoload 'ghc-init "ghc" nil t)


(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init)))

;;(add-hook 'haskell-mode-hook 'haskell-indent-mode)
;;(add-hook 'haskell-mode-hook 'interactive-haskell-mode)


;; ;; haskell-mode
;; ;; git clone git://github.com/haskell/haskell-mode.git
;; (custom-set-variables '(haskell-program-name "ghci"))
;; (custom-set-variables '(haskell-tags-on-save t))
;; (custom-set-variables '(haskell-stylish-on-save t))

;; (add-hook 'haskell-mode-hook
;;           (lambda ()
;;             (require 'inf-haskell)
;;             (turn-on-haskell-doc-mode)
;;             (turn-on-haskell-indent)
;;             (turn-on-haskell-font-lock)
;;             (turn-on-haskell-decl-scan)
;;             (define-key haskell-mode-map (kbd "C-x C-s") 'haskell-mode-save-buffer)
;;             (ghc-init)
;;             (flymake-mode)))
