;; cabal install ghc-mod

;; mac path
(add-to-list 'load-path
  "/Users/fabian/Library/Haskell/ghc-7.4.1/lib/ghc-mod-1.10.18/share")
;; linux path
(add-to-list 'load-path
  "/usr/local/share/ghc-mod-1.10.18")

(autoload 'ghc-init "ghc" nil t)



;; haskell-mode
;; git clone git://github.com/haskell/haskell-mode.git
(add-to-list 'load-path "~/git/other/haskell-mode")
(load "~/git/other/haskell-mode/haskell-site-file")

(custom-set-variables '(haskell-program-name "ghci"))
(custom-set-variables '(haskell-tags-on-save t))
(custom-set-variables '(haskell-stylish-on-save t))

(add-hook 'haskell-mode-hook
          (lambda ()
            (require 'inf-haskell)
            (turn-on-haskell-doc-mode)
            (turn-on-haskell-indent)
            (turn-on-haskell-font-lock)
            (turn-on-haskell-decl-scan)
            (define-key haskell-mode-map (kbd "C-x C-s") 'haskell-mode-save-buffer)
            (ghc-init)
            (flymake-mode)))
