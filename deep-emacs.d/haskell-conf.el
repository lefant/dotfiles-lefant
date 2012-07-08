;; cabal install ghc-mod

;; mac path
(add-to-list 'load-path
  "/Users/fabian/Library/Haskell/ghc-7.4.1/lib/ghc-mod-1.10.18/share")
;; linux path
(add-to-list 'load-path
  "/usr/local/share/ghc-mod-1.10.18")

(autoload 'ghc-init "ghc" nil t)



(custom-set-variables '(haskell-program-name "ghci"))

(add-hook 'haskell-mode-hook
          (lambda ()
            (require 'inf-haskell)
            (turn-on-haskell-doc-mode)
            (turn-on-haskell-indent)
            (turn-on-haskell-font-lock)
            (turn-on-haskell-decl-scan)
            (ghc-init)
            (flymake-mode)))
