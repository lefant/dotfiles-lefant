(require 'slime)

(slime-setup :autodoc t)
(setq inferior-lisp-program "sbcl")
(setq common-lisp-hyperspec-root "file:///usr/share/doc/hyperspec/")

;; (setq slime-complete-symbol-function `slime-fuzzy-complete-symbol)
(setq slime-truncate-lines nil)
(setq slime-multiprocessing t)

(load "paredit.el")
(setq lisp-mode-hook 'enable-paredit-mode)

