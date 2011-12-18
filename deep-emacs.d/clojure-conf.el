;; clojure-mode
;; git://github.com/technomancy/clojure-mode.git
(add-to-list 'load-path "~/git/other/clojure-mode")
(require 'clojure-mode)

;; sudo aptitude install clojure clojure-contrib leiningen
;; lein plugin install swank-clojure 1.3.3

(require 'paredit)

;; slime
(eval-after-load "slime" 
  '(progn (slime-setup '(slime-repl))))

;; slime
(eval-after-load "slime" 
  '(progn (slime-setup '(slime-repl))
          (defun paredit-mode-enable () (paredit-mode 1))
          (add-hook 'slime-mode-hook 'paredit-mode-enable)
          (add-hook 'slime-repl-mode-hook 'paredit-mode-enable)
          (setq slime-protocol-version 'ignore)))

;; git://github.com/nablaone/slime.git
;; git checkout f9f2b2858cb31697efd2a3728f0b453c29bf031b
(add-to-list 'load-path "~/git/other/slime")
(require 'slime)
(slime-setup)
