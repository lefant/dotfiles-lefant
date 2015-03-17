(require 'edit-server)
(setq edit-server-new-frame nil)
(edit-server-start)

(load "elpa-conf.el")

;(load "erlang-conf.el")

(load "haskell-conf.el")

;(load-theme 'solarized-dark t)
;(load-theme 'manoj-dark t)
;(load-theme 'zenburn t)

;; get this one from https://github.com/sellout/emacs-color-theme-solarized
(add-to-list 'custom-theme-load-path "/Users/fabian/git/other/emacs-color-theme-solarized")
(setq frame-background-mode 'light)
(set-terminal-parameter nil 'background-mode 'dark)
(load-theme 'solarized t)


(load "scala-conf.el")

(load "js-conf.el")

(require 'ess)

