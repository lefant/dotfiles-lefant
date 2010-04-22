(server-start)

;;(load "w3m-conf.el")

(setq browse-url-browser-function (quote browse-url-generic))
(setq browse-url-generic-program "chromium-browser")

(add-to-list 'load-path "/home/lefant/git/git-emacs")
;;(require 'git-emacs)

;;(require 'org)
;;(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
;;(global-set-key "\C-cl" 'org-store-link)
;;(global-set-key "\C-ca" 'org-agenda)

;;(load "bbdb-conf.el")
;;(load "auctex-conf.el")

;;(load "erc-conf.el")

;;(load "cperl-conf.el")
;;(load "perly-sense-conf.el")

;;(load "scheme-conf.el")

;;(require 'color-theme)
;;(color-theme-subtle-hacker)


;;(load "gnus-conf.el")
;;(load "gnus-conf-mm.el")



;;(load "erc-conf-work.el")


;; (add-to-list 'load-path "/home/lefant/shared/code/erlang/distel/elisp")
;; (require 'distel)
;; (distel-setup)
