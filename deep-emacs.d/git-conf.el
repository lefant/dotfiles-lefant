(load "/usr/share/doc/git-core/contrib/emacs/git.el" t)
(load "/usr/share/doc/git-core/contrib/emacs/git-blame.el" t)
(load "/usr/share/doc/git-core/contrib/emacs/vc-git.el" t)
(add-to-list 'vc-handled-backends 'GIT)

(add-to-list 'load-path "~/.elisp/git-emacs")
(load "git-emacs")
