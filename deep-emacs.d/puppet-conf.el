;; cd ~/git/other && git clone git://github.com/puppetlabs/puppet-syntax-emacs.git
(add-to-list 'load-path "~/git/other/puppet-syntax-emacs")
(autoload 'puppet-mode "puppet-mode" "Major mode for editing puppet manifests")
(add-to-list 'auto-mode-alist '("\\.pp$" . puppet-mode))

(require 'flymake-puppet)
(add-hook 'puppet-mode-hook (lambda () (flymake-puppet-load)))
