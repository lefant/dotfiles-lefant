(add-hook 'python-mode-hook 'blacken-mode)

(pyenv-mode)
(require 'pyenv-mode-auto)

(elpy-enable)

(require 'python)
(setq python-shell-interpreter "ipython")

