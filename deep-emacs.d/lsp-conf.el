(setq lsp-keymap-prefix "C-l")

(require 'lsp-mode)
(add-hook 'python-mode-hook #'lsp)
(add-hook 'web-mode-hook #'lsp)
(add-hook 'js2-mode-hook #'lsp)
