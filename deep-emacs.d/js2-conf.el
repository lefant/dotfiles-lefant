
(custom-set-variables
 '(js2-basic-offset 2)
 ;;'(js2-bounce-indent-p t)
)

(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))


(add-hook 'js2-mode-hook
          (defun my-js2-mode-setup ()
            (flycheck-mode t)
            (when (executable-find "eslint")
              (flycheck-select-checker 'javascript-eslint))))

