(require 'python)


;; (setq python-shell-interpreter "run_all-libs-and-services_local_ipython.sh"
;;       python-shell-interpreter-args ""
;;       python-shell-prompt-detect-failure-warning nil)
;; (add-to-list 'python-shell-completion-native-disabled-interpreters
;;              "run_all-libs-and-services_local_ipython.sh")

;; (setq python-shell-interpreter "jupyter"
;;       python-shell-interpreter-args "console --simple-prompt"
;;       python-shell-prompt-detect-failure-warning nil)
;; (add-to-list 'python-shell-completion-native-disabled-interpreters
;;              "jupyter")

(require 'auto-virtualenv)
(add-hook 'python-mode-hook 'auto-virtualenv-set-virtualenv)
(add-hook 'python-mode-hook 'blacken-mode)
(add-hook 'before-save-hook 'py-isort-before-save)

(elpy-enable)
;;(pyvenv-activate "~/Library/Caches/pypoetry/virtualenvs/all-libs-and-services-cuqknVEf-py3.8/")


;; Enable Flycheck
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))
