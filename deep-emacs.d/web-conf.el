;; as described here
;; http://codewinds.com/blog/2015-04-02-emacs-flycheck-eslint-jsx.html

;; package-list-packages
;; web-mode flycheck eslint-fix

;; npm install -g eslint babel-eslint eslint-plugin-babel prettier eslint-config-prettier eslint-plugin-prettier standard eslint-plugin-standard eslint-plugin-react eslint-plugin-import eslint-plugin-node eslint-plugin-promise eslint-config-standard eslint-config-standard-babel eslint-config-standard-react

;; .eslintrc.js
;; module.exports = {
;;   parser: 'babel-eslint',
;;   extends: ['standard', 'standard-babel', 'standard-react'],
;;   plugins: ['babel', 'prettier', 'standard']
;; }


;; use web-mode for .jsx files
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . web-mode))

(require 'flycheck)
;; turn on flychecking globally
(add-hook 'after-init-hook #'global-flycheck-mode)

;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint)))

;; use eslint with web-mode for jsx files
(flycheck-add-mode 'javascript-eslint 'web-mode)

;; https://github.com/purcell/exec-path-from-shell
;; only need exec-path-from-shell on OSX
;; this hopefully sets up path and other vars better
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))


(defun my-web-mode-hook ()
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (electric-indent-local-mode -1)
  (add-hook 'after-save-hook 'eslint-fix nil t))
(add-hook 'web-mode-hook 'my-web-mode-hook)

