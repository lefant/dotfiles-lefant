;;; cperl-mode is preferred to perl-mode                                        
;;; "Brevity is the soul of wit" <foo at acm.org>                               
;(defalias 'perl-mode 'cperl-mode)

(load "/usr/share/emacs/site-lisp/sepia/sepia.el")
(defalias 'perl-mode 'sepia-mode)
(require 'sepia)


(defun lefant-perl-indent-setup ()
  (setq cperl-hairy t)
  (setq cperl-auto-newline t)
  (setq cperl-indent-parens-as-block t)
  (setq cperl-clobber-lisp-bindings t)
  (setq cperl-info-on-command-no-prompt t)
  (setq cperl-electric-parens nil)
  
  (cperl-set-style "PerlStyle")
  )

(add-hook 'cperl-mode-hook 'lefant-perl-indent-setup)


;(require 'cperl-mode)
;(lefant-perl-indent-setup)

;;; load custom lib for tt support
(load "~/.elisp/tt-mode.el")

(setq auto-mode-alist
      (append '(("\\.t$" 	. cperl-mode)
		("\\.tt$" 	. tt-mode)
		("\\.tt2$" 	. tt-mode))
	      auto-mode-alist))




;(add-to-list 'load-path "~/.elisp/pde")
;(load "pde-load")


;;(load "perly-sense-conf.el")




;; (defun my-cperl-eldoc-documentation-function ()
;;   "Return meaningful doc string for `eldoc-mode'."
;;   (car
;;    (let ((cperl-message-on-help-error nil))
;;      (cperl-get-help))))

;; (add-hook 'cperl-mode-hook
;;           (lambda ()
;;             (set (make-local-variable 'eldoc-documentation-function)
;;                  'my-cperl-eldoc-documentation-function)))




(defun perltidy-region ()
  "Run perltidy on the current region."
  (interactive)
  (save-excursion
    (shell-command-on-region (point) (mark) "perltidy -q" nil t)))
(defun perltidy-defun ()
  "Run perltidy on the current defun."
  (interactive)
  (save-excursion (mark-defun)
                  (perltidy-region)))



