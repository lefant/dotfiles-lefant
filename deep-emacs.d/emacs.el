;; general personal stuff
(setq user-mail-address "e@lefant.net")
(setq user-host-name "lefant.net")
(setq user-full-name "Fabian Linzberger")


(server-start)



(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))


;;; try without automatic auto-fill for a while again
; (add-hook 'text-mode-hook 'turn-on-auto-fill)

(column-number-mode 1)
(display-time-mode 1)
(setq display-time-day-and-date t)
(global-font-lock-mode 1)
(setq show-paren-delay 0
      show-paren-style 'parenthesis)
(show-paren-mode 1)
(setq-default indent-tabs-mode nil)

(global-set-key "\C-w"     'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)
(global-set-key [f5]  'call-last-kbd-macro)

(global-set-key "\C-c f" 'ffap)



(setq backup-directory-alist `(("." . ,(expand-file-name "~/.backup"))))
(setq delete-old-versions t)

;;(require 'iswitchb)
;;(iswitchb-mode t)


(require 'saveplace)
(setq-default save-place t)

(require 'recentf)
(setq recentf-auto-cleanup 'never) ;; disable before we start recentf!
(recentf-mode 1)

(require 'dired)

(require 'rx)
(setq reb-re-syntax 'rx)

(setq auto-mode-alist
      (append '(("\\.lisp$"     . lisp-mode)
                ("\\.asd$"      . lisp-mode)
                ("\\.py$"       . python-mode)
                ("\\.wml$"      . html-mode)
                ("\\.dtml$"     . html-mode)
                ("\\.stml$"     . html-mode)
                ("\\.t$"        . cperl-mode)
                ("\\.org$"      . org-mode)
                ("\\.tt$"       . tt-mode)
                ("\\.tt2$"      . tt-mode)
                ("\\.erl$"      . erlang-mode)
                ("\\.escript$"  . erlang-mode)
                ("/mutt"        . message-mode))
	      auto-mode-alist))

(add-hook 'message-mode-hook 'turn-on-filladapt-mode)


(require 'org)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))



(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "~/.elisp/")


(autoload 'enable-paredit-mode "paredit.el" "structured editing for lisp" t)


;(load "~/.elisp/darcsum/darcsum.el")
;(load "~/.elisp/darcs-mode/darcs.el")

;(load "color-conf.el")
;(load "haskell-conf.el")

(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)

(defun ido-find-file-in-tag-files ()
  (interactive)
  (save-excursion
    (let ((enable-recursive-minibuffers t))
      (visit-tags-table-buffer))
    (ido-completing-read "Project file: "
                         (tags-table-files)
                         nil t)))


(autoload 'imenu-tree "imenu-tree" "Show imenu tree" t)
(setq imenu-tree-auto-update t)


(outline-minor-mode)
(load "outline-conf.el")


(load "haskell-conf.el")
;;(load "erlang-conf.el")


(setq safe-local-variable-values
      '((erlang-indent-level . 2)
       (erlang-indent-level . 4)
       (show-trailing-whitespace . t)
       (set-fill-column . 79)
       (highlight-beyond-fill-column . t)
       (allout-layout . t)))


(defun pretty-lambdas ()
  (font-lock-add-keywords
   nil `(("(\\(lambda\\>\\)"
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil))))))

(add-hook 'emacs-lisp-mode-hook 'pretty-lambdas)


(defun my-whitespace-setup()
  (require 'whitespace)
  (setq whitespace-style (list 'tabs 'trailing 'lines-tail)
        whitespace-line-column 79)
  (global-whitespace-mode t))



(if (locate-library "whitespace")
    (my-whitespace-setup))



(setq initial-buffer-choice "~/Dropbox/dot-state/gtd.org")


(setq split-height-threshold nil)
(setq split-height-threshold 140)
(split-window-horizontally 82)

(load "local.el")
