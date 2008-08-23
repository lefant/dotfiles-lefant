(add-to-list 'load-path "~/scheme/dist/slime48")

(autoload 'slime "slime"
  "Start an inferior^_superior Lisp and connect to its Swank server."
  t)
(autoload 'slime-mode "slime"
  "SLIME: The Superior Lisp Interaction Mode for Emacs (minor-mode)."
  t)

(eval-after-load "slime"
  '(progn
     (slime-setup)
     (setq slime-lisp-implementations
	   `((s48 ("scheme48") :init slime48-init-command)
	     ,@slime-lisp-implementations))))

(autoload 'slime48-init-command "slime48"
  "Return a string to initialize Scheme48 running under SLIME.")

;; This snippet lets you specify a scheme48-package local variable,
;; in a file's -*- line or local variables section, and have SLIME48
;; automatically evaluate code in the right package.  For instance,
;; all of my Scheme48 source files start with:
;;   ;;; -*- Mode: Scheme; scheme48-package: ... -*-
(eval-after-load "slime48"
  '(add-hook 'slime-mode-hook
	     (lambda ()
	       (if (and (boundp 'scheme48-package)
			scheme48-package)
		   (setq slime-buffer-package scheme48-package)))))


(require 'quack)
(setq quack-fontify-style 'emacs
      quack-default-program "scheme48"
      quack-newline-behavior 'newline)


;;; Examples for quick documentation access. Quack does similar stuff.
(defun s48-doc ()
  "Browse the Scheme48 documentation."
  (interactive)
  (browse-url "file:///usr/share/doc/scheme48-doc/html/manual.html"))

(defun scsh-doc ()
  "Browse the scsh documentation."
  (interactive)
  (browse-url "file:///usr/share/doc/scsh-0.6-doc/scsh-manual/html/man-Z-H-1.html"))


(require 'paredit)

(setq show-paren-mode 't)
(autoload 'enable-paredit-mode "paredit"
  "Turns on pseudo-structural editing of Lisp code."
  t)
(add-hook 'scheme-mode-hook 'enable-paredit-mode)
