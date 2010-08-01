;; This is needed for Erlang mode setup
(setq erlang-root-dir "/usr/lib/erlang")
(setq load-path (cons "/usr/lib/erlang/lib/tools-2.6.5/emacs" load-path))
(setq exec-path (cons "/usr/lib/erlang/bin" exec-path))
(setq erlang-indent-level '2)

(require 'erlang-start)

;; This is needed for Distel setup
(let ((distel-dir "/home/lefant/git/distel/elisp"))
  (unless (member distel-dir load-path)
    ;; Add distel-dir to the end of load-path
    (setq load-path (append load-path (list distel-dir)))))

(require 'distel)
(distel-setup)



;; (require 'esense-start)
;; (setq esense-indexer-program "/home/fabian/tmp/esense-1.12/esense.sh")

;; (setq-default show-trailing-whitespace t)



;; (provide 'erlang_mode_config)

;; (add-to-list 'load-path "~/emacs/erlware-mode")
;; (require 'erlang-start)
;; (add-hook 'erlang-mode-hook
;; 	  (lambda()
;; 	    (setq indent-tabs-mode nil)
;; 	    (setq erlang-indent-level 2)
;; 	    (setq erlang-tab-always-indent t)
;; 	    (add-to-list 'exec-path "/opt/bin")
;; 	    (setq erlang-root-dir "/opt")))



;; (provide 'flymake_config)

;; (require 'flymake)


;; (defun flymake-erlang-init ()
;;   (let* ((temp-file (flymake-init-create-temp-buffer-copy
;; 		     'flymake-create-temp-with-folder-structure))
;; 	 (local-file (file-relative-name
;; 		      temp-file
;; 		      (file-name-directory buffer-file-name))))
;;     (list "~/emacs/flymake/bin/eflymake" (list local-file))))

;; (add-to-list 'flymake-allowed-file-name-masks
;; 	     '("\\.erl\\'" flymake-erlang-init))

;; (add-hook 'find-file-hook 'flymake-find-file-hook)






;;; eflymake

;; #!/opt/bin/escript
;; -export([main/1]).

;; main([File_Name]) ->
;;     compile:file(File_Name, [warn_obsolete_guard, warn_unused_import,
;;     			    		   warn_shadow_vars, warn_export_vars,
;; 					   strong_validation, report,
;; 					   {i, "../include"},
;; 					   {outdir, "/tmp"}]).
