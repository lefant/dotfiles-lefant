;; not yet in melpa / marmelade
(add-to-list 'load-path "~/git/other/distel/elisp")
(require 'distel)
(distel-setup)

(setq erlang-indent-level '2)

(require 'flymake)
(defun flymake-erlang-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file
          (file-relative-name temp-file
                              (file-name-directory buffer-file-name))))
    (list "~/.emacs.d/eflymake.escript" (list local-file))))

(add-to-list 'flymake-allowed-file-name-masks
             '("\\.erl\\'" flymake-erlang-init))

(add-hook 'erlang-mode-hook 'flymake-mode)
