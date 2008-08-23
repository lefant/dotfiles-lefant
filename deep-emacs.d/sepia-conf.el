(add-to-list 'load-path "~/.elisp/sepia")
(setq sepia-perl5lib (list (expand-file-name "~/.elisp/sepia/lib")))
(defalias 'perl-mode 'sepia-mode)
(require 'sepia)

