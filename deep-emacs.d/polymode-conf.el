;; git clone https://github.com/vitoshka/polymode.git
(setq load-path
      (append '("~/git/other/polymode"  "~/git/other/polymode/modes")
              load-path))

(require 'poly-R)
(require 'poly-markdown)

(add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))
