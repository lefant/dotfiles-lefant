;; *** PerlySense Config ***

;; ** PerlySense **
;; The PerlySense prefix key (unset only if needed, like for \C-o)
(global-unset-key "\C-o")
(setq ps/key-prefix "\C-o")


;; ** Flymake **
;; Load flymake if t
;; Flymake must be installed.
;; It is included in Emacs 22
;;     (or http://flymake.sourceforge.net/, put flymake.el in your load-path)
(setq ps/load-flymake t)
;; Note: more flymake config below, after loading PerlySense


;; *** PerlySense load (don't touch) ***
(setq ps/external-dir (shell-command-to-string "perly_sense external_dir"))
(if (string-match "Devel.PerlySense.external" ps/external-dir)
    (progn
      (message
       "PerlySense elisp files  at (%s) according to perly_sense, loading..."
       ps/external-dir)
      (setq load-path (cons
                       (expand-file-name
                        (format "%s/%s" ps/external-dir "emacs")
                        ) load-path))
      (load "perly-sense")
      )
  (message "Could not identify PerlySense install dir.
    Is Devel::PerlySense installed properly?
    Does 'perly_sense external_dir' give you a proper directory? (%s)" ps/external-dir)
  )


;; ** Flymake Config **
;; If you only want syntax check whenever you save, not continously
(setq flymake-no-changes-timeout 9999)
(setq flymake-start-syntax-check-on-newline nil)

;; ** Code Coverage Visualization **
;; If you have a Devel::CoverX::Covered database handy and want to
;; display the sub coverage in the source, set this to t
(setq ps/enable-test-coverage-visualization t)

;; ** Color Config **
;; Emacs named colors: http://www.geocities.com/kensanata/colors.html
;; The following colors work fine with a white X11
;; background. They may not look that great on a console with the
;; default color scheme.
(set-face-background 'flymake-errline "black")
(set-face-background 'flymake-warnline "gray20")
(set-face-background 'dropdown-list-face "lightgrey")
(set-face-background 'dropdown-list-selection-face "grey")


;; *** PerlySense End ***
