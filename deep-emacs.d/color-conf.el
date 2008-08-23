(require 'color-theme)
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (set-variable 'color-theme-is-global nil)
            (select-frame frame)
            (if window-system
                (color-theme-subtle-hacker)
              (color-theme-tty-dark))))
(if window-system
    (color-theme-subtle-hacker)
  (color-theme-tty-dark))
