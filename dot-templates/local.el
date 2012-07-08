(server-start)

(require 'edit-server)
(edit-server-start)

(setq browse-url-browser-function (quote browse-url-generic))
(setq browse-url-generic-program "chromium-browser")

(load "elpa-conf.el")

(load "erlang-conf.el")
(setq erlang-indent-level '4)

(load "haskell-conf.el")

(load-theme 'solarized-dark t)
