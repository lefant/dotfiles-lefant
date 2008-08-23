(require 'w3m)

(setq browse-url-browser-function 'w3m-browse-url)
(global-set-key "\C-xm" 'browse-url-at-point)

(setq w3m-default-display-inline-images 't)
(setq w3m-use-cookies 't)
(setq w3m-make-new-session 't)
(setq w3m-cookie-accept-bad-cookies 'ask)
