(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)


(dolist (package
  '(
    ;; python
    auto-virtualenv
    blacken
    ein
    elpy
    py-isort
    python

    ;; javascript
    eslint-fix
    js2-mode
    prettier-js
    web-mode

    magit
    material-theme
    solarized-theme
    )
  )
 (unless (package-installed-p package)
   (package-install package))
 (require package))
