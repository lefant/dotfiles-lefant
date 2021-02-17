(defvar myPackages
  '(
    blacken
    ein
    elpy
    eslint-fix
    js2-mode
    magit
    material-theme
    prettier-js
    py-isort
    python
    solarized-theme
    web-mode
    )
  )

(mapc #'(lambda (package)
          (unless (package-installed-p package)
            (package-install package)))
      myPackages)
