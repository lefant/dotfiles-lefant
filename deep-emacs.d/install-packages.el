(defun package-install-list (pkg-list)
  ;; Install each package in pkg-list if necessary.
  (mapcar
   (lambda (pkg) (package-install pkg))
   pkg-list)
  (message "done installing packages"))

(defvar my-package-list
  '(clojure-mode
    edit-server
    ein
    el-get
    elpy
    ensime
    erlang
    ess
    flymake-easy
    flymake-jslint
    ghc
    git-commit-mode
    git-rebase-mode
    haskell-mode
    idomenu
    js2-mode
    magit
    magit-gerrit
    markdown-mode
    protobuf-mode
    psci
    purescript-mode
    sbt-mode
    scala-mode2
    web-mode
    yaml-mode
    ))

(defun install-packages ()
  ;; Install packages listed in global 'my-package-list'
  (interactive)
  (package-list-packages)
  (package-install-list my-package-list))

(install-packages)
