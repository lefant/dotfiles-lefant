(use-package org-roam
  :custom
  ;;(org-roam-directory "/Users/lefant/git/lefant/notes")
  (org-roam-directory (file-truename "~/Google Drive/My Drive/logseq/notes"))
  (org-roam-dailies-directory "journals/")
  (org-roam-capture-templates
   '(("d" "default" plain
      "%?" :target
      (file+head "pages/${slug}.org" "#+title: ${title}\n")
      :unnarrowed t))))
