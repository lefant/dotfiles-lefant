(use-package org-roam
  :custom
  ;;(org-roam-directory "/Users/lefant/git/lefant/notes")
  (org-roam-directory "/Users/lefant/Google Drive/My Drive/logseq/notes")
  (org-roam-dailies-directory "journals/")
  (org-roam-capture-templates
   '(("d" "default" plain
      #'org-roam-capture--get-point "%?"
      :file-name "pages/${slug}" :head "#+title: ${title}\n" :unnarrowed t))))
