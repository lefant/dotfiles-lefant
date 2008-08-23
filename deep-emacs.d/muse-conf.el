;;(setq load-path (add-to-list 'load-path "~/.elisp/muse/"))

(require 'muse-mode)
(require 'muse-colors)
(require 'muse-wiki)
(require 'muse-html)
(require 'muse-journal)
(require 'muse-latex)

;;(require 'muse-docbook)

(setq muse-project-alist
      '(("Plans"
         ("~/Plans"
	  :major-mode planner-mode
          :visit-link planner-visit-link
          :default "TaskPool")
         (:base "planner-html" :path "~/plans"))
        ("Blog"
         ("~/Blog" :default "index")
         (:base "journal-xhtml" :path "~/public_html/log/")
	 (:base "journal-rss" :path "~/public_html/log/"))
        ("Web"
         ("~/Web"
	  :default "index")
         (:base "xhtml" :path "~/public_html/"))))



(setq muse-html-style-sheet "<link rel='stylesheet' type='text/css' href='http://lefant.net/css/default.css' />")

(setq muse-journal-rdf-summarize-entries nil)
(setq muse-journal-rss-summarize-entries nil)

(setq muse-xhtml-footer "~/public_html/footer.html")
(setq muse-xhtml-header "~/public_html/header.html")

(setq muse-html-footer "~/public_html/footer.html")
(setq muse-html-header "~/public_html/header.html")
