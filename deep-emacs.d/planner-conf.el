(require 'planner)
(require 'planner-publish)

(setq planner-project "Plans")

(setq european-calendar-style t)
(planner-insinuate-calendar)
;;(setq mark-diary-entries-in-calendar t)

(require 'planner-gnus)
(planner-gnus-insinuate)

(setq planner-day-page-template
      "* Tasks\n\n\n* Schedule\n\n\n* Diary\n\n\n* Notes")

(require 'planner-diary)
(add-hook 'diary-display-hook 'fancy-diary-display)
(add-hook 'list-diary-entries-hook 'sort-diary-entries)
(add-hook 'list-diary-entries-hook 'include-other-diary-files)
(add-hook 'mark-diary-entries-hook 'mark-included-diary-files)

(setq planner-diary-use-diary t)
(planner-diary-insinuate)


;;(require 'planner-erc)
(require 'planner-w3m)


(require 'remember)
(require 'remember-planner)
(setq remember-handler-functions '(remember-planner-append))
(setq remember-annotation-functions planner-annotation-functions)


;; add this locally, where there is bbdb, if
;(require 'remember-bbdb)
;(setq remember-handler-functions '(remember-bbdb-store-in-mailbox))


;;(display-time)
;;(add-hook 'diary-hook 'appt-make-list)
;;(diary 0)

;;(setq appt-message-warning-time 30)


