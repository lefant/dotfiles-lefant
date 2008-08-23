(require 'gnus)
;;(require 'bbdb-gnus)

(setq mm-text-html-renderer 'w3m)
(setq mm-inline-text-html-with-images t)

(require 'pgg)
(setq pgg-query-keyserver nil)

(gnus-demon-init)
(gnus-demon-add-handler 'gnus-group-get-new-news 2 t)
(gnus-demon-add-handler 'gnus-demon-add-scanmail 2 t)

(setq mm-verify-option 'known)
(setq mm-decrypt-option 'known)

(setq mml2015-verify-function 'pgg-verify)

;;(setq gnus-buttonized-mime-types '("multipart/encrypted" "multipart/signed"))

;; Automcatically sign when sending mails
;;(add-hook 'message-send-hook 'mml-secure-message-sign-pgpmime)

;;(setq message-setup-hook 'mml-secure-message-sign-pgpmime)

;; Enough explicit settings
;; (setq pgg-passphrase-cache-expiry 300)
;;(setq pgg-default-user-id 37109E87)

(setq pgg-gpg-program "gpg")



;; Tells Gnus to inline the part
(eval-after-load "mm-decode"
   '(add-to-list 'mm-inlined-types "application/pgp$"))
;; Tells Gnus how to display the part when it is requested
(eval-after-load "mm-decode"
   '(add-to-list 'mm-inline-media-tests '("application/pgp$"
 					 mm-inline-text identity)))
;; Tell Gnus not to wait for a request, just display the thing
;; straight away.
(eval-after-load "mm-decode"
  '(add-to-list 'mm-automatic-display "application/pgp$"))
;; But don't display the signatures, please.
(eval-after-load "mm-decode"
  (quote (setq mm-automatic-display (remove "application/pgp-signature"
					    mm-automatic-display))))


(setq gnus-local-domain "lefant.net")

(setq gnus-posting-styles
      '((".*"
         (name "Fabian Linzberger")
         (address "e@lefant.net")
         ("X-URL" "http://lefant.net/")
         ("User-Agent"
          (if (= 0 (random 10))
              (format "Microsoft Gnus Express, Build %s (%s)"
                      (gnus-continuum-version (gnus-version))
                      gnus-version-number)
            (concat (gnus-extended-version)
                    " (" system-configuration ")")))
         (organization "e-lefants united")
         (signature-file "~/.signature"))
        ))


;; news backend
(setq gnus-select-method
      '(nntp "news.gmane.org"))

;;mail backend
(setq gnus-secondary-select-methods
      '((nnimap "lefant"
		(nnimap-address "lefant.net")
;;		(nnimap-server-port 993)
                (nnimap-stream tls)
		(nnimap-authinfo-file "~/.secret/gnus"))))

(setq gnus-message-archive-group "sent")
(setq gnus-message-archive-method '(nnimap "lefant"))





(setq gnus-read-active-file 'some)
(setq gnus-check-new-newsgroups '((nnimap "lefant")))
(setq gnus-subscribe-newsgroup-method 'gnus-subscribe-topic)

(setq gnus-read-newsrc-file nil)
(setq gnus-write-newsrc-file nil)
;;(setq gnus-group-default-list-level '6)
(setq gnus-group-sort-groups 'gnus-group-sort-by-score)

(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)






;;
;; Visual details

(setq gnus-thread-sort-functions
      '(gnus-thread-sort-by-number))
 
(setq gnus-article-sort-functions
      '(gnus-article-sort-by-number))


(setq gnus-visual t)
(setq gnus-use-full-window t)

(gnus-add-configuration
  '(article (vertical 1.0
                      (summary .20 point)
                      (article 1.0))))
 (gnus-add-configuration
  '(summary
    (vertical 1.0
              (group 5)
              (summary 1.0))))

;; Hide things
(setq gnus-boring-article-headers '(empty followup-to newsgroups
                                          many-to reply-to))

(setq gnus-visible-headers
      '("From:" "^Newsgroups:" "^Subject:" "^Date:" "^Followup-To:"
        "^Reply-To:" "^Organization:" "^Summary:" "^Keywords:" "^To:"
        "^Cc:" "^Posted-To:" "^Mail-Copies-To:" "^Apparently-To:"
        "^Gnus-Warning:" "X-Sent:" "^User-Agent:"
        "^X-Mailer:" "^Newsreader:" "^X-Newsreader:"
        "^X-Debian-PR-Message:" "^X-Debian-PR-Package:" "^List-Id:" "^Delivered-To:"
	"^X-Host-Lookup-Failed:" "^X-Sender-Verify-Failed:"))


;;
;; Killing
(setq gnus-kill-killed t)

;;
;; Scoring
(setq
 gnus-use-scoring t
 gnus-use-adaptive-scoring '(word line)
 gnus-update-score-entry-dates t
 gnus-summary-zcore-fuzz 2
 gnus-score-interactive-default-score 1024

 gnus-adaptive-word-no-group-words t
 gnus-adaptive-word-length-limit 5
 gnus-adaptive-word-minimum -32
 gnus-adaptive-word-no-group-words t
 gnus-default-ignored-adaptive-words
 '("a" "ab" "about" "al" "all" "also" "am" "an" "and" "another"
   "any" "are" "as" "at" "aw" "b" "back" "be" "because" "been"
   "before" "being" "bl" "bo" "both" "but" "by" "c" "can" "cf"
   "could" "current" "d" "de" "did" "do" "dw" "e" "each" "eb"
   "eg" "end" "ep" "even" "f" "first" "for" "from" "g" "gb" "get"
   "go" "good" "gp" "h" "had" "have" "hc" "he" "here" "his" "how"
   "i" "if" "ii" "il" "in" "into" "is" "it" "j" "just" "k" "know"
   "kt" "l" "last" "like" "long" "lx" "m" "make" "many" "may"
   "mb" "me" "might" "mo" "more" "most" "mp" "much" "must" "mx"
   "my" "n" "name" "nc" "ne" "new" "no" "not" "now" "o" "of"
   "off" "on" "one" "only" "or" "other" "our" "out" "over" "p"
   "part" "pb" "people" "pk" "point" "q" "qt" "qt" "r" "rc" "rd"
   "re" "rh" "right" "rs" "rx" "s" "same" "say" "sb" "sb" "see"
   "should" "since" "so" "some" "sp" "sr" "st" "start" "state"
   "still" "such" "t" "tc" "than" "that" "the" "their" "them"
   "then" "there" "these" "they" "this" "those" "ti" "time" "tk"
   "to" "too" "true" "try" "two" "u" "under" "up" "us" "use" "v"
   "value" "very" "vs" "vs" "w" "was" "way" "we" "well" "were"
   "what" "when" "where" "which" "while" "who" "why" "will"
   "with" "without" "work" "would" "wu" "x" "xf" "xp" "y" "yc"
   "you" "your" "z" "questions" "question" "problem" "problems")

 ;; Decaying scores
 gnus-decay-scores t
 gnus-score-decay-constant 32
 gnus-score-expiry-days 10
 gnus-update-score-entry-dates t
 )

(setq gnus-default-adaptive-score-alist
      '((gnus-unread-mark)
        (gnus-ticked-mark      (from 256)   (subject 512)  (followup 1024))
        (gnus-dormant-mark     (from 512)   (subject 1024))
        (gnus-del-mark                      (subject -512))
        (gnus-read-mark        (from 32)    (subject 256))
        (gnus-expirable-mark)
        (gnus-killed-mark     (from -64)    (subject -512) (followup -1024))
        (gnus-kill-file-mark   (from -256)  (subject -128))
        (gnus-ancient-mark)
        (gnus-low-score-mark)
        (gnus-catchup-mark     (from -32)   (subject -256))))

(setq gnus-default-adaptive-word-score-alist
      '((,gnus-read-mark . 300)
        (,gnus-ticked-mark . 600)
        (,gnus-dormant-mark . 800)
        (,gnus-catchup-mark . -100)
        (,gnus-killed-mark . -200)
        (,gnus-del-mark . -150)))

 
(setq gnus-score-find-score-files-function
      '(gnus-score-find-bnews bbdb/gnus-score))

(setq gnus-global-score-files
       '("~/News/all.SCORE"))
;; all.SCORE contains:
;;(("xref"
;;  ("gmane.spam.detected" -1000 nil s)))

;;(setq gnus-summary-expunge-below -999)


   





(setq nnimap-split-inbox
        '("INBOX" ))
(setq nnimap-split-predicate "UNDELETED")
(setq nnimap-split-crosspost nil)

;; (setq nnimap-split-rule '(("Example" ("INBOX" nnimap-split-fancy)))
;;       nnimap-split-fancy 
;;       '(| (any "auser@example\\.com" "INBOX.mail")
;;           ("X-Spam-Status" "Yes.*" "INBOX.Spamassassined")
;;           ("subject" "Backup report" "INBOX.backupreports")
;;           ("from" "MAILER-DAEMON@example\\.com" 
;;            "INBOX.example.mailer-daemon")
;;           (any "myaddr@example.com" "INBOX.mail")
;;           "INBOX.mail.unsorted"))


;; filtering into groups by header
(setq nnimap-split-rule
      '(("junk" "^X-Spam-Flag:.*YES\\|^X-Spam-Q-Flag:.*YES\\|^To:.*neu@web.de.*\\|^To:.*members@gmx.net.*\\|^Sender::.*autoresponder.*gmx.net.*")

	("robot.arpwatch" "^From:.*arpwatch@.*")

 	("junk" "^From:.*\\(virusalarm\\|spamcop\\|MAILER\\-DAEMON\\)@he.fdread.org.*")
	("e.fabian.slp" "^Delivered-To:.*fabian@he.fdread.org.*")

;; 	("junk" "^From:.*MAILER-DAEMON@gilean.luchs.at.*")

	("misc.lewiki" "^From.*moin@lefant.net.*")
	("misc.rss2email" "^User-Agent:.*rss2email.*")
	("misc.fussball" "^List-Id:.*fussball.lefant.net.*")
	("misc.techniq" "^\\(To\\|Cc\\):.*\\(techniq\\|qspot\\)@quintessenz.\\(at\\|org\\).*\\|^From:.*WikiAdmin.*<cyberkov@quintessenz.at>.*")

;;	("e.suspicious" "^X-Sender-Verify-Failed:.*\\|^X-Host-Lookup-Failed::.*")

	("robot.monit" "^X-Mailer: monit 4.5.1")
	("robot.nagios" "^From:.*nagios@.*")
	("robot.apticron" "^From:.*apticron@.*")
	("robot.cron" "^From:.*Cron.*Daemon.*\\|^From:.*Anacron.*root@.*")
	("robot.root" "^From:.*root@.*")
	("robot.tiger" "^From:.*Tiger.*root@.*")
	("robot.munin" "^From:.*munin@.*")
	("robot.logcheck" "^From:.*logcheck@.*")
	("robot.debianmirror" "^From:.*debmirror@rerun.lefant.net.*")

	("debienna.wiki" "^From:.*DebIenna.*debienna.at.*")
	("debienna.lists" "^List-Id:.*debian-at.gibraltar.at.*\\|^List-Id:.*debienna.rerun.lefant.net.*")

	("debian.bugs" "^X-Debian-PR-Message.*")
	("debian.packages" "^Delivered-To:.*@packages.qa.debian.org.*")


;; lists via regexps, if this works it will rock ;)
	("d.alioth.\\1" "^List-Id:.* <\\(.*\\).lists.alioth.debian.org.*")
	("d.l.\\1" "^List-Id:.* <\\(.*\\).lists.debian.org>$")

	("l.nongnu.\\1" "^List-Id: \\(.*\\).nongnu.org$")
	("l.sf.\\1" "^List-Id:.* <\\(.*\\).lists.sourceforge.net>$")
	("l.slp.\\1" "^List-Id:.* <\\(.*\\).slp.at>$")

	("l.wt.\\1" "^List-Id:.* <\\(.*\\).mailman.web-tech.at>$")
	("l.wt.trac" "^\\(To\\|Cc\\):.*trac@web-tech.at.*")


	("l.\\2.\\1" "^List-Id:.* <\\(.*\\)\\.\\(.*\\)>$")
	("l.\\2.\\1" "^List-Id:.* \\(.*\\)\\.\\(.*\\)$")


	("l.at.internetz" "^X-list:.*internetz.*")
	("l.at.agm" "^From:.*agm@agmarxismus.net.*")
	("l.at.al" "^Delivered-To:.*al_newsletter.*")



;; personal
	("e.lefant" "^\\(To\\|Cc\\):.*e@lefant.net.*")
	("e.linzberger.net" "^\\(To\\|Cc\\):.*fabian@linzberger.net.*")
	("e.inode.fabian" "^\\(To\\|Cc\\):.*fabian.linzberger@inode.at.*")
	("e.a0026057" "\\(To\\|Cc\\):.*a0026057@unet.univie.ac.at.*")
	("e.fabian.utv" "^\\(To\\|Cc\\):.*fabian@utv.at.*")
	("e.linzberger.gmx" "^X-Resent-By:.*forwarder@gmx.net.*")
	("e.linzberger.webde" "^X-WEBDE-FORWARD:.*linzberger@web.de.*")
	("e.quintessenz.lefant" "^To:.*lefant@quintessenz.org.*")

	("UNCATEGORIZED" "")))



(setq gnus-auto-expirable-newsgroups
      "robot\\.\\|misc\\.\\|d\\.\\|l\\.")

(setq nnmaildir-expiry-wait-function
      (lambda (group)
        (cond ((string-match "^e\\." group) 'never)
              ((string-match "^misc\\." group ) 7)
              ((string-match "^l\\." group) 7)
              ((string-match "^d\\." group)   7)
	      ((string-match "^robot\\." group) 3)
	      ((string-match "^UNCATEGORIZED" group) 7)
	      ((string-match "^junk" group) 3)
              (t 4))))



(setq gnus-use-cache 't)

(setq gnus-cacheable-groups ".*")
(setq gnus-uncacheable-groups "^nnmbox")
(setq gnus-cache-enter-articles '(ticked dormant unread))
