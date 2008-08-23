(require 'erc)
(load "~/.secret/erc_nickserv.el")


(setq erc-user-full-name "Fabian Linzberger")
(setq erc-user-information "http://lefant.net/")
(setq erc-auto-query 'buffer)



(setq erc-auto-discard-away t)
(setq erc-autoaway-idle-seconds 1200)
(setq erc-autoaway-message "once again i didn't chat for *a full 20 minutes or even longer*. amazing, isn't it?")


(setq erc-autojoin-channels-alist
           '(("oftc.net" "#debian.or.at")
             ("oftc.net" "#glob2")
;; 	    ("freenode.net" "#emacs")
;; 	    ("freenode.net" "#debian-mentors")
 	    ("freenode.net" "#metalab")))


(setq erc-modules (quote (autoaway autojoin button fill irccontrols log match netsplit noncommands notify pcomplete completion readonly ring scrolltobottom stamp track truncate)))
(erc-update-modules)


(setq erc-fill-function 'erc-fill-static)
(setq erc-fill-static-center 3)
(setq erc-fill-column 80)


(setq erc-log-insert-log-on-open t)
(setq erc-log-channels t)
(setq erc-log-channels-directory "~/irclogs/")
(setq erc-save-buffer-on-part t)
(setq erc-hide-timestamps nil)
(add-hook 'erc-insert-post-hook 'erc-save-buffer-in-logs)
(defadvice save-buffers-kill-emacs (before save-logs (arg) activate)
  (save-some-buffers t (lambda () (when (and (eq major-mode 'erc-mode)
                                             (not (null buffer-file-name)))))))
(add-hook 'erc-mode-hook '(lambda () (when (not (featurep 'xemacs))
                                       (set (make-variable-buffer-local
                                             'coding-system-for-write)
                                            'emacs-mule))))


(setq erc-input-line-position -1)


(setq erc-timestamp-only-if-changed-flag nil
      erc-timestamp-format "%H:%M "
      erc-insert-timestamp-function 'erc-insert-timestamp-left)


(setq erc-keywords '("\\bfabian\\b" "\\blinzberger\\b" "\\bLefant\\b" "\\bFabian\\b" "\\bLinzberger\\b"))
(setq erc-pals '("mariam" "Mariam" "Rhonda" "rhonda" "Alfie" "alfie" "float" "redtux" "Greek0" "greek0" "bijan" "lou" "maks" "wizard23" "enki" "danyx" "Dany_X" "DavidS" "nutmeg" "Rotty" "gundi" "symbiosis" "Symbiosis" "maxx" "markus" "koki" "CHS" "rotty" "xenus" "rotty-work" "rotty'" "giaco" "giac1" "jakob" "enki" "wizard23_" "acht" "erich" "cm" "cyberkov" "telehans" "lynx" "florentin" "mslunsky"))

(setq erc-current-nick-highlight-type 'nick)
(setq erc-current-pal-highlight-type 'keyword)
(setq erc-current-keyword-highlight-type 'keyword)


(setq erc-track-exclude-server-buffer t)
(setq erc-track-exclude-types '("JOIN" "PART" "QUIT" "NICK" "MODE"))
(setq erc-track-use-faces t)
(setq erc-track-faces-priority-list
      '(erc-current-nick-face erc-pal-face erc-keyword-face))
(setq erc-track-priority-faces-only 'all)



(setq erc-max-buffer-size 20000)
(setq erc-truncate-buffer-on-save t)



;;; Finally, connect to the networks.
(defun irc-maybe ()
  (when (y-or-n-p "IRC? ")
    (erc "irc.freenode.net" 6666 "lefant" "Fabian Linzberger" t "")
    (erc "irc.oftc.net" 6668 "lefant" "Fabian Linzberger" t "")
    (erc "localhost" 6667 "lefant" "Fabian Linzberger" t "")))

;;(irc-maybe)
