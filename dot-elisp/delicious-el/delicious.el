;; delicious.el --- functions to make productive use of the http://del.icio.us API

;; Copyright (C) 2004, 2005 John Sullivan

;; Author: John Sullivan <john@wjsullivan.net>
;; Created 25 October 2004
;; Version: 0.3 2005-11-07
;; Keywords: comm, hypermedia

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, a copy is available at
;; http://www.wjsullivan.net, or write to the Free
;; Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;; MA 02110-1301 USA

;;; Commentary

;; You should read the instructions in the source for `delicioapi.el' to get
;; the necessary setup instructions. This file is the collection of functions
;; I'm working on to actually put the API to productive use. Many things in
;; here are half-assed and half-finished, so it's probably not that productive
;; for anyone to contribute at the moment, but feature ideas and bug reports
;; are much appreciated.
;;
;; You don't need to add anything to your `.emacs' beyond what you already
;; added as per the instructions in `delicioapi.el' for these functions to
;; work. Just (require 'delicious).

;; Please report any bugs or suggestions to me at
;; john@wjsullivan.net. If enough people are interested, perhaps we
;; will open an area at http://www.emacswiki.org.

;;; Code:

(require 'delicioapi)

;;;;_+ Faces

(defface delicious-result-href-face
  '((t (:underline t :foreground "DeepSkyBlue1")))
  "Face for links in search results."
  :group 'delicious)

(defface delicious-result-description-face
  '((t (:foreground "SeaGreen2")))
  "Face for links in search results."
  :group 'delicious)

(defface delicious-result-extended-face
  '((t (:foreground "SeaGreen3")))
  "Face for links in search results."
  :group 'delicious)

(defface delicious-result-hash-face
  '((t (:foreground "DodgerBlue4")))
  "Face for the hash in search results."
  :group 'delicious)

(defface delicious-result-tag-face
  '((t (:foreground "LightCoral")))
  "Face for links in search results."
  :group 'delicious)

(defface delicious-result-time-face
  '((t (:foreground "DodgerBlue4")))
  "Face for timestamp in search results."
  :group 'delicious)

;;;;_+ Global stuff

(defconst delicious-version  "delicious.el/0.3 2005-11-07"
  "The version string for this copy of delicious.el.")

(defvar delicious-tags-list '()
  "Table of tags for use in completion.")

(defcustom delicious-xsel-prog nil
  "*The full path to a program that returns the X mouse selection, like xsel."
  :version "21.3.1"
  :group 'delicious
  :type 'string
  :tag "del.icio.us xsel program name")

(defcustom delicious-guess-url-methods 
  '(delicious-guess-check-point 
    delicious-guess-check-w3m 
    delicious-guess-check-buffer
    delicious-guess-check-selection
    delicious-guess-check-xsel
    delicious-guess-check-default)
  "*The list of methods to try, in order, to guess a URL to post."
  :version "21.3.1"
  :group 'delicious
  :type 'hook
  :tag "del.icio.us URL guess methods")

;;;;_+ Buffer management

(defun delicious-save-buffer (&optional no-bury)
  (let ((require-final-newline nil))
    (save-buffer)
    (unless no-bury
      (bury-buffer))))

(defun delicious-posts-file ()
  "Return the full path for `delicious-posts-file-name'."
  (let* ((home (getenv "HOME"))
         (posts-file (concat home "/" delicious-posts-file-name)))
    posts-file))

(defun delicious-get-posts-buffer ()
  "Switch to a buffer containing `delicious-posts-file-name'.
Return the buffer."
  (find-file (delicious-posts-file))
  (goto-char (point-min))
  (current-buffer))

(defun delicious-skip-timestamp ()
  (unless 
      (re-search-forward delicious-timestamp nil t)
    (message "No timestamp found, forcing refresh")
    (delicious-build-posts-list offline t)))

;;;;_+ Posting

;;;_+ Online and offline

(defun delicious-build-posts-list (&optional offline force )
  "Load local copy of posts, then update if server timestamp is newer.
If OFFLINE is non-nil, don't query the server.
If FORCE is non-nil, or if a prefix is given interactively, skip the
timestamp comparison and force a refresh from the server."
  (interactive)
  (if (and offline force)
      (error "Can't force an update while offline"))
  (cond (offline
	 (message "Reading local posts list..."))
        ((or force current-prefix-arg)
         (message "Reading posts from server..."))
        (t
         (message "Reading posts from last-modified source...")))
  (save-window-excursion
    (save-excursion
      (delicious-get-posts-buffer)
      (unless offline
        (when (or force
                  current-prefix-arg
                  (delicious-refresh-p))
          (erase-buffer)
          (delicious-update-timestamp)
          (mapc '(lambda (post)
                   (pp post (current-buffer)))
                (delicious-api-get-all))
          (delicious-save-buffer)))
      (message "Done."))))

(defun delicious-post (url description &optional tags extended time nolocal)
  "Post a url with arguments URL, DESCRIPTION, TAGS, EXTENDED, and TIME.
If NOLOCAL is non-nil, don't add the post to the local list."
  (interactive (list
                (delicious-read-url)
                (delicious-read-description)
                (delicious-complete-tags)
                (delicious-read-extended-description)
                (delicious-read-time-string)))
  (message "Waiting for server.")
  (delicious-api-post url description tags extended time)
  (let* ((hash (md5 url))
         (post (list
                (cons "href" url)
                (cons "description" description)
                (cons "hash" hash)
                (cons "tag" tags)
                (cons "extended" extended)
                (cons "time" time))))
    (unless nolocal (delicious-post-local post))
    (message "URL posted.")))

(defun delicious-post-local (post &optional offline)
  "Add POST to the local copy.
If OFFLINE is non-nil, don't update the local timestamp."
  (save-window-excursion
    (save-excursion
      (delicious-get-posts-buffer)
      (unless offline (delicious-update-timestamp))
      (goto-char (point-max))
      (pp post (current-buffer))
      (let ((tags (cdr (assoc "tag" post))))
	(delicious-rebuild-tags-maybe tags))
      (delicious-save-buffer))))

(defun delicious-check-input (input &optional name)
  "Verify that INPUT has content.  NAME is the name of the field being checked."
  (if (equal input "")
      (error "%s was a required field" name)
    input))

;;;_+ Offline

(defun delicious-post-offline (url description &optional tags extended time)
  "Input bookmarks to post later. Don't contact the server for anything."
  (interactive (list
                (delicious-read-url t)
                (delicious-read-description)
                (delicious-complete-tags nil nil nil nil nil t)
                (delicious-read-extended-description)
                (delicious-read-time-string)))
  (save-window-excursion
    (save-excursion
      (find-file delicious-cache-file)
      (goto-char (point-max))
      (let* ((hash (md5 url))
             (post (list (cons "href" url)
                         (cons "description" description)
                         (cons "hash" hash)
                         (cons "tag" (or tags nil))
                         (cons "extended" (or extended nil))
                         (cons "time" (or time nil)))))
        (prin1 post (current-buffer)))
      (delicious-save-buffer)))
  (when (y-or-n-p "Post another bookmark? ")
    (call-interactively 'delicious-post-offline))
  (message "Cache saved."))

(defun delicious-post-cache (&optional cache-file)
  "Post bookmarks from `delicious-cache-file', or CACHE-FILE if non-nil."
  (interactive)
  (save-window-excursion
    (save-excursion
      (find-file (or cache-file delicious-cache-file))
      (goto-char (point-min))
      (while (not (eobp))
        (let* ((delicious-cache-post (read (current-buffer)))
               (href (cdr (assoc "href" delicious-cache-post)))
               (description (cdr (assoc "description" delicious-cache-post)))
               (tags (cdr (assoc "tag" delicious-cache-post)))
               (extended (cdr (assoc "extended" delicious-cache-post)))
               (time (cdr (assoc "time" delicious-cache-post))))
          (delicious-api-post href description tags extended time)
          (delicious-post-local delicious-cache-post)
          (message "%s posted." description)
          (sleep-for 2)))))
  (when (y-or-n-p "Clear cache now? ")
    (save-excursion
      (let ((buf (or cache-file delicious-cache-file)))
        (find-file buf)
        (kill-buffer (current-buffer))
        (delete-file buf)))
    (message "Cache cleared.")))

(defun delicious-clear-cache (&optional cache-file)
  "Kill buffer visiting `delicious-cache-file' or CACHE-FILE, and delete file."
  (interactive)
  (save-excursion
    (let ((buf (or cache-file delicious-cache-file)))
      (find-file buf)
      (kill-buffer (current-buffer))
      (delete-file buf)))
  (message "Cache cleared."))

;;;_+ Timestamps

(defconst delicious-timestamp
  (concat
   "\\([1-9][0-9]\\{3\\}\\)-"		;year
   "\\([0-1][0-9]\\)-"			;month
   "\\([0-3][0-9]\\)T"			;day
   "\\([0-2][0-9]\\):"			;hour
   "\\([0-5][0-9]\\):"			;minute
   "\\([0-5][0-9]\\)Z")			;second
  "Regular expression matching the timestamp format.")
      
(defun delicious-read-time-string ()
  "Read a date string from a prompt and format it properly for the server.
 Use the current date and time if nothing entered."
  (let ((date (read-string "(Optional) Date/Time [yyyy-mm-dd hh:mm:ss]: ")))
    (if (equal date "") (setq date (delicious-format-time (current-time))))
    (unless 
        (string-match
         "^\\([1-9][0-9][0-9][0-9]\\).\\([0-1][0-9]\\).\\([0-9][0-9]\\).\\([0-9][0-9]\\).\\([0-5][0-9]\\).\\([0-5][0-9]\\)"
         date)
      (message "Incorrect time string format.")
      (sleep-for 1)
      (delicious-read-time-string))
    (let ((time-string (replace-match "\\1-\\2-\\3T\\4:\\5:\\6Z" t nil date)))
      time-string)))

(defun delicious-refresh-p ()
  "Return t if server timestamp is newer than local timestamp."
  (let ((server-timestamp (delicious-api-get-timestamp))
        (local-timestamp 
         (save-window-excursion
           (save-excursion
             (delicious-get-posts-buffer)
             (delicious-get-local-timestamp-value)))))
    (time-less-p local-timestamp server-timestamp)))

(defun delicious-get-local-timestamp ()
  "Return the timestamp recorded in the local posts as a string."
  (let ((local-timestamp 
         (if (re-search-forward delicious-timestamp nil t)
             (match-string 0))))
    local-timestamp))

(defun delicious-get-local-timestamp-value ()
  "Return the timestamp recorded in the local posts as a time value.
Return '(0) if there is no timestamp."
  (let* ((local-timestamp (delicious-get-local-timestamp))
         (time-value 
          (if (null local-timestamp)
              (list 0)
            (string-match delicious-timestamp local-timestamp)
            (encode-time
             (string-to-int
              (match-string 6 local-timestamp)) ;seconds
             (string-to-int
              (match-string 5 local-timestamp)) ;minutes
             (string-to-int
              (match-string 4 local-timestamp)) ;hours
             (string-to-int
              (match-string 3 local-timestamp)) ;days
             (string-to-int
              (match-string 2 local-timestamp)) ;month
             (string-to-int
              (match-string 1 local-timestamp)))))) ;year
    time-value))
         
(defun delicious-update-timestamp ()
  "Update or create the local timestamp in `delicious-posts-file-name'."
  ;; check to make sure we are in the right place
  (when (and 
         (eq (current-buffer) 
             (find-buffer-visiting (delicious-posts-file)))
         (bobp))
    (let ((time (delicious-format-time)))
      (if (looking-at delicious-timestamp)
          (replace-match time)
        (insert time)))
    (insert "\n")
    (delete-blank-lines)
    (delicious-save-buffer t)))

(defun delicious-format-time (&optional time)
  "Return TIME as a del.icio.us timestamp.
If TIME is not provided, use the server timestamp."
  (format-time-string "%Y-%m-%dT%H:%M:%SZ" 
                      (or time (delicious-api-get-timestamp))))

;;;;_+ URL input, guessing, and duplicate checking

(defun delicious-read-url (&optional offline)
  "Read a url from a prompt, suggesting an appropriate default.
Check the input to make sure it is valid and react if it is a duplicate.
If OFFLINE is non-nil, don't query the server for any information."
  (delicious-build-posts-list offline)
  (let ((url (delicious-check-input
              (read-string "(Required) URL: " (delicious-guess-url)) "URL")))
    (if (delicious-duplicate-url-p url)
        (delicious-post-duplicate-p url))
    url))

(defun delicious-guess-url ()
  (let ((methods delicious-guess-url-methods)
        (guess))
    (while (and methods 
                (not guess))
      (setq guess (funcall (pop methods))))
    guess))

(defun delicious-guess-check-w3m ()
  "If we're in a w3m buffer, use the current URL."
  (if (and (boundp 'w3m-current-url)
           w3m-current-url
           (eq major-mode 'w3m-mode))
      w3m-current-url))

(defun delicious-guess-check-point ()
  "If point is on a URL, return it."
  (if (thing-at-point-looking-at thing-at-point-url-regexp)
      (thing-at-point-url-at-point)))

(defun delicious-guess-check-buffer ()
  "Check the buffer for a URL and return it."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward thing-at-point-url-regexp nil t)
        (match-string 0))))

(defun delicious-guess-check-selection ()
  "Check the X selection for a URL and return it."
  (let ((selection)(url))
    (when (eq window-system 'x)
      (setq selection (condition-case nil
                          (x-get-selection)
                        (error "No `PRIMARY' selection") nil))
      (when (and selection
                 (string-match thing-at-point-url-regexp selection))
        (setq url (match-string-no-properties 0 selection))))))

(defun delicious-guess-check-xsel ()
  "Check output of `delicious-xsel-prog' for a URL."
  (when delicious-xsel-prog
    (let ((selection (shell-command-to-string delicious-xsel-prog)))
      (if (string-match thing-at-point-url-regexp selection)
          (match-string 0 selection)))))

(defun delicious-guess-check-default ()
  "Return some text to use for the URL guess."
  "http://")

(defun delicious-duplicate-url-p (url)
  "Check to see if URL is a duplicate. If so, return the post."
  (save-window-excursion
    (save-excursion
      (delicious-get-posts-buffer)
      (delicious-skip-timestamp)
      (let ((dup))
	(while (not (or dup
                        (eq
                         (condition-case nil
                             (let ((post (read (current-buffer))))
                               (if (member (cons "href" url) post)
                                   (setq dup post)))
                           (end-of-file t)) t))))
	dup))))

(defun delicious-post-duplicate-p (url)
  "Return t if existing post should be replaced with URL."
  (unless 
      (y-or-n-p 
       "This URL is a duplicate.\nIf you post it again, the old tags will be replaced by the new ones.\nPost? ")
    (error "Duplicate URL not posted")))

;;;;_+ Description input and suggestion

(defun delicious-read-description ()
  "Read a description from a prompt, suggesting an appropriate default."
  (delicious-check-input
   (read-string "(Required) Description: " (delicious-guess-description)) "Description"))

(defun delicious-guess-description ()
  "Try some different things to get a default description."
  (or
   (if (and (boundp 'w3m-current-url)
            (not (null w3m-current-url))
            (eq major-mode 'w3m-mode))
       (w3m-current-title))
   (if (memq major-mode '(gnus-summary-mode gnus-article-mode))
       (aref gnus-current-headers 1))))

;;;;_+ Extended description

(defun delicious-read-extended-description (&optional suggest truncated)
  "Read an extended description from a prompt."
  (let ((ext 
         (read-string 
          (concat 
           (when truncated
             "Trimmed to fit server limit of 253 characters; please edit or accept.\n")
           "(Optional) Extended Description: ") suggest)))
    (if (> (length ext) 253)
        (delicious-read-extended-description (substring ext 0 252) t)
      ext)))

;;;;_+ Tag completion, suggestion, and manipulation

(defun delicious-complete-tags (&optional nosuggest sofar quantity prompt-string
                                          require offline)
  "Get tags table if needed.  Do a completing read of tag input.
Blank line at the prompt ends the input.  If NOSUGGEST is non-nil, don't
suggest any tags. If SOFAR is non-nil, don't show tags entered so far.
Repeat the read QUANTITY times. If QUANTITY is nil, repeat until a blank
line is entered. If REQUIRE is non-nil, only a completion match or an empty string
are accepted as input. If OFFLINE is non-nil, don't contact the server."
  (unless delicious-tags-list
    (setq delicious-tags-list (delicious-build-tags-list t)))
  (let* ((base-prompt 
	  (or prompt-string
	      "(Enter one at a time, blank to end.) Tag: "))
	 (suggest-prompt
	  (unless nosuggest
	    (format "Suggested Tags: %s\n" (delicious-suggest-tags))))
	 (sofar-prompt
	  (unless sofar
	    "Tags so far: %s\n"))
	 (prompt (concat suggest-prompt sofar-prompt base-prompt)))
    (loop until (or (equal tag "")
                    (and
                     (numberp quantity)
                     (<= quantity 0)))
          for tag = (completing-read (format prompt tags)
                                     delicious-tags-list nil require)
          if (numberp quantity) do (setq quantity (1- quantity))
          if (member tag tags) do (and (message "Duplicate tag ignored.") 
                                       (sleep-for 1))
          else collect tag into tags
          finally return (progn
                           (message "%s" tags)
                           (mapconcat 'identity tags " ")))))

(defun delicious-rebuild-tags-maybe (tags)
  "If any tags in the space separated string TAGS are new, rebuild tags table."
  (let ((tags-list (split-string tags))
        (new))
    (mapc 
     (lambda (tag)
       (unless (assoc tag delicious-tags-list)
         (setq new t)))
     tags-list)
    (if new (delicious-build-tags-list))))

(defun delicious-build-tags-list (&optional offline)
  "Build the `delicious-tags-list' table of tags and and index number,
for use in completion. If OFFLINE is non-nil, don't query the server."
  (delicious-build-posts-list offline)
  (save-window-excursion
    (save-excursion
      (delicious-get-posts-buffer)
      (delicious-skip-timestamp)
      (setq delicious-tags-list
            (let ((index 0)
                  (tags-table '()))
              (while (not 
                      (eq
                       (condition-case nil
                           (let* 
                               ((post (read (current-buffer)))
                                (tags (split-string (cdr (assoc "tag" post)))))
                             (mapc
                              (lambda (tag) ; collect tags if new
                                (unless (assoc tag tags-table)
                                  (add-to-list 'tags-table (list tag index))
                                  (setq index (1+ index))))
                              tags))
                         (end-of-file t)) t)))
	      tags-table)))))

(defun delicious-suggest-tags ()
  "Suggest tags based on the contents of the current buffer and the current list of tags."
  (let ((buffer-words (delicious-buffer-words))
        (tags (loop for cell in delicious-tags-list
                    collect (downcase (car cell)) into tags
                    finally return tags)))
    (loop for word in buffer-words
          if (or (member word tags)
                 (member (concat word "s") tags)
                 (member (concat word "es") tags)
                 (if (and (> (length word) 1)
                          (or (member (substring word 0 -1) tags)
                              (if (and (equal (substring word -1) "y")
                                       (member (concat (substring word 0 -1) "ies") tags))
                                  t)))
                     t)
                 (if (and (> (length word) 2)
                          (member (substring word 0 -2) tags)) t))
          collect word into shared
          finally return shared)))
  
(defun delicious-buffer-words ()
  "Break the current buffer into a list of lowercase unique words."
  (save-excursion
    (goto-char (point-min))
    (loop until (eobp)
          with words = '()
          for word = (downcase (current-word))
          if (not (member word words))
	  collect word into words
	  end
          do (forward-word 1)
          finally return words)))
       
(defun delicious-rename-tag (old-tag new-tag)
  "Change all instances of OLD-TAG to NEW-TAG.
NEW-TAG can be multiple tags, space-separated."
  (interactive 
   (list
    (delicious-complete-tags t t 1 "Enter old tag: " t)
    (delicious-complete-tags
     t "Enter new tag(s) one at a time (blank to end): ")))
  (if (or (equal old-tag "")
          (equal new-tag ""))
      (message "Aborting due to empty input.")
    (message "Renaming...")
    (delicious-api-rename old-tag new-tag)
    (delicious-build-tags-list)
    (message "Done renaming %s to %s" old-tag new-tag)))

;;;;_+ Deleting and editing posts

(defun delicious-delete-post (href)
  "Delete the post with URL HREF."
  (interactive "sEnter URL to delete: ")
  (delicious-api-delete href)
  (delicious-delete-post-locally href)
  (message "%s deleted." href))

(defun delicious-delete-post-locally (href)
  "Delete the first local copy of the post with URL HREF."
  (save-window-excursion
    (delicious-get-posts-buffer)
    (re-search-forward (regexp-quote href))
    (beginning-of-line)
    (delete-region (point) (scan-sexps (point) 1))
    (delete-blank-lines)
    (delicious-update-timestamp)))

;;;;_+ w3m

(defun delicious-w3m-html (username count tag)
  "Visit the HTML page for USERNAME showing COUNT most recent posts under TAG.
With prefix, visit the page in a new w3m session."
  (interactive 
   "sUsername (RET for yours): \nsNumber of posts (RET for 15): \nsTag (RET for all): ")
  (let ((count (or (string-to-int count) 15)))
    (w3m-browse-url
     (format "http://%s%s%s" delicious-api-host delicious-api-html
             (delicious-api-html-uri username tag count))
     (not (null current-prefix-arg)))))

(defun delicious-w3m-bookmark-recent (count tag section)
  "Add your COUNT recent delicious posts under TAG to your w3m bookmarks file.
They will be stored under SECTION."
  (interactive
   "nNumber of recent posts to bookmark: \nsTag to filter by: \nsw3m bookmark section to use: ")
  (let ((delicious-list (delicious-api-get-recent tag count)))
    (loop for bookmark in delicious-list
          for url = (cdr (assoc "href" bookmark))
          for title = (cdr (assoc "description" bookmark))
          do (w3m-bookmark-write-file url title section)
          finally do (message "w3m bookmarks updated."))))

(defun delicious-w3m-export (section &optional tags extended time)
  "Export your w3m bookmarks from w3m SECTION to del.icio.us.
Optionally assign TAGS, an EXTENDED description, and TIME to the bookmarks."
  (interactive (list
                (delicious-w3m-read-section)
                (delicious-complete-tags t)
                (delicious-read-extended-description)
                (delicious-read-time-string)))
  (let* ((section-string (format "<h2>%s</h2>" section))
         (item-start "<li><a"))
    (save-excursion
      (with-temp-buffer
        (insert-file-contents w3m-bookmark-file)
        (goto-char (point-min))
        (re-search-forward section-string)
        (let* ((links 
                (loop until (looking-at w3m-bookmark-section-delimiter)
                      do (re-search-forward item-start)
                      for link = (progn
                                   (re-search-forward thing-at-point-url-regexp)
                                   (match-string 0))
                      do (re-search-forward ">")
                      for title = (buffer-substring 
                                   (point) (- (re-search-forward "</a>")  4))
                      for link = (cons link title)
                      collect link into links
                      do (beginning-of-line 2)
                      finally return links)))
          (loop for cell in links
                for link = (car cell)
                for title = (cdr cell)
                do (delicious-api-post link title tags extended time)
                do (message "%s posted." title)
                do (sleep-for 1)))))))

(defun delicious-w3m-read-section ()
  "Prompt for the name of a w3m bookmark section."
  (let* ((completions (w3m-bookmark-sections)))
    (completing-read "Section to export (required): " completions nil t)))

;;;;_+ Searching 

;;;_+ Search mode and results buffer

(defvar delicious-search-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [tab] 'delicious-search-next-result)
    (define-key map [(control ?i)] 'delicious-search-next-result)
    (define-key map [(shift tab)] 'delicious-search-previous-result)
    (unless (featurep 'xemacs)
      (define-key map [(shift iso-lefttab)]
	'delicious-search-previous-result)
      (define-key map [(shift control ?i)]
	'delicious-search-previous-result))
    (define-key map [(meta ?n)] 'delicious-search-next-result)
    (define-key map [(meta ?p)] 'delicious-search-previous-result)
    (define-key map [(? )] 'scroll-up)
    (define-key map "\C-?" 'scroll-down)
    (define-key map [?q] 'bury-buffer)
    map)
  "Keymap for function `delicious-search-mode'.")

(define-minor-mode delicious-search-mode
  "Toggle Delicious Search mode.
With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode.

When Delicious Search mode is enabled, the tab key
advances to the next search result."
  nil
  " Delicious Search"
  delicious-search-mode-map)

(defun delicious-search-buffer-prep ()
  "Prepare a *delicious search results* buffer for output."
  (unless (equal (buffer-name) "*delicious search results*")
    (switch-to-buffer-other-window
     (get-buffer-create "*delicious search results*")))
  (let ((inhibit-read-only t))
    (delete-region (point-min) (point-max)))
  (let ((view-read-only nil))(toggle-read-only 1)))

(defun delicious-search-buffer-finish (search-string matches)
  "Report search results in the *delicious search results* buffer.
SEARCH-STRING is the regexp pattern that was used in the search.
MATCHES is the number of matches found."
  (with-current-buffer "*delicious search results*"
    (let ((inhibit-read-only t))
      (goto-char (point-min))
      (insert (format "Your search for \"%s\" returned %d results.\n\n"
                      search-string matches)))
    (delicious-search-mode 1)))

(defun delicious-search-insert-match (post)
  "Insert POST with the fields propertized."
  (with-current-buffer "*delicious search results*"
    (let ((hash (cdr (assoc "hash" post)))
          (inhibit-read-only t))
      (dolist (cell post)
        (let ((map (make-sparse-keymap))
              (field (car cell))
              (content (cdr cell))
              face)
          (cond ((string= field "href")
                 (define-key map [mouse-2] 'browse-url-at-point)
                 (define-key map [(control ?m)] 'browse-url-at-point)
                 (define-key map [(?w)] 'delicious-search-who-else)
                 (define-key map [(?c)] 'delicious-search-copy-url)
                 (define-key map [(?d)] 'delicious-search-delete)
                 (setq face 'delicious-result-href-face))
                ((string= field "description")
                 (define-key map [(?w)] 'delicious-search-who-else)
                 (setq face 'delicious-result-description-face))
                ((string= field "hash")
                 (setq face 'delicious-result-hash-face))
                ((string= field "tag")
                 (define-key map [mouse-2] 
                   '(lambda () (interactive) 
                      (delicious-search-tags (word-at-point))))
                 (define-key map [(control ?m)] 
                   '(lambda () (interactive) 
                      (delicious-search-tags (word-at-point))))
                 (define-key map [(?a)] 'delicious-search-add-tags)
                 (define-key map [(?d)] 'delicious-search-delete-tags)
                 (define-key map [(?w)] 'delicious-search-who-else)
                 (setq face 'delicious-result-tag-face))
                ((string= field "time")
                 (define-key map [(?w)] 'delicious-search-who-else)
                 (setq face 'delicious-result-time-face))
                ((string= field "extended")
                 (define-key map [(?e)] 'delicious-search-edit-extended)
                 (define-key map [(?w)] 'delicious-search-who-else)
                 (setq face 'delicious-result-extended-face)))
          (if (string= content "") (setq content " "))
          (setq content (propertize content 'hash hash 'face face 'keymap map))
          (unless (string= field "hash")
            (insert content "\n"))))
      (insert "\n"))))

(defun delicious-search-next-result ()
  "Goto the next search result in a delicious search results list."
  (interactive)
  (if (thing-at-point-url-at-point)
      (goto-char (match-end 0)))
  (let ((last-match (match-beginning 0)))
    (re-search-forward thing-at-point-url-regexp nil t)
    (if (equal (match-beginning 0) last-match)
        (progn
          (goto-char (point-min))
          (delicious-search-next-result))
      (goto-char (match-beginning 0)))))

(defun delicious-search-previous-result ()
  "Goto the previous search result in a delicious search results list."
  (interactive)
  (if (thing-at-point-url-at-point)
      (goto-char (match-beginning 0)))
  (let ((last-match (match-beginning 0)))
    (re-search-backward thing-at-point-url-regexp nil t)
    (if (equal (match-beginning 0) last-match)
        (goto-char (point-max))
      (goto-char (match-beginning 0)))))

(defun delicious-search-who-else ()
  "Browse to the del.icio.us URL showing who else has bookmarked this post."
  (interactive)
  (let* ((hash (get-text-property (point) 'hash))
         (post (delicious-post-matching-hash hash))
         (href (cdr (assoc "href" post)))
         (url (format "http://%s/url?url=%s" delicious-api-host href)))
    (browse-url url)))

(defun delicious-search-copy-url ()
  "Copy the url under point to the kill ring, with no properties."
  (interactive)
  (let* ((beg (line-beginning-position))
         (end (line-end-position))
         (url (buffer-substring-no-properties beg end)))
    (kill-new url)
    (message url)))

(defun delicious-search-delete ()
  "Delete the post under point."
  (interactive)
  (let* ((hash (get-text-property (point) 'hash))
         (post (delicious-post-matching-hash hash))
         (href (cdr (assoc "href" post))))
    (delicious-delete-post href)))

;;;_+ Search by regexp

(defun delicious-search-posts-regexp (search-string)
  "Search DELICIOUS-POSTS-LIST for SEARCH-STRING, a regular expression.
Display the results in *delicious search results*.
If given a prefix, operate offline."
  (interactive "sEnter regexp search string: ")
  (let ((match)(matches)(match-count 0))
    (save-window-excursion
      (delicious-build-posts-list current-prefix-arg)
      (delicious-get-posts-buffer)
      (delicious-skip-timestamp) 

      (while (not (eq 
                   (condition-case nil
                       (let ((post (read (current-buffer))))
                         (mapc
                          '(lambda (field)
                             (if (string-match search-string (cdr field))
                                 (setq match post)))
                          post)
                         (when match
                           (setq match-count (1+ match-count))
                           (add-to-list 'matches match))
                         (setq match nil))
                     (end-of-file t)) t)))
      (bury-buffer))
    (delicious-search-buffer-prep)
    (mapc 
     '(lambda (post) 
        (delicious-search-insert-match post))
     matches)
    (delicious-search-buffer-finish search-string match-count)))

(defun delicious-search-description-regexp (search-string)
  "Search DELICIOUS-POSTS-LIST for descriptions matching SEARCH-STRING.
Display the results in *delicious search results*.
If given a prefix, operate offline."
  (interactive "sEnter regexp search string: ")
  (let ((match)(matches)(match-count 0))
    (save-window-excursion
      (delicious-build-posts-list current-prefix-arg)
      (delicious-get-posts-buffer)
      (delicious-skip-timestamp) 

      (while (not (eq
                   (condition-case nil
                       (let* ((post (read (current-buffer)))
                              (desc 
                               (or 
                                (cdr (assoc "description" post))
                                (error
                                 "Malformed bookmark missing description %s"
                                 post))))
                         (if (string-match search-string desc)
                             (setq match post))
                         (when match
                           (setq match-count (1+ match-count))
                           (add-to-list 'matches match))
                         (setq match nil))
                     (end-of-file t)) t)))
      (bury-buffer))
    (delicious-search-buffer-prep)
    (mapc
     '(lambda (post)
        (delicious-search-insert-match post))
     matches)
    (delicious-search-buffer-finish search-string match-count)))

(defun delicious-search-tags-regexp (search-string)
  "Search DELICIOUS-POSTS-LIST for tags matching SEARCH-STRING.
Display the results in *delicious search results*.
If given a prefix, operate offline."
  (interactive "sEnter regexp search string: ")
  (let ((match)(matches)(match-count 0))
    (save-window-excursion
      (delicious-build-posts-list current-prefix-arg)
      (delicious-get-posts-buffer)
      (delicious-skip-timestamp) 
      (while (not (eq
                   (condition-case nil
                       (let* ((post (read (current-buffer)))
                              (tags (or (cdr (assoc "tag" post))
                                        "")))
                         (if (string-match search-string tags)
                             (setq match post))
                         (when match
                           (setq match-count (1+ match-count))
                           (add-to-list 'matches match))
                         (setq match nil))
                     (end-of-file t)) t)))
      (bury-buffer))
    (delicious-search-buffer-prep)
    (mapc
     '(lambda (post)
        (delicious-search-insert-match post))
     matches)
    (delicious-search-buffer-finish search-string match-count)))

(defun delicious-search-href-regexp (search-string)
  "Search DELICIOUS-POSTS-LIST for urls matching SEARCH-STRING.
Display the results in *delicious search results*.
If given a prefix, operate offline."
  (interactive "sEnter regexp search string: ")
  (let ((match)(matches)(match-count 0))
    (save-window-excursion
      (delicious-build-posts-list current-prefix-arg)
      (delicious-get-posts-buffer)
      (delicious-skip-timestamp) 
      (while (not (eq
                   (condition-case nil
                       (let* ((post (read (current-buffer)))
                              (href 
                               (or (cdr (assoc "href" post))
                                   (error 
                                    "Malformed bookmark missing href %s" post))))
                         (if (string-match search-string href)
                             (setq match post))
                         (when match
                           (setq match-count (1+ match-count))
                           (add-to-list 'matches match))
                         (setq match nil))
                     (end-of-file t)) t)))
      (bury-buffer))
    (delicious-search-buffer-prep)
    (mapc
     '(lambda (post)
        (delicious-search-insert-match post))
     matches)
    (delicious-search-buffer-finish search-string match-count)))

;;;_+ Search by tag

(defun delicious-posts-matching-tags (tags)
  "Return a list of posts with TAGS."
  (let ((match)(matches)(match-count 0))
    (save-window-excursion
      (delicious-build-posts-list current-prefix-arg)
      (delicious-get-posts-buffer)
      (delicious-skip-timestamp) 
      (while (not (eq
                   (condition-case nil
                       (let* ((post (read (current-buffer)))
                              (tags (split-string tags))
                              (post-tags (split-string
                                          (or (cdr (assoc "tag" post)) " "))))
                         (setq match post)
                         (mapc
                          '(lambda (tag)
                             (unless (member tag post-tags)
                               (setq match nil)))
                          tags)
                         (when match
                           (setq match-count (1+ match-count))
                           (add-to-list 'matches match)
                           (setq match nil)))
                     (end-of-file t)) t)))
      (bury-buffer))
    matches))

(defun delicious-posts-matching-tags-any (tags)
  "Return a list of posts with any of TAGS."
  (let ((match)(matches)(match-count 0))
    (save-window-excursion
      (delicious-build-posts-list current-prefix-arg)
      (delicious-get-posts-buffer)
      (delicious-skip-timestamp) 
      (while (not (eq
                   (condition-case nil
                       (let* ((post (read (current-buffer)))
                              (tags (split-string tags))
                              (post-tags (split-string
                                          (or (cdr (assoc "tag" post)) " "))))
                         (while (and tags
                                     (null match))
                           (let ((tag (pop tags)))
                             (if (member tag post-tags)
                                 (setq match post))))
                         (when match
                           (setq match-count (1+ match-count))
                           (add-to-list 'matches match)
                           (setq match nil)))
                     (end-of-file t)) t)))
      (bury-buffer))
    matches))

(defun delicious-search-tags (tags)
  "Display all posts with TAGS. If given a prefix, operate offline."
  (interactive (list (delicious-complete-tags t)))
  (let* ((matches (delicious-posts-matching-tags tags))
         (match-count (length matches)))
    (delicious-search-buffer-prep)
    (mapc
     '(lambda (post)
        (delicious-search-insert-match post))
     matches)
    (delicious-search-buffer-finish tags match-count)))

;;;_+ Search by date

(defun delicious-posts-narrow-by-date (posts search-date)
  "Out of POSTS, return those that match regexp SEARCH-DATE."
  (let ((matches))
    (mapc
     (lambda (post)
       (let ((post-date (cdr (assoc "time" post))))
         (if (string-match search-date post-date)
             (add-to-list 'matches post))))
     posts)
    matches))

(defun delicious-posts-matching-date (search-date &optional offline)
  "Return all posts that match regexp SEARCH-DATE."
  (let ((matches)(match))
    (save-window-excursion
      (delicious-build-posts-list offline)
      (delicious-get-posts-buffer)
      (delicious-skip-timestamp)
      (while (not (eq (condition-case nil
			  (let* ((post (read (current-buffer)))
				 (date (cdr (assoc "time" post))))
			    (when (string-match search-date date)
			      (setq match post)
			      (add-to-list 'matches match)))
			(end-of-file t)) t)))
      (bury-buffer))
    matches))

;;;_+ Search by hash

(defun delicious-search-hash (search-hash)
  "Display the post with a hash matching SEARCH-HASH.
If given a prefix, work offline only."
  (interactive "sEnter the hash: ")
  (let ((match) (match-count 0))
    (setq match (delicious-post-matching-hash search-hash current-prefix-arg))
    (if match (setq match-count 1))
    (delicious-search-buffer-prep)
    (delicious-search-insert-match match)
    (delicious-search-buffer-finish search-hash match-count)))

(defun delicious-post-matching-hash (search-hash &optional offline)
  (let ((match))
    (save-window-excursion
      (delicious-build-posts-list offline)
      (delicious-get-posts-buffer)
      (delicious-skip-timestamp)
      (while (not (or match 
                      (eq (condition-case nil
                              (let* ((post (read (current-buffer)))
                                     (hash (cdr (assoc "hash" post))))
                                (when (string= hash search-hash)
                                  (setq match post)
                                  (setq match-count 1)))
                            (end-of-file t)) t))))
      (bury-buffer))
    match))

;;;_+ Editing posts

(defun delicious-search-add-tags (tags update)
  "Add TAGS to the post under point in Delicious Search mode.
If UPDATE is non-nil, update the post's timestamp."
  (interactive (list (delicious-complete-tags t t nil nil nil)
                     (y-or-n-p "Update timestamp? ")))
  (let* ((hash (get-text-property (point) 'hash))
         (post (progn
                 (save-window-excursion
                   (delicious-get-posts-buffer)
                   (search-forward hash)
                   (search-backward "href")
                   (beginning-of-line)
                   (read (current-buffer)))))
         (current-tags (cdr (assoc "tag" post)))
         (href (cdr (assoc "href" post)))
         (desc (cdr (assoc "description" post)))
         (extended (or (cdr (assoc "extended" post)) ""))
         (old-time (cdr (assoc "time" post)))
         (new-time (if update (delicious-format-time (current-time))
                     old-time))
         (new-tags (concat current-tags " " tags)))
    (delicious-post href desc new-tags extended new-time t)
    (with-current-buffer 
        (save-window-excursion (delicious-get-posts-buffer))
      (delicious-rebuild-tags-maybe new-tags))
    ;; rewrite the old record with the new record
    (let* ((new-fields (list (cons "tag" new-tags)
                             (cons "time" new-time)))
           (edit-post (delicious-edit-post-locally hash new-fields))
           (beg (search-backward href))
           (end (search-forward old-time))
           (inhibit-read-only t))
      (delete-region beg end)
      (delicious-search-insert-match edit-post)
      (delete-blank-lines)
      (search-backward new-tags))))

(defun delicious-search-delete-tags (tags update)
  "Delete TAGS from the post under point in Delicious Search mode.
If UPDATE is non-nil, update the post's timestamp."
  (interactive (list (delicious-complete-tags t t nil nil nil)
                     (y-or-n-p "Update timestamp? ")))
  (let* ((hash (get-text-property (point) 'hash))
         (post (delicious-post-matching-hash hash t))
         (old-tags (split-string (cdr (assoc "tag" post))))
         (href (cdr (assoc "href" post)))
         (desc (cdr (assoc "description" post)))
         (extended (or (cdr (assoc "extended" post)) ""))
         (old-time (cdr (assoc "time" post)))
         (new-time (if update (delicious-format-time (current-time))
                     old-time))
         (delete-tags (split-string tags))
         (new-tags))
    (setq new-tags
          (mapconcat #'identity
                     (delq nil
                           (mapcar
                            (lambda (tag)
                              (unless (member tag delete-tags) tag))
                            old-tags))
                     " "))
    (delicious-post href desc new-tags extended new-time t)
    (with-current-buffer 
        (save-window-excursion (delicious-get-posts-buffer))
      (delicious-rebuild-tags-maybe new-tags))
    (let* ((new-fields (list (cons "tag" new-tags)
                             (cons "time" new-time)))
           (edit-post (delicious-edit-post-locally hash new-fields))
           (beg (search-backward href))
           (end (search-forward old-time))
           (inhibit-read-only t))
      (delete-region beg end)
      (delicious-search-insert-match edit-post)
      (delete-blank-lines)
      (search-backward new-tags))))

(defun delicious-search-edit-extended ()
  "Edit the extended description under point."
  (interactive)
  (let* ((hash (get-text-property (point) 'hash))
         (post (delicious-post-matching-hash hash))
         (ext (cdr (assoc "extended" post)))
         (time (cdr (assoc "time" post)))
         (href (cdr (assoc "href" post)))
         (desc (cdr (assoc "description" post)))
         (tag (cdr (assoc "tag" post)))
         (new-ext (delicious-read-extended-description ext))
         (update-p (y-or-n-p "Update timestamp? "))
         (new-time (if update-p (delicious-format-time (current-time)) time)))
    (delicious-post href desc tag new-ext new-time t)
    (let* ((new-fields (list (cons "extended" new-ext)
                             (cons "time" new-time)))
           (edit-post (delicious-edit-post-locally hash new-fields))
           (beg (search-backward href))
           (end (search-forward time))
           (inhibit-read-only t))
      (delete-region beg end)
      (delicious-search-insert-match edit-post)
      (delete-blank-lines)
      (search-backward new-ext))))

(defun delicious-edit-post-locally (hash fields)
  "Replace old information in local copy of post with HASH using FIELDS.
FIELDS is a list of cons cells, with each cell being a field name and content.
Returns the updated post."
  (save-window-excursion
    (delicious-get-posts-buffer)
    (search-forward hash)
    (goto-char (line-end-position))
    (up-list -1)
    (let ((beg (point))
          (end (scan-sexps (point) 1))
          (post (read (current-buffer))))
      (delete-region beg end)
      (mapc
       (lambda (cell)
         (let ((target (car cell))
               (content (cdr cell)))
           (setcdr (assoc target post) content)))
       fields)
      (pp post (current-buffer)) 
      (delete-blank-lines)
      (backward-sexp)
      (setq post (read (current-buffer)))
      (delicious-update-timestamp)
      post)))

(provide 'delicious)
         
;;; delicious.el ends here
