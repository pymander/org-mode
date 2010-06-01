;;; org-atom.el --- Atom export for Org-mode

;; Copyright (C) 2010 by David Maus

;; Author: David Maus <dmaus [at] ictsoc.de>
;; Keywords: outlines, hypermedia
;; Version: 0.1beta
;;
;; This file is NOT part of Gnu Emacs.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This library implements an exporter to the Atom Syndication Format
;; (RFC 4287) for Org mode.

(require 'org-exp)
(eval-when-compile (require 'cl))

(declare-function atom-syndication-element-feed "ext:atom-syndication"
		  (attr elements))
(declare-function atom-syndication-sanitize "ext:atom-syndication"
		  (text))

(defvar atom-syndication-construct-text-html-function)

(defconst org-atom-infile-options
  '(("FEED_MAP_ENTRIES" :feed-map-entries)
    ("FEEd_ID" :feed-id)
    ("FEED_URL" :feed-url)
    ("FEED_CONTENT_URL" :feed-content-url)
    ("FEED_TITLE" :feed-title)
    ("FEED_DESCRIPTION" :feed-description)))

(defconst org-atom-generator-name "Org/Atom"
  "Name of the atom generator.")

(defconst org-atom-generator-version "0.1beta"
  "Version string of the atom generator.")

(defgroup org-export-atom nil
  "Options specific for Atom export of Org-mode files."
  :tag "Org Export Atom"
  :group 'org-export)

(defcustom org-atom-feed-extension "atom"
  "Extension of feed output file."
  :type 'string
  :group 'org-export-atom)

(defcustom org-atom-published-property-name "atom_published"
  "Name of property for publication date."
  :type 'string
  :group 'org-export-atom)

(defcustom org-atom-updated-property-name "atom_updated"
  "Name of property for date when entry was updated."
  :type 'string
  :group 'org-export-atom)

(defcustom org-atom-publish-content nil
  "Publish feed content.

If unset only publish link to content."
  :type 'boolean
  :group 'org-export-atom)

(defcustom org-atom-prefer-urn-uuid t
  "Create iri with urn:uuid prefix when id looks like a uuid."
  :type 'boolean
  :group 'org-export-atom)

(defcustom org-atom-publish-category-tags t
  "When non-nil, publish headline tags as category element."
  :type 'boolean
  :group 'org-export-atom)

(defcustom org-atom-try-prepare-headline-git nil
  "When non-nil, try to get headline creating date with git.")

;;;###autoload
(defun org-export-as-atom (&optional ext-plist to-buffer body-only pub-dir)
  "Export outline as atom feed.

EXT-PLIST is a property list with external parameters overriding
org-mode's default settings, but still inferior to file-local
settings.
When TO-BUFFER is non-nil, create a buffer with that name and
export to that buffer.  If TO-BUFFER is the symbol `string',
don't leave any buffer behind but just return the resulting atom
feed as a string.
When BODY-ONLY is set, return only the atom:entry elements.
When PUB-DIR is set, use this as the publishing directory."
  (interactive)
  (require 'atom-syndication)
  (run-hooks 'org-export-first-hook)
  (let* ((opt-plist (org-combine-plists (org-default-export-plist)
					ext-plist
					(org-infile-export-plist)))
	 (atom-url (org-trim (or (plist-get opt-plist :feed-url) "")))
	 (atom-content-url (org-trim (or (plist-get opt-plist :feed-content-url)
					 "")))
	 (atom-id (org-trim (plist-get opt-plist :feed-id)))
	 (atom-map-entries (org-trim (or (plist-get opt-plist :feed-map-entries)
					 "")))
	 (author (plist-get opt-plist :author))
	 (email (plist-get opt-plist :email))
	 (description (or (plist-get opt-plist :feed-description)
			  (plist-get opt-plist :description)))
	 (atom-title (or (plist-get opt-plist :feed-title)
			 (plist-get opt-plist :title)))
	 (atom-file (if (buffer-file-name)
			(concat
			 (if pub-dir pub-dir (file-name-directory
					      (buffer-file-name)))
			 (file-name-sans-extension
			  (file-name-nondirectory
			   (buffer-file-name)))
			 "." org-atom-feed-extension)))
	 (atom-publish-content (or (plist-get opt-plist :feed-publish-content)
				   org-atom-publish-content))
	 (atom-publish-tags (or
			     (plist-get opt-plist :feed-publish-category-tags)
			     org-atom-publish-category-tags))
	 (atom-publish-email (or (plist-get opt-plist :email-info)
				 org-export-email-info))
	 (body-only (or body-only (plist-get opt-plist :body-only)))
	 (atom-syndication-construct-text-html-function 'identity)
	 entries feed filebuf)
    ;; prepare headlines
    (when (and (not (string= atom-map-entries ""))
	       (> (length
		   (org-map-entries
		    'org-atom-prepare-headline atom-map-entries)) 0))
      ;; check mandatory options
      (when (and (not body-only) (string= atom-url ""))
	(error "Missing url for feed"))
      ;; atom entry w/o content MUST have link pointing to the content
      (when (and (not atom-publish-content) (string= atom-content-url ""))
	(error "Missing url for feed content"))
      (unless to-buffer
	(setq to-buffer (if atom-file
			    (or (find-buffer-visiting atom-file)
				(find-file-noselect atom-file))
			  (error "Need a file name to be able to export")))
	(with-current-buffer to-buffer (erase-buffer)))
      ;; maybe save modified headlines
      (save-buffer)
      (setq filebuf (buffer-string))
      (with-temp-buffer
	(insert filebuf)
	(org-mode)
	;; honour export tags
	(org-export-handle-export-tags (or (plist-get opt-plist :select-tags)
					   org-export-select-tags)
				       (or (plist-get opt-plist :exclude-tags)
					   org-export-exclude-tags))
	(message "Exporting...")
	(setq entries
	      (org-map-entries '(lambda ()
				  (append
				   (org-atom-export-headline
				    (concat atom-url ",")
				    atom-content-url
				    nil
				    atom-publish-content
				    atom-publish-tags)))
			       atom-map-entries))
	;; maybe add author
	(when body-only
	  (setq entries (mapcar '(lambda (e)
				   (append
				    (unless (assoc 'author e)
				      (list (if atom-publish-email
						(list 'author nil author email)
					      (list 'author nil author))))
				    e)))))
	(setq feed
	      (if body-only
		  (mapconcat 'atom-syndication-element-entry entries "")
		(atom-syndication-element-feed nil
		 (append
		  (unless (string= description "")
		    (list (list 'subtitle nil (org-trim description))))
		  (list
		   (list 'title nil (org-trim atom-title))
		   (list 'generator nil
			 org-atom-generator-name
			 org-atom-generator-version)
		   (list 'id nil (concat
				  (if (and org-atom-prefer-urn-uuid
					   (org-uuidgen-p atom-id))
				      "urn:uuid:" "") atom-id))
		   (list 'updated nil (if (buffer-file-name)
					  (nth 5 (file-attributes
						  (buffer-file-name)))
					(current-time)))
		   (list 'link nil atom-url nil "self")
		   (if atom-publish-email
		       (list 'author nil author email)
		     (list 'author nil author)))
		  (mapcar '(lambda (entry)
			     (cons 'entry (list nil entry))) entries)))))
	(if (eq to-buffer 'string)
	    feed
	  (with-current-buffer to-buffer
	    (insert feed)
	    (if (buffer-file-name)
		(save-buffer))))))))

;;;###autoload
(defun org-atom-publish-feed-sitemap (project &optional filename)
  "Publish feed for set of files in PROJECT.
Optional argument FILENAME is name of the output file.

This function collects all feed entries of all files in set
PROJECT and publishes them as one single atom feed."
  (let* ((project-plist (cdr project))
	 (dir (file-name-as-directory
	       (plist-get project-plist :base-directory)))
	 (exclude-regexp (plist-get project-plist :exclude))
	 (include-files (plist-get project-plist :include))
	 (files (append
		 include-files
		 (nreverse
		  (org-publish-get-base-files project exclude-regexp))))
	 (sitemap-filename (concat dir
				   (or (plist-get project-plist :sitemap-file)
				       (concat "feed."
					       org-atom-feed-extension))))
	 (sitemap-title (or (plist-get project-plist :sitemap-title)
			    (plist-get project-plist :feed-title)
			    (concat "Index for project " (car project))))
	 (pub-url (plist-get project-plist :publishing-url))
	 (atom-url (concat pub-url (if (string-match-p "/$" pub-url) "" "/")
			   sitemap-filename))
	 (atom-id (plist-get project-plist :feed-id))
	 (visiting (find-buffer-visiting sitemap-filename))
	 file sitemap-buffer)
    ;; maybe adjust publication url
    (unless (and pub-url (string-match-p "/$" pub-url))
      (setq pub-url (concat pub-url "/")))
    (setq project-plist (plist-put project-plist :feed-title sitemap-title)
	  project-plist (plist-put
			 project-plist :feed-description sitemap-description))
    (with-current-buffer (setq sitemap-buffer
			       (or visiting (find-file sitemap-filename)))
      (erase-buffer)
      (insert (concat "<?xml version=\"1.0\"?>"
		      (atom-syndication-element-feed
		       nil
		       (append
			(list
			 (list 'title nil sitemap-title)
			 (list 'id nil (concat
					(if (and org-atom-prefer-urn-uuid
						 (org-uuidgen-p
						  atom-id)
						 "urn:uuid:" "") atom-id))
			       (list 'updated nil (current-time))
			       (list 'link nil atom-url nil "self"))))))
	      (re-search-backward "</feed>")
	      (while (setq file (pop files))
		(let* ((entries-plist (org-combine-plists
				       project-plist
				       (plist-put nil :feed-content-url
						  (concat
						   pub-url
						   (file-relative-name
						    (file-name-sans-extension
						     file) dir)
						   (or
						    (plist-get
						     project-plist
						     :feed-content-extension)
						    ".html")))))
		       (visiting-file (find-buffer-visiting file))
		       entries)
		  (with-current-buffer (or visiting-file
					   (find-file-noselect file))
		    (setq entries (org-export-as-atom entries-plist 'string t))
		    (unless visiting-file (kill-buffer)))
		  (when entries (insert entries))))
	      (save-buffer))
      (or visiting (kill-buffer sitemap-buffer)))))

;;;###autoload
(defun org-publish-org-to-atom (plist filename pub-dir)
  "Publish an org file to atom.

PLIST is the property list for the given project.
FILENAME is the filename of the org file to be published.
PUB-DIR is the publishing directory."
  (require 'org)
  (unless (file-exists-p pub-dir)
    (make-directory pub-dir t))
  (let* ((visiting (find-buffer-visiting filename))
	 (pub-url-base (plist-get plist :publishing-url))
	 (pub-url-fse (and pub-url-base
			   (concat
			    pub-url-base
			    (if (string-match-p "/$" pub-url-base) "" "/")
			    (file-relative-name
			     (file-name-sans-extension
			      filename) (plist-get plist :base-directory))))))
    ;; maybe set feed content and feed url
    (save-excursion
      (switch-to-buffer (or visiting (find-file-noselect filename)))
      (org-export-as-atom
       (org-combine-plists plist
			   (if pub-url-fse
			       (list
				:feed-url
				(concat pub-url-fse "." org-atom-feed-extension)
				:feed-content-url
				(concat pub-url-fse
					(or (plist-get
					     plist :feed-content-extension)
					    ".html"))))) nil nil pub-dir)
      (unless visiting
	(kill-buffer)))))

(defun org-atom-export-headline (id-prefix content-url &optional
					   pom publish-content
					   publish-tags)
  "Return atom:entry alist for headline.

ID-PREFIX is a string that is used as prefix for the atom:id
element.
CONTENT-URL is a url pointing on the published html file.
Optional argument POM is point or marker of headline.  If not
set, export headline at point.
If optional argument PUBLISH-CONTENT is non-nil, publish subtree
of headline as feed entry content.
If optional argument PUBLISH-TAGS is non-nil, publish headline
tags as entry category terms."
  (save-excursion
    (goto-char (or pom (point)))
    (let* ((comps (org-heading-components))
	   (title (nth 4 comps))
	   (tags (and (nth 5 comps) (substring (nth 5 comps) 1 -1)))
	   (id (org-id-get))
	   (published (org-entry-get nil org-atom-published-property-name))
	   (updated (or (org-entry-get nil org-atom-updated-property-name)
			published))
	   (author (or (org-entry-get nil "atom_author")))
	   (href_alternate (org-entry-get nil "atom_href_alternate"))
	   (href_via (org-entry-get nil "atom_href_via"))
	   (href_related
	    (org-entry-get-multivalued-property nil "atom_href_related")))
      (append
       (if published
	   (list
	    (list 'published nil (org-time-string-to-time published))))
       (if author
	   (list (list 'author nil author)))
       (when publish-content
	 (let (beg end content)
	   (save-excursion
	     (org-back-to-heading)
	     (setq beg (point))
	     (outline-end-of-subtree)
	     (setq end (point))
	     (setq content (buffer-substring-no-properties beg end))
	     (list (list 'content nil
			 (org-atom-htmlize
			  content (file-name-directory content-url)) 'html)))))
       (if href_alternate
	   (list
	    (list 'link nil href_alternate nil 'alternate))
	 (if (and content-url (not (string= content-url "")))
	     (list
	      (list
	       'link nil (concat content-url "#ID-" id) nil "alternate"))))
       (if href_via
	   (list
	    (list 'link nil href_via nil 'via)))
       (if href_related
	   (mapcar '(lambda (url)
		      (list 'link nil url nil 'related))
		   href_related))
       (if (and publish-tags tags)
	   (mapcar '(lambda (tag)
		      (list 'category nil tag))
		   (split-string tags ":")))
       (list
	(list 'title
	      (list
	       (cons 'type 'html))
	      (org-atom-htmlize title (file-name-directory content-url)))
	(list 'updated nil (org-time-string-to-time updated))
	(list 'id nil (concat (if (and org-atom-prefer-urn-uuid
				       (org-uuidgen-p id))
				  "urn:uuid:" id-prefix) id)))))))

(defun org-atom-prepare-headline (&optional pom)
  "Prepare headline at point or marker POM for export.

If POM is ommited, prepare headline at point."
  (save-excursion
    (goto-char (or pom (point)))
    (let ((id (org-id-get-create))
	  (dtime (or (org-entry-get nil org-atom-published-property-name)
		     (org-entry-get nil org-atom-updated-property-name))))
      (unless dtime
	(org-entry-put nil org-atom-updated-property-name
		       (concat "["
			       (substring
				(format-time-string
				 (cdr org-time-stamp-formats)
				 (or (if org-atom-try-prepare-headline-git
					 (org-atom-prepare-headline-try-git))
				     (current-time)))
				1 -1) "]"))))))

(defun org-atom-prepare-headline-try-git ()
  "Return date when headline at point was last modified.

Return nil if calling git blame on current file failes."
  (let* ((comps (org-heading-components))
	 (file (buffer-file-name))
	 (re
	  (format "^\t\\*\\{%d\\}[ \t]+%s"
		  (nth 0 comps) (regexp-quote (nth 4 comps)))))
    (with-temp-buffer
      (cd (file-name-directory file))
      (let ((git
	     (shell-command (format "git blame -p %s" file) t)))
	(goto-char (point-min))
	(when (and (re-search-forward re nil t)
		   (re-search-backward
		    "^\\([[:xdigit:]]\\{40\\}\\)\\( [[:digit:]]+\\)\\{2\\}"))
	  (goto-char (point-min))
	  (when (re-search-forward (format "^%s" (match-string 1)))
	    (when (re-search-forward "^author-time \\([[:digit:]]+\\)")
	      (seconds-to-time (string-to-number (match-string 1))))))))))

(defun org-atom-htmlize (string url)
  "Return sanitized html markup for STRING.
URL is a string with a url that is used to resolve relative
links."
  (let* ((tmpfile (make-temp-file "org-atom-"))
	 (tmpbuf (find-file-noselect tmpfile))
	 html)
    (with-current-buffer tmpbuf
      (insert string)
      (org-mode)
      (save-buffer)
      (setq html (atom-syndication-sanitize
		  (org-export-region-as-html
		   (point-min) (point-max) t 'string))))
    (kill-buffer tmpbuf)
    (replace-regexp-in-string "\\(src\\|href\\)=\"\\([^:\"]+\\)"
			      (concat "\\1=\"" url "/\\2\"") html)))

;; add infile options
(dolist (opt org-atom-infile-options)
  (unless (memq opt org-export-inbuffer-options-extra)
    (setq org-export-inbuffer-options-extra
	  (append (list opt) org-export-inbuffer-options-extra))))

(provide 'org-atom)

;;; org-atom.el ends here
