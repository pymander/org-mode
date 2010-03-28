;;; org-atom.el --- Atom export for Org-mode

;; Copyright (C) 2010 by David Maus

;; Author: David Maus <dmaus@ictsoc.de>
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

(require 'atom-syndication)
(require 'org-exp)
(eval-when-compile (require 'cl))

(defconst org-atom-uuid-regexp
  "^[[:xdigit:]]\\{8\\}\\(-[[:xdigit:]]\\{4\\}\\)\\{3\\}-[[:xdigit:]]\\{12\\}$"
  "Regular expression matching a uuid.")

(defconst org-atom-infile-options
  '(("FEED_MAP_ENTRIES" :feed-map-entries)
    ("FEED_ID" :feed-id)
    ("FEED_URL" :feed-url)
    ("FEED_CONTENT_URL" :feed-content-url)))

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

(defcustom org-atom-published-property-name "Created"
  "Name of property for publication date."
  :type 'string
  :group 'org-export-atom)

(defcustom org-atom-updated-property-name "Updated"
  "Name of property for date when entry was updated."
  :type 'string
  :group 'org-export-atom)

(defcustom org-atom-publish-content t
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
  (run-hooks 'org-export-first-hook)
  (let* ((opt-plist (org-combine-plists (org-default-export-plist)
					ext-plist
					(org-infile-export-plist)))
	 (feed-url (org-trim (or (plist-get opt-plist :feed-url) "")))
	 (feed-content-url (org-trim (or (plist-get opt-plist :feed-content-url)
					 "")))
	 (feed-id (org-trim (or (plist-get opt-plist :feed-id) feed-url)))
	 (feed-map-entries (org-trim (or (plist-get opt-plist :feed-map-entries)
					 "")))
	 (author (plist-get opt-plist :author))
	 (email (plist-get opt-plist :email))
	 (description (or (plist-get opt-plist :feed-description)
			  (plist-get opt-plist :description)))
	 (feed-title (or (plist-get opt-plist :feed-title)
			 (plist-get opt-plist :title)))
	 (feed-file (if (buffer-file-name)
			(concat
			 (if pub-dir pub-dir (file-name-directory
					      (buffer-file-name)))
			 (file-name-sans-extension
			  (file-name-nondirectory
			   (buffer-file-name)))
			 "." org-atom-feed-extension)))
	 (feed-publish-content (or (plist-get opt-plist :feed-publish-content)
				   org-atom-publish-content))
	 (feed-publish-tags (or
			     (plist-get opt-plist :feed-publish-category-tags)
			     org-atom-publish-category-tags))
	 (body-only (or body-only (plist-get opt-plist :body-only)))
	 (atom-syndication-construct-text-html-function 'org-atom-htmlize)
	 entries feed filebuf)
    ;; check mandatory options
    (when (and (not body-only) (string= feed-url ""))
      (error "Missing url for feed"))
    ;; atom entry w/o content MUST have link pointing to the content
    (when (or (not feed-publish-content) (string= feed-content-url ""))
      (error "Missing url for feed content"))
    ;; prepare headlines
    (when (and (not (string= feed-map-entries ""))
	       (> (length
		   (org-map-entries
		    'org-atom-prepare-headline feed-map-entries)) 0))
      (unless to-buffer
	(setq to-buffer (if feed-file
			    (or (find-buffer-visiting feed-file)
				(find-file-noselect feed-file))
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
	;; there are entries in this file
	(setq entries
	      (org-map-entries '(lambda ()
				  (append
				   (org-atom-export-headline
				    (concat feed-url ",")
				    feed-content-url
				    nil
				    feed-publish-content
				    feed-publish-tags)))
			       feed-map-entries))
	;; maybe add author
	(when body-only
	 (setq entries (mapcar '(lambda (e)
				  (append
				   (unless (assoc 'author e)
				     (list (list 'author author)))
				   e)
				  ))))
	(setq feed
	      (if body-only
		  (mapconcat 'atom-syndication-element-entry entries "\n")
		(atom-syndication-element-feed
		 (append
		  (unless (string= description "")
		    (list (list 'subtitle (org-trim description))))
		  (list
		   (list 'title (org-trim feed-title))
		   (list 'generator
			 org-atom-generator-name
			 org-atom-generator-version)
		   (list 'id (concat
			      (if (and org-atom-prefer-urn-uuid
				       (org-atom-looks-like-uuid-p feed-id))
				  "urn:uuid:" "") feed-id))
		   (list 'updated (if (buffer-file-name)
				      (nth 5 (file-attributes
					      (buffer-file-name)))
				      (current-time)))
		   (list 'link feed-url nil "self")
		   (list 'author author))
		  (mapcar '(lambda (entry)
			     (cons 'entry (list entry))) entries)))))
	(if (eq to-buffer 'string)
	    feed
	  (with-current-buffer to-buffer
	    (insert feed)
	    (if (buffer-file-name)
		(save-buffer))))))))

;;;###autoload
(defun org-atom-publish-feed-index (project &optional filename)
  "Publish feed for set of files in PROJECT.
Optional argument FILENAME is name of the output file.

This function collects all feed entries of all files in set
PROJECT and publishes them as one single atom feed."
  (let* ((project-plist (cdr project))
	 (dir (file-name-as-directory
	       (plist-get project-plist :base-directory)))
	 (exclude-regexp (plist-get project-plist :exclude))
	 (files (nreverse (org-publish-get-base-files project exclude-regexp)))
	 (index-filename (concat dir (or index-filename
					 (concat "feed."
						 org-atom-feed-extension))))
	 (index-title (or (plist-get project-plist :index-title)
			  (concat "Index for project " (car project))))
	 (pub-url (plist-get project-plist :publishing-url))
	 (feed-url (concat pub-url (if (string-match-p "/$" pub-url) "" "/")
			   index-filename))
	 (feed-id (or (plist-get project-plist :feed-id) feed-url))
	 (visiting (find-buffer-visiting index-filename))
	 file index-buffer)
    ;; maybe adjust publication url
    (unless (and pub-url (string-match-p "/$" pub-url))
      (setq pub-url (concat pub-url "/")))
    (setq project-plist (plist-put project-plist :feed-title index-title))
    (with-current-buffer (setq index-buffer
			       (or visiting (find-file index-filename)))
      (erase-buffer)
      (insert (concat "<?xml version=\"1.0\"?>\n"
		      (atom-syndication-element-feed
		       (append
			(list
			 (list 'title index-title)
			 (list 'id (concat
				    (if (and org-atom-prefer-urn-uuid
					     (org-atom-looks-like-uuid-p
					      feed-id))
					"urn:uuid:" "") feed-id))
			 (list 'updated (current-time))
			 (list 'link feed-url nil "self"))))))
      (re-search-backward "</feed>")
      (while (setq file (pop files))
	(let* ((entries-plist (org-combine-plists
			       project-plist
			       (plist-put nil :feed-content-url
					  (concat
					   pub-url
					   (file-relative-name
					    (file-name-sans-extension file) dir)
					   ".html"))))
	       (entries
		(with-current-buffer (or (find-buffer-visiting file)
					 (find-file-noselect file))
		  (org-export-as-atom entries-plist 'string t))))
	  (when entries (insert entries))))
      (save-buffer))
    (or visiting (kill-buffer index-buffer))))

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
			     pub-dir (plist-get plist :publishing-directory))
			    (if (string-match-p "/$" pub-dir) "" "/")
			    (file-name-sans-extension
			     (file-name-nondirectory
			      filename))))))
    ;; maybe set feed content and feed url
    (save-excursion
      (switch-to-buffer (or visiting (find-file filename)))
      (org-export-as-atom
       (org-combine-plists plist
			   (if pub-url-fse
			       (list
				:feed-url
				(concat pub-url-fse "." org-atom-feed-extension)
				:feed-content-url
				(concat pub-url-fse ".html")))) nil nil pub-dir)
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
	   (author (org-entry-get nil "EXPORT_AUTHOR"))
	   (elist
	    (append
	     (if published
		 (list (list 'published (org-time-string-to-time published))))
	     (if author
		 (list (list 'author author)))
	     (when publish-content
	       (let (beg end content)
		 (save-excursion
		   (org-back-to-heading)
		   (setq beg (point))
		   (outline-end-of-subtree)
		   (setq end (point))
		   (setq content (buffer-substring-no-properties beg end))
		   (list (list 'content content "html")))))
	     (if (and content-url (not (string= content-url "")))
		 (list
		  (list 'link (concat content-url "#ID-" id) nil "alternate")))
	     (if (and publish-tags tags)
		 (mapcar '(lambda (tag)
			    (list 'category tag))
		       (split-string tags ":")))
	     (list
	      (list 'title title (cons 'type "html"))
	      (list 'updated (org-time-string-to-time updated))
	      (list 'id (concat (if (and org-atom-prefer-urn-uuid
					 (org-atom-looks-like-uuid-p id))
				    "urn:uuid:" id-prefix) id))))))
      elist)))

(defun org-atom-prepare-headline (&optional pom)
  "Prepare headline at point or marker POM for export.

If POM is ommited, prepare headline at point."
  (save-excursion
    (goto-char (or pom (point)))
    (let ((id (org-id-get-create))
	  (dtime (or (org-entry-get nil org-atom-published-property-name)
		     (org-entry-get nil org-atom-updated-property-name))))
      (unless dtime
	(org-entry-put nil org-atom-published-property-name
		       (concat "["
			       (substring
				(format-time-string
				 (cdr org-time-stamp-formats))
				1 -1) "]"))))))

(defun org-atom-htmlize (string)
  "Return sanitized html markup for STRING."
  (with-temp-buffer
    (insert string)
    (org-mode)
    (atom-syndication-sanitize
     (org-export-region-as-html (point-min) (point-max) t 'string))))

(defun org-atom-looks-like-uuid-p (string)
  "Return non-nil if STRING looks like a uuid."
  (string-match-p org-atom-uuid-regexp string))

;; add infile options
(dolist (opt org-atom-infile-options)
  (unless (memq opt org-export-inbuffer-options-extra)
    (setq org-export-inbuffer-options-extra
	  (append (list opt) org-export-inbuffer-options-extra))))

(provide 'org-atom)

;;; org-atom.el ends here
