;;; ox-blorg.el --- Your blog in Org -*- lexical-binding: t; -*-

;; ox-blorg.el, Your blog in Org
;; Copyright (C) 2025  Nikolaos Chatzikonstantinou
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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; Copyright (C) 2025 Nikolaos Chatzikonstantinou <nchatz314@gmail.com>
;; Author: Nikolaos Chatzikonstantinou
;; URL: https://github.com/createyourpersonalaccount/ox-blorg
;; Created: 2025
;; Version: 1.1.3
;; Keywords: outlines org blog
;; Package-Requires: ((emacs "30.1"))

;;; Commentary:
;;
;; This mode builds on ox-html to allow you to publish a spiffy blog
;; with Org-publish.

;;; Code:

;;; Dependencies

(require 'org-macs)
(org-assert-version)

(require 'ox)
(require 'ox-publish)

(defgroup ox-blorg nil
  "Your blog in Org."
  :tag "Org Export Blog"
  :group 'org-export)

;;; Ox-Blorg HTML backend

;;;; Auxiliary

(defmacro ox-blorg-html-aux-append-symbols (&rest symbols)
  "Append an arbitrary number of SYMBOLS into a new symbol."
  `(intern
    (apply 'concat
           (mapcar #'symbol-name
                   ',symbols))))

(defmacro ox-blorg-html-aux-$<> (element info string)
  "Encloses STRING in ELEMENT from template."
  (let ((template-func (make-symbol "template-func"))
        (info-var (make-symbol "info-var")))
    `(let ((,template-func
            (ox-blorg-html-aux-append-symbols ox-blorg-html-template-
                                              ,element))
           (,info-var ,info))
       (concat
        (funcall ,template-func 'begin ,info-var)
        ,string
        (funcall ,template-func 'end ,info-var)))))

(defun ox-blorg-html-aux-title (info)
  "Get the blog title."
  (let ((title (org-export-data (plist-get info :title) info)))
    (if (string-empty-p title)
        "&lrm;"                         ; <title> shouldn't be empty
      title)))

(defun ox-blorg-ensure-suffix (suffix string)
  "Append SUFFIX to STRING unless STRING already ends in SUFFIX."
  (if (string-suffix-p suffix string)
      string
    (concat string suffix)))

(defun ox-blorg-time-element-from-timestamp (timestamp info)
  "Get an HTML <time> element describing the TIMESTAMP."
  (let* ((date (org-timestamp-format timestamp "%b %d, %Y"))
         (machine-date (and date (org-timestamp-format timestamp "%Y-%m-%d"))))
    (if timestamp
        (format "<time datetime=\"%s\">%s</time>"
                machine-date date)
      "")))

(defun ox-blorg-get-root (info)
  "Grab the :blorg-root string (empty if unspecified)."
  (let ((ox-blorg-root (plist-get info :blorg-root)))
    (if ox-blorg-root
        (ox-blorg-ensure-suffix "/" ox-blorg-root)
      "")))

(defun ox-blorg-get-header (info)
  "Grab the :blorg-header string (empty if unspecified)."
  (or (plist-get info :blorg-header) ""))

(defun ox-blorg-replace-root-link (string info)
  "Replace instances of blorg: in STRING."
  (let ((ox-blorg-root (ox-blorg-get-root info)))
    (replace-regexp-in-string "blorg:" ox-blorg-root string)))

(defun ox-blorg-mappend (function list)
  "Apply FUNCTION to elements of LIST and concatenate results."
  (apply #'append (mapcar function list)))

(defun blorg-extract-date-from-org-file (file)
  "Extract the #+DATE: from an Org FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (if (re-search-forward "^#\\+DATE:[ \t]*\\(.*\\)$" nil t)
        (org-read-date nil nil (match-string 1))
      nil)))

;;;; Sitemap function

(defun ox-blorg-manipulate-sitemap-files (files)
  "Transform the FILES of :sitemap-function into a better form."
  (when files
    (cond
     ((symbolp (car files))
      (mapcar #'ox-blorg-manipulate-sitemap-files (cdr files)))
     ((and (stringp (car files)) (cdr files))
      (cons (car files) (ox-blorg-manipulate-sitemap-files (cadr files))))
     ((and (stringp (car files)))
      (car files)))))

(defun ox-blorg-manipulate-sitemap-separate-files (files &optional subsequent-run)
  "Separate the FILES from the directories."
  (when files
    (let ((files
           (remove nil
                   (mapcar (lambda (x) (when (stringp x) x)) files)))
          (directories
           ;; Sort directories alphabetically.
           (sort (remove nil
                         (mapcar (lambda (x) (when (listp x) x)) files))
                 :key #'car)))
      (cons files directories))))

(defun ox-blorg-format-directory-for-sitemap (entry)
  "Format a directory for the sitemap."
  (capitalize (replace-regexp-in-string "-" " " entry)))

(defun ox-blorg-render-sitemap-item (item depth)
  "Render according to DEPTH and type of ITEM (file or directory)."
  (concat
   (if (> depth 0)
       (make-string (* 2 depth) ? )
     "")
   (format "- %s\n"
           (if (not (string-prefix-p "[[file:" item))
               (ox-blorg-format-directory-for-sitemap item)
             ;; Files have their file: links replaced with blorg:
             ;; links, and their #+DATE: added next to their entry.
             (let* ((slice (substring item (length "[[file:")))
                    (file (substring slice 0 (string-match "\\]" slice)))
                    (date (blorg-extract-date-from-org-file file)))
               (concat "[[blorg:"
                       slice
                       (if (not date)
                           ""
                         (format ", [%s]" date))))))))

(defun ox-blorg-render-sitemap-from-files (files &optional depth subsequent-run)
  "Render the sitemap list from FILES."
  (if (null files)
      ""
    (let* ((depth (or depth 0))
           (all-files (ox-blorg-manipulate-sitemap-separate-files files))
           (top-files (if (not subsequent-run)
                          (car all-files)
                        (cdar all-files)))
           (top-directory (when subsequent-run
                            (caar all-files)))
           (directories (cdr all-files)))
      (concat
       (if (null top-directory)
           ""
         (ox-blorg-render-sitemap-item top-directory (1- depth)))
       (mapconcat (lambda (f) (ox-blorg-render-sitemap-item f depth))
                  top-files)
       (mapconcat (lambda (f) (ox-blorg-render-sitemap-from-files f (1+ depth) t))
                  directories)))))

(defun ox-blorg-sitemap-function (_ files)
  "Pass this to :sitemap-function in your project configuration."
  (let ((org-settings (format "#+TITLE: Sitemap\n#+OPTIONS: toc:nil num:nil\n"))
        (files (ox-blorg-manipulate-sitemap-files files)))
    (concat
     org-settings
     (ox-blorg-render-sitemap-from-files files))))

;;;; Src-block

(defun ox-blorg-html-src-block (src-block _contents info)
  "Transcode an SRC-BLOCK element from Org to HTML."
  (let* ((lang (org-element-property :language src-block))
	 (code (org-html-format-code src-block info))
	 (label (let ((lbl (org-html--reference src-block info t)))
		  (if lbl (format " id=\"%s\"" lbl) ""))))
    (format "<div class=\"org-src-container\">\n%s%s\n</div>"
	    ;; Build caption.
	    (let ((caption (org-export-get-caption src-block)))
	      (if (not caption) ""
		(let ((listing-number
		       (format
			"<span class=\"listing-number\">%s </span>"
			(format
			 (org-export-translate "Listing %d:" :html info)
			 (org-export-get-ordinal
			  src-block info nil
                          (lambda (element &optional info)
                            (org-element-property :caption element)))))))
		  (format "<label class=\"org-src-name\">%s%s</label>"
			  listing-number
			  (org-trim (org-export-data caption info))))))
	    ;; Contents.
            (format "<pre class=\"src src-%s\"%s><code>%s</code></pre>"
                    lang label code))))

;;;; Italic

(defun ox-blorg-html-italic (italic contents info)
  "Transcode ITALIC from Org to HTML for a blog.
Italicize if in title, otherwise emphasize."
  (let* ((markup (plist-get info :html-text-markup-alist))
         (italic-element (alist-get 'italic markup "<i>%s</i>"))
         (emphasis-element (alist-get 'emphasis markup "<em>%s</em>"))
         (parent-type
          (org-element-lineage-map italic #'org-element-type
            nil nil t)))
    (cond ((member parent-type '(headline section))
           (format italic-element contents))
          (t (format emphasis-element contents)))))

;;;; Headline

(defun ox-blorg-html-headline (headline contents info)
  "Transcode a HEADLINE from Org to HTML for a blog.
Do as ox-html does, but also include the DONE timestamp."
  (let ((todo (org-element-property :todo-keyword headline))
        (closed-timestamp (org-element-property :closed headline))
        (html-headline (org-html-headline headline contents info)))
    (if (and (string= todo "DONE") closed-timestamp)
        (replace-regexp-in-string
         (regexp-quote "DONE</span>")
         (format "DONE(%s)</span>" (ox-blorg-time-element-from-timestamp closed-timestamp info))
         html-headline)
      html-headline)))

;;;; Timestamp

(defun ox-blorg-html-timestamp (timestamp _ info)
  "Transcode a TIMESTAMP from Org to HTML for a blog.
Do as ox-html does, but include the semantic <time> element."
  (let ((date (org-html-plain-text (org-timestamp-translate timestamp) info))
        (machine-date (org-timestamp-format timestamp "%Y-%m-%d")))
    (format "<span class=\"timestamp-wrapper\"><span class=\"timestamp\"><time datetime=\"%s\">%s</time></span></span>"
            machine-date
	    (replace-regexp-in-string "--" "&#x2013;" date))))

;;;; Bibliography

(defvar ox-blorg-bibliography-hook-added nil
  "Track if the hook to attach bibliography has been added.")

(defun ox-blorg-attach-bibliography (backend)
  "Attach relevant bibliographic information to current buffer."
  ;; Write #+print_bibliography: at end unless already existing. Do
  ;; not write it if no citations have been made.
  (when (and (re-search-forward "\\[cite\\(/style\\)?:" nil t)
             (not (re-search-forward "^#\\+print_bibliography:" nil t)))
    (goto-char (point-max))
    (unless (bolp) (insert "\n"))
    (insert "* References\n#+print_bibliography:\n")))

(unless ox-blorg-bibliography-hook-added
  (add-hook 'org-export-before-processing-functions #'ox-blorg-attach-bibliography)
  (setq ox-blorg-bibliography-hook-added t))

;;;; LaTeX macros

(defvar ox-blorg-latex-hook-added nil
  "Track if the hook to attach LaTeX macros has been added.")

(defun ox-blorg-attach-latex (backend)
  "Attach LaTeX macro template to current buffer."
  (let* ((root-dir (locate-dominating-file "." "index.org"))
         (macro-file (concat (if root-dir (ox-blorg-ensure-suffix "/" root-dir) "")
                             "latex-template")))
    (when (file-readable-p macro-file)
      (re-search-forward "^[^#+]" nil t)
      (beginning-of-line)
      (insert
       (format "#+INCLUDE: %s\n" macro-file)))))

(unless ox-blorg-latex-hook-added
  (add-hook 'org-export-before-processing-functions #'ox-blorg-attach-latex)
  (setq ox-blorg-latex-hook-added t))

;;;; MathJax

;; This is taken from ox-html
(defun ox-blorg-html-build-mathjax-config (info)
  "Insert the user setup into the mathjax template."
  (if (not (and (memq (plist-get info :with-latex) '(mathjax t))
                (org-element-map (plist-get info :parse-tree)
                    '(latex-fragment latex-environment) #'identity info t nil t)))
      ""
    (let* ((template (plist-get info :html-mathjax-template))
           (options (plist-get info :html-mathjax-options))
           (in-buffer (or (plist-get info :html-mathjax) ""))
           (mathjax-path (alist-get 'path options)))
      (if mathjax-path
          (setf (alist-get 'path options)
                (list (ox-blorg-replace-root-link (car mathjax-path) info))))
      (dolist (e options (org-element-normalize-string template))
        (let ((symbol (car e))
              (value (cadr e)))
          (when value
            (while (string-match (format "\\(%%%s\\)[^A-Z]"
                                         (upcase (symbol-name symbol)))
                                 template)
              (setq template
                    (replace-match (format "%s" value)
                                   t
                                   t template 1)))))))))

;;;; Template

;;;;; Doctype and HTML elements

(defun ox-blorg-html-template-document (part info)
  "The Ox-Blorg HTML document template."
  (cond
   ((eq part 'begin)
    "<!doctype html>
<html lang=\"en-US\">")
   ((eq part 'end)
    "</html>")
   (t "")))

;;;;; Head element

(defun ox-blorg-html-template-head (part info)
  "The Ox-Blorg HTML head element template."
  (let ((ox-blorg-root (ox-blorg-get-root info))
        (html-head (or (plist-get info :html-head) "")))
    (cond
     ((eq part 'begin)
      (format
       "<head>
  <meta charset=\"utf-8\">
  <meta name=\"viewport\" content=\"width=device-width\">
  <title>%s</title>
  %s
  %s
</head>"
       (ox-blorg-html-aux-title info)
       (ox-blorg-html-build-mathjax-config info)
       (ox-blorg-replace-root-link html-head info)))
     ((eq part 'end)
      "")
     (t ""))))

;;;;; Body element

(defun ox-blorg-html-template-body (part info)
  "The Ox-Blorg HTML body element template."
  (cond
   ((eq part 'begin)
    "<body>")
   ((eq part 'end)
    "</body>")
   (t "")))

;;;;; Main element

(defun ox-blorg-html-template-main (part info)
  "The Ox-Blorg HTML main and article element template."
  (cond
   ((eq part 'begin)
    "<main>
  <article>")
   ((eq part 'end)
    "</article>
</main>")
   (t "")))

;;;;; Footer element

(defun ox-blorg-html-template-footer (part info)
  "The Ox-Blorg HTML footer element template."
  (cond
   ((eq part 'begin)
    (format
     "<footer>
  <p class=\"copyright-notice\">Copyright &copy; <span>%s</span>, <a href=\"mailto:%s\">contact</a>.</p>"
     (org-export-data (plist-get info :author) info)
     (plist-get info :email)))
   ((eq part 'end)
    "</footer>")
   (t "")))

;;;;; Header element

(defun ox-blorg-html-template-header (part info)
  "The Ox-Blorg HTML header element template."
  (let ((ox-blorg-root (ox-blorg-get-root info))
        (ox-blorg-header (ox-blorg-get-header info)))
    (cond
     ((eq part 'begin)
      (format
       "<header><h1 class=\"article-title\">%s"
       (ox-blorg-html-aux-title info)))
     ((eq part 'end)
      (format "</h1>%s%s</header>"
              (let ((timestamp (and (plist-get info :with-date)
                                    (org-export-get-date info))))
                (format "<span class=\"article-date\">%s</span>"
                        (if timestamp
                            (ox-blorg-time-element-from-timestamp (car timestamp) info)
                          "")))
              (ox-blorg-replace-root-link ox-blorg-header info)))
     (t ""))))

;;;;; Putting it together

(defun ox-blorg-html-template (contents info)
  "Return complete document string after HTML conversion."
  (ox-blorg-html-aux-$<>
   document info
   (ox-blorg-html-aux-$<>
    head info
    (ox-blorg-html-aux-$<>
     body info
     (concat
      (ox-blorg-html-aux-$<> header info "")
      "\n"
      (ox-blorg-html-aux-$<> main info contents)
      "\n"
      (ox-blorg-html-aux-$<> footer info ""))))))

;;;; Publish

(defun ox-blorg-html-publish-to-html (plist filename pub-dir)
  "Publish an org file to HTML.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (org-publish-org-to 'ox-blorg-html filename
		      (concat (when (> (length org-html-extension) 0) ".")
			      (or (plist-get plist :html-extension)
				  org-html-extension
				  "html"))
		      plist pub-dir))

;;;; Custom ox-blorg links

(defun ox-blorg-link-export (link description _ info)
  "Export a ox-blorg page LINK from Org files."
  (let* ((new-link (replace-regexp-in-string "\\.org$" ".html" link))
         (desc (or description new-link))
         (path (concat (ox-blorg-get-root info)
                       new-link)))
    (format "<a href=\"%s\">%s</a>" path desc)))

;;;;; `locate-dominating-file' for index.org
(defun ox-blorg-link-follow (path _)
  "Follow a ox-blorg link from inside the Emacs editor."
  (let ((root-dir (locate-dominating-file "." "index.org")))
    (find-file (concat (if root-dir (ox-blorg-ensure-suffix "/" root-dir) "")
                       path))))

(org-link-set-parameters "blorg"
                         :follow #'ox-blorg-link-follow
                         :export #'ox-blorg-link-export)

;;;; Body filter hook

(defvar ox-blorg-body-hook-added nil
  "Track if the hook to filter body has been added.")

;; Unfortunately oc-csl adds a <style> element in the body. We remove
;; it as style elements shouldn't be placed there. See
;; <https://lists.gnu.org/archive/html/emacs-orgmode/2025-05/msg00336.html>
(defun ox-blorg-body-remove-csl-style (str _ _)
  "Remove <style> (added by oc-csl) from bibliography."
  (replace-regexp-in-string "<style>\\.csl-left-margin[^<]*</style>"
                            ""
                            str))

(unless ox-blorg-body-hook-added
  (add-hook 'org-export-filter-body-functions #'ox-blorg-body-remove-csl-style)
  (setq ox-blorg-body-hook-added t))

;;;; Define the derived HTML backend

;;;###autoload
(defun ox-blorg-html-export-as-html
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to an HTML buffer."
  (interactive)
  (org-export-to-buffer 'ox-blorg-html "*Ox-Blorg HTML Export*"
    async subtreep visible-only body-only ext-plist
    (lambda () (set-auto-mode t))))

;;;###autoload
(defun ox-blorg-html-export-to-html
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to an HTML file."
  (interactive)
  (let* ((extension (concat
		     (when (> (length org-html-extension) 0) ".")
		     (or (plist-get ext-plist :html-extension)
			 org-html-extension
			 "html")))
	 (file (org-export-output-file-name extension subtreep))
	 (org-export-coding-system org-html-coding-system))
    (org-export-to-file 'ox-blorg-html file
      async subtreep visible-only body-only ext-plist)))

(org-export-define-derived-backend 'ox-blorg-html 'html
  :options-alist
  '((:html-doctype "HTML_DOCTYPE" nil "html5")
    (:html-html5-fancy nil "html5-fancy" t)
    ;; The options set below are per (org)Bare HTML, for a minimal
    ;; HTML export.
    (:html-head nil "html-head" "")
    (:html-head-extra nil "html-head-extra" "")
    (:html-head-include-default-style
     nil "html-style" nil)
    (:html-head-include-scripts
     nil "html-scripts" nil)
    (:html-postamble nil "html-postamble" nil)
    (:html-preamble nil "html-preamble" nil)
    (:html-use-infojs nil nil nil)
    (:blorg-root nil nil nil)
    (:blorg-header nil nil nil))
  :menu-entry
  '(?b "Export to Ox-Blorg HTML"
       ((?H "As HTML buffer" ox-blorg-html-export-as-html)
	(?h "As HTML file" ox-blorg-html-export-to-html)
	(?o "As HTML file and open"
	    (lambda (a s v b)
	      (if a (ox-blorg-html-export-to-html t s v b)
		(org-open-file (ox-blorg-html-export-to-html nil s v b)))))))
  :translate-alist
  '((italic . ox-blorg-html-italic)
    (src-block . ox-blorg-html-src-block)
    (headline . ox-blorg-html-headline)
    (timestamp . ox-blorg-html-timestamp)
    (template . ox-blorg-html-template)))

(provide 'ox-blorg)

;; End:

;;; ox-blorg.el ends here
