;;; blorg.el --- Your blog in Org

;; Copyright (C) 2025 Nikolaos Chatzikonstantinou <nchatz314@gmail.com>
;; Author: Nikolaos Chatzikonstantinou
;; URL: https://github.com/createyourpersonalaccount/blorg
;; Created: 2025
;; Version: 0.1
;; Keywords: org blog
;; Package-Requires: ()

;;; Commentary:
;;
;; This mode builds on Org to allow you to publish a spiffy blog.

;;; Code:

;;; Dependencies

(require 'org-macs)
(org-assert-version)

(require 'ox)
(require 'ox-publish)

(defgroup blorg nil
  "Your blog in Org"
  :tag "Org Export Blog"
  :group 'org-export)

;;; Blorg HTML backend

;;;; Auxiliary

(defmacro blorg-html-aux-append-symbols (&rest symbols)
  "Append an arbitrary number of SYMBOLS into a new symbol."
  `(intern
    (apply 'concat
           (mapcar #'symbol-name
                   ',symbols))))

(defmacro blorg-html-aux-$<> (element info string)
  "Encloses STRING in ELEMENT from template."
  (let ((template-func (make-symbol "template-func"))
        (info-var (make-symbol "info-var")))
    `(let ((,template-func
            (blorg-html-aux-append-symbols blorg-html-template-
                                           ,element))
           (,info-var ,info))
       (concat
        (funcall ,template-func 'begin ,info-var)
        ,string
        (funcall ,template-func 'end ,info-var)))))

(defun blorg-html-aux-title (info)
  "Get the blog title."
  (let ((title (org-export-data (plist-get info :title) info)))
    (if (string-empty-p title)
        "&lrm;"                         ; <title> shouldn't be empty
      title)))

(defun ensure-suffix (suffix string)
  "Append SUFFIX to STRING unless STRING already ends in SUFFIX."
  (if (string-suffix-p suffix string)
      string
    (concat string suffix)))

(defun time-element-from-timestamp (timestamp info)
  "Get an HTML <time> element describing the TIMESTAMP."
  (let* ((date (org-timestamp-format timestamp "%b %d, %Y"))
         (machine-date (and date (org-timestamp-format timestamp "%Y-%m-%d"))))
    (if timestamp
        (format "<time datetime=\"%s\">%s</time>"
                machine-date date)
      "")))

(defun blorg-get-root (info)
  "Grab the :blorg-root string (empty if unspecified)."
  (let ((blorg-root (plist-get info :blorg-root)))
    (if blorg-root
        (ensure-suffix "/" blorg-root)
      "")))

(defun blorg-get-header (info)
  "Grab the :blorg-header string (empty if unspecified)."
  (or (plist-get info :blorg-header) ""))

(defun blorg-replace-root-link (string info)
  "Replace instances of blorg: in STRING."
  (let ((blorg-root (blorg-get-root info)))
    (replace-regexp-in-string "blorg:" blorg-root string)))

;;;; Src-block

(defun blorg-html-src-block (src-block _contents info)
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

(defun blorg-html-italic (italic contents info)
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

(defun blorg-html-headline (headline contents info)
  "Transcode a HEADLINE from Org to HTML for a blog.
Do as ox-html does, but also include the DONE timestamp."
  (let ((todo (org-element-property :todo-keyword headline))
        (closed-timestamp (org-element-property :closed headline))
        (html-headline (org-html-headline headline contents info)))
    (if (and (string= todo "DONE") closed-timestamp)
        (replace-regexp-in-string
         (regexp-quote "DONE</span>")
         (format "DONE(%s)</span>" (time-element-from-timestamp closed-timestamp info))
         html-headline)
      html-headline)))

;;;; Timestamp

(defun blorg-html-timestamp (timestamp _ info)
  (let ((date (org-html-plain-text (org-timestamp-translate timestamp) info))
        (machine-date (org-timestamp-format timestamp "%Y-%m-%d")))
    (format "<span class=\"timestamp-wrapper\"><span class=\"timestamp\"><time datetime=\"%s\">%s</time></span></span>"
            machine-date
	    (replace-regexp-in-string "--" "&#x2013;" date))))

;;;; Bibliography

(defvar blorg-bibliography-hook-added nil
  "Track if the hook to attach bibliography has been added.")

(defun blorg-attach-bibliography (backend)
  "Attach relevant bibliographic information to current buffer."
  ;; Write #+print_bibliography: at end unless already existing. Do
  ;; not write it if no citations have been made.
  (when (and (re-search-forward "\\[cite\\(/style\\)?:" nil t)
             (not (re-search-forward "^#\\+print_bibliography:" nil t)))
    (goto-char (point-max))
    (unless (bolp) (insert "\n"))
    (insert "* References\n#+print_bibliography:\n")))

(unless blorg-bibliography-hook-added
  (add-hook 'org-export-before-processing-functions #'blorg-attach-bibliography)
  (setq blorg-bibliography-hook-added t))

;;;; MathJax

;; This is taken from ox-html
(defun blorg-html-build-mathjax-config (info)
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
                (list (blorg-replace-root-link (car mathjax-path) info))))
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

(defun blorg-html-template-document (part info)
  "The Blorg HTML document template."
  (cond
   ((eq part 'begin)
    "<!doctype html>
<html lang=\"en-US\">")
   ((eq part 'end)
    "</html>")
   (t "")))

;;;;; Head element

(defun blorg-html-template-head (part info)
  "The Blorg HTML head element template."
  (let ((blorg-root (blorg-get-root info))
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
       (blorg-html-aux-title info)
       (blorg-html-build-mathjax-config info)
       (blorg-replace-root-link html-head info)))
     ((eq part 'end)
      "")
     (t ""))))

;;;;; Body element

(defun blorg-html-template-body (part info)
  "The Blorg HTML body element template."
  (cond
   ((eq part 'begin)
    "<body>")
   ((eq part 'end)
    "</body>")
   (t "")))

;;;;; Main element

(defun blorg-html-template-main (part info)
  "The Blorg HTML main and article element template."
  (cond
   ((eq part 'begin)
    "<main>
  <article>")
   ((eq part 'end)
    "</article>
</main>")
   (t "")))

;;;;; Footer element

(defun blorg-html-template-footer (part info)
  "The Blorg HTML footer element template."
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

(defun blorg-html-template-header (part info)
  "The Blorg HTML header element template."
  (let ((blorg-root (blorg-get-root info))
        (blorg-header (blorg-get-header info)))
    (cond
     ((eq part 'begin)
      (format
       "<header><h1 class=\"article-title\">%s"
       (blorg-html-aux-title info)))
     ((eq part 'end)
      (format "</h1>%s%s</header>"
              (let ((timestamp (and (plist-get info :with-date)
                                    (org-export-get-date info))))
                (format "<span class=\"article-date\">%s</span>"
                        (if timestamp
                            (time-element-from-timestamp (car timestamp) info)
                          "")))
              (blorg-replace-root-link blorg-header info)))
     (t ""))))

;;;;; Putting it together

(defun blorg-html-template (contents info)
  "Return complete document string after HTML conversion."
  (blorg-html-aux-$<>
   document info
   (blorg-html-aux-$<>
    head info
    (blorg-html-aux-$<>
     body info
     (concat
      (blorg-html-aux-$<> header info "")
      "\n"
      (blorg-html-aux-$<> main info contents)
      "\n"
      (blorg-html-aux-$<> footer info ""))))))

;;;; Publish

(defun blorg-html-publish-to-html (plist filename pub-dir)
  "Publish an org file to HTML.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (org-publish-org-to 'blorg-html filename
		      (concat (when (> (length org-html-extension) 0) ".")
			      (or (plist-get plist :html-extension)
				  org-html-extension
				  "html"))
		      plist pub-dir))

;;;; Custom blorg links

(defun org-blorg-link-export (link description _ info)
  "Export a blorg page link from Org files."
  (let* ((new-link (replace-regexp-in-string "\\.org$" ".html" link))
         (desc (or description new-link))
         (path (concat (blorg-get-root info)
                       new-link)))
    (format "<a href=\"%s\">%s</a>" path desc)))

;;;;; locate-dominating-file for index.org
(defun org-blorg-link-follow (path _)
  (let ((root-dir (locate-dominating-file "." "index.org")))
    (find-file (concat (if root-dir (ensure-suffix "/" root-dir) "")
                       path))))

(org-link-set-parameters "blorg"
                         :follow #'org-blorg-link-follow
                         :export #'org-blorg-link-export)

;;;; Body filter hook

(defvar blorg-body-hook-added nil
  "Track if the hook to filter body has been added.")

;; Unfortunately oc-csl adds a <style> element in the body. We remove
;; it as style elements shouldn't be placed there. See
;; <https://lists.gnu.org/archive/html/emacs-orgmode/2025-05/msg00336.html>
(defun blorg-body-remove-csl-style (str _ _)
  "Remove <style> (added by oc-csl) from bibliography."
  (replace-regexp-in-string "<style>\\.csl-left-margin[^<]*</style>"
                            ""
                            str))

(unless blorg-body-hook-added
  (add-hook 'org-export-filter-body-functions #'blorg-body-remove-csl-style)
  (setq blorg-body-hook-added t))

;;;; Define the derived HTML backend

;;;###autoload
(defun blorg-html-export-as-html
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to an HTML buffer."
  (interactive)
  (org-export-to-buffer 'blorg-html "*Blorg HTML Export*"
    async subtreep visible-only body-only ext-plist
    (lambda () (set-auto-mode t))))

;;;###autoload
(defun blorg-html-export-to-html
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
    (org-export-to-file 'blorg-html file
      async subtreep visible-only body-only ext-plist)))

(org-export-define-derived-backend 'blorg-html 'html
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
  '(?b "Export to Blorg HTML"
       ((?H "As HTML buffer" blorg-html-export-as-html)
	(?h "As HTML file" blorg-html-export-to-html)
	(?o "As HTML file and open"
	    (lambda (a s v b)
	      (if a (blorg-html-export-to-html t s v b)
		(org-open-file (blorg-html-export-to-html nil s v b)))))))
  :translate-alist
  '((italic . blorg-html-italic)
    (src-block . blorg-html-src-block)
    (headline . blorg-html-headline)
    (timestamp . blorg-html-timestamp)
    (template . blorg-html-template)))

(provide 'blorg)

;; End:

;;; blorg.el ends here
