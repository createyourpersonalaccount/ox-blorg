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
        "&lrm;"
      title)))

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

;;;; Template

(defun blorg-html-template-document (part info)
  "The Blorg HTML document template."
  (cond
   ((eq part 'begin)
    "<!doctype html>
<html lang=\"en-US\">")
   ((eq part 'end)
    "</html>")
   (t "")))

(defun blorg-html-template-head (part info)
  "The Blorg HTML head element template."
  (cond
   ((eq part 'begin)
    (format
     "<head>
  <meta charset=\"utf-8\">
  <meta name=\"viewport\" content=\"width=device-width\">
  <title>%s</title>
</head>"
     (blorg-html-aux-title info)))
   ((eq part 'end)
    "")
   (t "")))

(defun blorg-html-template-body (part info)
  "The Blorg HTML body element template."
  (cond
   ((eq part 'begin)
    "<body>")
   ((eq part 'end)
    "</body>")
   (t "")))

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

(defun blorg-html-template-header (part info)
  "The Blorg HTML header element template."
  (cond
   ((eq part 'begin)
    (format
     "<header><h1>%s"
     (blorg-html-aux-title info)))
   ((eq part 'end)
    "</h1></header>")
   (t "")))

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
    (:html-use-infojs nil nil nil))
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
    (template . blorg-html-template)))

(provide 'blorg)

;; End:

;;; blorg.el ends here
