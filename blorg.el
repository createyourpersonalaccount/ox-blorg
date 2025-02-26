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

;;;; Italic

(defun blorg-html-italic (italic contents info)
  "Transcode ITALIC from Org to HTML for a blog.
Italicize if in title, otherwise emphasize."
  (let* ((markup (plist-get info :html-text-markup-alist))
         (italic-element (alist-get 'italic markup "<i>%s</i>"))
         (emphasis-element (alist-get 'emphasis markup "<em>%s</em>"))
         (parent-type (org-element-lineage-map italic #'org-element-type  nil nil t)))
    (cond ((member parent-type '(headline section))
           (format italic-element contents))
          (t (format emphasis-element contents)))))

;;;; Define the derived HTML backend

(org-export-define-derived-backend 'blorg-html 'html
  :translate-alist
  '((italic . blorg-html-italic)))

(provide 'blorg)

;; End:

;;; blorg.el ends here
