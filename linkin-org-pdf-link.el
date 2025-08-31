;;; linkin-org-pdf-link.el --- A linkin-org extension that defines links towards pdf files -*- lexical-binding: t -*-

;; Copyright 2025 Julien Dallot

;; Author: Julien Dallot <judafa@protonmail.com>
;; Maintainer: Julien Dallot <judafa@protonmail.com>
;; URL: https://github.com/Judafa/linkin-org
;; Version: 1.0
;; Package-Requires: ((emacs "30.1") (pdf-tools "1.1.0") (linkin-org "1.0") (s "1.30.0"))

;; This file is not part of GNU Emacs

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at

;;     http://www.apache.org/licenses/LICENSE-2.0

;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;;; Commentary:
;; This package defines a pdf link that jumps to a specific page
;; and highlights a specific region in the pdf.
;; The links are meant to be created with the function linkin-org-get
;; defined in the package linkin-org (https://github.com/Judafa/linkin-org)

(require 'ol)
(require 'org-element)
(require 'dired)
(require 'pdf-tools)
(require 'pdf-isearch)
(require 'linkin-org)



;;; Code:

(defun linkin-org-pdf-link-open (path link)
  "Open a LINK in an org element form with type pdf.
If link is nil, use PATH to open the pdf."
  ;; if link is nil, recompute the link data from the path
  (when (not link)
    ;; parse the link
    (setq link
	  (linkin-org-resolve-link
	   (with-temp-buffer
	     (let ((org-inhibit-startup nil))
	       (insert (concat "[[" path "]]"))
	       (org-mode)
	       (goto-char (point-min))
	       (org-element-link-parser)))
	   ;; do not resolve the link path
	   t)))
  (let* ((pdf-path (org-element-property :path link))
	 (metadata (org-element-property :metadata link))
	 (page
	  (let ((page (plist-get metadata :page)))
	    (if page page
	      ;; if the metadata is not a plist, then check if it is a number; it is the page number then
	      (when (numberp metadata ) metadata))))
	 (edges (plist-get metadata :edges)))
    ;; (start-process "view-pdf" nil "zathura" pdf-file (format "--page=%s" page))))
    (progn
      ;; (message edges-list)
      ;; check if the pdf file is already open
      (if-let (;; get the buffer of the pdf file
	       (pdf-buffer (get-file-buffer pdf-path))
	       ;; check if the buffer is visible
	       (pdf-window (get-buffer-window pdf-buffer 'visible))
	       ;; Find all windows that currently display the given buffer
               (windows
		(delq
		 (selected-window)
		 (get-buffer-window-list pdf-buffer 'nomini t)))
	       ;; initialized the final window to pick with the first of those windows
               (final-window (car windows))
	       ;; intialize the most recent timestamp with that of the first window
               (most-recent-time-stamp (window-use-time final-window)))
	  ;; if there already exists a window whose buffer displays the pdf then dont open a new frame
	  ;; rather switch to the last visisted window and highlight the edges
	  (progn
	    ;; Among the windows which currently display the pdf buffer, select the most recently used
            (dolist (fenetre (cdr windows))
              (if (>
		   (window-use-time fenetre)
		   most-recent-time-stamp)
                  (progn
                    (setq most-recent-time-stamp
			  (window-use-time fenetre))
                    (setq pdf-window fenetre))))
	    (let ((initial-window (selected-window)))
	      ;; switch to the frame
	      ;; (select-frame-set-input-focus (window-frame pdf-window) t)
	      ;; (select-frame (window-frame pdf-window))
	      ;; switch to the window
	      (select-window pdf-window)
	      ;; (if edges-list
	      ;;     ;; if the place to highlight is provided, then make sure that place is visible (ie, pdf is scrolled so that one can see it)
	      ;;     (let
	      ;; 	;; idk then do the same here
	      ;; 	;; [[file:~/.config/emacs/straight/repos/pdf-tools/lisp/pdf-occur.el::290][[file] pdf-occur.el_at_290]]
	      ;; 	(
	      ;; 	 (pdf-isearch-batch-mode t)
	      ;; 	 (pixel-match (pdf-util-scale-relative-to-pixel edges-list))
	      ;; 	 )
	      ;;       (pdf-isearch-focus-match-batch pixel-match)
	      ;;
	      ;;       (pdf-isearch-hl-matches pixel-match nil t)
	      ;;       )
	      ;;   ;; else, just go to the page
	      ;;   (unless (not page)
	      ;;     (pdf-view-goto-page page)
	      ;;     )
	      ;;   )
	      ;; go to the page
	      (when page (pdf-view-goto-page page))
	      ;; ;; hightlight the edges
	      (when edges
		(let ;; idk then do the same here
		    ;; [[file:~/.config/emacs/straight/repos/pdf-tools/lisp/pdf-occur.el::290][[file] pdf-occur.el_at_290]]
		    ((pdf-isearch-batch-mode t)
		     (pixel-match
		      (pdf-util-scale-relative-to-pixel (list edges))))
		  ;; dont forget to scale the edges to the current display!
		  (pdf-isearch-hl-matches pixel-match nil t)
		  (pdf-isearch-focus-match-batch pixel-match)))
	      ;; switch back to the initial buffer
	      (select-window initial-window)))
	;; if the pdf file is not visible, open a new frame
	;; or if I specifically asked to open a new frame for the pdf file
	(progn
	  ;; (message "not visible")
	  (clone-frame)
	  (find-file pdf-path)
	  (pdf-view-goto-page page))
	;; (if linkin-org-open-pdf-link-other-frame
	;; 	  (progn
	;; 	    (find-file-other-frame pdf-file)
	;; 	    (pdf-view-goto-page page)
	;; 	    )
	;; 	)
	))))

;; to copy a link towards the focused pdf buffer
(defun linkin-org-pdf-link-get-link ()
  "Return a link in string form to the pdf in the current buffer.
Highlighted text is included in the link."
  (pdf-tools-assert-pdf-buffer)
  (let* ((page (number-to-string (pdf-view-current-page)))
	 (file (abbreviate-file-name (pdf-view-buffer-file-name)))
	 (file-name (file-name-nondirectory file))
	 (file-name-sans-id
	  (linkin-org-strip-off-id-from-file-name file-name))
	 (file-name-sans-ext
	  (file-name-sans-extension file-name-sans-id))
	 (file-name-extension (file-name-extension file-name-sans-id))
	 (truncated-file-name
	  (if (> (length file-name-sans-ext) 70)
	      (concat
	       (substring file-name-sans-ext 0 50)
	       "[___]"
	       (if file-name-extension
		   (concat "." file-name-extension)))
	    file-name-sans-id))
	 ;; for selected text
	 (selected-text
	  (if (pdf-view-active-region-p)
	      ;; delete the newlines
	      (replace-regexp-in-string "\n" " "
					(car
					 (pdf-view-active-region-text)))
	    nil))
	 ;; ;; truncate the selected text
	 ;; (selected-text (if (and selected-text (> (length selected-text) 15))
	 ;; 			     (concat (substring selected-text 0 15) "...")
	 ;; 			   selected-text
	 ;; 			   )
	 ;; 			 )
	 ;; for the edges
	 (edges
	  (if (pdf-view-active-region-p)
              ;; the edges that were really hovered by the mouse.
              ;; not necessarily the visual highlightings, that are wordwise by default
              ;; (pdf-info-getselection
              ;;  (string-to-number page)
              ;;  pdf-view-active-region
              ;;  pdf-view-selection-style
              ;;  )
              (mapcar
               (lambda (edges)
		 (pdf-info-getselection
                  (string-to-number page)
                  edges
                  pdf-view-selection-style))
               pdf-view-active-region)
	    nil)))
    (other-window 1)
    ;; deselect the text, if there is an active region
    (if (pdf-view-active-region-p) (pdf-view-deactivate-region))
    (if (and selected-text edges)
	(progn
	  (let* (;; edges are actually outputed as a list of list of Lists-trees
		 (edges (car (car edges)))
		 ;; concat the edges with |
		 (string-edges
		  (concat
		   "("
		   (mapconcat #'prin1-to-string edges " ")
		   ")" )))
	    ;; (format "[[pdf:%s::%s::%s][[pdf] %s _ p%s _ \"%s\"]]" file page string-edges nom-fichier-tronque page selected-text)
	    ;; without the pdf name
	    ;; (format "[[pdf:%s::%s::%s][[pdf] p%s _ \"%s\"]]" file page string-edges page selected-text)
	    (format
	     "[[pdf:%s::(:page %s :edges %s)][[pdf] p%s _ \"%s\"]]" file page string-edges page selected-text)))
      ;; else, no selected text, just care about the path and page
      ;; (format "[[pdf:%s::%s][[pdf] %s _ p%s]]" file page nom-fichier-tronque page)
      (format
       "[[pdf:%s::(:page %s)][[pdf] %s _ p%s]]" file page truncated-file-name page))))



;;;; add the link type
(let ((inhibit-message t)) ;; dont print messages while loading the package
  (org-add-link-type "pdf" 'linkin-org-pdf-link-open nil))


;; add the facilities to obtain pdf links
(defun linkin-org-pdf-link-redirect-link-opening (func)
  (let ((mode (symbol-name major-mode)))
    (if (string= (symbol-name major-mode) "pdf-view-mode")
	(kill-new (linkin-org-pdf-link-get-link))
      ;; else, run the normal linkin-org-get function
      (funcall func))))

(advice-add 'linkin-org-get :around #'linkin-org-pdf-link-redirect-link-opening)

(provide 'linkin-org-pdf-link)

;;; linkin-org-pdf-link.el ends here
