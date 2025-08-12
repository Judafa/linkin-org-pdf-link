;;; linkin-org.el --- an emacs workflow with fast, reliable links -*- lexical-binding: t -*-

;; Copyright 2025 Julien Dallot

;; Author: Julien Dallot <judafa@protonmail.com>
;; Maintainer: Julien Dallot <judafa@protonmail.com>
;; URL: https://github.com/Judafa/linkin-org
;; Version: 1.0
;; Package-Requires: ((emacs "30.1") (pdf-tools "1.1.0"))

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
;; linkin-org proposes to access your data with reliable links to place your written notes at the center of your workflow.
;; The links work fast and are easy to create; most importantly, the links are reliable and can robustly support a whole link-based workflow.

(require 'ol)
(require 'dired)
(require 'pdf-tools)



(defun linkin-org-is-url (string)
  "Return non-nil if STRING is a valid URL."
  (string-match-p
   (rx string-start
       (seq (or "http" "https" "ftp") "://")
       (1+ (not (any " ")))  ;; Match one or more non-space characters
       string-end)
   string))

(defun org-video-link-get-path (link)
  "Extract the path of LINK."
  (let* (
         (path+timestamp (split-string link "::"))
         ;; for the video file path
         (video-file (car path+timestamp)))
    video-file))

(defun org-video-open (link)
  "Where timestamp is 00:15:37.366 , the LINK should look like:
[[video:/path/to/file.mp4::00:15:37.366][My description.]]
path can also be a youtube url."

  (let* (
	 (path-or-url+timestamp (split-string link "::"))
         (timestamp (car (cdr path-or-url+timestamp)))
         (video-address (car path-or-url+timestamp)))
    (progn
      ;; (message (concat "video address : " video-address))
      (cond
       ;; if its an url
       ((linkin-org-is-url video-address)
	    (progn
	      (start-process "mpv" nil "mpv" "--force-window" "--ytdl=no" (format "--start=%s" timestamp) video-address)))
       ;; if it's a local file
       ;; ((linkin-org-is-link-path-correct video-address)
       ((linkin-org-resolve-file video-address)
	    (progn
	      (message "Playing %s at %s" video-address timestamp)
	      (start-process "mpv" nil "mpv" (format "--start=%s" timestamp) video-address)))
       (t
	    (message "Not a valid video file or url"))))))



;;;; add the link type
(let ((inhibit-message t)) ;; dont print messages while loading the package
  (org-add-link-type "video" 'org-video-open nil))

(provide 'linkin-org-video-link-type)
