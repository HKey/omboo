;;; omboo-bookmark.el --- bookmark library for omboo -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Hiroki YAMAKAWA

;; Author: Hiroki YAMAKAWA <s06139@gmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'eieio)
(require 'dash)
(require 'omboo-ohd)

;;;; Variables

(defvar omboo-bookmark-url-property :URL)

(defvar omboo-bookmark-feed-property :FEED)

(defvar omboo-bookmark-org-file-regexp "\\.org\\'")

;;;; Methods

(defgeneric omboo-bookmark--to-generate-form (object))

;;;; Classes

;; bookmark

(defclass omboo-bookmark-bookmark ()
  ((title
    :type    string
    :initarg :title
    :reader  omboo-bookmark-bookmark-title)
   (url
    :type    string
    :initarg :url
    :reader  omboo-bookmark-bookmark-url)
   (feed
    :type    (or string null)
    :initarg :feed
    :reader  omboo-bookmark-bookmark-feed)))

(defun omboo-bookmark-make-bookmark (&rest keys)
  (apply #'make-instance
         'omboo-bookmark-bookmark
         keys))

(defmethod omboo-bookmark--to-generate-form ((this omboo-bookmark-bookmark))
  `(omboo-bookmark-make-bookmark
    :title ,(omboo-bookmark-bookmark-title this)
    :url   ,(omboo-bookmark-bookmark-url   this)
    :feed  ,(omboo-bookmark-bookmark-feed  this)))

;; directory

(defclass omboo-bookmark-directory ()
  ((title
    :type    string
    :initarg :title
    :reader  omboo-bookmark-directory-title)
   (children
    :type    list
    :initarg :children
    :reader  omboo-bookmark-directory-children)))

(defun omboo-bookmark-make-directory (&rest keys)
  (apply #'make-instance
         'omboo-bookmark-directory
         keys))

(defmethod omboo-bookmark--to-generate-form ((this omboo-bookmark-directory))
  `(omboo-bookmark-make-directory
    :title    ,(omboo-bookmark-directory-title this)
    :children (list ,@(-map #'omboo-bookmark--to-generate-form
                            (omboo-bookmark-directory-children this)))))

;;;; Functions

;; Conversion

(defun omboo-bookmark--convert-to-bookmark (headline)
  (-when-let* ((title (omboo-ohd-headline-title headline))
               (url   (omboo-ohd-headline-get-property
                       headline
                       omboo-bookmark-url-property)))
    (omboo-bookmark-make-bookmark
     :title title
     :url   url
     :feed  (omboo-ohd-headline-get-property
             headline
             omboo-bookmark-feed-property))))

(defun omboo-bookmark--convert-to-directory (headline)
  (-when-let* ((title (omboo-ohd-headline-title headline))
               (children (-keep #'omboo-bookmark--convert-headline
                                (omboo-ohd-headline-children headline))))
    (omboo-bookmark-make-directory
     :title    title
     :children children)))

(defun omboo-bookmark--convert-headline (headline)
  (if (omboo-ohd-headline-children headline)
      (omboo-bookmark--convert-to-directory headline)
    (omboo-bookmark--convert-to-bookmark headline)))

;; Parsing

(defun omboo-bookmark-parse-file (file)
  (-keep #'omboo-bookmark--convert-headline
         (omboo-ohd-parse-file file)))

(defun omboo-bookmark--parse-file-as-directory (file)
  (-when-let (children (omboo-bookmark-parse-file file))
    (omboo-bookmark-make-directory
     :title    (file-name-base file)
     :children children)))

(defun omboo-bookmark--parse-directory-as-directory (directory recursive)
  (-when-let (children (omboo-bookmark-parse-directory directory recursive))
    (omboo-bookmark-make-directory
     :title    (file-name-base (directory-file-name directory))
     :children children)))

(defun omboo-bookmark-parse-directory (directory &optional recursive)
  (--keep
   (if (file-directory-p it)
       (when (and recursive
                  (not (string-match-p (rx (or "/." "/..") eos) it)))
         (omboo-bookmark--parse-directory-as-directory it recursive))
     (when (string-match-p omboo-bookmark-org-file-regexp it)
       (omboo-bookmark--parse-file-as-directory it)))
   (directory-files directory t)))

(defun omboo-bookmark-parse-files (files)
  (--keep
   (if (file-directory-p it)
       (when (not (string-match-p (rx (or "/." "/..") eos) it))
         (omboo-bookmark--parse-directory-as-directory it nil))
     (when (string-match-p omboo-bookmark-org-file-regexp it)
       (omboo-bookmark--parse-file-as-directory it)))
   files))

;; Saving and Loading

(defun omboo-bookmark-save-file (file bookmarks)
  (with-temp-file file
    (insert ";; -*- mode: emacs-lisp -*-\n"
            (pp-to-string
             `(list
               ,@(-map #'omboo-bookmark--to-generate-form bookmarks))))))

(defun omboo-bookmark-load-file (file)
  (with-temp-buffer
    (insert-file-contents file)
    (eval (read (buffer-substring (point-min) (point-max))))))

;; Utilities

(defun omboo-bookmark-get-url-at-point ()
  "Get a bookmark url at the current point"
  (let* ((marker (get-text-property (point) 'org-marker))
         (point (if marker marker (point)))
         (buffer (if marker (marker-buffer marker) (current-buffer))))
    (with-current-buffer buffer
      (org-entry-get point
                     (substring (symbol-name omboo-bookmark-url-property) 1)))))

(provide 'omboo-bookmark)
;;; omboo-bookmark.el ends here
