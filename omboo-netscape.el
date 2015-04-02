;;; omboo-netscape.el --- netscape bookmark library for omboo  -*- lexical-binding: t; -*-

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
(require 'omboo-bookmark)


(defvar omboo-netscape-coding-system 'utf-8)

(defun omboo-netscape--indent (string)
  (mapconcat (lambda (line)
               (concat "    " line))
             (split-string string "\n")
             "\n"))

(defgeneric omboo-netscape--convert (object))

(defun omboo-netscape--convert-objects (objects)
  (mapconcat #'omboo-netscape--indent
             (-map #'omboo-netscape--convert objects)
             "\n"))

(defmethod omboo-netscape--convert ((directory omboo-bookmark-directory))
  (format "\
<DT><H3>%s</H3>
<DL><p>
%s
</DL><p>"
          (omboo-bookmark-directory-title directory)
          (omboo-netscape--convert-objects
           (omboo-bookmark-directory-children directory))))

(defmethod omboo-netscape--convert ((bookmark omboo-bookmark-bookmark))
  (format "<DT><A HREF=\"%s\">%s</A>"
          (omboo-bookmark-bookmark-url bookmark)
          (omboo-bookmark-bookmark-title bookmark)))

(defun omboo-netscape--coding-system ()
  (or omboo-netscape-coding-system
      buffer-file-coding-system))

(defun omboo-netscape-convert (bookmarks)
  (format "\
<!DOCTYPE NETSCAPE-Bookmark-file-1>
<!-- This is an automatically generated file.
     It will be read and overwritten.
     DO NOT EDIT! -->
<META HTTP-EQUIV=\"Content-Type\" CONTENT=\"text/html; charset=%s\">
<TITLE>Bookmarks</TITLE>
<H1>Bookmarks</H1>

<DL><p>
%s
</DL>
"
          (upcase
           (symbol-name
            (coding-system-base (omboo-netscape--coding-system))))
          (omboo-netscape--convert-objects bookmarks)))

(defun omboo-netscape-export-to-file (file bookmarks)
  (with-temp-file file
    (setq buffer-file-coding-system (omboo-netscape--coding-system))
    (insert (omboo-netscape-convert bookmarks))))

(provide 'omboo-netscape)
;;; omboo-netscape.el ends here
