;;; omboo.el --- Manage browser bookmarks in org-mode -*- lexical-binding: t; -*-

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

(require 'omboo-bookmark)
(require 'omboo-bamboo)
(require 'omboo-netscape)

;;;; Variables

(defvar omboo-bookmarks nil)

(defvar omboo-save-file (expand-file-name ".omboo" user-emacs-directory))

(defvar omboo-target-files nil
  "List of bookmark org-mode files or directories

The directories contain bookmark org-mode files.")

;;;; Commands

;;;###autoload
(defun omboo-save-bookmarks ()
  (interactive)
  (omboo-bookmark-save-file omboo-save-file omboo-bookmarks))

;;;###autoload
(defun omboo-load-bookmarks ()
  (interactive)
  (setq omboo-bookmarks (omboo-bookmark-load-file omboo-save-file)))

;;;###autoload
(defun omboo-update-bookmarks ()
  (interactive)
  (setq omboo-bookmarks
        (omboo-bookmark-parse-files omboo-target-files))
  (omboo-save-bookmarks))

;;;###autoload
(defun omboo-open-at-point ()
  "Open a bookmark url at the current point"
  (interactive)
  (let ((url (omboo-bookmark-get-url-at-point)))
    (if url
        (browse-url url)
      (user-error "Not a bookmark"))))

;; Bamboo

;;;###autoload
(defun omboo-export-to-bamboo-feeds-json (json-file)
  (interactive "Ffeeds.json: ")
  (omboo-bamboo-export-to-json-file
   (omboo-bamboo-convert-bookmark-objects-as-root omboo-bookmarks)
   json-file))

;;;###autoload
(defun omboo-update-bamboo-feeds-json (json-file &optional dont-backup)
  (interactive "ffeeds.json: \nP")
  (omboo-bamboo-update-json-file-with-bookmarks
   json-file
   omboo-bookmarks
   dont-backup))

;; Netscape Bookmark

;;;###autoload
(defun omboo-export-to-netscape-bookmark (bookmark-file)
  (interactive "Fbookmark file: ")
  (omboo-netscape-export-to-file bookmark-file
                                 omboo-bookmarks))

(provide 'omboo)
;;; omboo.el ends here
