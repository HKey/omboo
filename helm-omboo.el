;;; helm-omboo.el --- helm for omboo                 -*- lexical-binding: t; -*-

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

(require 'omboo)
(require 'omboo-bookmark)
(require 'helm)

(defvar helm-source-omboo
  (helm-make-source "Omboo Bookmarks" 'helm-source-sync
    :candidates #'helm-omboo-make-candidates
    :action     '(("Open with web browser" . browse-url))
    :multiline  t
    :migemo     t))

(defun helm-omboo--get-bookmarks (object)
  (cl-typecase object
    (omboo-bookmark-bookmark
     (list object))
    (omboo-bookmark-directory
     (-mapcat #'helm-omboo--get-bookmarks
              (omboo-bookmark-directory-children object)))))

(defun helm-omboo-make-candidates ()
  (--map (cons
          (concat (omboo-bookmark-bookmark-title it) "\n"
                  (omboo-bookmark-bookmark-url it))
          (omboo-bookmark-bookmark-url it))
         (-mapcat #'helm-omboo--get-bookmarks omboo-bookmarks)))

;;;###autoload
(defun helm-omboo ()
  (interactive)
  (helm :sources '(helm-source-omboo)))

(provide 'helm-omboo)
;;; helm-omboo.el ends here
