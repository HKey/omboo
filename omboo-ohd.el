;;; omboo-ohd.el --- org-mode headline data library for omboo -*- lexical-binding: t; -*-

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

(require 'cl-lib)
(require 'org)
(require 'org-element)


;;;; Objects

(cl-defstruct (omboo-ohd-headline
               (:constructor omboo-ohd-make-headline))
  title
  children
  org-element)

;;;; Functions

(defun omboo-ohd-headline-get-property (headline property)
  (org-element-property property
                        (omboo-ohd-headline-org-element headline)))

;; Parsing

(defun omboo-ohd--parse-headline (headline)
  (let ((children (omboo-ohd--parse-child-headlines headline))
        (title (org-element-property :raw-value headline)))
    (omboo-ohd-make-headline :title title
                             :children children
                             :org-element headline)))

(defun omboo-ohd--parse-child-headlines (element)
  (org-element-map (org-element-contents element)
      'headline #'omboo-ohd--parse-headline nil nil 'headline))

;;;###autoload
(defun omboo-ohd-parse-file (file)
  (with-temp-buffer
    (insert-file-contents file)
    (org-mode)
    (omboo-ohd--parse-child-headlines
     (org-element-parse-buffer))))

(provide 'omboo-ohd)
;;; omboo-ohd.el ends here