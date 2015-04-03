;;; omboo-bamboo.el --- bamboo feed reader library for omboo -*- lexical-binding: t; -*-

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
(require 'eieio)
(require 'dash)
(require 'json)
(require 'anaphora)
(require 'omboo-bookmark)

;;;; Variables

(defvar omboo-bamboo-ignore-empty-groups-p t)

(defvar omboo-bamboo--feed-integration-table nil)

(defvar omboo-bamboo--group-integration-table nil)

;;;; Methods

(defgeneric omboo-bamboo--update-visible-item-count (object))

(defgeneric omboo-bamboo--update-unreaded-count (object))

(defgeneric omboo-bamboo--convert-to-json-plist (object))

(defgeneric omboo-bamboo--register-integration-table (object))

(defgeneric omboo-bamboo--integration-table-get (object))

(defgeneric omboo-bamboo--make-integration-parameters (new old))

(defgeneric omboo-bamboo--integrate (object))

;;;; Classes

;; bamboo.data.base

(defvar omboo-bamboo--current-id 0)

(defun omboo-bamboo--create-id ()
  "bamboo.factory.create"
  (cl-incf omboo-bamboo--current-id))

(defclass omboo-bamboo-base nil
  ((id
    :type          integer
    :initarg       :id
    :reader        omboo-bamboo-data-id
    :documentation "bamboo.data.base.id")
   (selected
    :type          boolean
    :initform      nil
    :initarg       :selected
    :reader        omboo-bamboo-data-selected
    :documentation "bamboo.data.base.selected"))
  (:documentation "bamboo.data.base"))

;; eieioでは `defmethod' で:aroundが使えないようなので、
;; 個別に初期化関数を用意する。
(defun omboo-bamboo--make-base (class &rest keys)
  ;; :initformに書くと型が違うとしてエラーになるのでここに書く
  (cl-callf or (cl-getf keys :id) (omboo-bamboo--create-id))
  (apply #'make-instance class keys))

(defun omboo-bamboo-make-base (&rest keys)
  (apply #'omboo-bamboo--make-base 'omboo-bamboo-base keys))

(defmethod omboo-bamboo--convert-to-json-plist ((this omboo-bamboo-base))
  (append
   (list :id
         (omboo-bamboo-data-id this)
         :selected
         (or (omboo-bamboo-data-selected this) json-false))
   (when (next-method-p)
     (call-next-method this))))

(defmethod omboo-bamboo--register-integration-table ((_this omboo-bamboo-base))
  nil)

(defmethod omboo-bamboo--make-integration-parameters ((new omboo-bamboo-base)
                                                      old)
  (when (next-method-p)
    (call-next-method new old)))

(defmethod omboo-bamboo--integration-table-get ((_this omboo-bamboo-base))
  nil)

(defmethod omboo-bamboo--integrate ((this omboo-bamboo-base))
  this)

;; bamboo.data.item

(defclass omboo-bamboo-item (omboo-bamboo-base)
  ((title
    :type          string
    :initarg       :title
    :reader        omboo-bamboo-data-title
    :documentation "bamboo.data.item.name")
   (content
    :type          string
    :initarg       :content
    :reader        omboo-bamboo-data-content
    :documentation "bamboo.data.item.content")
   (url
    :type          string
    :initarg       :url
    :reader        omboo-bamboo-data-url
    :documentation "bamboo.data.item.url")
   (date
    :type          integer
    :initarg       :date
    :reader        omboo-bamboo-data-date
    :documentation "bamboo.data.item.date")
   (readed
    :type          boolean
    :initarg       :readed
    :reader        omboo-bamboo-data-readed
    :documentation "bamboo.data.item.readed")
   (author
    :type          (or string null)
    :initform      nil
    :initarg       :author
    :reader        omboo-bamboo-data-author
    :documentation "bamboo.data.item.author")
   (item-id
    :type          (or string null)
    :initarg       :item-id
    :reader        omboo-bamboo-data-item-id
    :documentation "bamboo.data.item.itemID"))
  (:documentation "bamboo.data.item"))

(defun omboo-bamboo--make-item (class &rest keys)
  (apply #'omboo-bamboo--make-base class keys))

(defun omboo-bamboo-make-item (&rest keys)
  (apply #'omboo-bamboo--make-item 'omboo-bamboo-item keys))

(defmethod omboo-bamboo--convert-to-json-plist ((this omboo-bamboo-item))
  (append
   (list :title
         (omboo-bamboo-data-title this)
         :content
         (omboo-bamboo-data-content this)
         :url
         (omboo-bamboo-data-url this)
         :date
         (omboo-bamboo-data-date this)
         :readed
         (or (omboo-bamboo-data-readed this) json-false)
         :author
         (omboo-bamboo-data-author this)
         :itemID
         (omboo-bamboo-data-item-id this))
   (when (next-method-p)
     (call-next-method this))))

;; bamboo.data.feed

(defclass omboo-bamboo-feed (omboo-bamboo-base)
  ((name
    :type          string
    :initarg       :name
    :reader        omboo-bamboo-data-name
    :documentation "bamboo.data.feed.name")
   (url
    :type          string
    :initarg       :url
    :reader        omboo-bamboo-data-url
    :documentation "bamboo.data.feed.url")
   (webpage
    :type          string
    :initarg       :webpage
    :reader        omboo-bamboo-data-webpage
    :documentation "bamboo.data.feed.webpage")
   (unread-item-count
    :type          integer
    :initform      0
    :initarg       :unread-item-count
    :reader        omboo-bamboo-data-unread-item-count
    :documentation "bamboo.data.feed.unreadItemCount")
   (visible-item-count
    :type          integer
    :initform      0
    :initarg       :visible-item-count
    :reader        omboo-bamboo-data-visible-item-count
    :documentation "bamboo.data.feed.visibleItemCount")
   (loading
    :type          boolean
    :initform      nil
    :initarg       :loading
    :reader        omboo-bamboo-data-loading
    :documentation "bamboo.data.feed.loading")
   (waiting
    :type          boolean
    :initform      nil
    :initarg       :waiting
    :reader        omboo-bamboo-data-waiting
    :documentation "bamboo.data.feed.waiting")
   (favorite
    :type          boolean
    :initform      nil
    :initarg       :favorite
    :reader        omboo-bamboo-data-favorite
    :documentation "bamboo.data.feed.favorite")
   (is-rtl
    :type          string
    :initform      "notset"
    :initarg       :is-rtl
    :reader        omboo-bamboo-data-is-rtl
    :documentation "bamboo.data.feed.isRTL")
   (error
    :initform      nil
    :initarg       :error
    :reader        omboo-bamboo-data-error
    :documentation "bamboo.data.feed.error")
   (items
    :type          list
    :initform      nil
    :initarg       :items
    :reader        omboo-bamboo-data-items
    :documentation "bamboo.data.feed.items"))
  (:documentation "bamboo.data.feed"))

(defun omboo-bamboo--make-feed (class &rest keys)
  (aprog1 (apply #'omboo-bamboo--make-base class keys)
    (omboo-bamboo--update-visible-item-count it)
    (omboo-bamboo--update-unreaded-count it)))

(defun omboo-bamboo-make-feed (&rest keys)
  (apply #'omboo-bamboo--make-feed 'omboo-bamboo-feed keys))

(defmethod omboo-bamboo--update-visible-item-count ((this omboo-bamboo-feed))
  (with-slots (visible-item-count items) this
    (setf visible-item-count (length items))))

(defmethod omboo-bamboo--update-unreaded-count ((this omboo-bamboo-feed))
  (with-slots (unread-item-count items) this
    (setf unread-item-count
          (--count (not (omboo-bamboo-data-readed it))
                   items))))

(defmethod omboo-bamboo--convert-to-json-plist ((this omboo-bamboo-feed))
  (append
   (list :name
         (omboo-bamboo-data-name this)
         :url
         (omboo-bamboo-data-url this)
         :webpage
         (omboo-bamboo-data-webpage this)
         :unreadItemCount
         (omboo-bamboo-data-unread-item-count this)
         :visibleItemCount
         (omboo-bamboo-data-visible-item-count this)
         :loading
         (or (omboo-bamboo-data-loading this) json-false)
         :waiting
         (or (omboo-bamboo-data-waiting this) json-false)
         :favorite
         (or (omboo-bamboo-data-favorite this) json-false)
         :isRTL
         (omboo-bamboo-data-is-rtl this)
         :error
         (omboo-bamboo-data-error this)
         :items
         (cl-coerce (-map #'omboo-bamboo--convert-to-json-plist
                          (omboo-bamboo-data-items this))
                    'vector))
   (when (next-method-p)
     (call-next-method this))))

(defmethod omboo-bamboo--register-integration-table ((this omboo-bamboo-feed))
  (when (hash-table-p omboo-bamboo--feed-integration-table)
    (puthash (omboo-bamboo-data-url this)
             this
             omboo-bamboo--feed-integration-table)))

(defmethod omboo-bamboo--integration-table-get ((this omboo-bamboo-feed))
  (when (hash-table-p omboo-bamboo--feed-integration-table)
    (gethash (omboo-bamboo-data-url this)
             omboo-bamboo--feed-integration-table)))

(defmethod omboo-bamboo--make-integration-parameters ((new omboo-bamboo-feed) old)
  (append
   (list :name (omboo-bamboo-data-name new)
         :url  (omboo-bamboo-data-url new)
         :webpage (omboo-bamboo-data-webpage new)
         ;; :unread-item-count
         ;; :visible-item-count
         :loading (omboo-bamboo-data-loading old)
         :waiting (omboo-bamboo-data-waiting old)
         :favorite (omboo-bamboo-data-favorite old)
         :is-rtl (omboo-bamboo-data-is-rtl old)
         :error (omboo-bamboo-data-error old)
         :items (omboo-bamboo-data-items old))
   (when (next-method-p)
     (call-next-method new old))))

(defmethod omboo-bamboo--integrate ((this omboo-bamboo-feed))
  (aif (omboo-bamboo--integration-table-get this)
      (apply #'omboo-bamboo-make-feed
             (omboo-bamboo--make-integration-parameters this it))
    this))

;; bamboo.data.group

(defclass omboo-bamboo-group (omboo-bamboo-base)
  ((name
    :type          string
    :initarg       :name
    :reader        omboo-bamboo-data-name
    :documentation "bamboo.data.group.name")
   (loading
    :type          boolean
    :initform      nil
    :initarg       :loading
    :reader        omboo-bamboo-data-loading
    :documentation "bamboo.data.group.loading")
   (waiting
    :type          boolean
    :initform      nil
    :initarg       :waiting
    :reader        omboo-bamboo-data-waiting
    :documentation "bamboo.data.group.waiting")
   (error-feeds
    :type          list                 ;list of strings
    :initform      nil
    :initarg       :error-feeds
    :reader        omboo-bamboo-data-error-feeds
    :documentation "bamboo.data.group.errorFeeds")
   (open
    :type          boolean
    :initform      nil
    :initarg       :open
    :reader        omboo-bamboo-data-open
    :documentation "bamboo.data.group.open")
   (unread-item-count
    :type          integer
    :initform      0
    :initarg       :unread-item-count
    :reader        omboo-bamboo-data-unread-item-count
    :documentation "bamboo.data.group.unreadItemCount")
   (visible-item-count
    :type          integer
    :initform      0
    :initarg       :visible-item-count
    :reader        omboo-bamboo-data-visible-item-count
    :documentation "bamboo.data.group.visibleItemCount")
   (groups
    :type          list
    :initform      nil
    :initarg       :groups
    :reader        omboo-bamboo-data-groups
    :documentation "bamboo.data.group.groups")
   (feeds
    :type          list
    :initform      nil
    :initarg       :feeds
    :reader        omboo-bamboo-data-feeds
    :documentation "bamboo.data.group.feeds"))
  (:documentation "bamboo.data.feed"))

(defun omboo-bamboo--make-group (class &rest keys)
  (let ((not-found (cl-gensym)))
    (when (eq not-found (cl-getf keys :name not-found))
      (error "name parameter is required")))
  (aprog1 (apply #'omboo-bamboo--make-base class keys)
    (omboo-bamboo--update-visible-item-count it)
    (omboo-bamboo--update-unreaded-count it)))

(defun omboo-bamboo-make-group (&rest keys)
  (apply #'omboo-bamboo--make-group 'omboo-bamboo-group keys))

(defmethod omboo-bamboo--update-visible-item-count ((this omboo-bamboo-group))
  (with-slots (visible-item-count groups feeds) this
    (setf visible-item-count
          (-sum
           (-map #'omboo-bamboo-data-visible-item-count
                 (append groups feeds))))))

(defmethod omboo-bamboo--update-unreaded-count ((this omboo-bamboo-group))
  (with-slots (unread-item-count groups feeds) this
    (setf unread-item-count
          (-sum
           (-map #'omboo-bamboo-data-unread-item-count
                 (append groups feeds))))))

(defmethod omboo-bamboo--convert-to-json-plist ((this omboo-bamboo-group))
  (append
   (list :name
         (omboo-bamboo-data-name this)
         :loading
         (or (omboo-bamboo-data-loading this) json-false)
         :waiting
         (or (omboo-bamboo-data-waiting this) json-false)
         :errorFeeds
         (cl-coerce (omboo-bamboo-data-error-feeds this) 'vector)
         :open
         (or (omboo-bamboo-data-open this) json-false)
         :unreadItemCount
         (omboo-bamboo-data-unread-item-count this)
         :visibleItemCount
         (omboo-bamboo-data-visible-item-count this)
         :groups
         (cl-coerce (-map #'omboo-bamboo--convert-to-json-plist
                          (omboo-bamboo-data-groups this))
                    'vector)
         :feeds
         (cl-coerce (-map #'omboo-bamboo--convert-to-json-plist
                          (omboo-bamboo-data-feeds this))
                    'vector))
   (when (next-method-p)
     (call-next-method this))))

(defmethod omboo-bamboo--register-integration-table ((this omboo-bamboo-group))
  (when (hash-table-p omboo-bamboo--group-integration-table)
    (puthash (omboo-bamboo-data-name this)
             this
             omboo-bamboo--group-integration-table)
    (-map #'omboo-bamboo--register-integration-table
          (append (omboo-bamboo-data-groups this)
                  (omboo-bamboo-data-feeds this)))))

(defmethod omboo-bamboo--integration-table-get ((this omboo-bamboo-group))
  (when (hash-table-p omboo-bamboo--group-integration-table)
    (gethash (omboo-bamboo-data-name this)
             omboo-bamboo--group-integration-table)))

(defmethod omboo-bamboo--make-integration-parameters ((new omboo-bamboo-group) old)
  (append
   (list :name (omboo-bamboo-data-name new)
         :loading (omboo-bamboo-data-loading old)
         :waiting (omboo-bamboo-data-waiting old)
         :error-feeds (omboo-bamboo-data-error-feeds old)
         :open (omboo-bamboo-data-open old)
         ;; :unread-item-count
         ;; :visible-item-count
         :groups (-map #'omboo-bamboo--integrate
                       (omboo-bamboo-data-groups new))
         :feeds (-map #'omboo-bamboo--integrate
                      (omboo-bamboo-data-feeds new)))
   (when (next-method-p)
     (call-next-method new old))))

(defmethod omboo-bamboo--integrate ((this omboo-bamboo-group))
  (apply #'omboo-bamboo-make-group
         (omboo-bamboo--make-integration-parameters
          this
          (or (omboo-bamboo--integration-table-get this) this))))

;; bamboo.data.root

(defclass omboo-bamboo-root (omboo-bamboo-group)
  ((old-unread-item-count
    :type          integer
    :initarg       :old-unread-item-count
    :initform      0
    :reader        omboo-bamboo-data-old-unread-item-count
    :documentation "bamboo.data.root.oldUnreadItemCount")
   (favorite-unread-item-count
    :type          integer
    :initarg       :favorite-unread-item-count
    :initform      0
    :reader        omboo-bamboo-data-favorite-unread-item-count
    :documentation "bamboo.data.root.favoriteUnreadItemCount"))
  (:documentation "bamboo.data.root"))

(defun omboo-bamboo--make-root (class &rest keys)
  (apply #'omboo-bamboo--make-group
         class
         :name "ROOT"
         :open t
         keys))

(defun omboo-bamboo-make-root (&rest keys)
  (apply #'omboo-bamboo--make-root 'omboo-bamboo-root keys))

(defmethod omboo-bamboo--convert-to-json-plist ((this omboo-bamboo-root))
  (append
   (list :oldUnreadItemCount
         (omboo-bamboo-data-old-unread-item-count this)
         :favoriteUnreadItemCount
         (omboo-bamboo-data-favorite-unread-item-count this))
   (when (next-method-p)
     (call-next-method this))))

(defmethod omboo-bamboo--register-integration-table ((this omboo-bamboo-root))
  (-map #'omboo-bamboo--register-integration-table
        (append (omboo-bamboo-data-groups this)
                (omboo-bamboo-data-feeds this))))

(defmethod omboo-bamboo--integration-table-get ((_this omboo-bamboo-root))
  nil)

;;;; Conversion to JSON

(defun omboo-bamboo-convert-to-json (object)
  (json-encode-plist
   (omboo-bamboo--convert-to-json-plist object)))

(defun omboo-bamboo-output-json-file (object file)
  (with-temp-file file
    (insert
     (omboo-bamboo-convert-to-json object))))

;;;; Conversion from omboo-bookmark objects

(defun omboo-bamboo--convert-bookmark (bookmark)
  (omboo-bamboo-make-feed
   :name    (omboo-bookmark-bookmark-title bookmark)
   :url     (omboo-bookmark-bookmark-feed  bookmark)
   :webpage (omboo-bookmark-bookmark-url   bookmark)))

(defun omboo-bamboo--empty-group-p (group)
  (and (null (omboo-bamboo-data-feeds group))
       (-every-p #'omboo-bamboo--empty-group-p
                 (omboo-bamboo-data-groups group))))

(defun omboo-bamboo--convert-directory (directory)
  (omboo-bamboo-make-group
   :name (omboo-bookmark-directory-title directory)
   :groups (--keep
            (when (omboo-bookmark-directory-p it)
              (let ((group (omboo-bamboo--convert-directory it)))
                (unless (and omboo-bamboo-ignore-empty-groups-p
                             (omboo-bamboo--empty-group-p group))
                  group)))
            (omboo-bookmark-directory-children directory))
   :feeds (--keep
           (when (and (omboo-bookmark-bookmark-p it)
                      (omboo-bookmark-bookmark-feed it))
             (omboo-bamboo--convert-bookmark it))
           (omboo-bookmark-directory-children directory))))

(defun omboo-bamboo-convert-bookmark-object (object)
  (cond ((omboo-bookmark-bookmark-p object)
         (omboo-bamboo--convert-bookmark object))
        ((omboo-bookmark-directory-p object)
         (omboo-bamboo--convert-directory object))))

(defun omboo-bamboo-convert-bookmark-objects-as-root (objects)
  (let ((bamboo-objects (-map #'omboo-bamboo-convert-bookmark-object objects)))
    (omboo-bamboo-make-root
     :groups (-filter #'omboo-bamboo-group-p bamboo-objects)
     :feeds  (-filter #'omboo-bamboo-feed-p  bamboo-objects))))

;;;; Parsing JSON

;; Predicates

(defun omboo-bamboo--item-plist-p (plist)
  (--every-p (plist-member plist it)
             '(:title
               :content
               :date
               :readed
               ;; :author
               :itemID)))

(defun omboo-bamboo--feed-plist-p (plist)
  (--every-p (plist-member plist it)
             '(:webpage :favorite :isRTL :error :items)))

(defun omboo-bamboo--group-plist-p (plist)
  (--every-p (plist-member plist it)
             '(:errorFeeds :open :groups :feeds)))

(defun omboo-bamboo--root-plist-p (plist)
  (and (omboo-bamboo--group-plist-p plist)
       (string= "ROOT" (cl-getf plist :name))
       (plist-member plist :oldUnreadItemCount)
       (plist-member plist :favoriteUnreadItemCount)))

;; Parsing plist

(defun omboo-bamboo--convert-json-keywords (plist)
  (--map (cl-case it
           (:itemID                  :item-id)
           (:unreadItemCount         :unread-item-count)
           (:visibleItemCount        :visible-item-count)
           (:isRTL                   :is-rtl)
           (:errorFeeds              :error-feeds)
           (:oldUnreadItemCount      :old-unread-item-count)
           (:favoriteUnreadItemCount :favorite-unread-item-count)
           (t                        it))
         plist))

(defun omboo-bamboo--parse-json-plist-to-item (plist)
  (apply #'omboo-bamboo-make-item
         (omboo-bamboo--convert-json-keywords plist)))

(defun omboo-bamboo--parse-json-plist-to-feed (plist)
  (let ((keys (omboo-bamboo--convert-json-keywords plist)))
    (cl-callf2 -map #'omboo-bamboo--parse-json-plist-to-item
               (cl-getf keys :items))
    (apply #'omboo-bamboo-make-feed keys)))

(defun omboo-bamboo--parse-json-plist-to-group (plist)
  (let ((keys (omboo-bamboo--convert-json-keywords plist)))
    (cl-callf2 -map #'omboo-bamboo--parse-json-plist-to-group
               (cl-getf keys :groups))
    (cl-callf2 -map #'omboo-bamboo--parse-json-plist-to-feed
               (cl-getf keys :feeds))
    (apply #'omboo-bamboo-make-group keys)))

(defun omboo-bamboo--parse-json-plist-to-root (plist)
  (let ((keys (omboo-bamboo--convert-json-keywords plist)))
    (cl-callf2 -map #'omboo-bamboo--parse-json-plist-to-group
               (cl-getf keys :groups))
    (cl-callf2 -map #'omboo-bamboo--parse-json-plist-to-feed
               (cl-getf keys :feeds))
    (apply #'omboo-bamboo-make-root keys)))

(defun omboo-bamboo-parse-json-plist (plist)
  (cond ((omboo-bamboo--item-plist-p plist)
         (omboo-bamboo--parse-json-plist-to-item plist))
        ((omboo-bamboo--feed-plist-p plist)
         (omboo-bamboo--parse-json-plist-to-feed plist))
        ((omboo-bamboo--root-plist-p plist)
         (omboo-bamboo--parse-json-plist-to-root plist))
        ((omboo-bamboo--group-plist-p plist)
         (omboo-bamboo--parse-json-plist-to-group plist))))

;; Parsing JSON file

(defun omboo-bamboo-parse-json-file (file)
  (let ((json-array-type  'list)
        (json-object-type 'plist)
        (json-key-type    'keyword)
        (json-false       nil))
    (omboo-bamboo-parse-json-plist
     (json-read-file file))))

;;;; Integration

(defun omboo-bamboo-integrate (new old)
  (let ((omboo-bamboo--feed-integration-table
         (make-hash-table :test #'equal))
        (omboo-bamboo--group-integration-table
         (make-hash-table :test #'equal)))
    (omboo-bamboo--register-integration-table old)
    (omboo-bamboo--integrate new)))

(defun omboo-bamboo-update-json-file-with-bookmarks (json-file
                                                     bookmarks
                                                     &optional
                                                     dont-backup)
  (unless dont-backup
    (copy-file json-file (concat json-file ".bak") t t))
  (omboo-bamboo-output-json-file
   (omboo-bamboo-integrate
    (omboo-bamboo-convert-bookmark-objects-as-root bookmarks)
    (omboo-bamboo-parse-json-file json-file))
   json-file))


(provide 'omboo-bamboo)
;;; omboo-bamboo.el ends here
