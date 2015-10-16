;;; qmltypes-parser.el --- Naive parser for plugins.qmltypes  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Junpeng Qiu

;; Author: Junpeng Qiu <qjpchmail@gmail.com>
;; Keywords: convenience

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

(require 'json)
(require 'cl-lib)

(defun qmltypes-parse ()
  (let (name)
    (json-skip-whitespace)
    (setq name (thing-at-point 'word t))
    (forward-word)
    (qmltypes--do-parse name)))

(defun qmltypes--do-parse (name)
  ;; TODO error checking
  (let ((elements '())
        (json-key-type 'string)
        word key value)
    (json-skip-whitespace)
    ;; skip {
    (json-advance)
    (json-skip-whitespace)
    (while (not (char-equal (json-peek) ?}))
      (setq key (thing-at-point 'word t))
      (forward-word)
      (if (char-equal (json-peek) ?:)
          (let (start)
            (json-advance)
            (json-skip-whitespace)
            (setq start (point))
            (forward-sexp)
            (setq value
                  (json-read-from-string
                   (buffer-substring-no-properties start (point))))
            (push (list key value) elements))
        (push (qmltypes--do-parse key) elements))
      (json-skip-whitespace)
      (skip-chars-forward ";")
      (json-skip-whitespace))
    (json-advance)
    `(,name . ,(reverse elements))))

(defun qmltypes-read-table (component-alist lookup-table)
  (dolist (component (cdr component-alist))
    (let* (name prototype exports enums properties methods signals)
      (dolist (key-value (cdr component))
        (pcase (car key-value)
          ("name" (setq name (cadr key-value)))
          ("prototype" (setq prototype (cadr key-value)))
          ("exports" (setq exports (cadr key-value)))
          ("Enum" (let ((values (cadr (assoc "values" (cdr key-value)))))
                    (mapc (lambda (x) (push (car x) enums)) values)))
          ("Property" (push (cadr (assoc "name" (cdr key-value))) properties))
          ("Method" (push (cadr (assoc "name" (cdr key-value))) methods))
          ("Signal" (let ((name (cadr (assoc "name" (cdr key-value)))))
                      (push (concat "on" (upcase-initials name))  signals)))))
      (let ((item (make-qmltype :name name
                                :prototype prototype
                                :exports exports
                                :enums enums
                                :properties properties
                                :methods methods
                                :signals signals)))
        (puthash name item lookup-table)))))

(cl-defstruct qmltype name prototype exports enums properties methods signals)
(setq table (my-parse-file "/usr/lib/qt/imports/builtins.qmltypes"))
(setq lookup-table (make-hash-table :test 'equal))
(qmltypes-read-table table lookup-table)
(cl-defstruct completion-data name path qmltype-name)
(setq user-lookup-table (setup-user-lookup-table lookup-table))
(get-all-completions "Animation" "QtQuick1.0" lookup-table user-lookup-table)

(defun setup-user-lookup-table (lookup-table)
  (let ((user-lookup-table (make-hash-table :test 'equal)))
    (maphash (lambda (k v)
               (let ((exports (qmltype-exports v))
                     parts names)
                 (when exports
                   (mapc (lambda (expo)
                           (setq parts (split-string expo " "))
                           (setq names (split-string (car parts) "/"))
                           (puthash (cadr names)
                                    (make-completion-data
                                     :name (cadr names)
                                     :path (concat (car names) (cadr parts))
                                     :qmltype-name (qmltype-name v))
                                    user-lookup-table))
                         exports))))
             lookup-table)
    user-lookup-table))


(defun do-get-all-completions (name lookup-table results)
  (let* ((item (gethash name lookup-table))
         (item-name name)
         (completions (car results))
         (visited (cdr results)))
    (while (and item (not (member item-name visited)))
      (push item-name visited)
      (setq completions (append completions
                                (qmltype-enums item)
                                (qmltype-properties item)
                                (qmltype-methods item)
                                (qmltype-signals item)))
      (setq item-name (qmltype-prototype item))
      (setq item (gethash item-name lookup-table)))
    `(,completions . ,visited)))

(defun get-all-completions (name path lookup-table user-lookup-table)
  (let ((suffix "Attached")
        (data (gethash name user-lookup-table))
        results
        real-name)
    (when (and data (string= (completion-data-path data) path))
      (setq real-name (completion-data-qmltype-name data))
      (setq results (do-get-all-completions real-name lookup-table results))
      (setq results (do-get-all-completions (concat real-name suffix) lookup-table results))
      (car results))))

(defun my-parse (s)
  (with-temp-buffer
    (insert s)
    (goto-char (point-min))
    (qmltypes-parse)))
(defun my-parse-file (file)
  "Read the first JSON object contained in FILE and return it."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (re-search-forward "Module")
    (beginning-of-line)
    (qmltypes-parse)))

(my-parse "
    Component {
        name: \"QDeclarativePropertyChanges\"
        isGood: true
        prototype: \"QDeclarativeStateOperation\"
        exports: [\"QtQuick/PropertyChanges 1.0\"]
        Property { name: \"target\"; type: \"QObject\"; isPointer: true }
        Property { name: \"restoreEntryValues\"; type: \"bool\" }
        Property { name: \"explicit\"; type: \"bool\" }
    }
")

;;; object = word {name:json(;|\n)object}

(provide 'qmltypes-parser)
;;; qmltypes-parser.el ends here
