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
(require 'pcase)

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
                      (push (concat "on" (upcase-initials name)) signals)))))
      (let ((item (make-qmltype :name name
                                :prototype prototype
                                :exports exports
                                :enums enums
                                :properties properties
                                :methods methods
                                :signals signals)))
        (puthash name item lookup-table)))))
(defvar qmltypes-file-list '("/usr/lib/qt/qml/QtQuick/Controls/plugins.qmltypes"
                             "/usr/lib/qt/qml/QtQuick/Dialogs/plugins.qmltypes"
                             "/usr/lib/qt/qml/QtQuick/Extras/plugins.qmltypes"
                             "/usr/lib/qt/qml/QtQuick/Layouts/plugins.qmltypes"
                             "/usr/lib/qt/qml/QtQuick/LocalStorage/plugins.qmltypes"
                             "/usr/lib/qt/qml/QtQuick/Particles.2/plugins.qmltypes"
                             "/usr/lib/qt/qml/QtQuick/PrivateWidgets/plugins.qmltypes"
                             "/usr/lib/qt/qml/QtQuick/Window.2/plugins.qmltypes"
                             "/usr/lib/qt/qml/QtQuick/XmlListModel/plugins.qmltypes"
                             "/usr/lib/qt/qml/QtQuick.2/plugins.qmltypes"))
(defvar global-lookup-table (make-hash :test 'equal))
("Qt" '("atob" "binding" "btoa" "colorEqual" "createComponent" "createQmlObject"
        "darker" "font" "fontFamilies" "formatDate" "formatDateTime" "formatTime"))

(cl-defstruct qmltype name prototype exports enums properties methods signals)
(cl-defstruct qml-completion name path completions)
(defvar user-lookup-table (make-hash-table :test 'equal))
(qmltypes-init)
(get-all-completions "Window" "QtQuick.Window2.2" t)
(get-all-completions "Win" "QtQuick.Window2.2" t)

(defun qmltypes-init ()
  (let ((lookup-table (make-hash-table :test 'equal))
        table)
    (dolist (fn qmltypes-file-list)
      (setq table (my-parse-file fn))
      (qmltypes-read-table table lookup-table))
    (setup-user-lookup-table lookup-table)))

(defun setup-user-lookup-table (lookup-table)
  (maphash
   (lambda (k v)
     (let ((exports (qmltype-exports v))
           parts modules name path completions results)
       (when exports
         (mapc
          (lambda (expo)
            (setq parts (split-string expo " "))
            (setq modules (split-string (car parts) "/"))
            (setq name (cadr modules))
            (setq path (concat (car modules) (cadr parts)))
            (push (cons name (construct-completions
                              (qmltype-name v)
                              lookup-table))
                  (gethash path user-lookup-table)))
          exports))))
   lookup-table))

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

(defun construct-completions (name lookup-table)
  (let ((suffix "Attached")
        results)
    (setq results (do-get-all-completions name lookup-table results))
    (setq results (do-get-all-completions (concat name suffix) lookup-table results))
    (car results)))

(defun get-all-completions (name path try-match-name-p)
  (let* ((alist (gethash path user-lookup-table)))
    (if try-match-name-p
        (delq nil
              (mapcar
               (lambda (x) (and (string-prefix-p name (car x)) (car x))) alist))
      (let ((comp (assoc name alist)))
        (and comp (cdr comp))))))

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
