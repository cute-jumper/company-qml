;;; qmltypes-parser.el --- Naive parser for plugins.qmltypes file

;; Copyright (C) 2015  Junpeng Qiu

;; Author: Junpeng Qiu <qjpchmail@gmail.com>
;; Package-Requires: ((cl-lib "0.5"))
;; Keywords: extensions

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

;; A naive parser for plugins.qmltypes file. This file is internally used by
;; company-qml.el.

;;; Code:

(require 'json)
(require 'cl-lib)
(require 'pcase)

(cl-defstruct qmltypes-parser-type-info
  name prototype exports enums properties methods signals)

;;; Syntax:
;;; object = symbol {name:json(;|\n)object}

(defun qmltypes-parser--do-parse (name)
  "Recursively parse the current QML structure of NAME."
  ;; TODO error checking
  (let ((elements '())
        (json-key-type 'string)
        word key value)
    (json-skip-whitespace)
    ;; skip {
    (json-advance)
    (json-skip-whitespace)
    (while (not (char-equal (json-peek) ?}))
      (while (looking-at "//")
        (forward-line)
        (json-skip-whitespace))
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
        (push (qmltypes-parser--do-parse key) elements))
      (json-skip-whitespace)
      (skip-chars-forward ";")
      (json-skip-whitespace))
    (json-advance)
    `(,name . ,(reverse elements))))

(defun qmltypes-parser-parse-from-point ()
  "Parse plugins.qmltypes file from current point.
The point should be placed before \"name {...}.\""
  (let (name)
    (json-skip-whitespace)
    (setq name (thing-at-point 'word t))
    (forward-word)
    (qmltypes-parser--do-parse name)))

(defun qmltypes-parser-parse-string (s)
  "Parse from string S."
  (with-temp-buffer
    (insert s)
    (goto-char (point-min))
    (qmltypes-parser-parse-from-point)))

(defun qmltypes-parser-parse-file (file)
  "Parse from FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (re-search-forward "Module")
    (beginning-of-line)
    (qmltypes-parser-parse-from-point)))

(defun qmltypes-parser--extract-type-info (component-alist type-info-table)
  "Extract and append type info from COMPONENT-ALIST to TYPE-INFO-TABLE."
  (dolist (component (cdr component-alist))
    (when (string= (car component) "Component")
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
        (let ((type-info (make-qmltypes-parser-type-info :name name
                                                         :prototype prototype
                                                         :exports exports
                                                         :enums enums
                                                         :properties properties
                                                         :methods methods
                                                         :signals signals)))
          (puthash name type-info type-info-table))))))

(defun qmltypes-parser-init (file-list)
  "Initialize the parser and return the table of type information."
  (let ((type-info-table (make-hash-table :test 'equal))
        component-alist)
    (dolist (fn file-list)
      (setq component-alist (qmltypes-parser-parse-file fn))
      (qmltypes-parser--extract-type-info component-alist type-info-table))
    type-info-table))

(provide 'qmltypes-parser)
;;; qmltypes-parser.el ends here
