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

(defun my-read-string (type str)
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    (cond ((eq type :property) (read-property))
          ((eq type :enum) (read-enum))
          ((eq type :method) (read-method))
          ((eq type :signal) (read-signal)))))

(my-read-string :enum "{
            name: \"HAlignment\"
            values: {
                \"AlignLeft\": 1,
                \"AlignRight\": 2,
                \"AlignHCenter\": 4,
                \"AlignJustify\": 8
            }
foo: abc

}
")
(defun read-enum ()
  (let ((elements '()))
    (json-advance)
    (json-skip-whitespace)
    (while (not (char-equal (json-peek) ?}))
      (setq word (word-at-point))
      (message "%s" word)
      (if (string= word "name")
          (progn
            (json-advance (length word))
            ;; TODO: check ?:
            (json-advance)
            (json-skip-whitespace)
            (push `(,word . ,(json-read-string)) elements))
        (if (string= word "values")
            (progn
              (json-advance (length word))
              ;; TODO: check ?:
              (json-advance)
              (json-skip-whitespace)
              (push `(,word . ,(json-read-object)) elements))))
      (skip-to-next-item))
    elements))
(defun skip-to-next-item ()
  (skip-chars-forward "^;\r\n")
  (json-advance)
  (json-skip-whitespace)
  (point))
(my-read-string :property "{
name: \"abc\"
type: \"int\";revision: 2;   isReadOnly: true
}")
(defun read-property ()
  "Starting from {."
  (let ((elements '()))
    (json-advance)
    (json-skip-whitespace)
    (while (not (char-equal (json-peek) ?}))
      (setq word (word-at-point))
      (message "word: %s" word)
      (if (or (string= word "name") (string= word "type"))
          (progn
            (json-advance (length word))
            ;; TODO: check ?:
            (json-advance)
            (json-skip-whitespace)
            (push `(,word . ,(json-read-string)) elements)))
      (skip-to-next-item))
    (json-advance)
    elements))
(my-read-string :method "{
            name: \"select\"
            Parameter { name: \"start\"; type: \"int\" }
            Parameter { name: \"end\"; type: \"int\" }
}")
(defun read-method ()
  (let ((elements '(("Parameters" . '()))))
    (json-advance)
    (json-skip-whitespace)
    (while (not (char-equal (json-peek) ?}))
      (setq word (word-at-point))
      (message "word: %s" word)
      (if (string= word "name")
          (progn
            (json-advance (length word))
            ;; TODO: check ?:
            (json-advance)
            (json-skip-whitespace)
            (push `(,word . ,(json-read-string)) elements))
        (if (string= word "Parameter")
            (progn
              (setq v (assoc "Parameters" elements))
              (json-advance (length word))
              ;; TODO: check ?:
              (json-skip-whitespace)
              (push (read-property) v))
          (skip-to-next-item)))
      (json-skip-whitespace))
    elements))
(defun read-signal ())
(defun read-exports ())
(defun read-prototype ())

(defun read-object ()
  "Read qml object at point."
  (let ((elements (json-new-object)) object-name
        key value)
    (json-skip-whitespace)
    (setq object-name (thing-at-point 'word t))
    (json-skip-whitespace)
    (unless (not (char-equal (json-peek) ?{))
      (error "Error"))
    (json-advance)
    (json-skip-whitespace)
    (setq key (thing-at-point 'word t))
    (json-skip-whitespace)
    (if (char-equal (json-peek) ?:)
        (setq value (read-json))
      (if (char-equal (json-peek) :{)
          (setq value (read-member))))
    (setq elements (push (word key value) elements))
    (unless (char-equal (json-peek) ?})
      (json-skip-whitespace)
      (setq key (thing-at-point 'word t))
      (message "%s" key)
      (json-skip-whitespace)
      (if (char-equal (json-peek) ?:)
          (json-advance)
        (if (char-equal (json-peek) ?{)
            (setq value (read-object) v)
          (setq value (thing-at-point 'word t))
          (setq elements (json-add-to-object elements key value))
          (json-skip-whitespace))))
    ;; Skip over the "}"
    (json-advance)
    elements))

(my-read-string "Module
{
   Component {
      name: \"abc\"
}
}")

(defun qmltypes-parse ()
  (let (name)
    (json-skip-whitespace)
    (setq name (thing-at-point 'word t))
    (forward-word)
    (qmltypes--do-parse name)))

(defun qmltypes--do-parse (name)
  ;; TODO error checking
  (let ((elements '())
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
                                        ;    (re-search-forward "Component")
                                        ;   (re-search-forward "Component")
    (beginning-of-line)
    (qmltypes-parse)))

(my-parse-file "/usr/lib/qt/imports/builtins.qmltypes")

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
;;; types = string | number | word
;;; array = [types]
;;; data = types | array

(provide 'qmltypes-parser)
;;; qmltypes-parser.el ends here
