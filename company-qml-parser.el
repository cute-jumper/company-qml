;;; company-qml-parser.el --- Parser utilities for company-qml  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Junpeng Qiu

;; Author: Junpeng Qiu <qjpchmail@gmail.com>
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

(require 'parsec)

(cl-defstruct company-qml-imports module version qualifier)

(defmacro company-qml--lexeme (p)
  `(parsec-return ,p
     (parsec-many (parsec-ch ? ))))

(defun company-qml--parse-integer ()
  (company-qml--lexeme (parsec-many1-as-string (parsec-digit))))

(defun company-qml--parse-word ()
  (company-qml--lexeme (parsec-many1-as-string (parsec-letter))))

(defun company-qml--parse-symbol ()
  (company-qml--lexeme (parsec-until-s (parsec-or (parsec-ch ? )
                                                  (parsec-eol-or-eof)))))

(defun company-qml--parse-import (import-line)
  (let (module version qualifier)
    (parsec-with-input import-line
      (setq module (company-qml--parse-symbol))
      (setq version (string-to-number
                     (company-qml--parse-symbol)))
      (setq qualifier (parsec-optional
                       (parsec-and
                         (company-qml--lexeme (parsec-str "as"))
                         (company-qml--parse-symbol)))))
    (make-company-qml-imports :module module
                              :version version
                              :qualifier qualifier)))

(defun company-qml--grab-imports ()
  (save-match-data
    (save-excursion
      (goto-char (point-min))
      (let (start imports)
        (while (re-search-forward "^[ ]*import[ ]+" nil t)
          (goto-char (match-end 0))
          (push (company-qml--parse-import
                 (buffer-substring-no-properties (point) (point-at-eol)))
                imports))
        imports))))

(provide 'company-qml-parser)
;;; company-qml-parser.el ends here
