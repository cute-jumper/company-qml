;;; company-qml-parsec.el --- Parsec utilities for company-qml  -*- lexical-binding: t; -*-

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

(provide 'company-qml-parsec)
;;; company-qml-parsec.el ends here
