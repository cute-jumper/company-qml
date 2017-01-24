;;; company-qml-parsec-tests.el --- Tests for company-qml-parsec.el  -*- lexical-binding: t; -*-

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

(require 'company-qml-parsec)
(require 'ert)

(ert-deftest test-company-qml-parse ()
  (should (equal (parsec-with-input "12 "
                   (company-qml--parse-integer))
                 12))
  (should (equal (parsec-with-input "QtQuick 12"
                   (company-qml--parse-word))
                 "QtQuick")))


(provide 'company-qml-parsec-tests)
;;; company-qml-parsec-tests.el ends here
