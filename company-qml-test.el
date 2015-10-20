;;; company-qml-test.el --- Test for company-qml backend  -*- lexical-binding: t; -*-

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

(add-to-list 'company-backends 'company-qml)
(setq company-backends (delete 'company-qml company-backends))

(company-qml--get-qmltypes-completions "Window" "QtQuick.Window2.2" t)
(company-qml--get-qmltypes-completions "Win" "QtQuick.Window2.2" t)
(company-qml--get-global-completions "Qt" "a")
(company-qml--get-global-completions "qs")

(provide 'company-qml-test)
;;; company-qml-test.el ends here
