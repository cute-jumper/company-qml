;;; company-qml.el --- Company backend for QML file

;; Copyright (C) 2015  Junpeng Qiu

;; Author: Junpeng Qiu <qjpchmail@gmail.com>
;; Package-Requires: ((qml-mode "0.1"))
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

;;

;;; Code:

(require 'qml-mode)
(require 'qmltypes-parser)

(defvar qml-global-completion-table
  '(("Qt" . ("atob" "binding" "btoa" "colorEqual" "createComponent" "createQmlObject"
             "darker" "font" "fontFamilies" "formatDate" "formatDateTime" "formatTime"
             "hsla" "hsva" "include" "isQtObject" "lighter" "locale" "md5" "matrix4x4"
             "openUrlExternally" "point" "qsTr" "qsTrId" "qsTrNoOp" "qsTranslate"
             "qsTranslateNoOp" "quaternion" "quit" "rect" "resolvedUrl" "rgba" "size"
             "tint" "vector2d" "vector3d" "vector4d"))
    ("console" . ("log" "assert" "time" "timeEnd" "count" "profile" "profileEnd" "exception"))
    ("XMLHttpRequest" . ("nodeName" "nodeValue" "nodeType" "parentNode" "childNodes"
                         "firstChild" "lastChild" "previousSibling" "nextSibling"
                         "attributes" "xmlVersion" "xmlEncoding" "xmlStandalone"
                         "documentElement" "tagName" "name" "value" "ownerElement"
                         "data" "length" "isElementContentWhitespace" "wholeText"))
    ("qsTr")
    ("qsTranslate")
    ("qsTrId")
    ("QT_TR_NOOP")
    ("QT_TRANSLATE_NOOP")
    ("QT_TRID_NOOP")
    ("gc")
    ("print")
    ("DOMException"))
  "Predefined completion table for global objects available in QML.")

(defvar company-qml--syntax-list nil
  "Store syntax information for completion.")

(defvar company-qml--current-line nil
  "The current line that is being processed.")

(defsubst company-qml--remove-whitespaces (s)
  (replace-regexp-in-string "[ ]+" "" s))

(defun company-qml--initial-upcase-p (s)
  (when (> (length s) 0)
    (let ((initial (aref s 0)))
      (and (>= initial ?A) (<= initial ?Z)))))

(defun company-qml--parse-toplevel-paths ()
  (save-match-data
    (save-excursion
      (goto-char (point-min))
      (let (start toplevel-paths)
        (while (re-search-forward "import[ ]+" nil t)
          (setq start (point))
          (end-of-line)
          (push
           (company-qml--remove-whitespaces (buffer-substring-no-properties start (point)))
           toplevel-paths))
        toplevel-paths))))

(defun company-qml--parse-parents ()
  (save-excursion
    (ignore-errors
      (backward-up-list)
      (let* ((end (point))
             (name (company-qml--remove-whitespaces
                    (buffer-substring-no-properties (progn (beginning-of-line) (point)) end))))
        (if (company-qml--initial-upcase-p name)
            name
          (concat name "." (company-qml--parse-parents)))))))

(defun company-qml-grab-prefix ()
  (save-excursion
    (let ((current (point)))
      (skip-chars-backward "^;\n")
      (setq company-qml--current-line (company-qml--remove-whitespaces
                                       (buffer-substring-no-properties (point) current)))
      (if (company-qml--initial-upcase-p company-qml--current-line)
          (progn
            (setq company-qml--syntax-list (split-string company-qml--current-line "\\."))
            (if (> (length company-qml--syntax-list) 1)
                (cadr company-qml--syntax-list)
              company-qml--current-line))
        (setq company-qml--syntax-list (list (company-qml--parse-parents) company-qml--current-line))
        company-qml--current-line))))

(defun company-qml--get-global-completions (name &optional field-name)
  "Get completions for global object."
  (let ((type-info-table (if field-name
                             (cdr (assoc name qml-global-completion-table))
                           qml-global-completion-table))
        (completion-name (or field-name name))
        candidate
        completions)
    (when type-info-table
      (mapc
       (lambda (x)
         (setq candidate (or (car-safe x) x))
         (and (string-prefix-p completion-name candidate)
              (push candidate completions)))
       type-info-table)
      completions)))

(defun company-qml--setup-completion-table (type-info-table)
  "Transform TYPE-INFO-TABLE and return a table for completion.
Parse the `exports' field to determine user-visible paths and
names."
  (let ((completion-table (make-hash-table :test 'equal)))
    (maphash
     (lambda (type-name type-info)
       (let ((exports (qmltypes-parser-type-info-exports type-info))
             path-parts name-parts name path completions results)
         (when exports
           (mapc
            (lambda (expo)
              (setq path-parts (split-string expo " "))
              (setq name-parts (split-string (car path-parts) "/"))
              (setq name (cadr name-parts))
              (setq path (concat (car name-parts) (cadr path-parts)))
              (push (cons name (company-qml--construct-qmltypes-completions
                                type-name
                                type-info-table))
                    (gethash path completion-table)))
            exports))))
     type-info-table)
    completion-table))

(defun company-qml--construct-qmltypes-completions (name type-info-table)
  (let ((suffix "Attached")
        results)
    (setq results
          (company-qml--do-get-qmltypes-completions
           name type-info-table results))
    (setq results
          (company-qml--do-get-qmltypes-completions
           (concat name suffix) type-info-table results))
    (car results)))

(defun company-qml--do-get-qmltypes-completions (name type-info-table results)
  (let* ((type-info (gethash name type-info-table))
         (type-name name)
         (completions (car results))
         (visited (cdr results)))
    (while (and type-info (not (member type-name visited)))
      (push type-name visited)
      (setq completions (append completions
                                (qmltypes-parser-type-info-enums type-info)
                                (qmltypes-parser-type-info-properties type-info)
                                (qmltypes-parser-type-info-methods type-info)
                                (qmltypes-parser-type-info-signals type-info)))
      (setq type-name (qmltypes-parser-type-info-prototype type-info))
      (setq type-info (gethash type-name type-info-table)))
    `(,completions . ,visited)))

(defun company-qml--get-qmltypes-completions (name path try-match-name-p)
  "Get completions from plugins.qmltypes file."
  (let* ((alist (gethash path company-qml--completion-table)))
    (if try-match-name-p
        (delq nil
              (mapcar
               (lambda (x) (and (string-prefix-p name (car x)) (car x))) alist))
      (let ((comp (assoc name alist)))
        (and comp (cdr comp))))))

;; Simple cases:
;; 1.
;;   1.1 ("Item" "visi"), !try-match & prefix-filtering
;;   1.2 ("Item" "") !try-match + try-match ""
;; 2.
;;   2.1 ("Lay"), try-match
;;   2.2 ("Layout" "min"), !try-match & prefix filtering

(defvar company-qml--completion-table
  (company-qml--setup-completion-table (qmltypes-parser-init))
  "A lookup table for finding all possible completions.")

(defun company-qml-get-completions (arg)
  (let* ((name (car company-qml--syntax-list))
         (field-name (cadr company-qml--syntax-list))
         (completions
          (mapcan (lambda (x)
                    (company-qml--get-qmltypes-completions name x (not field-name)))
                  (company-qml--parse-toplevel-paths))))
    ;; Try to get completions for global objects
    (if (string-match-p "\\." company-qml--current-line)
        (setq completions
              (append completions
                      (company-qml--get-global-completions
                       (car company-qml--syntax-list)
                       (cadr company-qml--syntax-list))))
      (setq completions
            (append completions
                    (company-qml--get-global-completions
                     company-qml--current-line))))
    (if field-name
        ;;1.2
        (if (string= company-qml--current-line "")
            (append completions
                    (mapcan (lambda (x)
                              (company-qml--get-qmltypes-completions "" x t))
                            (company-qml--parse-toplevel-paths)))
          ;;1.1, 2.2
          (delq nil
                (mapcar
                 (lambda (x) (and (string-prefix-p field-name x) x)) completions)))
      ;;2.1
      completions)))

;;;###autoload
(defun company-qml (command &optional arg &rest ignored)
  "A `company-mode' completion back-end for qml-mode."
  (interactive (list 'interactive))
  (cl-case command
    ('interactive (company-begin-backend 'company-qml))
    ('prefix (and (eq 'qml-mode major-mode) (company-qml-grab-prefix)))
    ('candidates (company-qml-get-completions arg))
    ('sorted nil)))

(provide 'company-qml)
;;; company-qml.el ends here
