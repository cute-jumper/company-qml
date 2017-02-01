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
(cl-defstruct company-qml-context scope prefix)

(defsubst company-qml--remove-whitespaces (s)
  (replace-regexp-in-string "[ ]+" "" s))

(defsubst company-qml--get-text (beg end)
  (company-qml--remove-whitespaces
   (buffer-substring-no-properties beg end)))

(defun company-qml--initial-upcase-p (s)
  (when (> (length s) 0)
    (let ((initial (aref s 0)))
      (and (>= initial ?A) (<= initial ?Z)))))

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

(defun company-qml--parse-scope ()
  (save-excursion
    (backward-up-list)
    (let* ((name (company-qml--get-text (point-at-bol) (point))))
      (if (string= name "")
          (company-qml--parse-context)
        (if (company-qml--initial-upcase-p name)
            name
          ;; FIXME
          (concat (company-qml--parse-scope) "." name))))))

(defun company-qml--parse-context ()
  (let ((line-text (company-qml--get-text (save-excursion
                                            (skip-chars-backward "^;\n")
                                            (point))
                                          (point)))
        (pt (point)) start end)
    (cond
     ;; if there is `:' before the point, only need to look at current line
     ((or (nth 3 (syntax-ppss))       ; string
          (nth 4 (syntax-ppss))))     ; comment
     ((setq start (string-match ":" line-text))
      ;; if there is `.' after `:'
      (if (setq end (string-match "\\." line-text start))
          (make-company-qml-context
           :scope (company-qml--remove-whitespaces
                   (substring line-text (1+ start) end))
           :prefix (substring line-text (1+ end)))
        ;; FIXME: otherwise, it could be an enum, id,...
        (make-company-qml-context :scope nil
                                  :prefix (substring line-text (1+ start)))))
     ;; otherwise, look at the upper level to find scope
     (t (make-company-qml-context :scope (company-qml--parse-scope)
                                  :prefix line-text)))))

(provide 'company-qml-parser)
;;; company-qml-parser.el ends here
