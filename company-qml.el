;;; company-qml.el --- Company backend for QML file  -*- lexical-binding: t; -*-

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

(require 'qmltypes-parser)

(defsubst company-qml--remove-whitespaces (s)
  (replace-regexp-in-string "[ ]+" "" s))

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

(defun company-qml--initial-upcase-p (s)
  (when (> (length s) 0)
    (let ((initial (aref s 0)))
      (and (>= initial ?A) (<= initial ?Z)))))

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
      (setq current-line (company-qml--remove-whitespaces
                          (buffer-substring-no-properties (point) current)))
      (if (company-qml--initial-upcase-p current-line)
          (progn
            (setq global-prefix (split-string current-line "\\."))
            (if (> (length global-prefix) 1)
                (cadr global-prefix)
              current-line))
        (setq global-prefix (list (company-qml--parse-parents) current-line))
        current-line))))
;; 1.1 ("Item" "visi"), !try-match & prefix-filtering
;; 1.2 ("Item" "") !try-match + try-match ""
;; 2.1 ("Lay"), try-match
;; 2.2 ("Layout" "min"), !try-match & prefix filtering
;; 1.1==2.2
;; 1.2 line == ""

(defvar global-prefix nil)
(defvar current-line nil)
(defun company-qml-get-completions (arg)
  (let* ((name (car global-prefix))
         (member-name (cadr global-prefix))
         (completions
          (mapcan (lambda (x)
                    (get-all-completions name x (not member-name)))
                  (company-qml--parse-toplevel-paths))))
    (if (string-match-p "\\." current-line)
        (setq completions
              (append completions
                      (get-global-completions
                       (car global-prefix)
                       (cadr global-prefix))))
      (setq completions
            (append completions
                    (get-global-completions
                     current-line))))
    (if member-name
        ;;1.2
        (if (string= current-line "")
            (append completions
                    (mapcan (lambda (x)
                              (get-all-completions "" x t))
                            (company-qml--parse-toplevel-paths)))
          ;;1.1, 2.2
          (delq nil
                (mapcar
                 (lambda (x) (and (string-prefix-p member-name x) x)) completions)))
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

(add-to-list 'company-backends 'company-qml)
(setq company-backends (delete 'company-qml company-backends))

(provide 'company-qml)
;;; company-qml.el ends here
