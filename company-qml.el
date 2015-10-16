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
    (let ((current (point))
          line)
      (skip-chars-backward "^;\n")
      (setq line (company-qml--remove-whitespaces
                  (buffer-substring-no-properties (point) current)))
      (if (company-qml--initial-upcase-p line)
          (let ((start (string-match "\\." line)))
            (if start
                (substring-no-properties name (1+ start))
              line))
        (setq global-prefix (split-string line "\\."))
        (setq global-prefix (list (company-qml--parse-parents) line))
        line))))
(split-string "ab.c" "\\.")
(defvar global-prefix)
(defun company-qml-get-completions (prefix)
  (let* ((pair global-prefix)
         (prefix (car pair))
         (suffix (cadr pair))
         completions)
    (mapc (lambda (x)
            (when (string-prefix-p suffix x)
              (push x completions)))
          (mapcan (lambda (x)
                    (get-all-completions prefix x))
                  (company-qml--parse-toplevel-paths)))
    completions))

;;;###autoload
(defun company-qml (command &optional arg &rest ignored)
  "A `company-mode' completion back-end for qml-mode."
  (interactive (list 'interactive))
  (cl-case command
    ('interactive (company-begin-backend 'company-qml))
    ('prefix (and (eq 'qml-mode major-mode) (company-qml-grab-prefix)))
    ('candidates (company-qml-get-completions arg))
    ('sorted t)))

(add-to-list 'company-backends 'company-qml)
(setq company-backends (delete 'company-qml company-backends))

(provide 'company-qml)
;;; company-qml.el ends here
