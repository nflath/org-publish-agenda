;;; org-publish-agenda.el --- Publish org agenda with links to other files

;; Copyright (C) 2014 Nathaniel Flath <flat0103@gmail.com>

;; Author: Nathaniel Flath <flat0103@gmail.com>
;; Version: 1.7

;; This file is not part of GNU Emacs.

;;; Commentary:

;; org-publish-agenda provides a function `org-publish-agenda` that will export
;; every org file in (org-agenda-files).  It will then export the agenda,
;; creating links between the items on the agenda and the sections in the other
;; files.

;;; Installation

;; To use this mode, put the following in your init.el:
;; (require 'org-publish-agenda)

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'org)
(require 'htmlize)

(defun org-publish-agenda-after-last (regexp string)
  "Returns the part of the string after the last occurrence of regexp."
  (let ((index (string-match regexp string)))
    (if index
        (org-publish-agenda-after-last regexp (substring string (match-end 0) (length string)))
      string)))

(defun org-publish-agenda-current-section-number (&optional pos)
  "Returns the subsection number at pos"
  (save-excursion
    (if pos (goto-char pos))
    (let ((retn 1))
      (ignore-errors
        (while t
          (outline-backward-same-level 1)
          (incf retn)))
      retn)))

(defun org-publish-agenda-full-sections (&optional pos)
  "Returns a list coresponding to the full section number at pos"
  (save-excursion
    (if pos (goto-char pos))
    (let* ((retn 1)
           (curnum (org-publish-agenda-current-section-number))
           (retlist (list curnum)))
      (condition-case nil
          (while t
            (progn
              (outline-up-heading 1)
              (setq retlist (append (list (org-publish-agenda-current-section-number)) retlist))))
        (error retlist))
      retlist)))

(defun org-publish-agenda-full-sections-string (&optional pos)
  "Returns a string corresponding to the section at pos"
  (substring (reduce (lambda (x y) (concat x "." (number-to-string y)))
                     (org-publish-agenda-full-sections)
                     :initial-value "") 1))

(defun org-publish-agenda-line-matches (regexp)
  "Returns non-nil if the current line matches the given regexp, nil otherwise."
  (interactive "sRegex: ")
  (save-excursion
    (end-of-line)
    (let ((end (point)))
      (beginning-of-line)
      (re-search-forward regexp end t))))

(defun org-publish-agenda ()
  "Writes out the agenda and all agenda files as HTML."
  (interactive)
  (save-window-excursion
    (mapcar (lambda (file)
              (find-file file)
              (org-html-export-to-html))
            (org-agenda-files) ))
  (org-agenda 0 "a")
  (org-agenda-month-view)
  (let ((html-buffer (htmlize-buffer (get-buffer "*Org Agenda*")))
        (agenda-buffer (get-buffer "*Org Agenda*")))
    (switch-to-buffer html-buffer)
    (goto-char (point-min))
    (search-forward "<body>")
    (let ((line-start (line-number-at-pos)))
      (while (< (point) (point-max))
        (beginning-of-line)
        (cond
         ((org-publish-agenda-line-matches "org-agenda-structure") (forward-line))
         ((org-publish-agenda-line-matches "org-agenda-dat") (forward-line))
         ((org-publish-agenda-line-matches "org-time-grid") (forward-line))
         ((org-publish-agenda-line-matches " *\\([^:]+\\):")
          (let ((calendar (org-publish-agenda-after-last " "(match-string 1))))
            (let ((agenda-line-no (1- (- (line-number-at-pos) line-start))))
              (switch-to-buffer agenda-buffer)
              (goto-line agenda-line-no)
              (let* ((marker (or (get-text-property (point) 'org-marker)
                                 (org-agenda-error)))
                     (buffer (marker-buffer marker))
                     (pos (marker-position marker)))
                (switch-to-buffer buffer)
                (goto-char pos)
                (setq sec-string (concat "sec-" (org-publish-agenda-full-sections-string)))
                (switch-to-buffer html-buffer)))
            (insert (concat "<a href=\"" calendar ".html#" sec-string "\">"))
            (end-of-line)
            (insert "</a>")
            (forward-line)))
         (t (forward-line)))))
    (let ((org-dir (if (file-directory-p (car org-agenda-files))
                       (car org-agenda-files)
                     (file-name-directory (car org-agenda-files)))))
      (write-file (concat org-dir "/agenda.html"))
      (let ((default-directory (file-name-as-directory org-dir))
            (publish-dir (concat default-directory "publish")))
            (if (not (file-directory-p publish-dir)) (shell-command (concat "mkdir " publish-dir) nil))
            (shell-command (concat "mv " default-directory "*.html " publish-dir nil))
        (kill-buffer "agenda.html")))))

(provide 'org-publish-agenda)
;;; org-publish-agenda.el ends here
