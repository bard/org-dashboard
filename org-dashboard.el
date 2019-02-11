;;; org-dashboard.el --- Visually summarize progress in org files

;; Copyright (C) 2015-2017 Massimiliano Mirra

;; Author: Massimiliano Mirra <hyperstruct@gmail.com>
;; Version: 1.0
;; Maintainer: Massimiliano Mirra <hyperstruct@gmail.com>
;; Keywords: outlines, calendar
;; URL: http://github.com/bard/org-dashboard
;; Package-Requires: ((cl-lib "0.5"))

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>

;;; Commentary:

;; Org Dashboard provides a visual summary of progress on projects and
;; tasks.
;;
;; For example, if an org file (known by `org-dashboard-files') contains
;; the following:
;;
;;     * Project: Better Health
;;     :PROPERTIES:
;;     :CATEGORY: health
;;     :END:
;;
;;     ** Milestones
;;     *** [66%] run 10 km/week
;;     **** TODO learn proper warmup
;;     **** DONE look for a jogging partner
;;     **** DONE run 10 minutes on monday
;;     
;;     * Project: Super Widget
;;     :PROPERTIES:
;;     :CATEGORY: widget
;;     :END:
;;
;;     ** Milestones
;;     *** [1/6] release 0.1
;;     **** DONE git import
;;     **** TODO create github project
;;     **** TODO add readme
;;     **** TODO publish
;;
;; Then `M-x org-dashboard-display' generates the following report and
;; displays it in a new buffer:
;;
;;     health                run 10 km/week [||||||||||||||||||||||           ]  66%
;;     widget                   0.1 release [||||||                           ]  18%
;;
;; A dynamic block form is also supported. Writing the following in an
;; org file and then running `org-dblock-update', or placing the
;; cursor on the first line of the block and then typing `C-c C-c',
;; will insert the same report shown above into the block:
;;
;;     #+BEGIN: block-dashboard
;;     #+END:

;; Configuration:
;;
;; You can customize the following variables:
;;
;; - `org-dashboard-files': list of files to search for progress entries; defaults to `org-agenda-files'
;; - `org-dashboard-show-category': whether to show or not the project category
;; - `org-dashboard-filter': a function that decides whether an entry should be displayed or not
;;
;; For example, to avoid displaying entries that are finished
;; (progress = 100), not started (progress = 0), or are tagged with
;; "archive", use the following:
;;
;;    (defun my/org-dashboard-filter (entry)
;;      (and (> (plist-get entry :progress-percent) 0)
;;           (< (plist-get entry :progress-percent) 100)
;;           (not (member "archive" (plist-get entry :tags)))))
;;
;;    (setq org-dashboard-filter 'my/org-dashboard-filter)

;; Notes:
;;
;; Labels link back to the trees where they were found. 
;;
;; The color of the progress bar is (naively, for now) chosen based on
;; the progress value, from dark red to bright green.
;;
;; If not set per-tree through a property or per-file through a
;; keyword, the category defaults to the file name without extension.
;; To set category on a per-file basis, you can add the following at
;; the bottom of the org file:
;;
;;    #+CATEGORY: xyz

;; Related work:
;;
;; This module was inspired by Zach Peter's [A Dashboard for your
;; Life](http://thehelpfulhacker.net/2014/07/19/a-dashboard-for-your-life-a-minimal-goal-tracker-using-org-mode-go-and-git/).

;; Contributions:
;;
;; - one feature or fix per pull request
;; - provide an example of the problem it addresses
;; - please adhere to the existing code style

;;; Code:

(require 'org)
(require 'cl-lib)
(require 'subr-x)

(defgroup org-dashboard nil
  "Options concerning org dashboard."
  :tag "Org Dashboard"
  :group 'org)

(defcustom org-dashboard-files
  org-agenda-files
  "Files to search for progress items."
  :type '(repeat :tag "List of files and directories" file)
  :group 'org-dashboard)

(defcustom org-dashboard-show-category
  t
  "Whether to display categories in a progress report.

Note that, if not set with per-file or per-tree properties,
category defaults to the org file name."
  :group 'org-dashboard
  :type 'boolean)

(defcustom org-dashboard-category-label-width 10
  "Width in characters of the column where category label is displayed."
  :group 'org-dashboard
  :type 'integer)

(defcustom org-dashboard-goal-label-width 25
  "Width in characters of the column where goal label is displayed."
  :group 'org-dashboard
  :type 'integer)

(defcustom org-dashboard-filter nil
  "Function to use to filter progress entries."
  :group 'org-dashboard
  :type 'function)

;;;###autoload
(defun org-dashboard-display ()
  (interactive)
  (with-current-buffer (get-buffer-create "*Org Dashboard*")
    (setq buffer-read-only nil)
    (erase-buffer)
    (org-mode)
    (save-excursion
      (org-dashboard--insert-progress-summary
       (org-dashboard--collect-progress)))
    (setq buffer-read-only t)
    (display-buffer (current-buffer))))

;;;###autoload
(defun org-dblock-write:block-dashboard (params)
  "Generate a progress report inside an org dynamic block.

Progress information is retrieved by searching files in
`org-dashboard-files' for headings containing a \"progress cookie\",
e.g.:

  ** [50%] release v0.1
  *** TODO publish on github
  *** DONE import in git

See Info node `(org) Breaking down tasks'."
  (org-dashboard--insert-progress-summary
   (org-dashboard--collect-progress)))

(defvar org-dashboard--cookie-re
  "\\[[0-9]+\\(%\\|/[0-9]+\\)\\]")

(defun org-dashboard--collect-progress ()
  (cl-remove-if-not
   org-dashboard-filter
   (cl-loop for file in org-dashboard-files
            append (with-current-buffer (find-file-noselect file)
                     (org-with-wide-buffer
                      (org-dashboard--collect-progress-current-buffer))))))

(defun org-dashboard--insert-progress-summary (progress-summary)
  (cl-labels
      ((make-category-label (category)
                            (truncate-string-to-width category org-dashboard-category-label-width 0 ?\s "…"))
       (make-goal-label (goal)
                        (truncate-string-to-width goal org-dashboard-goal-label-width 0 nil "…"))
       (make-progress-bar (progress-percent)
                          (let ((color (org-dashboard--progress-color progress-percent)))
                            (concat (propertize 
                                     (make-string (/ progress-percent 4) ?|)
                                     'font-lock-face (list :foreground color))
                                    (make-string (- (/ 100 4) (/ progress-percent 4)) ?\s))))
       (make-link (target label)
                  (format "[[%s][%s]]" target label)))

    (insert "\n")
    (cl-loop for entry in progress-summary
             do (cl-destructuring-bind
                    (&key category heading id progress-percent filename tags)
                    entry
                  (let* ((category-label (make-category-label category))
                         (goal-label (make-goal-label heading))
                         (goal-link (make-link (if id (concat "id:" id)
                                                 (concat filename "::*" heading))
                                               goal-label))
                         (goal-label-padding (make-string (- org-dashboard-goal-label-width
                                                             (string-width goal-label))
                                                          ?\s))
                         (progress-bar (make-progress-bar progress-percent))
                         (percent-indicator (format "%3d%%" progress-percent)))

                    (insert (format "%s %s%s [%s] %s\n"
                                    (if org-dashboard-show-category
                                        category-label
                                      "")
                                    goal-label-padding
                                    goal-link
                                    progress-bar
                                    percent-indicator)))))))

(defun org-dashboard--collect-progress-current-buffer ()
  (save-excursion
    (goto-char (point-min))
    (org-refresh-category-properties)
    (cl-loop while (re-search-forward org-dashboard--cookie-re nil t)
             if (org-at-heading-p)
             collect (list :category (substring-no-properties (org-get-category))
                           :heading (org-dashboard--get-heading-text)
                           :id (org-id-get)
                           :progress-percent (org-dashboard--get-heading-progress)
                           :filename (buffer-file-name)
                           :tags (org-get-tags)))))

(defun org-dashboard--get-heading-text ()
  (string-trim
   (replace-regexp-in-string org-dashboard--cookie-re
                             ""
                             (nth 4 (org-heading-components)))))

(defun org-dashboard--get-heading-progress ()
  (let* ((heading-with-cookie (nth 4 (org-heading-components)))
         (_ (string-match org-dashboard--cookie-re (nth 4 (org-heading-components))))
         (cookie (substring heading-with-cookie 0 (match-end 0))))
    (cond ((string-match "\\([0-9]+\\)%" cookie)
           (string-to-number (match-string 1 cookie)))
          ((string-match "\\([0-9]+\\)/\\([0-9]+\\)" cookie)
           (/ (* 100 (string-to-number (match-string 1 cookie)))
              (string-to-number (match-string 2 cookie)))))))

(defun org-dashboard--progress-color (percent)
  (cond ((< percent 33) "red")
        ((< percent 66) "dark green")
        ((< percent 100) "forest green")
        (t "green")))

(provide 'org-dashboard)
;;; org-dashboard.el ends here

