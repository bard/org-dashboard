;;; org-dashboard --- Visually summarize progress in org files

;; Copyright (C) 2015 Massimiliano Mirra

;; Author: Massimiliano Mirra <hyperstruct@gmail.com>
;; Version: 1.0
;; Maintainer: Massimiliano Mirra <hyperstruct@gmail.com>
;; Keywords: outlines, calendar
;; URL: http://github.com/bard/org-dashboard

;; This file is not part of GNU Emacs. However, it is distributed
;; under the same license.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Visually summarize progress information in org-mode files.

;; org-dashboard makes a new dynamic block available in org
;; files. When the block is updated, progress information is fetched
;; from files in `org-agenda-files' and used to generate a visual
;; progress summary with progress bars and hyperlinked labels.

;; For example, if one of your `org-agenda-files' contains the
;; following:
;;
;;     * Project: Better Health
;;    :PROPERTIES:
;;    :CATEGORY: health
;;    :END:
;;
;;     ** Milestones
;;     *** [33%] run 10 km/week
;;     **** TODO learn proper warmup
;;     **** DONE look for jogging partner
;;     **** TODO run 10 minutes on monday
;;     
;;     * Project: Super Widget
;;    :PROPERTIES:
;;    :CATEGORY: widget
;;    :END:
;;
;;     ** Milestones
;;     *** [1/6] release 0.1
;;     **** DONE git import
;;     **** TODO create github project
;;     **** TODO add readme
;;     **** TODO publish
;;     
;; And any org file contains the following block:
;;
;;     #+BEGIN: block-display-dashboard
;;     #+END:
;;
;; Updating the block (`C-c C-c' anywhere on its first line) will
;; search agenda files for headings that include a "progress cookie"
;; (e.g. [33%], [1/6]) and generate the following:
;;
;;    #+BEGIN: block-display-dashboard
;;
;;    health                run 10 km/week [███████████                      ]  33%
;;    widget                   0.1 release [██████                           ]  18%
;;
;;    #+END:
;;
;; Labels link back to the trees where they were found. The first
;; column displays categories; you can turn categories off by
;; customizing the `org-dashboard-display-category' option. Note that,
;; if not set per-tree through a property or per-file through a
;; keyword, the category defaults to the file name without extension.

;; This module was inspired by Zach Peter's "A Dashboard for your
;; Life"
;; (http://thehelpfulhacker.net/2014/07/19/a-dashboard-for-your-life-a-minimal-goal-tracker-using-org-mode-go-and-git/)

;;; Code:

(require 'org)
(require 'cl)

(defgroup org-dashboard nil
  "Options concerning org dashboard."
  :tag "Org Dashboard"
  :group 'org)

(defcustom org-dashboard-progress-display-category
  t
  "Whether to display categories in a progress report.

Note that, if not set with per-file or per-tree properties,
category defaults to the org file name."
  :type 'boolean)

;;;###autoload
(defun org-dblock-write:block-dashboard (params)
  "Generate a progress report inside an org dynamic block.

Progress information is retrieved by searching files in
`org-agenda-files' for headings containing a \"progress cookie\",
e.g.:

  ** [50%] release v0.1
  *** TODO publish on github
  *** DONE import in git

See Info node `(org) Breaking down tasks'."
  (org-dashboard--insert-progress-summary
   (cl-loop for file in (org-agenda-files)
            append (with-current-buffer (find-file-noselect file)
                     (org-dashboard--collect-progress)))))

(defun org-dashboard--search-heading-with-progress ()
  (let ((cookie-re "\\[\\(\\([0-9]+\\)%\\|\\([0-9]+\\)/\\([0-9]+\\)\\)\\]"))
    (cl-labels ((read-progress ()
                               (let ((progress-percent (match-string 2))
                                     (progress-ratio-done (match-string 3))
                                     (progress-ratio-total (match-string 4)))
                                 (if progress-percent
                                     (string-to-number progress-percent)
                                   (/ (* 100 (string-to-number progress-ratio-done))
                                      (string-to-number progress-ratio-total)))))
                (trim-string (string)
                             (replace-regexp-in-string
                              "^ +\\| +$" "" string))
                (remove-cookie (heading)
                               (replace-regexp-in-string
                                cookie-re "" heading))
                (clean-heading (heading)
                               (trim-string
                                (remove-cookie
                                 (substring-no-properties heading)))))
      
      (and (re-search-forward cookie-re nil t)
           (let* ((progress-percent (read-progress))
                  (heading (clean-heading (org-get-heading t t))))
             (cons heading progress-percent))))))

(defun org-dashboard--insert-progress-summary (progress-summary)
  (cl-labels
      ((make-category-label (category)
                            (truncate-string-to-width category 10 0 ?\s "…"))
       (make-goal-label (goal)
                        (truncate-string-to-width goal 25 0 nil "…"))
       (make-progress-bar (percent)
                          (concat (propertize 
                                   (make-string (/ percent 3) ?█)
                                   'font-lock-face '(:foreground "green"))
                                  (make-string (- (/ 100 3) (/ percent 3)) ?\s)))
       (make-link (file goal goal-label)
                  (format "[[%s::*%s][%s]]" file goal goal-label)))

    (insert "\n")
    (cl-loop for (category goal-heading percent file) in progress-summary
             do (let* ((category-label (make-category-label category))
                       (goal-label (make-goal-label goal-heading))
                       (goal-link (make-link file goal-heading goal-label))
                       (goal-label-padding (make-string (- 25 (length goal-label)) ?\s))
                       (progress-bar (make-progress-bar percent))
                       (percent-indicator (format "%3d%%" percent)))

                  (insert (format "%s %s%s [%s] %s\n"
                                  (if org-dashboard-progress-display-category
                                      category-label
                                    "")
                                  goal-label-padding
                                  goal-link
                                  progress-bar
                                  percent-indicator))))))

(defun org-dashboard--collect-progress ()
  (save-excursion
    (goto-char (point-min))
    (org-refresh-category-properties)
    
    (cl-loop for (heading . progress) = (org-dashboard--search-heading-with-progress)
             while heading
             collect (let ((category (substring-no-properties
                                      (org-get-category))))
                       (list category
                             heading
                             progress
                             (buffer-file-name))))))

(provide 'org-dashboard)
;;; org-dashboard.el ends here

