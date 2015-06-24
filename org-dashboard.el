;;; org-dashboard.el --- Visually summarize progress in org files

;; Copyright (C) 2015 Massimiliano Mirra

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
;; For example, if an org file (known by `org-agenda-files') contains
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
;;     health                run 10 km/week [██████████████████████           ]  66%
;;     widget                   0.1 release [██████                           ]  18%
;;
;; A dynamic block form is also supported. Writing the following in an
;; org file and then running `org-dblock-update', or placing the
;; cursor on the first line of the block and then typing `C-c C-c',
;; will insert the same report shown above into the block:
;;
;;     #+BEGIN: block-dashboard
;;     #+END:

;; Notes:
;;
;; Labels link back to the trees where they were found.
;;
;; The color of the progress bar is (naively, for now) chosen based on
;; the progress value, from dark red to bright green.
;;
;; The first column displays categories. You can turn this off by
;; customizing the `org-dashboard-display-category' option. Note that,
;; if not set per-tree through a property or per-file through a
;; keyword, the category defaults to the file name without extension.

;; Related work:
;;
;; This module was inspired by Zach Peter's [A Dashboard for your
;; Life](http://thehelpfulhacker.net/2014/07/19/a-dashboard-for-your-life-a-minimal-goal-tracker-using-org-mode-go-and-git/).

;;; Code:

(require 'org)
(require 'cl-lib)

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
(defun org-dashboard-display ()
  (interactive)
  (with-current-buffer (get-buffer-create "*Org Dashboard*")
    (setq buffer-read-only nil)
    (erase-buffer)
    (org-mode)
    (save-excursion
      (org-dashboard--insert-progress-summary
       (org-dashboard--collect-progress-agenda-files)))
    (setq buffer-read-only t)
    (display-buffer (current-buffer))))

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
   (org-dashboard--collect-progress-agenda-files)))

(defun org-dashboard--collect-progress-agenda-files ()
  (cl-loop for file in (org-agenda-files)
           append (with-current-buffer (find-file-noselect file)
                    (org-with-wide-buffer
                     (org-dashboard--collect-progress-current-buffer)))))

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
                          (let ((color (org-dashboard--progress-color percent)))
                            (concat (propertize 
                                     (make-string (/ percent 3) ?█)
                                     'font-lock-face (list :foreground color))
                                    (make-string (- (/ 100 3) (/ percent 3)) ?\s))))
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

(defun org-dashboard--collect-progress-current-buffer ()
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

(defun org-dashboard--progress-color (percent)
  (cond ((< percent 33) "red")
        ((< percent 66) "dark green")
        ((< percent 100) "forest green")
        (t "green")))

(provide 'org-dashboard)
;;; org-dashboard.el ends here

