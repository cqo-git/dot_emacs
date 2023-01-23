;;; utility-functions.el --- Description -*- lexical-binding: t; -*-

(defun today ()
  (interactive)
  (let* ((current-date (format-time-string "%Y-%m-%d" (current-time)))
         (full-file-path (concat org-roam-directory "/" org-roam-dailies-directory current-date ".org")))
    (find-file full-file-path)))




(provide 'utility-functions)

