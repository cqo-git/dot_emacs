;;; macos.el -*- lexical-binding: t; -*-

(add-to-list 'exec-path "/opt/homebrew/bin/")

;; add path to custom Apple related scripts 
(add-to-list 'load-path "~/Apple/scripts/elisp/")


;; Update PATH environment
(setenv "PATH" (format "%s:%s" "/opt/homebrew/bin/" (getenv "PATH")))

;; Font size on Aqua
(set-face-attribute 'default nil :font "Iosevka Term" :height 155)
;;(set-face-attribute 'default nil :font "Iosevka Term" :height 140)



;; hlt functions
(require 'hlt)

;; org mode functions
(defun get-past-sunday ()
  (let* ((now (current-time))
         (dow (nth 6 (decode-time now))) 
         (days-ago (- dow))) 
    (format-time-string "%Y-%m-%d" (time-add now (* days-ago 86400))))) 

(defun this-week ()
  (interactive)
  (let* ((current-date (get-past-sunday))
         (full-file-path (concat org-roam-directory
                                 "/"
                                 org-roam-dailies-directory
                                 "/Week-"
                                 current-date
                                 ".org")))
    (find-file full-file-path)
    (save-excursion
      (let ((title (concat "#+title: Week of " current-date)))
        (goto-char (point-min))
        (unless (looking-at-p (regexp-quote title))
          (insert title))))))




(provide 'macos-config)

