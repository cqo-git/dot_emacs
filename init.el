;;; init.el -*- lexical-binding: t; -*-

;; Add modules folder
(add-to-list 'load-path (expand-file-name "modules/" user-emacs-directory))


;; Load default settings
(require 'default-settings)

;; Installing straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Load settings per platform
(cond
 ;; darwin
 ((string-equal system-type "darwin")
  (require 'macos-config))
 ;; linux
 ((string-equal system-type "gnu/linux")
  (require 'linux-config))
 ;; Terminal 
 (t nil))

;; install packages with general configuration
(require 'packages)


;; Load utility functions
(require 'utility-functions)


