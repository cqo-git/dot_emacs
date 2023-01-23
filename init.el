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

;; install packages with general configuration
(require 'packages)

;; Load settings per platform
(cond
 ;; macos
 ((equal window-system 'ns) (require 'macos-config))
 ;; Linux X11
 ((equal window-system 'x) (require 'linux-config))
 ;; terminal 
 (t (require 'terminal-config)))

;; Load utility functions
(require 'utility-functions)
