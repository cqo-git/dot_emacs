;;; init.el -*- lexical-binding: t; -*-

;; Add modules folder
(add-to-list 'load-path (expand-file-name "modules/" user-emacs-directory))

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

;; Load default settings
(require 'default-settings)

;; Load settings per platform
(cond
 ((equal window-system 'ns) (require 'macos-config))
 (t (require 'terminal-config)))


;; Install Evil mode
(straight-use-package 'evil)
(setq evil-want-keybinding nil)
(evil-mode 1)
(straight-use-package 'evil-collection)
(evil-collection-init)

;; Evil cursors for normal/insert/visual mode
(setq evil-normal-state-cursor '(box "floralwhite")
      evil-insert-state-cursor '(bar "floralwhite")
      evil-visual-state-cursor '(hollow "orange"))


;; Install Arc-Dark-Theme
(straight-use-package '(arc-dark-theme
                        :type git
                        :host github
                        :repo "emacsattic/arc-dark-theme"))
                      
;; Install Modus Themes
(straight-use-package '(modus-themes
			:type git
			:flavor melpa
			:host sourcehut
			:repo "protesilaos/modus-themes"))

;; Default Theme
(load-theme 'arc-dark t)

;; auto completion?
(straight-use-package `(company-mode
                        :type git
                        :host github
                        :repo "company-mode/company-mode"
                        :files (:defaults)))
(add-hook 'after-init-hook 'global-company-mode)

;; common lisp
(straight-use-package '(sly
                        :type git
                        :flavor melpa
                        :files (:defaults "lib" "slynk" "contrib" "doc/images"
                                          (:exclude "sly-autoloads.el") "sly-pkg.el")
                        :host github :repo "joaotavora/sly"))



(straight-use-package '(org
                        :type git
                        :repo "https://git.savannah.gnu.org/git/emacs/org-mode.git"
                        :local-repo "org"
                        :depth full
                        :pre-build (straight-recipes-org-elpa--build)
                        :build (:not autoloads)
                        :files (:defaults "lisp/*.el" ("etc/styles/" "etc/styles/*"))))

(straight-use-package '(org-bullets
                        :type git
                        :flavor melpa
                        :host github
                        :repo "integral-dw/org-bullets"))
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
