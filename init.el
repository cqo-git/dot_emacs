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
 ;; macos
 ((equal window-system 'ns) (require 'macos-config))
 ;; Linux X11
 ((equal window-system 'x) (require 'linux-config)) 
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

;; Which key, helps with finding key prefixes etc.
(straight-use-package '(which-key
                        :type git
                        :flavor melpa
                        :host github
                        :repo "justbur/emacs-which-key"))
(which-key-mode)

;; IDO TODO: find and alternative to this. 
(setq ido-everywhere t)
(setq ido-enable-flex-matching t)
(ido-mode t)

;; perspective
(straight-use-package '(perspective
                        :type git
                        :flavor melpa
                        :host github
                        :repo "nex3/perspective-el"))

(global-set-key (kbd "C-x C-b") 'persp-list-buffers)
(global-set-key (kbd "C-x k") 'persp-kill-buffer*)
(customize-set-variable 'persp-mode-prefix-key (kbd "C-c p"))
(persp-mode)

;; common lisp
(straight-use-package '(sly
                        :type git
                        :flavor melpa
                        :files (:defaults "lib" "slynk" "contrib" "doc/images"
                                          (:exclude "sly-autoloads.el") "sly-pkg.el")
                        :host github :repo "joaotavora/sly"))

;; use roswell's sbcl as the default lisp
(load (expand-file-name "~/.roswell/helper.el"))
(setq inferior-lisp-program "ros run -- --dynamic-space-size 8192")

;; setup autocomplete for sly
(straight-use-package '(ac-sly :type git :flavor melpa :host github :repo "qoocku/ac-sly"))
;; setup sly ac after loading sly
(add-hook 'sly-mode-hook 'set-up-sly-ac)
(eval-after-load 'auto-complete
  '(add-to-list 'ac-modes 'sly-mrepl-mode))

;;; end lisp


;;;;; Org mode setup
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

(straight-use-package '(org-roam
                        :type git
                        :flavor melpa
                        :files (:defaults "extensions/*" "org-roam-pkg.el")
                        :host github
                        :repo "org-roam/org-roam"))

;; org roam keybindings
(setq org-roam-directory (file-truename "~/org/roam"))
(setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
(org-roam-db-autosync-mode)

(global-set-key (kbd "C-c n") 'my-org-roam)
(defalias 'my-org-roam
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "l") 'org-roam-buffer-toggle)
    (define-key map (kbd "f") 'org-roam-node-find)
    (define-key map (kbd "g") 'org-roam-graph)
    (define-key map (kbd "i") 'org-roam-node-insert)
    (define-key map (kbd "c") 'org-roam-capture)
    (define-key map (kbd "j") 'org-roam-dailies-capture-today)
    map))

;;; End Org mode setup

;;; Magit
(straight-use-package 'magit)


;;; vterm
(straight-use-package '(vterm
                        :type git
                        :flavor melpa
                        :files ("CMakeLists.txt"
                                "elisp.c"
                                "elisp.h"
                                "emacs-module.h"
                                "etc"
                                "utf8.c"
                                "utf8.h"
                                "vterm.el"
                                "vterm-module.c"
                                "vterm-module.h"
                                "vterm-pkg.el")
                        :host github
                        :repo "akermu/emacs-libvterm"))

;; Open keybindings
(global-set-key (kbd "C-c o") 'my-open)
(defalias 'my-open
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "t") 'vterm)
    (define-key map (kbd "e") 'eshell)
    map))

