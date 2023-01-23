;;; packages.el -*- lexical-binding: t; -*-

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

;; using ivy, swiper and counsel mode
(straight-use-package '(ivy
                        :type git
                        :flavor melpa
                        :files (:defaults "doc/ivy-help.org"
                                          (:exclude "ivy-hydra.el" "ivy-avy.el") "ivy-pkg.el")
                        :host github
                        :repo "abo-abo/swiper"))
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")
(ivy-mode 1)
(global-set-key (kbd "C-s") 'swiper-isearch)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "M-y") 'counsel-yank-pop)
(global-set-key (kbd "C-x b") 'ivy-switch-buffer)
(global-set-key (kbd "C-c v") 'ivy-push-view)
(global-set-key (kbd "C-c V") 'ivy-pop-view)

;; common lisp
(straight-use-package '(sly
                        :type git
                        :flavor melpa
                        :files (:defaults "lib" "slynk" "contrib" "doc/images"
                                          (:exclude "sly-autoloads.el") "sly-pkg.el")
                        :host github :repo "joaotavora/sly"))

;; use roswell's sbcl as the default lisp
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

;; always timestamp when a task was completed
(setq org-log-done t)

;; todo states
(setq org-todo-keywords
      '((sequence  "TODO(t)"
                   "STARTED(s)"
                   "|"
                   "DONE(d)"
                   "CANCELED(c)")))

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



;; provide 
(provide 'packages)
