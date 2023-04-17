;;; packages.el -*- lexical-binding: t; -*-


;; Install Evil mode

(straight-use-package 'evil)
(setq evil-want-keybinding nil)
(evil-mode 1)

(straight-use-package 'evil-collection)
(evil-collection-init)


;; Install Arc-Dark-Theme
(straight-use-package '(arc-dark-theme
                        :type git
                        :host github
                        :repo "emacsattic/arc-dark-theme"))

(straight-use-package '(ef-themes
                        :type git
                        :host github
                        :repo "emacs-straight/ef-themes"
                        :files ("*" (:exclude ".git"))))

(straight-use-package '(modus-themes
                        :type git
                        :flavor melpa
                        :host sourcehut
                        :repo "protesilaos/modus-themes"))

;; Default Theme
(load-theme 'modus-operandi-tinted t)

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
(global-set-key (kbd "C-c v") 'ivy-push-view)
(global-set-key (kbd "C-c V") 'ivy-pop-view)
(global-set-key (kbd "C-c /") 'counsel-rg)


;; counsel etags
(straight-use-package '(counsel-etags :type git :flavor melpa :host github :repo "redguardtoo/counsel-etags"))
(setq counsel-etags-update-interval 60)

;; common lisp
(straight-use-package '(sly
                        :type git
                        :flavor melpa
                        :files (:defaults "lib" "slynk" "contrib" "doc/images"
                                          (:exclude "sly-autoloads.el") "sly-pkg.el")
                        :host github :repo "joaotavora/sly"))

(straight-use-package '(sly-macrostep
                        :type git
                        :flavor melpa
                        :files (:defaults "*.lisp" "*.asd" "sly-macrostep-pkg.el")
                        :host github
                        :repo "joaotavora/sly-macrostep"))

(straight-use-package '(sly-repl-ansi-color
                        :type git
                        :flavor melpa
                        :host github
                        :repo "PuercoPop/sly-repl-ansi-color"))

;; use roswell's sbcl as the default lisp
(setq inferior-lisp-program "ros run -- --dynamic-space-size 8192")

;; setup autocomplete for sly
(straight-use-package '(ac-sly :type git :flavor melpa :host github :repo "qoocku/ac-sly"))
;; setup sly ac after loading sly
(add-hook 'sly-mode-hook 'set-up-sly-ac)
(eval-after-load 'auto-complete
  '(add-to-list 'ac-modes 'sly-mrepl-mode))


(load (expand-file-name "~/.roswell/helper.el"))


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
(setq org-startup-indented t)
;; todo states
(setq org-todo-keywords
      '((sequence  "TODO(t)"
                   "STARTED(s)"
                   "|"
                   "DONE(d)"
                   "CANCELED(c)")))
(setq org-todo-keyword-faces
      '(("TODO" . "IndianRed")
        ("STARTED" . "orange3")
        ("CANCELED" . "SlateGrey")
        ("DONE" . "SpringGreen4")))

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
(setq org-roam-dailies-directory "daily")
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

;;; vterm support
(straight-use-package '(vterm
                        :type git
                        :flavor melpa
                        :files ("CMakeLists.txt" "elisp.c" "elisp.h" "emacs-module.h" "etc" "utf8.c" "utf8.h" "vterm.el" "vterm-module.c" "vterm-module.h" "vterm-pkg.el")
                        :host github
                        :repo "akermu/emacs-libvterm"))
(setq vterm-kill-buffer-on-exit t)

(global-set-key (kbd "C-c o") 'my-open)
(defalias 'my-open
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "e") 'eshell)
    (define-key map (kbd "t") 'vterm-other-window)
    (define-key map (kbd "T") 'vterm)
    map))


;; eglot
(straight-use-package '(eglot :type git :flavor melpa :host github :repo "joaotavora/eglot"))

;; geiser
(straight-use-package '(geiser
                        :type git
                        :flavor melpa
                        :files ("elisp/*.el" "doc/dir" "doc/geiser.texi" "geiser-pkg.el")
                        :host gitlab
                        :repo "emacs-geiser/geiser"))

(straight-use-package '(geiser-guile
                        :type git
                        :flavor melpa
                        :files (:defaults ("src" "src/*") "geiser-guile-pkg.el")
                        :host gitlab
                        :repo "emacs-geiser/guile"))
;; Racket
(straight-use-package '(racket-mode
                        :type git
                        :flavor melpa
                        :files (:defaults "*.rkt" ("racket" "racket/*") (:exclude "racket/example/*" "racket/test/*") "racket-mode-pkg.el")
                        :host github
                        :repo "greghendershott/racket-mode"))

;; babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (shell . t)
   (python . t)))


;; skewer mode for live html
(straight-use-package '(skewer-mode
                        :type git
                        :flavor melpa
                        :files ("*.html" "*.js" "*.el" "skewer-mode-pkg.el")
                        :host github
                        :repo "skeeto/skewer-mode"))

(add-hook 'js2-mode-hook 'skewer-mode)
(add-hook 'css-mode-hook 'skewer-css-mode)
(add-hook 'html-mode-hook 'skewer-html-mode)


;; Highlight TODOs
(straight-use-package '(hl-todo
                        :type git
                        :flavor melpa
                        :host github
                        :repo "tarsius/hl-todo"))
(add-hook 'fundamental-mode 'hl-todo-mode)
(setq hl-todo-highlight-punctuation ":")


;; Perspectives
(straight-use-package '(perspective
                        :type git
                        :flavor melpa
                        :host github
                        :repo "nex3/perspective-el"))
(global-set-key (kbd "C-c p") 'my-perspectives)
(defalias 'my-perspectives
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "s") 'persp-switch)
    (define-key map (kbd "k") 'persp-kill-buffer*)
    (define-key map (kbd "b") 'persp-ivy-switch-buffer)
    (define-key map (kbd "n") 'persp-next)
    (define-key map (kbd "p") 'persp-prev)
    (define-key map (kbd "a") 'persp-add-buffer)
    (define-key map (kbd "A") 'persp-set-buffer)
    (define-key map (kbd "c") 'persp-kill)
    (define-key map (kbd "r") 'persp-remove-buffer)
    map))

(global-set-key (kbd "C-x b") 'persp-ivy-switch-buffer)
(global-set-key (kbd "C-x C-b") 'persp-ibuffer)

(persp-mode)

;; provide 
(provide 'packages)
