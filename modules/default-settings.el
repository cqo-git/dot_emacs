;;; defaul-settings.el --- Description -*- lexical-binding: t; -*-

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

;; map Esc to Ctrl-g?
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; revert dired and other buffers
(customize-set-variable 'global-auto-revert-non-file-buffers t)

;; Use spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; Do not save duplicates in kill-ring
(customize-set-variable 'kill-do-not-save-duplicates t)

;; Make scrolling less stuttered
(setq auto-window-vscroll nil)
(customize-set-variable 'fast-but-imprecise-scrolling t)
(customize-set-variable 'scroll-conservatively 101)
(customize-set-variable 'scroll-margin 0)
(customize-set-variable 'scroll-preserve-screen-position t)

;; Better support for files with long lines
(setq-default bidi-paragraph-direction 'left-to-right)
(setq-default bidi-inhibit-bpa t)
(global-so-long-mode 1)

;; Make shebang (#!) file executable when saved
(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

;; Enable savehist-mode for command history
(savehist-mode 1)

;;
(setq inhibit-startup-message t)

;; Disable visible scrollbar
(scroll-bar-mode -1)

;; Disable Toolbar
(tool-bar-mode -1)

;; Disable tooltips
(tooltip-mode -1)

;; disable menu bar
(menu-bar-mode -1)

;; visual bell
(setq visible-bell nil)

;; TODO fringes
(set-fringe-mode 10)

;; Display line numbers in prog mode
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Move windows better ?
(global-set-key (kbd "C-c h") 'windmove-left)
(global-set-key (kbd "C-c j") 'windmove-down)
(global-set-key (kbd "C-c k") 'windmove-up)
(global-set-key (kbd "C-c l") 'windmove-right)

;; Switch windows
(global-set-key (kbd "C-c H") 'windmove-swap-states-left)
(global-set-key (kbd "C-c J") 'windmove-swap-states-down)
(global-set-key (kbd "C-c K") 'windmove-swap-states-up)
(global-set-key (kbd "C-c L") 'windmove-swap-states-right)

;; no backup files
(setq make-backup-files nil)

;; in org mode RET follows the link
(setq org-return-follows-link t)

;; warnings
(setq warning-minimum-level :error)

;; disable auto-save
(setq auto-save-default nil)

(provide 'default-settings)
;;; default-settings.el ends here
