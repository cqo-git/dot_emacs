;;; macos.el -*- lexical-binding: t; -*-


;;(setq default-frame-alist '((undecorated . t)))
;;(add-to-list 'default-frame-alist '(drag-internal-border . 1))
;;(add-to-list 'default-frame-alist '(internal-border-width . 5))

;; add path to custom Apple related scripts 
(add-to-list 'load-path "~/Apple/scripts/elisp/")

;; Font size on Aqua
(set-face-attribute 'default nil :font "Iosevka Term" :height 155)


(provide 'macos-config)

