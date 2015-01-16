(add-to-list 'load-path "~/.emacs.d/site-lisp")

;; markdown
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; default directory
(setq default-directory "~/")

;; gtags
(setq gtags-suggested-key-mapping t)
(require 'gtags)
(setq c-mode-hook
      '(lambda()
	 (gtags-mode t)))
(setq c++-mode-hook
      '(lambda()
	 (gtags-mode t)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(global-linum-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Maximize after startup
;; (run-with-idle-timer 1 nil 'w32-send-sys-command 61488)
