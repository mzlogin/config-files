(add-to-list 'load-path "~/.emacs.d/site-lisp")

;; set up package repository
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;; markdown
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; generic settings
(setq default-directory "~/")
(set-language-environment "utf-8")

;; gtags
(setq gtags-suggested-key-mapping t)
(require 'ggtags)
(setq c-mode-hook
      '(lambda()
	 (ggtags-mode t)))
(setq c++-mode-hook
      '(lambda()
	 (ggtags-mode t)))

;; code auto completion
(add-hook 'after-init-hook 'global-company-mode)
(setq company-idle-delay 0)
(setq semanticdb-default-save-directory "~/.emacs.d/semanticdb")
(add-hook 'markdown-mode-hook
      '(lambda()
	 (company-mode 0)))
(add-hook 'eshell-mode-hook
      '(lambda()
	 (company-mode 0)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (tango-dark)))
 '(global-linum-mode t)
 '(inhibit-startup-screen t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Setting English Font
(set-face-attribute
 'default nil :font "Consolas 11")
;; Setting Chinese Font
(dolist (charset '(kana han symbol cjk-misc bopomofo))
  (set-fontset-font (frame-parameter nil 'font)
		    charset
		    (font-spec :family "Microsoft Yahei" :size 16)))

;; Maximize after startup
(run-with-idle-timer 1 nil 'w32-send-sys-command 61488)

;; ido-mode
(ido-mode 1)

;; projectile
(projectile-global-mode)
(setq projectile-require-project-root nil)

;; org-mode
(add-hook 'org-mode-hook (lambda() (setq truncate-lines nil)))

;; evil-mode
(evil-mode t)
