;;; init.el --- Personal emacs settings entry point
;;; Commentary:
;;; Code:

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; use-package default
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

;; Install exec-path-from-shell here
(when (not (package-installed-p 'exec-path-from-shell))
  (package-refresh-contents)
  (package-install 'exec-path-from-shell))

;; Initialize - OS X compatible
(exec-path-from-shell-initialize)

;; custom dracula theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'dracula t)

;; ready to load
(add-to-list 'load-path "~/.emacs.d/lisp")
(require 'kernel)

;; auto-package-update
(use-package auto-package-update
  :ensure t
  :config
  (setq auto-package-update-delete-old-versions t)
  (auto-package-update-maybe))

(provide 'init)
;;; init.el ends here
