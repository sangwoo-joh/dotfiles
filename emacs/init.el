;; emacs config settings

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))


;; key bindings - for macos
(when (eq system-type 'darwin) ;; mac specific settings
  (setq mac-option-modifier 'alt)
  (setq mac-command-modifier 'meta)
  ;;(global-set-key [kp-delete] 'delete-char) ;; sets fn-delete to be right-delete
  )

;; Install exec-path-from-shell here
(when (not (package-installed-p 'exec-path-from-shell))
  (package-refresh-contents)
  (package-install 'exec-path-from-shell))

;; Initialize - OS X compatible
(exec-path-from-shell-initialize)

;; default: desktop env, shortcuts, fonts, etc
(load "~/.emacs.d/config/default.el")

;; packages: basic necessary package setting
(load "~/.emacs.d/config/packages.el")

;; setting for ocaml
(load "~/.emacs.d/config/ocaml.el")

;; setting for python
(load "~/.emacs.d/config/python.el")

;; setting for others
(load "~/.emacs.d/config/others.el")

;; auto-package-update
(use-package auto-package-update
  :ensure t
  :config
  (setq auto-package-update-delete-old-versions t)
  (auto-package-update-maybe))
