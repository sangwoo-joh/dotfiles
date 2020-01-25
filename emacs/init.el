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


;; default: desktop env, shortcuts, fonts, etc
(load "~/.emacs.d/config/default.el")

;; packages: basic necessary package setting
(load "~/.emacs.d/config/packages.el")

;; setting for c/c++
(load "~/.emacs.d/config/cpp.el")

;; setting for python
(load "~/.emacs.d/config/python.el")

;; setting for rust
(load "~/.emacs.d/config/rust.el")

;; setting for ocaml
(load "~/.emacs.d/config/ocaml.el")
