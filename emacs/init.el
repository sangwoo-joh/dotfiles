;; emacs config settings

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

;; default: desktop env, shortcuts, fonts, etc
(load "~/.emacs.d/config/default.el")

;; packages: basic necessary package setting
(load "~/.emacs.d/config/packages.el")

;; setting for ocaml
(load "~/.emacs.d/config/ocaml.el")

;; setting for c/c++
(load "~/.emacs.d/config/cpp.el")

;; setting for python
(load "~/.emacs.d/config/python.el")

;; setting for rust
(load "~/.emacs.d/config/rust.el")
