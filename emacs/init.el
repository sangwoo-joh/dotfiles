;;; init.el --- Personal emacs settings entry point
;;; Commentary:
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Performance Tweaks thanks to https://github.com/trev-dev/emacs
;; Minimize gc during startup i.e. set max threshold for triggering gc
(setq gc-cons-threshold most-positive-fixnum)

;; Lower threshold back to 8MiB (default is 800kB)
(add-hook 'emacs-startup-hook
	        (lambda ()
	          (setq gc-cons-threshold (expt 2 23))))

(setq read-process-output-max (* 1024 1024))

;; Don't die when handling large, minified files
(global-so-long-mode t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(package-initialize)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(setq package-enable-at-startup nil)
(require 'package)

;; use-package default
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package)
  (package-install 'bind-key))

;; Install exec-path-from-shell here
(when (not (package-installed-p 'exec-path-from-shell))
  (package-refresh-contents)
  (package-install 'exec-path-from-shell))

;; Initialize - OS X compatible
(exec-path-from-shell-initialize)

;; ready to load
(add-to-list 'load-path "~/.emacs.d/lisp")
;; (byte-recompile-directory "~/.emacs.d/lisp" 0)
(native-compile-async "~/.emacs.d/lisp" 'recursively)
(require 'kernel)

(provide 'init)
;;; init.el ends here
