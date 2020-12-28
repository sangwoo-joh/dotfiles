;;; packages.el --- Personal helpful packages.
;;; Commentary:
;; Use use-package to automatically setup & load package & setup package in the same block
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; category: helpful
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-banner-logo-title "Emacs, the only true, genuine, and beloved editor")
  (setq dashboard-set-navigator t)
  (setq dashboard-set-init-info t))

;; Usage: grep some keywords, and C-c C-p in result buffer.
;; After editing, just save with C-x C-s as usual file in the buffer.
;; Others:
;; C-c C-e: Apply changes to file buffers
;; C-c C-u: All changes are unmarked and ignored
;; C-c C-d: Mark as delete to current line
;; C-c C-r: Remove the changes in the region
;; C-c C-p: Toggle read-only area
;; C-c C-k: Discard all changes and exit
;; C-c C-q: Exit wgrep mode
(use-package wgrep
  :ensure t)

(use-package helpful
  :ensure t
  :bind
  ("C-h f" . helpful-function)
  ("C-h F" . helpful-command)
  ("C-h v" . helpful-variable)
  ("C-h o" . helpful-symbol))

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

(use-package which-key
  :ensure t
  :init
  (add-hook 'prog-mode-hook #'which-key-mode))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package flycheck-color-mode-line
  :ensure t
  :init
  (eval-after-load "flycheck"
    '(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)))

(use-package flycheck-pos-tip
  :ensure t
  :init
  (with-eval-after-load 'flycheck
    (flycheck-pos-tip-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; category: auto-complete
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package autopair
  :ensure t
  :init (autopair-global-mode))

(use-package company
  :ensure t
  :init
  (setq company-idle-delay 0)
  (setq company-show-numbers t)
  (progn
    (add-hook 'c-mode-hook 'company-mode)
    (add-hook 'c++-mode-hook 'company-mode)))

(use-package popup :ensure t)

(use-package undo-tree
  :ensure t
  :init (global-undo-tree-mode)
  :bind (("C-z" . undo-tree-visualize)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; category: chore
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package ag :ensure t)

(use-package swiper
  :ensure t
  :bind (("C-c C-s" . swiper)))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package hungry-delete
  :ensure t
  :init (global-hungry-delete-mode)
  :bind (("S-<backspace>" . hungry-delete-backward)
	 ("S-<delete>" . hungry-delete-forward)))

(use-package multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
	 ("C->" . mc/mark-next-like-this)
	 ("C-<" . mc/mark-previous-like-this)
	 ("C-c C-<" . mc/mark-all-like-this)))

(use-package iedit
  :ensure t
  :bind (("C-C C-e" . iedit-mode)
          ("C-C C-q" . iedit-quit)))

(use-package switch-window
  :ensure t
  :init
  (setq switch-window-shortcut-style 'qwerty)
  (setq switch-window-qwerty-shortcuts
	'("a" "s" "d" "f" "j" "k" "l" ";" "q" "w" "e" "r" "i" "o" "z" "x" "c" "v"))
  :bind (("C-x o" . switch-window)
	 ("C-x 1" . switch-window-then-maximize)
	 ("C-x 2" . switch-window-then-split-below)
	 ("C-x 3" . switch-window-then-split-right)
	 ("C-x 0" . switch-window-then-delete)
	 ("C-x 4 d" . switch-window-then-dired)
	 ("C-x 4 f" . switch-window-then-find-file)
	 ;; ("C-x 4 m" . switch-window-then-compose-mail)
	 ;; ("C-x 4 r" . switch-window-then-find-file-read-only)
	 ("C-x 4 0" . switch-window-then-kill-buffer)))

(use-package eyebrowse
  :ensure t
  :init (eyebrowse-mode t)
  :config
  (setq eyebrowse-mode-line-style 'always)
  (setq eyebrowse-mode-line-separator "; ")
  (custom-set-faces
    '(eyebrowse-mode-line-active
       ((t (:foreground "black" :background "gold" :inherit (mode-line-emphasis))))
       ))
  :bind (("M-0" . eyebrowse-switch-to-window-config-0)
          ("M-1" . eyebrowse-switch-to-window-config-1)
          ("M-2" . eyebrowse-switch-to-window-config-2)
          ("M-3" . eyebrowse-switch-to-window-config-3)
          ("M-4" . eyebrowse-switch-to-window-config-4)
          ("M-5" . eyebrowse-switch-to-window-config-5)
          ("M-6" . eyebrowse-switch-to-window-config-6)
          ("M-7" . eyebrowse-switch-to-window-config-7)
          ("M-8" . eyebrowse-switch-to-window-config-8)
          ("M-9" . eyebrowse-switch-to-window-config-9)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; category: aesthetic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package rainbow-delimiters
  :ensure t
  :init
  (progn
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)))

(use-package smart-mode-line
  :ensure t
  :init
  ;; for ignoring y/n question.
  ;; move custom-variables to the top of init.el is proper solution, but it's bothersome.
  (setq sml/no-confirm-load-theme t)
  (setq sml/shorten-directory t)
  (setq sml/shorten-modes t)
  (sml/setup))

(use-package smart-mode-line-powerline-theme
  :ensure t
  :init
  (sml/apply-theme 'powerline))

;;; packages.el ends here
