;; Package settings
;; use use-package
;; to automatically setup & load package & setup package in the same block
(use-package company
  :ensure t
  :init
  (setq company-idle-delay 0)
  (setq company-show-numbers t)
  (progn
    (add-hook 'c-mode-hook 'company-mode)
    (add-hook 'c++-mode-hook 'company-mode)))

(use-package irony
  :ensure t
  :init
  (progn
    ;; (unless (irony--find-server-executable) (call-interactively #'irony-install-server))
    (add-hook 'c-mode-hook 'irony-mode)
    (add-hook 'c++-mode-hook 'irony-mode)
    ;; (setq-default irony-cdb-compilation-databases '(irony-cdb-libclang irony-cdb-clang-complete))
    ;; (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
    ))

(use-package flycheck
  :ensure t)

(use-package hungry-delete
  :ensure t
  :init (global-hungry-delete-mode)
  :bind (("S-<backspace>" . hungry-delete-backward)
	 ("S-<delete>" . hungry-delete-forward)))

(use-package rainbow-delimiters
  :ensure t
  :init
  (progn
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)))

(use-package popup :ensure t)
(use-package yaml-mode :ensure t)
(use-package toml-mode :ensure t)

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
	 ("C->" . mc/mark-next-like-this)
	 ("C-<" . mc/mark-previous-like-this)
	 ("C-c C-<" . mc/mark-all-like-this)))

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package auto-complete
  :ensure t
  :init (global-auto-complete-mode t)
  (ac-set-trigger-key "C-c <tab>"))

(use-package web-mode
  :ensure t
  :init
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-indent-style 2))
(add-to-list 'auto-mode-alist '("\\.php$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))

(use-package auctex
  :defer t
  :ensure t)

(use-package graphviz-dot-mode
  :ensure t
  :init
  (setq graphviz-dot-indent-width 2)
  (setq graphviz-dot-complete-word t))

(use-package smart-mode-line
  :ensure t
  :init
  (setq sml/no-confirm-load-theme t) ;; for ignoring y/n question. move custom-variables to the top of init.el is proper solution, but its bothering.
  (sml/setup))

(use-package smart-mode-line-powerline-theme
  :ensure t
  :init
  (sml/apply-theme 'powerline))

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

(use-package yasnippet-snippets :ensure t)
(use-package yasnippet-classic-snippets :ensure t)
(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode t)
  (add-hook 'prog-mode-hook #'yas-minor-mode))

(use-package undo-tree
  :ensure t
  :bind (("C-z" . undo-tree-visualize)))

(use-package ag
  :ensure t)

(use-package neotree
  :ensure t
  :bind (("C-c C-l" . neotree)))

(use-package editorconfig
  :ensure t
  :config (editorconfig-mode t))

;; found amazing pacakge; just like work space
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

;; little alternative for grep?
(use-package swiper
  :ensure t
  :bind (("C-c C-s" . swiper)
          ("C-c C-a" . swiper-all)))

(use-package which-key
  :ensure t
  :init (add-hook 'prog-mode-hook #'which-key-mode))

(use-package projectile
  :ensure t
  :init (add-hook 'prog-mode-hook #'projectile-mode)
  :bind (("C-c C-p" . projectile-find-regexp)
          ("C-x f" . projectile-find-file)))

(use-package iedit
  :ensure t
  :bind (("C-c C-a" . iedit-mode)))

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

(use-package wakatime-mode
  :ensure t)

(use-package company-tabnine
  :ensure t
  :init (add-to-list 'company-backends #'company-tabnine))

;; use as //docker:<container-id>
;; for remote: /sshx:<remote>|docker:<container-name>:<path> is fantastic. MAGIC CHARM.
(use-package docker-tramp
  :ensure t)

(use-package helpful
  :ensure t
  :bind
  ("C-h f" . helpful-function)
  ("C-h F" . helpful-command)
  ("C-h v" . helpful-variable)
  ("C-h o" . helpful-symbol))

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-itmes '((recents . 10)
                           (bookmarks . 10))))

(use-package google-translate
  :ensure t
  :init
  (require 'google-translate)
  (require 'google-translate-smooth-ui)
  (setq google-translate-translation-directions-alist '(("en" . "ko") ("ko" . "en")))
  (setq google-translate-pop-up-buffer-set-focus t)
  (setq google-translate-output-destination 'echo-area)
  (setq max-mini-window-height 0.5)
  :bind
  ("C-c C-g" . google-translate-smooth-translate))

(use-package json-mode :ensure t)
