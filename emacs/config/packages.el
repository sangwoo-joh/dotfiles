;; Package settings
;; use use-package
;; to automatically setup & load package & setup package in the same block
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
(use-package rust-mode :ensure t)

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
  (ac-set-trigger-key "C-c <tab"))

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
  :ensure t)

(use-package smart-mode-line-powerline-theme
  :ensure t
  :init
  (setq sml/no-confirm-load-theme t) ;; for ignoring y/n question. move custom-variables to the top of init.el is proper solution, but its bothering.
  (sml/setup)
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
