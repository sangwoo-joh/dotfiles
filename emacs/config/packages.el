;; Package settings
;; use use-package
;; to automatically setup & load package & setup package in the same block
(use-package hungry-delete
  :ensure t
  :init (global-hungry-delete-mode)
  :bind (("S-<backspace>" . hungry-delete-backward)
	 ("S-<delete>" . hungry-delete-forward)))
(use-package rainbow-delimiters :ensure t)
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
  :ensure t)
  ;; :bind ("C-c <tab>" . ac-complete-merlin))

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
