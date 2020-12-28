;;; others.el --- Other languages settings.
;;; Commentary:
;;; Code:
(use-package yaml-mode :ensure t)

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

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

(use-package dockerfile-mode :ensure t)

;; use as //docker:<container-id>
;; for remote: /sshx:<remote>|docker:<container-name>:<path> is fantastic. MAGIC CHARM.
(use-package docker-tramp :ensure t)

(use-package json-mode :ensure t)

(use-package rust-mode :ensure t)

;; flycheck emacs
(use-package flycheck-cask
  :ensure t
  :init
  (eval-after-load 'flycheck
    '(add-hook 'flycheck-mode-hook #'flycheck-cask-setup)))

;;; others.el ends here
