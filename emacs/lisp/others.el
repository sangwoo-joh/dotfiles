;;; others.el --- Other languages settings.
;;; Commentary:
;;; Code:
(use-package yaml-mode :ensure t)


(defun fill-markdown-link-at-point ()
  "FILL MARKDOWN LINK IN PARENTHESES FROM BRACKET."
  (interactive)
  (save-excursion
    (progn
      (let* ((start (search-backward "["))
             (end (search-forward "]"))
             (title (buffer-substring (+ start 1) (- end 1)))
             (title (replace-regexp-in-string "[^a-zA-Z0-9]" "-" title))
             (title (downcase title)))
        (goto-char (match-end 0))
        (insert (format "(%s)" title))
        ))))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md$" . gfm-mode)
	       ("\\.md$" . markdown-mode)
	       ("\\.markdown$" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  :bind
  ("C-c C-t C-f" . markdown-table-align)
  ("C-c C-c C-l" . fill-markdown-link-at-point)
  )

(defun unify-web-mode-spacing ()
  "Stole from https://github.com/trev-dev/emacs"
  (setq web-mode-markup-indent-offset tab-width)
  (setq web-mode-css-indent-offset tab-width)
  (setq web-mode-code-indent-offset tab-width)
  (setq web-mode-style-padding tab-width)
  (setq web-mode-script-padding tab-width)
  (setq web-mode-indent-style 2))

(use-package web-mode
  :ensure t
  :hook (web-mode . unify-web-mode-spacing)
  :mode
  ("\\.php$" . web-mode)
  ("\\.html$" . web-mode))

(use-package auctex
  :defer t
  :ensure t
  :config
  (setq TeX-PDF-mode t))

(use-package company-auctex
  :ensure t
  :init
  (company-auctex-init))

(use-package graphviz-dot-mode
  :ensure t
  :init
  (setq graphviz-dot-indent-width 2))

(use-package dockerfile-mode :ensure t)

;; use as //docker:<container-id>
;; for remote: /sshx:<remote>|docker:<container-name>:<path> is fantastic. MAGIC CHARM.
(use-package docker-tramp :ensure t)

(use-package json-mode :ensure t)

(use-package rust-mode :ensure t)

;; flycheck emacs
(use-package flycheck-cask
  :ensure t
  :config
  (eval-after-load 'flycheck
    '(add-hook 'flycheck-mode-hook #'flycheck-cask-setup)))

(provide 'others)
;;; others.el ends here
