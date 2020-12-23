(use-package python-mode
  :ensure t
  :init
  (lambda ()
    (setq-default tab-width 4)
    (setq-default indent-tabs-mode t)
    (setq-default py-indent-tabs-mode t)
    (add-to-list 'write-file-functions 'delete-trailing-whitespace)))

;; M-r : find reference
;; M-. : find definition
;; M-, : goto previous
(use-package anaconda-mode
  :ensure t
  :init
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode))

(use-package elpy
  :ensure t
  :init (elpy-enable))
