;; (add-hook 'python-mode-hook 'guess-style-guess-tabs-mode)
;; (add-hook 'python-mode-hook (lambda ()
;; 			      (guess-style-guess-tab-width)))
(use-package python-mode
  :ensure t
  :init
  (lambda ()
    (setq-default tab-width 2)
    (setq-default indent-tabs-mode t)
    (setq-default py-indent-tabs-mode t)
    (add-to-list 'write-file-functions 'delete-trailing-whitespace)))

(use-package elpy
  :ensure t
  :init (elpy-enable))
