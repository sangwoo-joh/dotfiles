;;; python.el --- Personal python settings.
;;; Commentary:
;;; Code:

(use-package python-mode
  :ensure t
  :config
  (setq-default tab-width 4)
  (setq-default py-indent-tabs-mode t)
  (add-to-list 'write-file-functions 'delete-trailing-whitespace))

(use-package elpy
  :ensure t
  :config (elpy-enable))

;;; python.el ends here
