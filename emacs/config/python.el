;;; python.el --- Personal python settings.
;;; Commentary:
;;; Code:

(use-package python-mode
  :ensure t
  :config
  (setq-default tab-width 4))

(use-package elpy
  :ensure t
  :config (elpy-enable))

;;; python.el ends here
