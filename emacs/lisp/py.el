;;; py.el --- Personal python settings.
;;; Commentary:
;;; Code:
(use-package elpy
  :ensure t
  :config (elpy-enable))

(with-eval-after-load 'flycheck
  (setq flycheck-python-flake8-executable "python3"))

(provide 'py)
;;; py.el ends here
