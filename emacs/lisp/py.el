;;; py.el --- Personal python settings.
;;; Commentary:
;;; Code:

(defun python-hook ()
  "PYTHON MODE HOOK"
  (highlight-indentation-mode -1)
  (setq mode-name "🐍"))

(use-package elpy
  :ensure t
  :config
  (elpy-enable)
  (setq elpy-rpc-timeout nil)
  :init
  (add-hook 'elpy-mode-hook #'python-hook))

(with-eval-after-load 'flycheck
  (setq flycheck-python-flake8-executable "python3"))

(provide 'py)
;;; py.el ends here
