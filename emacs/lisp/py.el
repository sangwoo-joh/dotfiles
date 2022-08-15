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
  (setq elpy-rpc-python-command "/Users/sangwoo-joh/opt/anaconda3/bin/python")
  (setq elpy-rpc-backend "jedi")
  :init
  (add-hook 'elpy-mode-hook #'python-hook))

(with-eval-after-load 'flycheck
  (setq flycheck-python-flake8-executable "python3")
  (setq flycheck-flake8rc "~/.config/flake8"))

(org-babel-do-load-languages
 'org-babel-load-languages '((python . t)))

(provide 'py)
;;; py.el ends here
