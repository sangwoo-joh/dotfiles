;;; cpp.el --- c/c++ Settings
;;; Commentary:
;;; C/C++ settings
;;; Code:

;; for C++
(use-package clang-format
  :ensure t
  :bind (("C-c C-f" . clang-format-buffer)))

(defun set-key-c++-mode ()
  (interactive)
  (define-key c++-mode-map (kbd "C-c ;") 'comment-line)
  (define-key c++-mode-map (kbd "C-c :") 'uncomment-region)
  (define-key c-mode-map (kbd "C-c ;") 'comment-line)
  (define-key c-mode-map (kbd "C-c :") 'uncomment-region))

(add-hook 'c-mode-hook 'set-key-c++-mode)
(add-hook 'c++-mode-hook 'set-key-c++-mode)

(use-package rtags
  :ensure t
  :init (setq rtags-path "~/.rtags-config/bin")
  :bind
  ("M-." . rtags-find-symbol-at-point)
  ("M-," . rtags-location-stack-back)
  ("C-," . rtags-location-stack-foward)
  ("C-c C-t" . rtags-symbol-type)
  ("C-c C-r" . rtags-rename-symbol)
  ("C-c a" . rtags-references-tree))


(use-package company-rtags
  :ensure t
  :config
  (setq rtags-completions-enabled t)
  (eval-after-load 'company
    '(add-to-list 'company-backends 'company-rtags))
  (setq rtags-autostart-diagnostics t))

(provide 'cpp)
;;; cpp.el ends here
