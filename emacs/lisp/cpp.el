;;; cpp.el --- c/c++ Settings
;;; Commentary:
;;; C/C++ settings
;;; Code:

;; for C++
(use-package clang-format
  :ensure t
  :bind (("C-c C-f" . clang-format-buffer)))

(use-package rtags
  :ensure t
  :init (setq rtags-path "~/.dotfiles/rtags/bin"))

(require 'rtags)

(defun set-key-c++-mode ()
  (interactive)
  (define-key c++-mode-map (kbd "C-c ;") 'comment-line)
  (define-key c++-mode-map (kbd "C-c :") 'uncomment-region)
  (define-key c++-mode-map (kbd "M-.") 'rtags-find-symbol-at-point)
  (define-key c++-mode-map (kbd "M-.") 'rtags-find-symbol-at-point)
  (define-key c++-mode-map (kbd "M-," ) 'rtags-location-stack-back)
  (define-key c++-mode-map (kbd "C-,") 'rtags-location-stack-foward)
  (define-key c++-mode-map (kbd "C-c C-t") 'rtags-symbol-type)
  (define-key c++-mode-map (kbd "C-c C-r") 'rtags-rename-symbol)
  (define-key c++-mode-map (kbd "C-c a")  'rtags-references-tree)

  (define-key c-mode-map (kbd "C-c ;") 'comment-line)
  (define-key c-mode-map (kbd "C-c :") 'uncomment-region)
  (define-key c-mode-map (kbd "M-.") 'rtags-find-symbol-at-point)
  (define-key c-mode-map (kbd "M-.") 'rtags-find-symbol-at-point)
  (define-key c-mode-map (kbd "M-," ) 'rtags-location-stack-back)
  (define-key c-mode-map (kbd "C-,") 'rtags-location-stack-foward)
  (define-key c-mode-map (kbd "C-c C-t") 'rtags-symbol-type)
  (define-key c-mode-map (kbd "C-c C-r") 'rtags-rename-symbol)
  (define-key c-mode-map (kbd "C-c a")  'rtags-references-tree)
  (setq c-basic-offset 4))

(add-hook 'c-mode-hook 'set-key-c++-mode)
(add-hook 'c++-mode-hook 'set-key-c++-mode)

(use-package company-rtags
  :ensure t
  :config
  (setq rtags-completions-enabled t)
  (eval-after-load 'company
    '(add-to-list 'company-backends 'company-rtags))
  (setq rtags-autostart-diagnostics t))

(provide 'cpp)
;;; cpp.el ends here
