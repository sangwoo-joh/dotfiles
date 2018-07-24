;; for C++
(use-package autopair
  :ensure t
  :init
  (progn
    (add-hook 'c-mode-hook 'autopair-mode)
    (add-hook 'c++-mode-hook 'autopair-mode)))

(use-package company
  :ensure t
  :init
  (setq company-idle-delay 0)
  (progn
  (add-hook 'c-mode-hook 'company-mode)
  (add-hook 'c++-mode-hook 'company-mode)))

(use-package irony
  :ensure t
  :init
  (progn
    ;; (unless (irony--find-server-executable) (call-interactively #'irony-install-server))
    (add-hook 'c-mode-hook 'irony-mode)
    (add-hook 'c++-mode-hook 'irony-mode)
    (setq-default irony-cdb-compilation-databases '(irony-cdb-libclang irony-cdb-clang-complete))
    (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)))

(use-package flycheck
  :ensure t)

(use-package company-irony
  :ensure t
  :bind (("M-/" . company-irony)))

(use-package flycheck-irony
  :ensure t
  :init
  (progn
    (eval-after-load 'flycheck '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))))

(defun my-c-c++-mode-flycheck-hook ()
  (interactive)
  (flycheck-select-checker 'c/c++-clang)
  (flycheck-mode))
(progn
  (add-hook 'c-mode-hook 'my-c-c++-mode-flycheck-hook)
  (add-hook 'c++-mode-hook 'my-c-c++-mode-flycheck-hook))

(use-package irony-eldoc
  :ensure t
  :init
  (progn
    (add-hook 'irony-mode-hook #'irony-eldoc)))

(use-package clang-format
  :ensure t
  :bind (("C-c C-f" . clang-format-buffer)))

(defun set-key-c++-mode ()
  (interactive)
  (define-key c++-mode-map (kbd "C-c ;") 'comment-line)
  (define-key c++-mode-map (kbd "C-c :") 'uncomment-region)
  (define-key c-mode-map (kbd "C-c ;") 'comment-line)
  (define-key c-mode-map (kbd "C-c :") 'uncomment-region))

(progn
  (add-hook 'c-mode-hook 'set-key-c++-mode)
  (add-hook 'c++-mode-hook 'set-key-c++-mode))
