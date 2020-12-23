;; for C++
(use-package irony
  :ensure t
  :init
  (progn
    ;; (unless (irony--find-server-executable) (call-interactively #'irony-install-server))
    (add-hook 'c-mode-hook 'irony-mode)
    (add-hook 'c++-mode-hook 'irony-mode)
    ;; (setq-default irony-cdb-compilation-databases '(irony-cdb-libclang irony-cdb-clang-complete))
    ;; (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
    ))
