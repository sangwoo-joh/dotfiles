;; for C++
(use-package autopair
  :ensure t
  :init
  (progn
    (add-hook 'c-mode-hook 'autopair-mode)
    (add-hook 'c++-mode-hook 'autopair-mode)))

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

;; (use-package ivy
;;   :ensure t
;;   :init
;;   (progn
;;     (add-hook 'c-mode-hook 'ivy-mode)
;;     (add-hook 'c++-mode-hook 'ivy-mode)))

(use-package rtags
  :ensure t
  :init (setq rtags-path "~/bin/rtags")
  :bind
  (("C-c C-r C-t" . rtags-symbol-type)
    ;; ("C-c C-r C-l" . rtags-find-symbol-at-point)
    ("M-." . rtags-find-symbol-at-point)
    ("C-c C-r C-l" . rtags-find-reference-at-point)
    ;; ("C-c C-r C-." . rtags-location-stack-forward) ;; >
    ("C-," . rtags-location-stack-forward)
    ;; ("C-c C-r C-," . rtags-location-stack-back) ;; <
    ("M-," . rtags-location-stack-back)
    ("C-c C-r C-z" . rtags-location-stack-visualize)
    ("C-c C-r C-r" . rtags-rename-symbol) ;; rename all reachable symbols
    ("C-c C-r C-s" . rtags-display-summary) ;; symbol description
    ("C-c C-r C-d" . rtags-dependency-tree) ;; show dependency at point
    ("C-c C-r C-f" . rtags-reference-tree) ;; show reference at point
    )
  )

;; (use-package ivy-rtags
;;   :ensure t)
