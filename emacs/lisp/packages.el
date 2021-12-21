;;; packages.el --- Personal helpful packages.
;;; Commentary:
;; Use use-package to automatically setup & load package & setup package in the same block
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helpful
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Usage: grep some keywords, and C-c C-p in result buffer.
;; After editing, just save with C-x C-s as usual file in the buffer.
;; Others:
;; C-c C-e: Apply changes to file buffers
;; C-c C-u: All changes are unmarked and ignored
;; C-c C-d: Mark as delete to current line
;; C-c C-r: Remove the changes in the region
;; C-c C-p: Toggle read-only area
;; C-c C-k: Discard all changes and exit
;; C-c C-q: Exit wgrep mode
(use-package wgrep :ensure t)

(use-package helpful
  :ensure t
  :bind
  ("C-h f" . helpful-function)
  ("C-h F" . helpful-command)
  ("C-h v" . helpful-variable)
  ("C-h o" . helpful-symbol))

(use-package magit
  :ensure t
  :bind
  ("C-x g" . magit-status)
  ("C-c C-u" . smerge-keep-upper) ;; mine
  ("C-c C-l" . smerge-keep-lower) ;; other
  )

(use-package which-key
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'which-key-mode))

(use-package flycheck
  :ensure t
  :config (global-flycheck-mode))

(use-package flycheck-color-mode-line
  :ensure t
  :config
  (eval-after-load "flycheck"
    '(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)))

(use-package flycheck-pos-tip
  :ensure t
  :config
  (with-eval-after-load 'flycheck
    (flycheck-pos-tip-mode))
  (setq flycheck-pos-tip-timeout 60))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auto-complete
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package smartparens
  :ensure t
  :config (smartparens-global-mode))

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0)
  (setq company-show-numbers t)
  (global-company-mode))

(use-package undo-tree
  :ensure t
  :config (global-undo-tree-mode)
  :bind (("C-z" . undo-tree-visualize)))

(use-package projectile
  :ensure t
  :config (projectile-mode t))

(use-package yasnippet-snippets
  :ensure nil
  :load-path "~/.emacs.d/yasnippet-snippets")

(use-package yasnippet
  :ensure t
  :config (yas-global-mode t)
  :bind (("C-c y n" . yas-new-snippet)
         ("C-c y i" . yas-insert-snippet)
         ("C-c y v" . yas-visit-snippet-file)
         ("C-c <tab>" . yas-expand)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; chore
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package ag :ensure t)

;; This is much better then searching one by one by C-s
(use-package swiper
  :ensure t
  :bind (("C-c C-s" . swiper-thing-at-point)))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package hungry-delete
  :ensure t
  :config (global-hungry-delete-mode)
  :bind (("S-<backspace>" . hungry-delete-backward)
	 ("S-<delete>" . hungry-delete-forward)))

(use-package multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
	 ("C->" . mc/mark-next-like-this)
	 ("C-<" . mc/mark-previous-like-this)
	 ("C-c C-<" . mc/mark-all-like-this)))

;; This is much better then changing one by one by multiple-cursors (C->)
;; After iedit-mode is on (by C-C C-e),
;; <tab>, S-<tab>, M->, M-<: navigation
;; M-;: toggle
;; M-N: numbering
;; M-R: replace
;; M-D: delete
(use-package iedit
  :ensure t
  :bind (("C-C C-e" . iedit-mode)))

(use-package switch-window
  :ensure t
  :config
  (setq switch-window-shortcut-style 'qwerty)
  (setq switch-window-qwerty-shortcuts
	'("a" "s" "d" "f" "j" "k" "l" ";" "q" "w" "e" "r" "i" "o" "z" "x" "c" "v"))
  :bind (("C-x o" . switch-window)
	 ("C-x 1" . switch-window-then-maximize)
	 ("C-x 2" . switch-window-then-split-below)
	 ("C-x 3" . switch-window-then-split-right)
	 ("C-x 0" . switch-window-then-delete)
	 ("C-x 4 d" . switch-window-then-dired)
	 ("C-x 4 f" . switch-window-then-find-file)
	 ;; ("C-x 4 m" . switch-window-then-compose-mail)
	 ;; ("C-x 4 r" . switch-window-then-find-file-read-only)
	 ("C-x 4 0" . switch-window-then-kill-buffer)))

(use-package eyebrowse
  :ensure t
  :config
  (eyebrowse-mode t)
  :bind (("M-0" . eyebrowse-switch-to-window-config-0)
          ("M-1" . eyebrowse-switch-to-window-config-1)
          ("M-2" . eyebrowse-switch-to-window-config-2)
          ("M-3" . eyebrowse-switch-to-window-config-3)
          ("M-4" . eyebrowse-switch-to-window-config-4)
          ("M-5" . eyebrowse-switch-to-window-config-5)
          ("M-6" . eyebrowse-switch-to-window-config-6)
          ("M-7" . eyebrowse-switch-to-window-config-7)
          ("M-8" . eyebrowse-switch-to-window-config-8)
          ("M-9" . eyebrowse-switch-to-window-config-9)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; aesthetic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; NOTE: after install this package, run M-x all-the-icons-install-fonts to download the actual fonts,
;; and then run `fc-cache -f -v` for actual installation.
(use-package all-the-icons :ensure t)
(use-package all-the-icons-dired
  :requires all-the-icons
  :ensure t
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package doom-themes
  :ensure nil
  :load-path "~/.emacs.d/emacs-doom-themes"
  :requires all-the-icons
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-dracula t)
  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode t))

(use-package ssh-config-mode :ensure t)

(use-package emojify
  :ensure t
  :hook (after-init . global-emojify-mode)
  :bind (("C-S-i" . emojify-insert-emoji)))

(use-package fzf :ensure t)

;; git-timemachine https://gitlab.com/pidu/git-timemachine
;; n, p: visit next, previous historic version
;; w: copy the abbreviated hash of the current historic version
;; W: copy the full hash ...
;; g: visit n-th revision
;; t: visit revision by selected commit message
;; q: exit
;; b: run magit-blame on the currently visited revision (if magit available)
;; c: show current commit using magit (if magit available)
(use-package git-timemachine :ensure t)

(use-package centered-window :ensure t
  :bind (("C-M-l" . centered-window-mode)))
(provide 'packages)
;;; packages.el ends here
