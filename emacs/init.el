;; initial package setup
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

(require 'desktop)
(desktop-save-mode 1)
(defun my-desktop-save ()
  (interactive)
  ;; Don't call desktop-save-in-desktop-dir, as it prints a message.
  (if (eq (desktop-owner) (emacs-pid))
      (desktop-save desktop-dirname)))
(add-hook 'auto-save-hook 'my-desktop-save)

(defalias 'yes-or-no-p 'y-or-n-p)

(defun select-next-window ()
  (interactive)
  (select-window (next-window (selected-window))))

(defun select-previous-window ()
  (interactive)
  (select-window (previous-window (selected-window))))

(defun up-down-case-char ()
  (interactive)
  (set-mark-command ())
  (forward-char 1)
  (setq myStr (buffer-substring (region-beginning) (region-end)))
  (if (string-equal myStr (upcase myStr))
      (downcase-region (region-beginning) (region-end))
    (upcase-region (region-beginning) (region-end)))
  (backward-char 1))

(defun unfill-paragraph ()	  ; by Stefan Monnier (foo at acm.org)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defun eshell-clear ()		  ; by Sailor (http://www.khngai.com/)
  "Clear the eshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

(defun set-font-size (size)
  "Set font size."
  (interactive "nSize: ")
  (set-face-attribute 'default nil :height (* size 12)))

;; Default settings
(set-fontset-font "fontset-default" 'latin "D2Coding")
(set-fontset-font "fontset-default" 'hangul "D2Coding")
(set-face-attribute 'default nil :font "fontset-default")

(setq inhibit-startup-message t)
(setq auto-save-default nil)
(setq auto-save-list-file-name nil)
(setq make-backup-files nil)

(set-language-environment-input-method "Korean")
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)

(column-number-mode t)
(tool-bar-mode -1)

(setq x-alt-keysym 'meta)
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta))

(global-font-lock-mode 1)
(show-paren-mode 1)
(setq search-highlight t)
(setq query-replace-highlight t)
(setq TeX-PDF-mode t)
(setq tramp-default-method "sshx")
(add-to-list 'tramp-remote-process-environment "JEKYLL_ENV=production")

;; auto highlight
(defun highlight-and-mark ()
  "Highlight all symbols that are same with under the cursor and mark it."
  (interactive)
  (progn
    (highlight-symbol-at-point)
    (forward-sexp)
    (backward-sexp)
    (mark-sexp)))

(defun unhighlight-all ()
  "Remove all highlights made by `hi-lock' from the current buffer."
  (interactive)
  (unhighlight-regexp t))

;; keyboard shortcuts
(global-set-key (kbd "C-c h") 'highlight-and-mark)
(global-set-key (kbd "C-c H") 'unhighlight-all)
(global-set-key (kbd "<C-tab>") 'select-next-window)
(global-set-key (kbd "<C-S-tab>") 'select-previous-window)
(global-set-key (kbd "<C-iso-lefttab>") 'select-previous-window)
(global-set-key (kbd "C-x C-k") 'kill-this-buffer)
(global-set-key (kbd "C-x C-n") 'next-buffer)
(global-set-key (kbd "C-x C-p") 'previous-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-c ;") 'comment-region)
(global-set-key (kbd "C-c :") 'uncomment-region)
(global-set-key (kbd "C-`") 'up-down-case-char)
(global-set-key (kbd "M-Q") 'unfill-paragraph)
(global-set-key (kbd "C-x p") 'bookmark-jump)
(global-set-key (kbd "C-x P") 'bookmark-set)

(when window-system			; Disable suspend
  (global-unset-key (kbd "C-z")))
(if (eq system-type 'darwin)
    (set-font-size 18)
  (set-font-size 14))
(global-linum-mode 1)

;; Package settings
;; use use-package
;; to automatically setup & load package & setup package in the same block
(use-package hungry-delete
  :ensure t
  :init (global-hungry-delete-mode)
  :bind (("S-<backspace>" . hungry-delete-backward)
	 ("S-<delete>" . hungry-delete-forward)))
(use-package rainbow-delimiters :ensure t)
(use-package popup :ensure t)
(use-package yaml-mode :ensure t)
(use-package toml-mode :ensure t)
(use-package rust-mode :ensure t)
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))
(use-package multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
	 ("C->" . mc/mark-next-like-this)
	 ("C-<" . mc/mark-previous-like-this)
	 ("C-c C-<" . mc/mark-all-like-this)))
(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))
(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))
(use-package auto-complete
  :ensure t
  :bind ("C-c <tab>" . ac-complete-merlin))
;; (use-package dracula-theme
;;   :ensure t)
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'dracula t)
(use-package web-mode
  :ensure t
  :init
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-indent-style 2))
(add-to-list 'auto-mode-alist '("\\.php$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
(use-package auctex
  :defer t
  :ensure t)

;; OPAM packages: ocp-indent & merlin & tuareg
;; NOTE: they share a load path.
;; NOTE2: opam package cannot be set up by use-package...
(setq opam-share
      (substring
       (shell-command-to-string "opam config var share 2> /dev/null") 0 -1))
(add-to-list 'load-path (concat opam-share "/emacs/site-lisp"))
(require 'ocp-indent)
(require 'merlin)
(add-hook 'tuareg-mode-hook 'merlin-mode t)
(add-hook 'caml-mode-hook 'merlin-mode t)
(add-hook 'tuareg-mode-hook 'rainbow-delimiters-mode t)
(add-hook 'caml-mode-hook 'rainbow-delimiters-mode t)
(setq merlin-use-auto-complete-mode 'easy)
(setq merlin-command 'opam)
(load (concat opam-share "/emacs/site-lisp/tuareg-site-file"))
;; ocamlformat setting
(load (concat opam-share "/emacs/site-lisp/ocamlformat"))
(add-hook 'before-save-hook 'ocamlformat-before-save)

(add-hook 'python-mode-hook
      (lambda ()
        (setq indent-tabs-mode t)
        (setq tab-width 2)
        (setq python-indent 2)))

;; rust formatting: need to install `cargo install rustfmt` and add path `export PATH="$PATH:$HOME/.cargo/bin"
(setq rust-format-on-save t)

(display-time)
(defun chomp-end (str)
  (replace-regexp-in-string (rx (* (any " \t\n")) eos) "" str))

;; my local setting for z3 smt2 syntax
;; (use-package flycheck :ensure t)
;; (use-package boogie-friends :ensure t)
;; (setq flycheck-z3-smt2-executable "/home/falcon/git/AA/z3")
;; (add-hook 'z3-smt2-mode-hook 'rainbow-delimiters-mode t)
(defun increment-number-at-point ()
  (interactive)
  (skip-chars-backward "0-9")
  (or (looking-at "[0-9]+")
      (error "No number at point"))
  (replace-match (number-to-string (1+ (string-to-number (match-string 0))))))
(global-set-key (kbd "C-c +") 'increment-number-at-point)

;; Disable Eshell's scroll feature
;; from https://emacs.stackexchange.com/questions/28819/eshell-goes-to-the-bottom-of-the-page-after-executing-a-command
(add-hook 'eshell-mode-hook
          (defun chunyang-eshell-mode-setup ()
            (remove-hook 'eshell-output-filter-functions
                         'eshell-postoutput-scroll-to-bottom)))
;; for C++
(use-package company
  :ensure t)

(use-package irony
  :ensure t)

(use-package company-irony
  :ensure t)

(use-package flycheck
  :ensure t)

(use-package clang-format
  :ensure t)
;; for C++
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
    (unless (irony--find-server-executable) (call-interactively #'irony-install-server))
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
