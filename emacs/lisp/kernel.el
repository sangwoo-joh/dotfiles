;;; kernel.el --- Kernel of my settings
;;; Commentary:
;;; Code:

(require 'cl-lib)

;; desktop
(require 'desktop)
(desktop-save-mode t)

(defun my-desktop-save ()
  "MY DESKTOP SAVE."
  (interactive)
  ;; Don't call desktop-save-in-desktop-dir, as it prints a message.
  (if (eq (desktop-owner) (emacs-pid))
    (desktop-save desktop-dirname)))

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'kill-emacs-hook 'my-desktop-save)

(setq desktop-auto-save-timeout nil)
(setq auto-save-default nil)
(setq auto-save-list-file-name nil)
(setq make-backup-files nil)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

(defalias 'yes-or-no-p 'y-or-n-p)

(defun select-next-window ()
  "SELECT NEXT WINDOW."
  (interactive)
  (select-window (next-window (selected-window))))

(defun select-previous-window ()
  "SELECT PREVIOUS WINDOW."
  (interactive)
  (select-window (previous-window (selected-window))))

(defun up-down-case-char ()
  "TOGGLE CASE OF A CHARACTER."
  (interactive)
  (set-mark-command ())
  (forward-char 1)
  (let* ((myStr (buffer-substring (region-beginning) (region-end))))
    (if (string-equal myStr (upcase myStr))
	(downcase-region (region-beginning) (region-end))
      (upcase-region (region-beginning) (region-end)))
    (backward-char 1)))

(defun unfill-paragraph ()	  ; by Stefan Monnier (foo at acm.org)
  "TAKE A MULTI-LINE PARAGRAPH AND MAKE IT INTO A SINGLE LINE OF TEXT."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defun eshell-clear ()		  ; by Sailor (http://www.khngai.com/)
  "CLEAR THE ESHELL BUFFER."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

(defun set-font-size (size)
  "SET FONT SIZE."
  (interactive "nSize: ")
  (set-face-attribute 'default nil :height (* size 11)))

(display-time)

(defun increment-number-at-point ()
  "INCREASE NUMBER AT CURSOR."
  (interactive)
  (skip-chars-backward "0-9")
  (or (looking-at "[0-9]+")
      (error "No number at point"))
  (replace-match (number-to-string (1+ (string-to-number (match-string 0))))))

;; default font settings
(when (eq system-type 'darwin)
  (set-fontset-font "fontset-default" 'unicode "Apple Color Emoji"))
(when (eq system-type 'gnu/linux)
  (set-fontset-font "fontset-default" 'unicode "Noto Color Emoji"))
(set-fontset-font "fontset-default" 'latin "Ubuntu Mono derivative Powerline")
(set-fontset-font "fontset-default" 'hangul "D2Coding")
(set-face-attribute 'default nil :font "fontset-default")

(set-language-environment-input-method "Korean")
(set-language-environment "Korean")
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(column-number-mode t)
(tool-bar-mode -1)
(global-font-lock-mode t)
(show-paren-mode)

(setq inhibit-startup-message t)
(setq x-alt-keysym 'meta)
(setq search-highlight t)
(setq query-replace-highlight t)
(defvar tramp-default-method)
(setq tramp-default-method "sshx")
(setq password-cache-expiry nil) ;; to disable password expiration

;; auto highlight
(defun highlight-and-mark ()
  "HIGHLIGHT ALL SYMBOLS THAT ARE SAME WITH UNDER THE CURSOR AND MARK IT."
  (interactive)
  (progn
    (highlight-symbol-at-point)
    (forward-sexp)
    (backward-sexp)
    (mark-sexp)))

(defun unhighlight-all ()
  "REMOVE ALL HIGHLIGHT MADE BY `HI-LOCK' FROM THE CURRENT BUFFER."
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
(global-set-key (kbd "C-c n") 'make-frame)
(global-set-key (kbd "C-c k") 'delete-frame)
(global-set-key (kbd "C-c o") 'other-frame)
(global-set-key (kbd "C-c m") 'bookmark-jump)
(global-set-key (kbd "C-c M") 'bookmark-set)
(global-set-key (kbd "C-c L") 'list-bookmarks)
(global-set-key (kbd "C-c d") 'delete-trailing-whitespace)
(global-set-key (kbd "C-c +") 'increment-number-at-point)

(setq abbrev-file-name "~/.emacs.d/abbrev_defs")
(setq save-abbrevs 'silently)
(define-abbrev-table 'global-abbrev-table
  '(("greekalpha" "α")
    ("greekbeta" "β")
    ("greekgamma" "γ")
    ("greekdelta" "δ")
    ("greekepsilon" "ε")
    ("greekzeta" "ζ")
    ("greeketa" "η")
    ("greektheta" "θ")
    ("greekiota" "ι")
    ("greekkappa" "κ")
    ("greeklambda" "λ")
    ("greekmu" "μ")
    ("greeknu" "ν")
    ("greekxi" "ξ")
    ("greekomicron" "ο")
    ("greekpi" "π")
    ("greekrho" "ρ")
    ("greeksigma" "σ")
    ("greektau" "τ")
    ("greekupsilon" "υ")
    ("greekphi" "φ")
    ("greekchi" "χ")
    ("greekpsi" "ψ")
    ("greekomega" "ω")

    ("greekAlpha" "Α")
    ("greekBeta" "Β")
    ("greekGamma" "Γ")
    ("greekDelta" "Δ")
    ("greekEpsilon" "Ε")
    ("greekZeta" "Ζ")
    ("greekEta" "Η")
    ("greekTheta" "Θ")
    ("greekIota" "Ι")
    ("greekKappa" "Κ")
    ("greekLambda" "Λ")
    ("greekMu" "Μ")
    ("greekNu" "Ν")
    ("greekXi" "Ξ")
    ("greekOmicron" "Ο")
    ("greekPi" "Π")
    ("greekRho" "Ρ")
    ("greekSigma" "Σ")
    ("greekTau" "Τ")
    ("greekUpsilon" "Υ")
    ("greekPhi" "Φ")
    ("greekChi" "Χ")
    ("greekPsi" "Ψ")
    ("greekOmega" "Ω")
    ))

(set-default 'abbrev-mode t)


(when window-system			; Disable suspend
  (global-unset-key (kbd "C-z")))

;; key bindings - for macos
(when (eq system-type 'darwin) ;; mac specific settings
  (defvar mac-option-modifier)
  (defvar mac-command-modifier)
  (setq mac-option-modifier 'alt)
  (setq mac-command-modifier 'meta)
  ;;(global-set-key [kp-delete] 'delete-char) ;; sets fn-delete to be right-delete
  )

(if (eq system-type 'darwin)
    (set-font-size 15)
  (set-font-size 13))
(global-display-line-numbers-mode 1)

;; Disable Eshell's scroll feature
;; from https://emacs.stackexchange.com/questions/28819/eshell-goes-to-the-bottom-of-the-page-after-executing-a-command
(add-hook 'eshell-mode-hook
          (defun chunyang-eshell-mode-setup ()
            (remove-hook 'eshell-output-filter-functions
                         'eshell-postoutput-scroll-to-bottom)))

;; Org-mode settings
(require 'org)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(setq org-todo-keywords '((sequence "TODO" "IN-PROGRESS" "DONE")))


;; recent file mode
(require 'recentf)
(recentf-mode t)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)
(setq recentf-max-saved-items 50)

;; load remainigs sequentially
(require 'packages)
(require 'ocaml)
(require 'py)
(require 'others)
(require 'cpp)
(require 'ts)

(provide 'kernel)
;;; kernel.el ends here
