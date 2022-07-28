;;; others.el --- Other languages settings.
;;; Commentary:
;;; Code:
(use-package yaml-mode :ensure t)


(defun get-leetcode-title ()
  "GET LEETCODE TITLE IN INDEX PAGE"
  (interactive)
  (save-excursion
    (goto-char (line-end-position))
    (let* ((start (search-backward "["))
           (end (search-forward "]")))
      (buffer-substring-no-properties (+ start 1) (- end 1)))))

(defun normalize-title-as-link-text (title)
  "NORMALIZE TITLE AS LINK TEXT"
  (interactive)
  (downcase
   (replace-regexp-in-string "[^a-zA-Z0-9]" "-"
                             (replace-regexp-in-string "[,'.()]" "" title))))

(defun leetcode-problem-link (problem)
  "CREATE LEETCODE PROBLEM LINK"
  (interactive)
  (format "https://leetcode.com/problems/%s/" problem))

(defun markdown-title-with-leetcode-link (title link)
  "CREATE MARKDOWN H1 TITLE WITH LEETCODE LINK"
  (interactive)
  (let* ((url (leetcode-problem-link link)))
    (format "# [%s](%s)" title url)))

(defun ready-leetcode-markdown (title link)
  "1. COPY .template.md AS link-text.md, 2. SET TITLE AND LINK. THIS MUST BE CALLED"
  (interactive)
  (let* ((mdfile (format "%s.md" link))
         (title-text (markdown-title-with-leetcode-link title link)))
    (if (file-exists-p mdfile)
        ;; if exists, just open it
        (find-file mdfile)
      ;; otherwise, create file from template and set titles
      (copy-file ".template.md" mdfile)
      (find-file mdfile)
      (goto-char 1)
      (re-search-forward "^title:")
      (insert " ")
      (insert title)
      (goto-char (point-max))
      (insert title-text))))

(defun ready-to-leetcode (&optional remain )
  "GET READY TO LEETCODE!"
  (interactive)
  (save-excursion
    (goto-char (line-end-position))
    (let* ((title (get-leetcode-title))
           (link (normalize-title-as-link-text title))
           (line-text
            (buffer-substring-no-properties
             (line-beginning-position)
             (line-end-position))))
      (if (string-match-p link line-text)
          (progn
            (message "link exists")
            (string-match "(.*)" line-text)
            (let* ((matched
                    (substring line-text
                               (+ (match-beginning 0) 1)
                               (- (match-end 0) 1)))
                   (matched
                    (substring matched
                               0
                               (string-match "#" matched))))
              ;; sanitize existing link text
              (setq link matched)))
        (insert (format "(%s)" link)))
      (unless remain
        (ready-leetcode-markdown title link)))))

(defun fill-link ()
  "JUST FILL LINK"
  (interactive)
  (ready-to-leetcode t))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md$" . gfm-mode)
	       ("\\.md$" . markdown-mode)
	       ("\\.markdown$" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  :bind
  ("C-c C-t C-f" . markdown-table-align)
  ("C-c C-c C-r" . fill-link)
  ("C-c C-c C-f" . ready-to-leetcode)
  )

(defun unify-web-mode-spacing ()
  "Stole from https://github.com/trev-dev/emacs"
  (setq web-mode-markup-indent-offset tab-width)
  (setq web-mode-css-indent-offset tab-width)
  (setq web-mode-code-indent-offset tab-width)
  (setq web-mode-style-padding tab-width)
  (setq web-mode-script-padding tab-width)
  (setq web-mode-indent-style 2))

(use-package web-mode
  :ensure t
  :hook (web-mode . unify-web-mode-spacing)
  :mode
  ("\\.php$" . web-mode)
  ("\\.html$" . web-mode))

(use-package auctex
  :defer t
  :ensure t
  :config
  (setq TeX-PDF-mode t))

(use-package company-auctex
  :ensure t
  :init
  (company-auctex-init))

(use-package graphviz-dot-mode
  :ensure t
  :init
  (setq graphviz-dot-indent-width 2))

(use-package dockerfile-mode :ensure t)

;; use as //docker:<container-id>
;; for remote: /sshx:<remote>|docker:<container-name>:<path> is fantastic. MAGIC CHARM.
(use-package docker-tramp :ensure t)

(use-package json-mode :ensure t)

(use-package rust-mode :ensure t)

;; flycheck emacs
(use-package flycheck-cask
  :ensure t
  :config
  (eval-after-load 'flycheck
    '(add-hook 'flycheck-mode-hook #'flycheck-cask-setup)))

(provide 'others)
;;; others.el ends here
