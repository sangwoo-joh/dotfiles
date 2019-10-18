;; OPAM packages: ocp-indent & merlin & tuareg & ocamlformat
;; NOTE: they share a load path.
;; NOTE2: opam package cannot be set up by use-package...
(setq opam-share
      (substring
       (shell-command-to-string "opam config var share 2> /dev/null") 0 -1))
(add-to-list 'load-path (concat opam-share "/emacs/site-lisp"))

;; OCaml-related emacs packages are installed from OPAM ...
(require 'ocp-indent)

;; ocamlformat setting
(load (concat opam-share "/emacs/site-lisp/ocamlformat"))

(require 'merlin)
(setq merlin-command 'opam)
;; merlin can use company-mode for TabNine autocompletion
(with-eval-after-load 'company
  (add-to-list 'company-backends 'merlin-company-backend))
(add-hook 'merlin-mode-hook 'company-mode)

;; merlin can use auto-complete
(use-package auto-complete
  :ensure t
  :init (setq merlin-ac-setup 'easy)
  :bind (("C-c <tab>" . auto-complete)))

;; Tuareg mode is augmented mode for caml-mode
(load (concat opam-share "/emacs/site-lisp/tuareg-site-file"))
(require 'tuareg)
(add-to-list 'auto-mode-alist '("\\.mly$" . tuareg-mode))
(add-to-list 'auto-mode-alist '("\\.mll" . tuareg-mode))
(add-hook 'tuareg-mode-hook 'merlin-mode t)
(add-hook 'tuareg-mode-hook 'rainbow-delimiters-mode t)
(setq tuareg-highlight-all-operators t)
(setq tuareg-prettify-symbols-full t)
(setq tuareg-match-patterns-aligned t)
(add-hook 'tuareg-mode-hook (lambda ()
                              (when (functionp 'prettify-symbols-mode)
                                (prettify-symbols-mode))))
(add-hook 'tuareg-mode-hook
  (lambda ()
    (define-key tuareg-mode-map (kbd "C-c C-f") #'ocamlformat)
    (define-key tuareg-mode-map (kbd "M-.") #'merlin-locate)
    (define-key tuareg-mode-map (kbd "M-,") #'merlin-pop-stack)
    (define-key tuareg-mode-map (kbd "C-c C-o") #'merlin-occurrences)
    (add-hook 'before-save-hook #'ocamlformat-before-save)))

(add-hook 'caml-mode-hook 'merlin-mode t)
(add-hook 'caml-mode-hook 'rainbow-delimiters-mode t)
