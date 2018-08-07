;; OPAM packages: ocp-indent & merlin & tuareg
;; NOTE: they share a load path.
;; NOTE2: opam package cannot be set up by use-package...
(setq opam-share
      (substring
       (shell-command-to-string "opam config var share 2> /dev/null") 0 -1))
(add-to-list 'load-path (concat opam-share "/emacs/site-lisp"))
(require 'ocp-indent)
(require 'merlin)
(setq merlin-use-auto-complete-mode 'easy)
(setq merlin-command 'opam)

(require 'tuareg)
(add-hook 'tuareg-mode-hook 'merlin-mode t)
(add-hook 'tuareg-mode-hook 'rainbow-delimiters-mode t)

(add-hook 'caml-mode-hook 'merlin-mode t)
(add-hook 'caml-mode-hook 'rainbow-delimiters-mode t)
(load (concat opam-share "/emacs/site-lisp/tuareg-site-file"))
;; ocamlformat setting
(load (concat opam-share "/emacs/site-lisp/ocamlformat"))
(add-hook 'before-save-hook 'ocamlformat-before-save)
(use-package auto-complete
  :ensure t
  :bind
  (("C-c <tab>" . ac-complete-merlin)))

