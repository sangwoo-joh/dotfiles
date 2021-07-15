;;; ocaml --- OCaml settings
;;; Commentary:
;;; Before settings this, you need to install following packages via opam:
;;; > opam install tuareg merlin ocp-indent ocamlformat

;;; Code:

(defun string-of-command (command)
  "EXECUTE SHELL COMMAND AND RETURN TRIMMED STRING."
  (let* ((exit-code 0)
          (stdout
            (string-trim
              (with-output-to-string
                (setq exit-code
                  (with-current-buffer standard-output
                    (process-file shell-file-name nil t nil shell-command-switch command)))))))
    (if (= exit-code 0)
      stdout
      (progn
        (lwarn 'sw/ocaml-setting :warning "Command(%s) failure: %d" command exit-code)
        nil))))

(defun opam/check ()
  "CHECK WHETHER THERE IS OPAM OR NOT."
  (string-of-command "which opam"))

(defun opam/switch (switch)
  "CHANGE TO SWITCH."
  (interactive)
  (string-of-command (format "opam switch %s" switch)))

(defun opam/current-switch ()
  "AUXILIARY FUNCTION TO CHECK THE CURRENT SWITCH NAME."
  (interactive)
  (string-of-command "opam switch show --safe --short"))

(defun opam/switch-list ()
  "GET ALL AVAILABLE LIST OF OPAM SWITCH."
  (interactive)
  (split-string (string-of-command "opam switch list --safe --short")))

(defun opam/share-site-lisp-path ()
  "GET THE ALSOLUTE PATH OF OPAM/SHARE."
  (let ((opam/share (string-of-command "opam config var share --safe")))
    (concat opam/share "/emacs/site-lisp/")))

;; cache
(defvar current-lisp-path nil)
(defvar current-switch nil)

(defun opam/env-update ()
  "UPDATE OPAM ENVIRONMENT WITH CURRENT SWITCH."
  (interactive)
  (let* ((env (string-of-command "opam config env --safe --sexp")))
    (dolist (var (car (read-from-string env)))
      (setenv (car var) (cadr var))
      (when (string-equal (car var) "PATH")
        (setq exec-path (split-string (cadr var) path-separator))))
    (message "Update opam env path for %s" env)))


(defun opam/load-site-lisp (site-lisp-path)
  "ADD SITE-LISP-PATH to LOAD-PATH.  THIS WILL LOAD OCP-INDENT AND."
  (add-to-list 'load-path site-lisp-path))

(defun ocaml/load-tuareg (site-lisp-path)
  "LOAD TUAREG WITH SITE-LISP-PATH."
  (load (concat site-lisp-path "tuareg-site-file"))
  (require 'tuareg)
  (add-to-list 'auto-mode-alist '("\\.ml[ip]?\\'" . tuareg-mode))
  (add-to-list 'auto-mode-alist '("\\.ml[ly]\\'" . tuareg-menhir-mode))
  (setq tuareg-highlight-all-operartors t)
  (setq tuareg-prettify-symbols-full t)
  (setq tuareg-match-patterns-aligned t)
  (when (functionp 'prettify-symbols-mode) ;; works for >= emacs 24.4
    (add-hook 'tuareg-mode-hook #'prettify-symbols-mode)))

(defun ocaml/load-merlin (site-lisp-path)
  "LOAD MERLIN WITH SITE-LISP-PATH."
  (require 'merlin)
  (add-hook 'tuareg-mode-hook #'merlin-mode t)
  (with-eval-after-load 'company
    (add-to-list 'company-backends #'merlin-company-backend))
  (add-hook 'merlin-mode-hook #'company-mode)
  (define-key tuareg-mode-map (kbd "M-.") #'merlin-locate)
  (define-key tuareg-mode-map (kbd "M-,") #'merlin-pop-stack))

(defun ocaml/load-ocp-indent ()
  "LOAD OCP-INDENT."
  (require 'ocp-indent))

(defun ocaml/load-ocamlformat (site-lisp-path)
  "LOAD OCAMLFORMAT WITH SITE-LISP-PATH."
  (load (concat site-lisp-path "ocamlformat"))
  (define-key tuareg-mode-map (kbd "C-c C-f") #'ocamlformat))

(defun ocaml/load-dune ()
  "LOAD DUNE-MODE."
  (require 'dune))

(defun ocaml/load (site-lisp-path)
  "LOAD ALL WITH SITE-LISP-PATH."
  (opam/env-update) ;; update env vars
  (opam/load-site-lisp site-lisp-path)
  (ocaml/load-tuareg site-lisp-path)
  (ocaml/load-merlin site-lisp-path)
  (ocaml/load-ocp-indent)
  (ocaml/load-ocamlformat site-lisp-path)
  (ocaml/load-dune))

(defun ocaml/auto-load ()
  "LOAD ALL OCAML SETTINGS DYNAMICALLY."
  (interactive)
  (unless (opam/check) (error "Opam is not installed!"))
  (let* ((switch (opam/current-switch))
          (site-lisp-path (opam/share-site-lisp-path)))

    (if current-switch
      ;; re-init (current-* are not nil /\ current-switch != switch)
      (unless (string-equal current-switch switch)
        (setq load-path (delete current-lisp-path load-path)) ;; unset previous load
        (setq current-lisp-path site-lisp-path)
        (setq current-switch switch)
        (ocaml/load current-lisp-path)
	(message "Reload switch: %s -> %s" current-switch switch))
      ;; first init (current-* are all nil)
      (progn
        (setq current-lisp-path site-lisp-path)
        (setq current-switch switch)
        (ocaml/load current-lisp-path)
        (message "Current switch: %s" current-switch)))))

;; init
(ocaml/auto-load)

(defun opam/change-switch (switch)
  "UPDATE OPAM SWITCH."
  (interactive)
  (let* ((switch-available (opam/switch-list)))
    (if (member switch switch-available)
	(progn
	  (opam/switch switch)
	  (ocaml/auto-load))
      (progn
	(message "Invalid switch: %s" switch)))))

(provide 'ocaml)
;;; ocaml.el ends here
