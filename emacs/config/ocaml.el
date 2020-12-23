;; OCaml settings
;; Before settings this, you need to install following packages via opam:
;; > opam install tuareg merlin ocp-indent ocamlformat

(defun string-of-command (command)
  "executes shell command and returns trimmed string"
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
  "check whether there is opam or not"
  (string-of-command "which opam"))

(defun opam/switch ()
  "auxiliary function to check the current switch name"
  (string-of-command "opam switch show --safe --short"))

(defun opam/share-site-lisp-path ()
  "get the alsolute path of opam/share"
  (let ((opam/share (string-of-command "opam config var share --safe")))
    (concat opam/share "/emacs/site-lisp")))

;; cache
(defvar current-lisp-path nil)
(defvar current-switch nil)

(defun opam/env-update ()
  "update opam environment with current switch"
  (let* ((env (string-of-command "opam config env --safe --sexp")))
    (dolist (var (car (read-from-string env)))
      (message "%s -> %s" (car var) (cadr var))
      (setenv (car var) (cadr var))
      (when (string-equal (car var) "PATH")
        (message "update PATH as %s" (cadr var))
        (setq exec-path (split-string (cadr var) path-separator))))))


(defun opam/load-site-lisp (site-lisp-path)
  "add opam/share/emacs/site-lisp to load-path. this will load ocp-indent and ."

  (add-to-list 'load-path site-lisp-path))

(defun ocaml/setup-tuareg (site-lisp-path)
  "setup tuareg"
  (load (concat site-lisp-path "/tuareg-site-file"))
  (require 'tuareg)
  (add-to-list 'auto-mode-alist '("\\.ml[iylp]?\\'" . tuareg-mode))
  (setq tuareg-highlight-all-operartors t)
  (setq tuareg-prettify-symbols-full t)
  (setq tuareg-match-patterns-aligned t)
  (when (functionp 'prettify-symbols-mode) ;; works for >= emacs 24.4
    (add-hook 'tuareg-mode-hook #'prettify-symbols-mode)))

(defun ocaml/setup-merlin (site-lisp-path)
  "setup merlin"
  (require 'merlin)
  (add-hook 'tuareg-mode-hook #'merlin-mode t)
  (with-eval-after-load 'company
    (add-to-list 'company-backends #'merlin-company-backend))
  (add-hook 'merlin-mode-hook #'company-mode)
  (define-key tuareg-mode-map (kbd "M-." #'merlin-locate))
  (define-key tuareg-mode-map (kdb "M.," #'merlin-pop-stack)))

(defun ocaml/setup-ocp-indent ()
  "setup ocp-indent"
  (require 'ocp-indent))

(defun ocaml/setup-ocamlformat (site-lisp-path)
  "setup ocamlformat"
  (load (concat site-lisp-path "/ocamlformat"))
  (define-key tuareg-mode-map (kbd "C-c C-f") #'ocamlformat))

(defun ocaml/setup (site-lisp-path)
  "setup all"
  (opam/env-update) ;; update env vars
  (opam/load-site-lisp current-lisp-path)
  (ocaml/setup-tuareg current-lisp-path)
  (ocaml/setup-merlin current-lisp-path)
  (ocaml/setup-ocn-indent)
  (ocaml/setup-ocamlformat current-lisp-path))

(defun ocaml/auto-setup ()
  "setup all ocaml settings"
  (interactive)
  (unless (opam/check) (error "opam is not installed!"))
  (let* ((switch (opam/switch))
          (site-lisp-path (opam/share-site-lisp-path)))

    (if current-switch
      ;; re-init (current-* are not nil /\ current-switch != switch)
      (unless (string-equal current-switch switch)
        (message "Reload switch: %s -> %s" current-switch switch)
        (delete current-lisp-path load-path) ;; unset previous load
        (setq current-lisp-path site-lisp-path)
        (setq current-switch switch)
        (ocaml/setup current-lisp-path))
      ;; first init (current-* are all nil)
      (progn
        (setq current-lisp-path site-lisp-path)
        (setq current-switch switch)
        (message "Current switch: %s" current-switch)
        (ocaml/setup current-lisp-path)))))

;; init
(ocaml/auto-setup)
