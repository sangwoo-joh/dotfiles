;; rust formatting: need to install `cargo install rustfmt` and add path `export PATH="$PATH:$HOME/.cargo/bin"
(setq rust-format-on-save t)

(use-package rust-mode :ensure t)

;; Need to install racer: cargo install racer (nightly only)
(use-package racer
  :ensure t
  :init
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode)
  (add-hook 'racer-mode-hook #'company-mode))
