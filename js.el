(use-package flycheck
  :ensure t)
(use-package flycheck-popup-tip
  :ensure t)

(add-hook 'flycheck-mode-hook 'flycheck-popup-tip-mode)

(use-package tide
  :ensure t
  :config
  (setq tide-allow-popup-select (quote (code-fix jump-to-implementation refactor)))
  (setq tide-completion-detailed t)
  (setq tide-default-mode "JSX"))

(use-package emmet-mode
  :ensure t)

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1)

  ;; aligns annotation to the right hand side
  (setq company-tooltip-align-annotations t)

  ;; formats the buffer before saving
  ;; (add-hook 'before-save-hook 'tide-format-before-save)
  )

(add-hook 'rjsx-mode-hook #'setup-tide-mode)
(add-hook 'rjsx-mode-hook #'emmet-mode)

(use-package rjsx-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '(".*\\.js\\'" . rjsx-mode))
  ;; Use space instead of tab
  (setq indent-tabs-mode nil)
  ;; disable the semicolon warning
  (setq js2-strict-missing-semi-warning nil))
