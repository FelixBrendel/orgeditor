(use-package elpy
  :ensure t
  :defer t
  :diminish
  :config
  (setq
   elpy-modules
   '(elpy-module-company
     elpy-module-eldoc
     elpy-module-flymake
     elpy-module-pyvenv
     ; elpy-module-yasnippet
     ; elpy-module-django
     elpy-module-autodoc
     elpy-module-sane-defaults))
  :init
  (advice-add 'python-mode :before 'elpy-enable))
