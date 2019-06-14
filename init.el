;; UTF-8 everywhere
;; (setq locale-coding-system 'utf-8)
;; (set-terminal-coding-system 'utf-8)
;; (set-keyboard-coding-system 'utf-8)
;; (set-selection-coding-system 'utf-8)
;; (prefer-coding-system 'utf-8)

(add-to-list 'load-path "~/.emacs.d/auto-load/")
(require 'redo+)
;; package business
(require 'package)
(load-library "url-handlers")

(add-to-list
 'package-archives
 '("melpa" . "http://melpa.org/packages/"))

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package diminish
  :ensure t)

(if (version< emacs-version "26.0")
    (message "is before 26.0 - skipping company-childframe")
  (use-package company-posframe
    :ensure t
    :diminish
    :config
    (company-posframe-mode 1)))

(use-package company :ensure t :config
  (setq company-dabbrev-downcase nil)
  (setq-default company-lighter-base "(C)")
  (setq-default company-show-numbers          1)
  (setq-default company-idle-delay            0) ; start completion immediately
  (setq-default company-minimum-prefix-length 1) ; start completion after 1 character.
  (setq-default company-tooltip-align-annotations t))

(use-package magit :ensure t)
(setenv "GIT_ASKPASS" "git-gui--askpass")
(setenv "SSH_ASKPASS" "git-gui--askpass")

(defadvice magit-start-process (after spam-echo-area activate)
  (set-process-filter
   magit-this-process
   `(lambda (process string)
      (,(process-filter magit-this-process) process string)
      (message "git> %s" string))))

(use-package ivy
  :ensure t
  :diminish
  :config
  (ivy-mode 1)
  (setq ivy-on-del-error-function #'ignore))

(use-package swiper
  :ensure t)

(use-package multiple-cursors
  :ensure t)

(defun mark-word-or-next-word-like-this ()
  "if there is no active region the word under
   the point will be marked, otherwise the next word is selected."
  (interactive)
  (if (region-active-p)
      ;; then
      (progn
        (mc/mark-more-like-this nil 'forwards)
        (mc/maybe-multiple-cursors-mode)
        (mc/cycle-forward))
    ;; else
    (mc--select-thing-at-point 'word)))

(global-set-key (kbd "C-d") 'mark-word-or-next-word-like-this)
(global-set-key (kbd "C-M-<down>") 'mc/edit-lines)

(defun duplicate-line()
  (interactive)
  (save-excursion
    (move-beginning-of-line 1)
    (set-mark (point-marker))
    (move-end-of-line 1)
    (copy-region-as-kill (region-beginning) (region-end))
    (open-line 1)
    (next-line 1)
    (yank)))

(global-set-key (kbd "S-M-<down>") 'duplicate-line)

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if (executable-find "python3") 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-follow-delay             0.2
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-desc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-width                         35)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null (executable-find "python3"))))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

;(use-package powerline :ensure t)
;(powerline-default-theme)

(desktop-save-mode 1)

(use-package solarized-theme
  :ensure t
  :config
  (load-theme 'solarized-dark t)
  (set-face-attribute 'font-lock-comment-face nil :italic t)
  (set-face-attribute 'font-lock-doc-face nil :italic t))

(load-file "~/.emacs.d/tabbar.el")
(load-file "~/.emacs.d/org+calendar.el")
(load-file "~/.emacs.d/js.el")
(load-file "~/.emacs.d/py.el")

(use-package rainbow-mode
  :diminish
  :ensure t
  :hook (prog-mode . rainbow-mode))

(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode)
  (diff-hl-flydiff-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))


(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode))

(use-package quickrun
  :ensure t)

(global-set-key (kbd "<f5>") 'quickrun-shell)

;; parenthesis stuff
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(add-hook 'prog-mode-hook
          (lambda ()
            ;(ignore-errors
            ;  (flyspell-prog-mode))
            (company-mode 1)
            (show-paren-mode 1)
            (linum-mode 1)))


;; Don't litter the direcory with backup files but keep them in another folder
(setq backup-directory-alist `(("." . "~/.emacs-saves")))
(setq auto-save-list-file-prefix nil)
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t
      create-lockfiles nil)

;; Srcolling
(setq mouse-wheel-scroll-amount '(3 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil)            ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't)                  ;; scroll window under mouse
(setq scroll-step 1)                                ;; keyboard scroll one line at a time
(setq scroll-conservatively 101)
(setq fast-but-imprecise-scrolling t)
(set-window-scroll-bars (minibuffer-window) nil nil)

;; Garbage collection
(setq gc-cons-threshold (eval-when-compile (* 1024 1024 1024)))
(run-with-idle-timer 2 t (lambda () (garbage-collect)))


;; Save hooks
(add-hook 'focus-out-hook          (lambda () (when (and buffer-file-name (buffer-modified-p)) (save-buffer))))
(add-hook 'mouse-leave-buffer-hook (lambda () (when (and buffer-file-name (buffer-modified-p)) (save-buffer))))

(defadvice switch-to-buffer (before save-buffer-now activate)
  (when (and buffer-file-name (buffer-modified-p)) (save-buffer)))
(defadvice other-window (before other-window-now activate)
  (when (and buffer-file-name (buffer-modified-p)) (save-buffer)))

(defun delete-trailing-whitespace-except-current-line ()
  (interactive)
  (let ((begin (line-beginning-position))
        (end (line-end-position)))
    (save-excursion
      (when (< (point-min) begin)
        (save-restriction
          (narrow-to-region (point-min) (1- begin))
          (delete-trailing-whitespace)))
      (when (> (point-max) end)
        (save-restriction
          (narrow-to-region (1+ end) (point-max))
          (delete-trailing-whitespace))))))
(add-hook 'before-save-hook 'delete-trailing-whitespace-except-current-line)
(defun untabify-except-makefiles ()
  "Replace tabs with spaces except in makefiles."
  (unless (derived-mode-p 'makefile-mode)
    (untabify (point-min) (point-max))))

(add-hook 'before-save-hook 'untabify-except-makefiles)

;; Misc
(global-auto-revert-mode t) ;; always show the latest version of every file
(setq visible-bell nil ring-bell-function #'ignore) ;; Never ring the bell
(defalias 'yes-or-no-p 'y-or-n-p)
(delete-selection-mode 1) ;; Why is this not standard??
(cua-mode 1)
(setq-default cursor-type 'bar)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(auto-save-mode -1)
(tool-bar-mode -1)
(setq frame-title-format '("" "Emacs - %b"))
(set-default 'truncate-lines t)

;; Disable recursive edits when cursor leaves
(defun stop-using-minibuffer ()
  "kill the minibuffer"
  (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
    (abort-recursive-edit)))
;; (add-hook 'mouse-leave-buffer-hook 'stop-using-minibuffer)

;; Additional shortcuts
(defun org-agenda-show-agenda-and-todo (&optional arg)
  (interactive "P")
  (org-agenda arg "n" "buffer"))

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-a") 'mark-whole-buffer)
  (define-key org-mode-map (kbd "C-y") 'redo)
  (define-key org-mode-map (kbd "C-d") 'org-deadline)
  (define-key org-mode-map (kbd "<f2>") 'org-agenda-show-agenda-and-todo))

(with-eval-after-load 'with-editor
  (define-key with-editor-mode-map (kbd "C-s") 'with-editor-finish))

(with-eval-after-load 'org-agenda
  (define-key org-agenda-mode-map (kbd "<mouse-1>") 'org-agenda-goto-mouse))

(with-eval-after-load 'flyspell
  (setq flyspell-prog-text-faces '(font-lock-doc-face))
  (define-key flyspell-mouse-map (kbd "<mouse-3>") 'flyspell-correct-word))

(defadvice find-file-read-args (around find-file-read-args-always-use-dialog-box act)
  "Simulate invoking menu item as if by the mouse; see `use-dialog-box'."
  (let ((last-nonmenu-event nil))
     ad-do-it))
(global-set-key (kbd "C-o") 'find-file)

(global-set-key (kbd "C-f") 'swiper)

(global-set-key (kbd "<S-down-mouse-1>") 'mouse-save-then-kill) ;; Extend selectin by shift clicking
(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "C-S-s") 'save-some-buffers)
(global-set-key (kbd "C-a") 'mark-whole-buffer)
(global-set-key (kbd "C-y") 'redo)
(global-set-key (kbd "C-w") (lambda() (interactive) (kill-buffer)))
(global-set-key (kbd "C-q") 'delete-window)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)
(global-set-key (kbd "C-S-g") 'abort-recursive-edit)

(global-set-key (kbd "<f1>") (lambda() (interactive)(find-file-other-frame "~/.emacs.d/help.org")))
(global-set-key (kbd "<f3>") 'magit-status)


(define-key key-translation-map (kbd "ESC") (kbd "C-g"))
(global-unset-key (kbd "ESC ESC ESC"))
(global-unset-key (kbd "<f2> <f2>"))
(global-unset-key (kbd "<mouse-3>"))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Cousine" :foundry "outline" :slant normal :weight normal :height 113 :width normal))))
 '(font-lock-doc-face ((t (:foreground "#d33682" :slant italic))))
 '(org-level-1 ((t (:inherit outline-1 :height 1.8))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.5))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.3)))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(elpy-syntax-check-command "flake8 --ignore=E301,E302,E226,E231")
 '(package-selected-packages
   (quote
    (multiple-cursors treemacs swiper popup-imenu quickrun elpy minimap diminish solarized-theme reykjavik-theme rainbow-delimiters tide company atom-one-dark-theme org-bullets diff-hl tabbar powerline ivy magit use-package))))





(defvar emax-root (concat (expand-file-name "~") "/emax"))
(defvar emax-bin (concat emax-root "/bin"))
(defvar emax-bin64 (concat emax-root "/bin64"))
(defvar emax-mingw64 (concat emax-root "/mingw64/bin"))
(defvar emax-lisp (concat emax-root "/lisp"))

;; Changes made for Aspell
(setq-default ispell-program-name "~/emax/mingw64/bin/aspell.exe")
(setq-default ispell-extra-args  '("--sug-mode=ultra"))
;; (setq ispell-dictionary "en_US")

;; Set "DICTDIR" variable
(setenv "DICTDIR" (concat emax-mingw64 "/lib/aspell-0.60/"))


;; Automatically enable flyspell-mode in text-mode
;;(require 'flyspell)
(add-hook 'text-mode-hook 'flyspell-mode)
;(setq text-mode-hook (lambda () (flyspell-mode t)))

(add-hook 'flyspell-mode-hook      'flyspell-buffer)
(add-hook 'flyspell-prog-mode-hook 'flyspell-buffer)
;;(setq text-mode-hook '(lambda()
;;                        (flyspell-mode t)))

;;(dolist (hook '(text-mode-hook))
;;  (add-hook hook (lambda () (flyspell-mode 1))))
;;(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
;;  (add-hook hook (lambda () (flyspell-mode -1))))

;;(setq flyspell-issue-message-flag nil)

;;(require 'auto-dictionary)
;;(add-hook 'flyspell-mode-hook (lambda () (auto-dictionary-mode 1)))


(require 'ispell)
