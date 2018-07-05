;; UTF-8 everywhere
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

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

(use-package magit :ensure t)
(use-package ivy :ensure t :config (ivy-mode 1))
(use-package powerline :ensure t)
(powerline-default-theme)

(use-package tabbar :ensure t :config
  ;; (set-face-attribute
  ;;  'tabbar-default nil
  ;;  :background "gray60")
 (set-face-attribute
   'tabbar-unselected nil
   :background "#282C34"
   :foreground "gray30"
   :box nil)
  (set-face-attribute
   'tabbar-selected nil
   :background "#282C34"
   :foreground "white"
   :box nil)
   (set-face-attribute
    'tabbar-button nil
    :box '(:line-width 1 :color "#282C34" :style released-button :underline
                       nil))
   (set-face-attribute
    'tabbar-default nil
    :background "#21252B")
   (set-face-attribute
    'tabbar-highlight nil
    :foreground "white"
    :box nil
    :underline nil)
   (set-face-attribute
   'tabbar-separator nil
   :height 0.7)

   (setq
    tabbar-scroll-left-help-function nil ;don't show help information
    tabbar-scroll-right-help-function nil
    tabbar-help-on-tab-function nil
    tabbar-home-help-function nil
    tabbar-buffer-home-button (quote (("") "")) ;don't show tabbar button
    tabbar-scroll-left-button (quote (("") ""))
    tabbar-scroll-right-button (quote (("") "")))

   (tabbar-mode)
   (defun tabbar-buffer-groups ()
     "Returns the list of group names the current buffer belongs to."
     (list
      (cond

       ;; ADD RULES TO SPLIT BUFFERS IN GROUPS HERE!
       ((member (buffer-name)
                '("*scratch*" "*Messages*" "*Help*" "diary"))
        "Special Buffers" ;; this is a group name
        )
       ((string-match "^magit" (buffer-name))
        "Magit Buffers"
        )
       ;; if buffer is not grouped by the rules you would add above
       ;; put it in the "General" group:
       (t
        "General"
        )))))


;; Add a buffer modification state indicator in the tab label, and place a
;; space around the label to make it looks less crowd.
(defadvice tabbar-buffer-tab-label (after fixup_tab_label_space_and_flag activate)
  (setq ad-return-value
        (if (and (buffer-modified-p (tabbar-tab-value tab))
                 (buffer-file-name (tabbar-tab-value tab)))
            (concat " + " (concat ad-return-value " "))
          (concat " " (concat ad-return-value " ")))))
;; Called each time the modification state of the buffer changed.
(defun ztl-modification-state-change ()
  (tabbar-set-template tabbar-current-tabset nil)
  (tabbar-display-update))
;; First-change-hook is called BEFORE the change is made.
(defun ztl-on-buffer-modification ()
  (set-buffer-modified-p t)
  (ztl-modification-state-change))
(add-hook 'after-save-hook 'ztl-modification-state-change)
;; This doesn't work for revert, I don't know.
;;(add-hook 'after-revert-hook 'ztl-modification-state-change)
(add-hook 'first-change-hook 'ztl-on-buffer-modification)

(defvar my/tabbar-left "/" "Separator on left side of tab")
(defvar my/tabbar-right "\\" "Separator on right side of tab")
(defun my/tabbar-tab-label-function (tab)
  (powerline-render (list my/tabbar-left
                          (format " %s  " (car tab))
                          my/tabbar-right)))
(with-eval-after-load 'powerline
  (setq my/tabbar-left  (powerline-wave-right 'tabbar-default nil 24))
  (setq my/tabbar-right (powerline-wave-left nil 'tabbar-default 24))
  (setq tabbar-tab-label-function #'my/tabbar-tab-label-function))

(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode t)
  (diff-hl-flydiff-mode t)
  (setq diff-hl-fringe-bmp-function (lambda (&rest _ignore) 'empty-line))
  ;; Workaround for displaying correctly in other window
  (use-package frame
    :defer t
    :config
    (progn
      (setq window-divider-default-places 'right-only) ;Default 'right-only
      ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=27830#20
      ;; Workaround on emacs 26+ to prevent fringe truncation. You need to use
      ;; either scroll bars or window dividers to prevent that.
      ;; I dislike the default face of `window-divider', so I customize that in my
      ;; `smyx-theme`.
      (setq window-divider-default-right-width 1) ;Default 6
      (window-divider-mode 1))))

(use-package org-bullets :ensure t)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(use-package atom-one-dark-theme :ensure t)
(load-theme 'atom-one-dark t)

;; Don't litter the direcory with backup files but keep them in another folder
(setq backup-directory-alist `(("." . "~/.emacs-saves")))
(setq auto-save-list-file-prefix nil)
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

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

;; Searching
;; auto overwrap i-search
;; Prevents issue where you have to press backspace twice when
;; trying to remove the first character that fails a search
(define-key isearch-mode-map [remap isearch-delete-char] 'isearch-del-char)

(defadvice isearch-search (after isearch-no-fail activate)
  (unless isearch-success
    (ad-disable-advice 'isearch-search 'after 'isearch-no-fail)
    (ad-activate 'isearch-search)
    (isearch-repeat (if isearch-forward 'forward))
    (ad-enable-advice 'isearch-search 'after 'isearch-no-fail)
    (ad-activate 'isearch-search)))


;; search for highlighted if exist
(defun jrh-isearch-with-region ()
  "Use region as the isearch text."
  (when mark-active
    (let ((region (funcall region-extract-function nil)))
      (deactivate-mark)
      (isearch-push-state)
      (isearch-yank-string region))))
(add-hook 'isearch-mode-hook #'jrh-isearch-with-region)

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

;; Text/org editing
(setq-default fill-column 80)
(auto-fill-mode 1)
(setq org-src-fontify-natively t)
(setq org-support-shift-select t)
(setq org-todo-keyword-faces
      '(("TODO[All]"    . "LightSalmon")
        ("TODO[Felix]"  . "CadetBlue")
        ("TODO[Jonas]"  . "pink3")
        ("TODO[Marcus]" . "MediumSeaGreen")))

;; fix flyspell add word to dict
(defun flyspell-buffer-after-pdict-save (&rest _)
  (flyspell-buffer))

(setq flyspell-issue-message-flag nil)
(advice-add 'ispell-pdict-save :after #'flyspell-buffer-after-pdict-save)

;; Agenda & Calendar
(add-hook 'calendar-load-hook
          (lambda ()
            (calendar-set-date-style 'european)))

(setq calendar-week-start-day 1
          calendar-day-name-array ["Sonntag" "Montag" "Dienstag" "Mittwoch"
                                   "Donnerstag" "Freitag" "Samstag"]
          calendar-month-name-array ["Januar" "Februar" "März" "April" "Mai"
                                     "Juni" "Juli" "August" "September"
                                     "Oktober" "November" "Dezember"])
(setq solar-n-hemi-seasons
      '("Frühlingsanfang" "Sommeranfang" "Herbstanfang" "Winteranfang"))

(setq holiday-general-holidays
      '((holiday-fixed 1 1 "Neujahr")
        (holiday-fixed 5 1 "1. Mai")
        (holiday-fixed 10 3 "Tag der Deutschen Einheit")))

;; Feiertage für Bayern, weitere auskommentiert
(setq holiday-christian-holidays
      '((holiday-float 12 0 -4 "1. Advent" 24)
        (holiday-float 12 0 -3 "2. Advent" 24)
        (holiday-float 12 0 -2 "3. Advent" 24)
        (holiday-float 12 0 -1 "4. Advent" 24)
        (holiday-fixed 12 25 "1. Weihnachtstag")
        (holiday-fixed 12 26 "2. Weihnachtstag")
        (holiday-fixed 1 6 "Heilige Drei Könige")
        (holiday-easter-etc -48 "Rosenmontag")
        (holiday-easter-etc -3 "Gründonnerstag")
        (holiday-easter-etc  -2 "Karfreitag")
        (holiday-easter-etc   0 "Ostersonntag")
        (holiday-easter-etc  +1 "Ostermontag")
        (holiday-easter-etc +39 "Christi Himmelfahrt")
        (holiday-easter-etc +49 "Pfingstsonntag")
        (holiday-easter-etc +50 "Pfingstmontag")
        (holiday-easter-etc +60 "Fronleichnam")
        (holiday-fixed 8 15 "Mariae Himmelfahrt")
        (holiday-fixed 11 1 "Allerheiligen")
        (holiday-float 11 3 1 "Buss- und Bettag" 16)
        (holiday-float 11 0 1 "Totensonntag" 20)))

(setq holiday-bahai-holidays nil)
(setq holiday-hebrew-holidays nil)
(setq holiday-islamic-holidays nil)
(setq org-agenda-include-diary t)


;; Disable recursive edits when cursor leaves
(defun stop-using-minibuffer ()
  "kill the minibuffer"
  (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
    (abort-recursive-edit)))
(add-hook 'mouse-leave-buffer-hook 'stop-using-minibuffer)


;; Additional shortcuts
(defun org-agenda-show-agenda-and-todo (&optional arg)
  (interactive "P")
  (org-agenda arg "n" "buffer"))

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-a") 'mark-whole-buffer)
  (define-key org-mode-map (kbd "C-y") 'redo)
  (define-key org-mode-map (kbd "<f2>") 'org-agenda-show-agenda-and-todo))

(with-eval-after-load 'org-agenda
  (define-key org-agenda-mode-map (kbd "<mouse-1>") 'org-agenda-goto-mouse))

(with-eval-after-load 'flyspell
  (define-key flyspell-mouse-map (kbd "<mouse-3>") 'flyspell-correct-word))

(defadvice find-file-read-args (around find-file-read-args-always-use-dialog-box act)
  "Simulate invoking menu item as if by the mouse; see `use-dialog-box'."
  (let ((last-nonmenu-event nil))
     ad-do-it))
(global-set-key (kbd "C-o") 'find-file)

(global-set-key (kbd "C-f") 'isearch-forward)
(define-key isearch-mode-map "\C-f" 'isearch-repeat-forward)

(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "C-S-s") 'save-some-buffers)
(global-set-key (kbd "C-a") 'mark-whole-buffer)
(global-set-key (kbd "C-y") 'redo)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)
(global-set-key (kbd "C-S-g") 'abort-recursive-edit)

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
 '(org-level-1 ((t (:inherit outline-1 :height 1.8))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.5))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.3))))
 '(tabbar-modified ((t (:inherit tabbar-default :foreground "green" :box nil))))
 '(tabbar-selected-modified ((t (:inherit tabbar-default :foreground "green")))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (tabbar ivy magit atom-one-dark-theme use-package org-bullets diff-hl)))
 '(tabbar-mode t nil (tabbar))
 '(tabbar-mwheel-mode t nil (tabbar)))
