(use-package tabbar :ensure t :config
 ;; (set-face-attribute
 ;;   'tabbar-unselected nil
 ;;   :background "#282C34"
 ;;   :foreground "gray30"
 ;;   :box nil)
 ;;  (set-face-attribute
 ;;   'tabbar-selected nil
 ;;   :background "#282C34"
 ;;   :foreground "white"
 ;;   :box nil)
 ;;   (set-face-attribute
 ;;    'tabbar-button nil
 ;;    :box '(:line-width 1 :color "#282C34" :style released-button :underline
 ;;                       nil))
 ;;   (set-face-attribute
 ;;    'tabbar-default nil
 ;;    :background "#21252B")
 ;;   (set-face-attribute
 ;;    'tabbar-highlight nil
 ;;    :foreground "white"
 ;;    :box nil
 ;;    :underline nil)
 ;;   (set-face-attribute
 ;;    'tabbar-separator nil
 ;;    :height 0.7)
 ;;   (set-face-attribute
 ;;    'tabbar-modified nil
 ;;    :inherit 'tabbar-default :foreground "green" :box nil)
 ;;   (set-face-attribute
 ;;    'tabbar-selected-modified nil
 ;;    :inherit 'tabbar-default :foreground "green" :box nil :background "#282C34")

   (setq
    tabbar-scroll-left-help-function nil ;don't show help information
    tabbar-scroll-right-help-function nil
    tabbar-help-on-tab-function nil
    tabbar-home-help-function nil
    tabbar-buffer-home-button (quote (("") "")) ;don't show tabbar button
    tabbar-scroll-left-button (quote (("") ""))
    tabbar-scroll-right-button (quote (("") "")))

   (tabbar-mode)
   (global-set-key [C-tab] 'tabbar-forward-tab)
   (global-set-key [C-S-tab] 'tabbar-backward-tab)

   (defun tabbar-buffer-groups ()
     "Returns the list of group names the current buffer belongs to."
     (list
      (cond
       ;; ADD RULES TO SPLIT BUFFERS IN GROUPS HERE!
       ((member (buffer-name)
                '("*scratch*"     "*Messages*"
                  "*Compile-Log*" "*Flymake log*"
                  "*Help*" "*eshell-quickrun*" "*Backtrace*" "diary"))
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
            (concat " " (concat ad-return-value " "))
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
