;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq package-enable-at-startup nil)
(package-initialize)
(setq package-archives '(("myelpa" . "~/myelpa/")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; History
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq global-auto-revert-non-file-buffers t) ; Revert dired & other buffers.
(global-auto-revert-mode 1)                  ; Revert buffer after file changed in external system.

(setq history-length 10) ; Minibuffer history length.
(save-place-mode 1)      ; Last place visited in file.
(savehist-mode   1)      ; Last 10 commands entered in minibuffer
(recentf-mode    1)      ; List recent files visited.
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Editor Appearance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package alect-themes)           ; Color theme.
(load-theme 'alect-dark t)

(setq inhibit-start-message t        ; Hide start screen.
      visible-bell t                 ; Display visible bell.
      )

(setq-default cursor-type 'bar)      ; Cursor.
(column-number-mode)                 ; Include column location.
(global-display-line-numbers-mode 1) ; Display line numbers.          
(scroll-bar-mode                 -1) ; Hide scroll bar.

;; Disable line numbers for some modes.
(dolist (mode '(org-mode-hook
                              term-mode-hook
                              eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Workspace Configurations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq-default cursor-type 'bar)                           ; Change default cursor to line.
(add-hook 'prog-mode-hook 'display-line-numbers-mode)     ; Display line numbers
(set-face-foreground 'line-number-current-line "#FFC300") ; Change font color of active line.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :config
  (setq which-key-idle-delay 0.3))

(use-package treemacs
  :ensure t
  :defer t
  :init
  :bind
  (:map global-map
               ("C-c c" . treemacs)
               ("M-0" . treemacs-select-window))
  :config
  (progn
    (setq treemacs-is-never-other-window t)
    (setq show-hidden-files t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global Keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "<escape>") 'keyboard-escape-quit  ;; ESC quits prompts.
