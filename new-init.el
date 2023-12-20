;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq package-enable-at-startup nil)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

;; Install use-package if not already installed
(unless (package-installed-p 'use-package)
        (package-refresh-contents)
        (package-install 'use-package))

;(add-to-list 'load-path "~/.config/emacs/site-lisp/elpa-mirror")
;(require 'elpa-mirror)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Workspace Configurations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; History
(setq global-auto-revert-non-file-buffers t)              ; Revert dired & other buffers.
(global-auto-revert-mode 1)                               ; Revert buffer after file changed in external system.
(setq history-length 10)                                  ; Minibuffer history length.
(save-place-mode 1)                                       ; Last place visited in file.
(savehist-mode   1)                                       ; Last 10 commands entered in minibuffer
(recentf-mode    1)                                       ; List recent files visited.

;; General Settings
(set-face-foreground 'line-number-current-line "#FFC300") ; Change font color of active line.
(global-display-line-numbers-mode 1)                      ; Display line numbers.
(setq-default cursor-type 'bar)                           ; Cursor.
(delete-selection-mode t)                                 ; Delete selected text on type.
(electric-pair-mode 1)                                    ; Auto close brackets.
(column-number-mode)                                      ; Include column location.
(scroll-bar-mode -1)                                      ; Hide scroll bar.
(tool-bar-mode   -1)                                      ; Hide tool bar.


;; Disable line numbers for some modes.
(dolist (mode '(treemacs-mode-hook
		shell-mode-hook
		term-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Theme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(use-package alect-themes)           ; Color theme.
;(load-theme 'alect-dark t)
(use-package treemacs-all-the-icons)
(treemacs-load-theme "all-the-icons")

;(use-package vscode-dark-plus-theme
;  :ensure t
;  :config
;  (load-theme 'vscode-dark-plus t))

(use-package spacemacs-theme
  :ensure t
  :config
  (load-theme 'spacemacs-dark t))


(setq inhibit-start-message t        ; Hide start screen.
      inhibit-splash-screen t        ; Hide splash.
      visible-bell t                 ; Display visible bell.
      auto-save-default nil          ; Disable autosave.
      make-backup-files nil          ; Don't create backup files.
      create-lockfiles nil           ; Don't create lock files.
      )

;; Custom font with ligatures.
;; This assumes you've installed the package via MELPA.
(use-package ligature
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       "\\\\" "://"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

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

(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)
	 ("C--" . er/contract-region)))

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

;; Open shell below currently selected buffer.
(defun my-open-shell-below ()
  (interactive)
  (let ((display-buffer-alist '(("^\\*shell\\*$" . (display-buffer-below-selected)))))
    (pop-to-buffer-same-window (shell "*my-shell*"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Development config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package typescript-mode
  :delight "ts"
  :mode "\\.ts\\'" "\\.tsx\\'"
  :hook ((typescript-mode . lsp-deferred)
         (before-save . lsp-format-buffer)
         (before-save . lsp-organize-imports))
  :custom
  (typescript-indent-level 2))

(use-package lsp-mode
  :ensure t
  :hook ((prog-mode . lsp))
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (setq lsp-prefer-flymake nil) ; Use lsp-ui and flycheck instead of flymake
  (lsp-enable-which-key-integration t))

(use-package company
  :after lsp-mode
  :hook (prog-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :ensure t
  :hook (company-mode . company-box-mode))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

;(setq lsp-ui-doc-position 'bottom)
;(setq lsp-ui-sideline-enable nil)
;(setq lsp-ui-sideline-show-hover nil)

(use-package lsp-treemacs
  :after lsp)

(use-package lsp-ivy)

(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

(use-package flycheck
  :ensure t
  :hook (typescript-mode . flycheck-mode))

(use-package smartparens
  :ensure t
  :hook (typescript-mode . smartparens-mode))

(use-package prettier-js
  :ensure t
  :hook (typescript-mode . prettier-js-mode))

  ;; Potential modification to prettier for default settings
  ;:config
  ;(defun my/prettier-js-setup ()
  ;  "Configure prettier-js with default settings if no prettierrc is present."
  ; (unless (file-exists-p (expand-file-name ".prettierrc" (projectile-project-root)))
  ;    (setq prettier-js-args '("--single-quote" "true" "--trailing-comma" "all"))))
  
  ;(add-hook 'typescript-mode-hook 'my/prettier-js-setup))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global Keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)   ;; ESC quits prompts.
(global-set-key (kbd "C-c t") 'my-open-shell-below)
(global-set-key (kbd "C-c C-t C-c") 'evilnc-copy-to-line) ;; Evil-Nerd copy from current line to specified line.
(global-set-key (kbd "C-c C-t C-k") 'evilnc-kill-to-line) ;; Evil-Nerd kill from current line to specified line.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(spacemacs-theme evil-nerd-commenter yasnippet which-key web-mode vscode-dark-plus-theme typescript-mode treemacs-all-the-icons tree-sitter-langs tide smartparens rainbow-delimiters prettier-js lsp-ui lsp-ivy ligature json-mode helm expand-region exec-path-from-shell eslintd-fix company-box alect-themes)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
