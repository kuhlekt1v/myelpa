;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq package-enable-at-startup nil)

;(require 'package)
;(add-to-list 'package-archives '(("org" . "https://orgmode.org/elpa/")
;                       ("melpa" . "https://melpa.org/packages/")
;                       ("gnu" . "https://elpa.gnu.org/packages/")
		       ;("myelpa" . "~/myelpa")
;		       ))
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Install use-package if not already installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(add-to-list 'load-path "~/.config/emacs/site-lisp/elpa-mirror")
(require 'elpa-mirror)


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

;(use-package alect-themes)           ; Color theme.
;(load-theme 'alect-dark t)
(use-package treemacs-all-the-icons)
(treemacs-load-theme "all-the-icons")

(use-package vscode-dark-plus-theme
  :ensure t
  :config
  (load-theme 'vscode-dark-plus t))

(setq inhibit-start-message t        ; Hide start screen.
      visible-bell t                 ; Display visible bell.
      auto-save-default nil          ; Disable autosave.
      make-backup-files nil          ; Don't create backup files.
      create-lockfiles nil           ; Don't creae lock files.
      )

(setq-default cursor-type 'bar)      ; Cursor.
(column-number-mode)                 ; Include column location.
(global-display-line-numbers-mode 1) ; Display line numbers.          
(scroll-bar-mode                 -1) ; Hide scroll bar.
(delete-selection-mode            t) ; Delete selected text on type.


;; Disable line numbers for some modes.
(dolist (mode '(treemacs-mode-hook
		shell-mode-hook
		term-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

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
;; Workspace Configurations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq-default cursor-type 'bar)                           ; Change default cursor to line.
(add-hook 'prog-mode-hook 'display-line-numbers-mode)     ; Display line numbers
(set-face-foreground 'line-number-current-line "#FFC300") ; Change font color of active line.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :ensure t
  :init
  (which-key-mode)
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


(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

;; Open shell below currently selected buffer.
(defun my-open-shell-below ()
  (interactive)
  (let ((display-buffer-alist '(("^\\*shell\\*$" . (display-buffer-below-selected)))))
    (pop-to-buffer-same-window (shell "*my-shell*"))))

(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)
	 ("C--" . er/contract-region)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Development config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Ensure ESLint and Prettier are installed globally
;; You can install them using npm: npm install -g eslint prettier

(use-package typescript-mode
  :ensure t)

(use-package web-mode
  :ensure t
  :mode (("\\.tsx\\'" . web-mode)
         ("\\.ts\\'" . web-mode)
         ("\\.jsx\\'" . web-mode)
         ("\\.js\\'" . web-mode)
         ("\\.html\\'" . web-mode)
         ("\\.css\\'" . web-mode)
         ("\\.scss\\'" . web-mode)
         ("\\.sass\\'" . web-mode)
         ("\\.json\\'" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-enable-auto-pairing t
        web-mode-enable-css-colorization t
        web-mode-enable-current-element-highlight t
        web-mode-enable-current-column-highlight t))

(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

(use-package company
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-company-mode))

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode))

(use-package prettier-js
  :ensure t
  :hook ((typescript-mode . prettier-js-mode)
         (web-mode . prettier-js-mode)))

(use-package eslintd-fix
  :ensure t
  :hook (web-mode . eslintd-fix-mode))

;; Optional: Helm for better auto-completion experience
(use-package helm
  :ensure t
  :init
  (helm-mode 1))

;; Optional: Magit for Git integration
;; (use-package magit
;;   :ensure t
;;   :bind ("C-x g" . magit-status))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global Keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)  ;; ESC quits prompts.
(global-set-key (kbd "C-c t") 'my-open-shell-below)

