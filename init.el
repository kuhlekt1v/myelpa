(package-initialize)			 

(add-to-list 'load-path "~/.config/emacs/site-lisp/elpa-mirror")
(require 'elpa-mirror)

;(require 'package)
;(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;; myelpa is the ONLY repository now, dont forget trailing slash in the directory
(setq package-archives '(("myelpa" . "~/test-elpa/")))

;; ESC quits prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;;;;;;;;;;;;;;;;;;;;;;;
;; Editor Appearance ;;
;;;;;;;;;;;;;;;;;;;;;;;

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



;;;;;;;;;;;;;
;; History ;;
;;;;;;;;;;;;;

(setq global-auto-revert-non-file-buffers t) ; Revert dired & other buffers.
(global-auto-revert-mode 1)                  ; Revert buffer after file changed in external system.

(setq history-length 10) ; Minibuffer history length.
(save-place-mode 1)      ; Last place visited in file.
(savehist-mode   1)      ; Last 10 commands entered in minibuffer
(recentf-mode    1)      ; List recent files visited.

;;;;;;;;;;;;;;;
;; Set theme ;;
;;;;;;;;;;;;;;;

(load-theme 'alect-dark t)

;;;;;;;;;;;;;;
;; Packages ;;
;;;;;;;;;;;;;;

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window)))
