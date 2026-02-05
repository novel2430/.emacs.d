;; ==== Custom File Saving ====
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; ==== Set UI ====
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-screen t)
(setq ring-bell-function 'ignore)
(column-number-mode 1)
(line-number-mode 1)
(blink-cursor-mode -1)
(global-display-line-numbers-mode 1)
(global-hl-line-mode 1)
;; ;; do not show line number in mode below
(dolist (mode '(term-mode-hook
                eshell-mode-hook
                shell-mode-hook
                vterm-mode-hook
                help-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; ==== fonts ====
(set-face-attribute 'default nil
                    :family "Hack Nerd Font"
                    :height 160)
(setq-default line-spacing 0.12)

;; ==== Alpha ====
(set-frame-parameter nil 'alpha-background 85)
(add-to-list 'default-frame-alist '(alpha-background . 85))


;; Minibuffer completion UI: like a built-in picker
(icomplete-mode 1)
(fido-mode 1)          ;; fuzzy-ish matching for many commands (incl. C-x b)
(fido-vertical-mode 1) 

;; Cursor Type
(setq-default cursor-type '(bar . 3))

;; ==== my moudles ====
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'packages)
(require 'keybind)
(require 'tab)

;; === Theme ===
(use-package vscode-dark-plus-theme
  :ensure t
  :config
  (load-theme 'vscode-dark-plus t))
