;; ==== Custom File Saving ====
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; ==== No Backup ====
(setq make-backup-files nil)
(setq auto-save-default nil)

;; ==== Temp File Location ====
(defconst my/emacs-var-dir (locate-user-emacs-file "var/"))
(make-directory my/emacs-var-dir t)
(setq
 ;; save-place / projects
 save-place-file (expand-file-name "places" my/emacs-var-dir)
 project-list-file (expand-file-name "projects" my/emacs-var-dir)
 ;; history-ish
 recentf-save-file (expand-file-name "recentf" my/emacs-var-dir)
 savehist-file (expand-file-name "savehist" my/emacs-var-dir)
 bookmark-default-file (expand-file-name "bookmarks" my/emacs-var-dir)
 ;; url, tramp
 url-configuration-directory (expand-file-name "url/" my/emacs-var-dir)
 tramp-persistency-file-name (expand-file-name "tramp" my/emacs-var-dir)
 ;; auto-save
 auto-save-list-file-prefix (expand-file-name "auto-save-list/.saves-" my/emacs-var-dir))

(save-place-mode 1)
(savehist-mode 1)
(recentf-mode 1)

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
                    :height 140)
(setq-default line-spacing 0.12)

;; ==== Alpha ====
(set-frame-parameter nil 'alpha-background 95)
(add-to-list 'default-frame-alist '(alpha-background . 95))

;; ==== TAB ====
(setq-default tab-width 4)
(setq-default standard-indent 4)

;; Cursor Type
(setq-default cursor-type '(bar . 3))

;; ==== my moudles ====
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'packages)
(require 'keybind)
(require 'consult-setup)
(require 'completion-setup)
(require 'treesit-setup)
(require 'treesit-modes)
(require 'vterm-setup)
(require 'lsp-core)
(require 'lsp-servers)
(require 'lsp-modes)
(require 'lsp-completion)
(require 'fringe-setup)

;; === Theme ===
(use-package vscode-dark-plus-theme
  :ensure t
  :config
  (load-theme 'vscode-dark-plus t))
(set-face-attribute 'mode-line nil :weight 'bold)
(set-face-attribute 'mode-line-inactive nil
                    :background "gray30")  ;; 想更亮就 gray40/gray50

(require 'workspace)
