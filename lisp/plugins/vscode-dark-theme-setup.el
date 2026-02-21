(use-package vscode-dark-plus-theme
  :ensure t
  :config
  (load-theme 'vscode-dark-plus t))
(set-face-attribute 'mode-line nil :weight 'bold)
(set-face-attribute 'mode-line-inactive nil
                    :background "gray30")  ;; 想更亮就 gray40/gray50
(set-face-attribute 'show-paren-match nil :weight 'bold :underline t)
(set-face-attribute 'show-paren-mismatch nil :weight 'bold :underline t)

(provide 'vscode-dark-theme-setup)
