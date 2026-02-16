;;; treesit-modes.el --- choose major modes (ts-mode first) -*- lexical-binding: t; -*-

;; Vue
(require 'vue-ts-mode nil t)
(add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-ts-mode))

;; HTML
(require 'html-ts-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . html-ts-mode))

;; Python: prefer python-ts-mode when grammar is available
(when (and (fboundp 'python-ts-mode)
           (fboundp 'treesit-language-available-p)
           (treesit-language-available-p 'python))
  (add-to-list 'major-mode-remap-alist
               '(python-mode . python-ts-mode)))

;; Nix: prefer nix-ts-mode; fallback to nix-mode.
(use-package nix-ts-mode
  :ensure t
  :mode ("\\.nix\\'" . nix-ts-mode)
  :init
  ;; If nix-ts-mode isn't available for any reason, fall back to nix-mode (if installed).
  (unless (fboundp 'nix-ts-mode)
    (with-eval-after-load 'nix-mode
      (add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode)))))
(use-package nix-mode
  :ensure t
  :defer t)

;; Lua
(require 'lua-ts-mode nil t)
(add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-ts-mode))

;; C / C++
(when (and (fboundp 'treesit-language-available-p)
           (treesit-language-available-p 'c)
           (fboundp 'c-ts-mode))
  (add-to-list 'major-mode-remap-alist
               '(c-mode . c-ts-mode)))
(when (and (fboundp 'treesit-language-available-p)
           (treesit-language-available-p 'cpp)
           (fboundp 'c++-ts-mode))
  (add-to-list 'major-mode-remap-alist
               '(c++-mode . c++-ts-mode)))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c-ts-mode))
(add-to-list 'auto-mode-alist '("\\.hpp\\'" . c++-ts-mode))

;; Javascript, Jsx
(when (fboundp 'js-ts-mode)
  (add-to-list 'auto-mode-alist '("\\.m?js\\'" . js-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . js-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.cjs\\'" . js-ts-mode)))
;; Typescript, Tsx
(when (fboundp 'typescript-ts-mode)
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode)))
(when (fboundp 'tsx-ts-mode)
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode)))

;; Markdown
(use-package markdown-ts-mode
  :ensure t
  :mode ("\\.md\\'" . markdown-ts-mode)
  :defer 't)

(provide 'treesit-modes)
;;; treesit-modes.el ends here
