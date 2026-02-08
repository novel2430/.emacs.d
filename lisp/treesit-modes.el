;;; treesit-modes.el --- choose major modes (ts-mode first) -*- lexical-binding: t; -*-

;; Vue
(use-package web-mode
  :ensure t
  :mode ("\\.vue\\'" . web-mode)
  :config
  ;; 常见：缩进 2
  (setq web-mode-markup-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-css-indent-offset 2))

;; HTML: prefer built-in html-ts-mode (Emacs 30.1+),
;; otherwise use vendored lisp/html-ts-mode.el, fallback to html-mode.
(cond
 ((fboundp 'html-ts-mode)
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . html-ts-mode)))
 ((require 'html-ts-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . html-ts-mode)))
 (t
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . html-mode))))


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
;; Optional fallback: if you don't want to rely on with-eval-after-load,
;; just ensure nix-mode is installed too:
(use-package nix-mode
  :ensure t
  :defer t)

(provide 'treesit-modes)
;;; treesit-modes.el ends here
