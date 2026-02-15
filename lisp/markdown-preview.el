;;; markdown-preview.el --- xxx -*- lexical-binding: t; -*-
(add-to-list 'load-path "~/.emacs.d/lisp/mdxw-preview")
(require 'mdxw-preview)
;; 例：自訂位置與 debounce
(setq mdxw-preview-side 'right
      mdxw-preview-window-width 0.45
      mdxw-preview-debounce-seconds 0.35)
;; Markdown Preview
(with-eval-after-load 'mdxw-preview
  ;; 確保 markdown-ts-mode 的 keymap 已載入再綁
  (with-eval-after-load 'markdown-ts-mode
    (define-key markdown-ts-mode-map (kbd "C-c m t") #'mdxw-preview-toggle)
    (define-key markdown-ts-mode-map (kbd "C-c m r") #'mdxw-preview-render-current)
    (define-key markdown-ts-mode-map (kbd "C-c m d") #'mdxw-preview-close)))

(provide 'markdown-preview)
