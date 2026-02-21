;;; vertico-setup.el --- minibuffer cycle keys (M-j/M-k) -*- lexical-binding: t; -*-
;; --- Minibuffer UI: Vertico ---
(use-package vertico
  :ensure t
  :init
  (vertico-mode 1)
  ;; 讓候選在你上下移動時穩定滾動（你想要的效果）
  (setq vertico-scroll-margin 2
        vertico-count 12
        vertico-resize t))

;; --- Matching: Orderless ---
(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        ;; file 類別通常保留 partial-completion 比較符合路徑習慣
        completion-category-overrides '((file (styles basic partial-completion)))))

;; （可選）如果你習慣 M-j/M-k
(with-eval-after-load 'vertico
  (define-key vertico-map (kbd "M-j") #'vertico-next)
  (define-key vertico-map (kbd "M-k") #'vertico-previous))

(provide 'vertico-setup)
;;; vertico-setup.el ends here
