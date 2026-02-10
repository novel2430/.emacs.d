;;; completion-setup.el --- minibuffer cycle keys (M-j/M-k) -*- lexical-binding: t; -*-
(setq completion-styles '(basic partial-completion substring))
;; Minibuffer completion UI: like a built-in picker
(icomplete-mode 1)
(fido-mode 1)          ;; fuzzy-ish matching for many commands (incl. C-x b)
(fido-vertical-mode 1) 
;; 让 minibuffer 的候选列表在你 cycle 时自动滚动，确保当前选中项可见
(setq icomplete-scroll t)
;;（可选）控制垂直候选最多显示多少行
(setq icomplete-prospects-height 10)
;;（可选）允许 minibuffer 适当变高一些，避免候选太多挤出屏幕
(setq resize-mini-windows t)
(setq max-mini-window-height 0.4)  ;; 0.25~0.5 之间你自己调

(with-eval-after-load 'icomplete
  (define-key icomplete-minibuffer-map (kbd "M-j") #'icomplete-forward-completions)
  (define-key icomplete-minibuffer-map (kbd "M-k") #'icomplete-backward-completions))

(provide 'completion-setup)
;;; completion-setup.el ends here
