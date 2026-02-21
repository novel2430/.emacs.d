;;; curfu-setup.el --- corfu completion UI -*- lexical-binding: t; -*-

;; Corfu: completion popup for code buffers (CAPF)
(use-package corfu
  :ensure t
  :init
  (global-corfu-mode 1)
  :custom
  ;; 自动弹出
  (corfu-auto t)
  (corfu-auto-delay 0.08)
  (corfu-auto-prefix 1)

  ;; 到底回到第一个（你想要 cycle）
  (corfu-cycle t)

  ;; 默认预选第一个候选
  (corfu-preselect 'first)

  ;; 弹窗最多显示几行
  (corfu-count 12)

  ;; 不在 minibuffer 里启用（避免跟你现有 icomplete/fido 打架）
  (corfu-echo-documentation nil)
  :config
  ;; 更像 IDE：显示候选说明（可选）
  (corfu-popupinfo-mode 1)
  (setq corfu-popupinfo-delay 0.2)

  ;; 常用键（Corfu 默认也能用 C-n/C-p）
  (define-key corfu-map (kbd "ESC") nil)
  (define-key corfu-map (kbd "C-n") #'corfu-next)
  (define-key corfu-map (kbd "C-p") #'corfu-previous)
  (define-key corfu-map (kbd "M-j") #'corfu-next)
  (define-key corfu-map (kbd "M-k") #'corfu-previous)
  ;; (define-key corfu-map (kbd "TAB") #'corfu-insert)      ;; 插入当前候选
  ;; (define-key corfu-map (kbd "<tab>") #'corfu-insert)
  (define-key corfu-map (kbd "RET") #'corfu-insert)      ;; 回车直接确认（更像 nvim）
  (define-key corfu-map (kbd "<escape>") #'corfu-quit))

;; 终端 Emacs 的补全弹窗支持（如果你在 TUI 用 Emacs）
;; GUI 不需要；TUI 建议加上会更稳定显示
(use-package corfu-terminal
  :ensure t
  :if (not (display-graphic-p))
  :config
  (corfu-terminal-mode 1))

(provide 'corfu-setup)
;;; corfu-setup.el ends here

