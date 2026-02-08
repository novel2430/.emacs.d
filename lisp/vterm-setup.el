;;; vterm-setup.el --- vterm install/config/keybind in one place -*- lexical-binding: t; -*-

;; If you use use-package, keep everything here.

(use-package vterm
  :ensure t
  :commands (vterm)
  :init
  ;; scrollback lines
  (setq vterm-max-scrollback 10000)

  ;; buffer naming (optional)
  (setq vterm-buffer-name-string "vterm: %s")

  :bind (("C-c t" . vterm)
         ("C-c T" . kill-buffer-and-window))

  ;; Bind vterm keys only when vterm-mode is actually active (avoids void vterm-mode-map)
  :hook ((vterm-mode . (lambda ()
        (local-set-key (kbd "C-c C-y") #'vterm-copy-mode)
        ;; ✅ 关闭“kill vterm buffer 时确认杀进程”的提示（仅当前 vterm buffer）
        (setq-local kill-buffer-query-functions
            (delq 'process-kill-buffer-query-function
				  kill-buffer-query-functions))

        ;; ✅ 关闭“退出 Emacs 时询问是否有进程在跑”的提示（仅此 vterm 进程）
            (let ((p (get-buffer-process (current-buffer))))
              (when (processp p)
				(set-process-query-on-exit-flag p nil))))))

  :config
  ;; Create a new vterm buffer each time
  (defun my/vterm-new ()
    "Create a new vterm buffer."
    (interactive)
    (let ((buf (generate-new-buffer-name "*vterm*")))
      (vterm buf)))

  (global-set-key (kbd "C-c n") #'my/vterm-new)

  ;; copy-mode bindings (guarded)
  (with-eval-after-load 'vterm
    (when (boundp 'vterm-copy-mode-map)
      (define-key vterm-copy-mode-map (kbd "q") #'vterm-copy-mode)
      (define-key vterm-copy-mode-map (kbd "<escape>") #'vterm-copy-mode))))

(provide 'vterm-setup)
;;; vterm-setup.el ends here
