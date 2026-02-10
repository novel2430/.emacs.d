;;; lsp-core.el --- eglot core -*- lexical-binding: t; -*-

(require 'eglot)
(require 'xref)

;; 少一点噪音（可选）
(setq eglot-report-progress nil)

;; Disable automatic eldoc in echo area (manual only)
(setq eldoc-idle-delay 999999)   ;; 等同於幾乎不會自動觸發
;; (setq eldoc-echo-area-use-multiline-p nil) ;; 可選：避免多行撐高 echo area

(defun my/flymake-show-line-diagnostics ()
  "Show Flymake diagnostics for current line in the echo area (multiline)."
  (interactive)
  (let* ((beg (line-beginning-position))
         (end (line-end-position))
         (diags (flymake-diagnostics beg end)))
    (if (null diags)
        (message "No Flymake diagnostics on this line.")
      (message "%s"
               (string-join
                (mapcar (lambda (d)
                          (string-trim (flymake-diagnostic-text d)))
                        diags)
                "\n")))))

;; 常用键（你可以按自己习惯改）
(with-eval-after-load 'eglot
  (define-key eglot-mode-map (kbd "C-c l r") #'eglot-rename)
  (define-key eglot-mode-map (kbd "C-c l c a") #'eglot-code-actions)
  (define-key eglot-mode-map (kbd "C-c l g f") #'eglot-format)
  (define-key eglot-mode-map (kbd "C-c l q") #'eglot-shutdown)
  (define-key eglot-mode-map (kbd "C-c l R") #'eglot-reconnect)
  (define-key eglot-mode-map (kbd "C-c l g d") #'xref-find-definitions)
  (define-key eglot-mode-map (kbd "C-c l g r") #'xref-find-references)
  (define-key eglot-mode-map (kbd "C-c l g i") #'eglot-find-implementation)
  (define-key eglot-mode-map (kbd "C-c l g l") #'my/flymake-show-line-diagnostics)
  (define-key eglot-mode-map (kbd "C-c l f d") #'flymake-show-buffer-diagnostics)
  (define-key eglot-mode-map (kbd "C-c l ]") #'flymake-goto-next-error)
  (define-key eglot-mode-map (kbd "C-c l [") #'flymake-goto-prev-error)
  (define-key eglot-mode-map (kbd "C-c l k") #'eldoc))

;; 让 xref 结果用 *xref* buffer 显示，不在 minibuffer 里选
(setq xref-show-xrefs-function #'consult-xref)
(setq xref-show-definitions-function #'consult-xref)

(provide 'lsp-core)
;;; lsp-core.el ends here
