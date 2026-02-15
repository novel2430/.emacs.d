;;; lsp-modes.el --- smart eglot root for single files -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'eglot)

;; Avoid massive file-watching registrations (e.g. pyright asks for "**")
(cl-defmethod eglot-register-capability :around
  (_server (_method (eql workspace/didChangeWatchedFiles)) _id &rest _args)
  "Ignore didChangeWatchedFiles capability registration to prevent huge scans."
  ;; Return nil => we don't register any watchers
  nil)

(defun my/eglot-ensure ()
  "Start eglot for current buffer when it makes sense."
  (when (and (buffer-file-name) (not (minibufferp)))
    (eglot-ensure)))

;; 先从你最常用/最好装 server 的语言开始（比如 python / ts / js）
;; 你之后告诉我你先跑哪一个，我再补对应的 server 配置。
;; Python
(add-hook 'python-mode-hook #'my/eglot-ensure)
(add-hook 'python-ts-mode-hook #'my/eglot-ensure)
;; Lua
(add-hook 'lua-ts-mode-hook #'my/eglot-ensure)
;; C/C++
(add-hook 'c-ts-mode-hook #'my/eglot-ensure)
(add-hook 'c++-ts-mode-hook #'my/eglot-ensure)
;; Nix
(add-hook 'nix-ts-mode-hook #'my/eglot-ensure)
;; Vue
(add-hook 'vue-ts-mode-hook #'my/eglot-ensure)
;; Javascript, Typescript, Tsx
(add-hook 'js-ts-mode-hook #'my/eglot-ensure)
(add-hook 'typescript-ts-mode-hook #'my/eglot-ensure)
(add-hook 'tsx-ts-mode-hook #'my/eglot-ensure)

(provide 'lsp-modes)
;;; lsp-modes.el ends here
