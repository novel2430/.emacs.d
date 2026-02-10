;;; lsp-servers.el --- eglot servers -*- lexical-binding: t; -*-

(require 'eglot)

;; 下一步我们会在这里加各语言 server，比如：
;; Python
(add-to-list 'eglot-server-programs '(python-ts-mode . ("pyright-langserver" "--stdio")))
(add-to-list 'eglot-server-programs '(typescript-ts-mode . ("typescript-language-server" "--stdio")))

(provide 'lsp-servers)
;;; lsp-servers.el ends here
