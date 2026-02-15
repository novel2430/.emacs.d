;;; lsp-servers.el --- eglot servers -*- lexical-binding: t; -*-

(require 'eglot)

(defun my-vue-ts-plugin-path ()
  "Return @vue/typescript-plugin location under nix store."
  (let* ((exe (or (executable-find "vue-language-server")
                  (user-error "Can't find vue-language-server in PATH")))
         (root (file-name-directory
                (directory-file-name (file-name-directory (file-truename exe))))))
    (expand-file-name "lib/language-tools/packages/language-server/node_modules/@vue/typescript-plugin" root)))

(defun eglot-vue-ts-init-options ()
  (let ((plugin-path (my-vue-ts-plugin-path)))
    `(:plugins [(:name "@vue/typescript-plugin"
           :location ,plugin-path
           :languages ["javascript" "typescript" "vue"])]
    )))

;; Server Configuration
(with-eval-after-load 'eglot
  (let ((vue-ls-path (my-vue-ts-plugin-path)))
    (setq-default
     eglot-workspace-configuration
     `(
       ;; Python
       (:python
        (:analysis
         (:autoSearchPaths t
          :useLibraryCodeForTypes t
          :typeCheckingMode "basic"
          :diagnosticMode "workspace")))

       ;; Nix (nil)
       (:nil
        (:formatting
         (:command ["nixfmt"]))
        (:nix
         (:binary "nix"
          :maxMemoryMB 2560
          :flake
          (:autoArchive :json-false
           :autoEvalInputs :json-false
           :nixpkgsInputName "nixpkgs"))))

       ))))

;; Server Start Cmd
(with-eval-after-load 'eglot
  ;; Python
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode) . ("pyright-langserver" "--stdio")))
  ;; Lua
  (add-to-list 'eglot-server-programs
			   '((lua-mode lua-ts-mode) . ("lua-language-server")))
  ;; C/C++
  (add-to-list 'eglot-server-programs
               '((c-mode c-ts-mode c++-mode c++-ts-mode)
                 . ("clangd"
                    "--clang-tidy"
                    "--completion-style=detailed"
                    "--header-insertion=iwyu"
                    "--background-index")))
  ;; Nix
  (add-to-list 'eglot-server-programs
			   '((nix-ts-mode) . ("nil")))
  
  ;; Vue TS JS TSX
  (add-to-list 'eglot-server-programs
             `((vue-ts-mode :language-id "vue"
                tsx-ts-mode :language-id "typescriptreact"
                ts-ts-mode  :language-id "typescript"
                js-ts-mode  :language-id "javascript")
               . ("typescript-language-server" "--stdio"
                  :initializationOptions ,(eglot-vue-ts-init-options))))

)

(provide 'lsp-servers)
;;; lsp-servers.el ends here
