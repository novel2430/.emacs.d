;;; treesit-setup.el --- auto install tree-sitter grammars -*- lexical-binding: t; -*-

;;;; ===== User config (edit here) =====

;; 1) Which languages you want to ensure installed.
;; NOTE: Use tree-sitter language symbols.
(defconst my/treesit-languages
  '(nix
    python
    java
    javascript
    typescript
    tsx
    c
    cpp
    c-sharp
    css
    html)
  "Tree-sitter languages to auto-install if missing.")

;; 2) Where to fetch grammar sources for each language.
;; Each entry is: (LANG REPO &optional REV DIR CC CFLAGS)
;; Most of the time you only need (LANG REPO), but TS/TSX need DIR.
(defconst my/treesit-language-repos
  '((nix        "https://github.com/nix-community/tree-sitter-nix")
    (python     "https://github.com/tree-sitter/tree-sitter-python")
    (java       "https://github.com/tree-sitter/tree-sitter-java")
    (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
    ;; TS/TSX share the same repo, different subdirs:
    (typescript "https://github.com/tree-sitter/tree-sitter-typescript" nil "typescript/src")
    (tsx        "https://github.com/tree-sitter/tree-sitter-typescript" nil "tsx/src")
    (c          "https://github.com/tree-sitter/tree-sitter-c")
    (cpp        "https://github.com/tree-sitter/tree-sitter-cpp")
    (c-sharp    "https://github.com/tree-sitter/tree-sitter-c-sharp")
    (css        "https://github.com/tree-sitter/tree-sitter-css")
    (html       "https://github.com/tree-sitter/tree-sitter-html"))
  "Tree-sitter grammar sources. See `treesit-language-source-alist` for the schema.")

;;;; ===== Implementation =====

(defun my/treesit--register-repos ()
  "Register `my/treesit-language-repos` into `treesit-language-source-alist`."
  (when (boundp 'treesit-language-source-alist)
    (dolist (spec my/treesit-language-repos)
      ;; SPEC is (LANG REPO &optional REV DIR CC CFLAGS)
      ;; `treesit-language-source-alist` expects (LANG REPO REV DIR CC CFLAGS)
      (add-to-list 'treesit-language-source-alist spec))))

(defun my/treesit-ensure-language (lang)
  "Ensure tree-sitter grammar for LANG is installed.
If missing, automatically download+compile via `treesit-install-language-grammar`."
  (when (and (fboundp 'treesit-language-available-p)
             (fboundp 'treesit-install-language-grammar)
             (not (treesit-language-available-p lang)))
    (message "[treesit] Installing grammar: %s" lang)
    (condition-case err
        (progn
          (treesit-install-language-grammar lang)
          (message "[treesit] Installed grammar: %s" lang))
      (error
       (message "[treesit] Failed installing %s: %s" lang err)))))

(defun my/treesit-bootstrap ()
  "Auto-register repos and auto-install required tree-sitter grammars."
  (interactive)
  (when (featurep 'treesit)
    (my/treesit--register-repos)
    (dolist (lang my/treesit-languages)
      (my/treesit-ensure-language lang))))

;; Run once after init, so it doesn't interrupt early startup too much.
(add-hook 'after-init-hook
          (lambda ()
            (when (require 'treesit nil t)
              (my/treesit-bootstrap))))

(provide 'treesit-setup)
;;; treesit-setup.el ends here
