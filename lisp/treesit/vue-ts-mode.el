;;; vue-ts-mode.el --- Major mode for editing Vue templates  -*- lexical-binding: t; -*-

;; Version: 1.0.0
;; Package-Requires: ((emacs "29.1"))

;;; Code:

(require 'cl-lib)
(require 'treesit)

(require 'typescript-ts-mode nil t)
(require 'css-ts-mode nil t)
(require 'css-mode)

(defgroup vue ()
  "Major mode for editing Vue templates."
  :group 'languages)

(defcustom vue-ts-mode-indent-offset 2
  "Number of spaces for each indentation step in `vue-ts-mode'."
  :type 'integer
  :group 'vue)

;; -------------------------
;; Safe accessors (optional)
;; -------------------------

(defun vue-ts-mode--ts-base-font-lock-settings ()
  "Return TypeScript base treesit font-lock settings, or nil."
  (when (fboundp 'typescript-ts-mode--font-lock-settings)
    (typescript-ts-mode--font-lock-settings 'typescript)))

(defun vue-ts-mode--ts-base-indent-rules ()
  "Return TypeScript base treesit indent rules, or nil."
  (when (fboundp 'typescript-ts-mode--indent-rules)
    (alist-get 'typescript (typescript-ts-mode--indent-rules 'typescript))))

(defun vue-ts-mode--css-base-font-lock-settings ()
  "Return CSS base treesit font-lock settings, or nil."
  (when (boundp 'css--treesit-settings)
    css--treesit-settings))

(defun vue-ts-mode--css-base-indent-rules ()
  "Return CSS base treesit indent rules, or nil."
  (when (boundp 'css--treesit-indent-rules)
    (alist-get 'css css--treesit-indent-rules)))

(defun vue-ts-mode--prefix-font-lock-features (prefix settings)
  "Prefix with PREFIX the font lock features in SETTINGS."
  (mapcar (lambda (s)
            (list (nth 0 s)
                  (nth 1 s)
                  (intern (format "%s-%s" prefix (nth 2 s)))
                  (nth 3 s)))
          settings))

;; -------------------------
;; Faces
;; -------------------------

(defface vue-ts-mode-template-tag-bracket-face
  '((t :foreground "#86e1fc"))
  "Face for Vue template brackets and moustaches."
  :group 'vue)

;; -------------------------
;; Indentation rules
;; -------------------------

(defvar vue-ts-mode--indent-rules
  (let ((css-rules (vue-ts-mode--css-base-indent-rules))
        (ts-rules  (vue-ts-mode--ts-base-indent-rules)))
    `((vue
       ((node-is "/>") parent-bol 0)
       ((node-is ">") parent-bol 0)
       ((node-is "end_tag") parent-bol 0)
       ((parent-is "comment") prev-adaptive-prefix 0)
       ((parent-is "element") parent-bol vue-ts-mode-indent-offset)
       ((parent-is "script_element") parent-bol 0)
       ((parent-is "style_element") parent-bol 0)
       ((parent-is "template_element") parent-bol vue-ts-mode-indent-offset)
       ((parent-is "start_tag") parent-bol vue-ts-mode-indent-offset)
       ((parent-is "self_closing_tag") parent-bol vue-ts-mode-indent-offset))
      (css . ,(append (or css-rules '())
                      '(((parent-is "stylesheet") parent-bol 0))))
      (typescript . ,(or ts-rules nil))))
  "Tree-sitter indentation rules for `vue-ts-mode'.")

;; -------------------------
;; Font-lock settings builder
;; -------------------------

(defun vue-ts-mode--vue-font-lock-rules ()
  "Return Vue-specific treesit font-lock rules."
  (treesit-font-lock-rules
   :language 'vue
   :override t
   :feature 'vue-attr
   '((attribute_name) @font-lock-property-name-face)

   :language 'vue
   :override t
   :feature 'vue-definition
   '((tag_name) @font-lock-function-name-face)

   :language 'vue
   :override t
   :feature 'vue-directive
   '((_ (_
         (directive_attribute
          (directive_name) @font-lock-keyword-face
          (directive_argument) @font-lock-type-face))))

   :language 'vue
   :override t
   :feature 'vue-bracket
   '(([ "{{" "}}" "<" ">" "</" "/>"]) @vue-ts-mode-template-tag-bracket-face)

   :language 'vue
   :override t
   :feature 'vue-string
   '((attribute (quoted_attribute_value) @font-lock-string-face))

   ;; TS customization (works even if TS base settings absent)
   :language 'typescript
   :override t
   :feature 'typescript-custom-property
   '(((property_identifier) @font-lock-property-name-face))

   :language 'typescript
   :override t
   :feature 'typescript-custom-variable
   '(((identifier) @font-lock-variable-name-face))))

(defun vue-ts-mode--font-lock-settings ()
  "Return full treesit font-lock settings for vue-ts-mode (safe)."
  (let ((out '()))
    ;; Add TS/CSS base settings if available (prefixed to avoid feature name collisions)
    (let ((ts (vue-ts-mode--ts-base-font-lock-settings)))
      (when ts
        (setq out (append out (vue-ts-mode--prefix-font-lock-features "typescript" ts)))))
    (let ((cs (vue-ts-mode--css-base-font-lock-settings)))
      (when cs
        (setq out (append out (vue-ts-mode--prefix-font-lock-features "css" cs)))))

    ;; Add our Vue + custom rules
    (setq out (append out (vue-ts-mode--vue-font-lock-rules)))
    out))

(defvar vue-ts-mode--computed-font-lock-settings
  (vue-ts-mode--font-lock-settings)
  "Computed font-lock settings for `vue-ts-mode'.")

;; -------------------------
;; Range settings (embedded TS/CSS)
;; -------------------------

(defvar vue-ts-mode--range-settings
  (if (member :local (help-function-arglist 'treesit-range-rules t))
      (treesit-range-rules
       :embed 'typescript :host 'vue :local t '((interpolation (raw_text) @capture))
       :embed 'typescript :host 'vue '((script_element (raw_text) @capture))
       :embed 'typescript :host 'vue :local t '((directive_attribute (quoted_attribute_value (attribute_value) @capture)))
       :embed 'css :host 'vue '((style_element (raw_text) @capture)))
    (treesit-range-rules
     :embed 'typescript :host 'vue
     '((script_element (raw_text) @capture)
       (interpolation (raw_text) @capture)
       (directive_attribute (quoted_attribute_value (attribute_value) @capture)))
     :embed 'css :host 'vue
     '((style_element (raw_text) @capture))))
  "Treesit range settings for Vue SFC embedded languages.")

(defun vue-ts-mode--treesit-language-at-point (point)
  "Return the treesit language at POINT."
  (let ((lang nil))
    (cl-loop
     for parser in (treesit-parser-list)
     while (not lang)
     do (cl-loop
         for r in (treesit-parser-included-ranges parser)
         when (and (>= point (car r)) (<= point (cdr r)))
         do (setq lang (treesit-parser-language parser))))
    (or lang 'vue)))

;; -------------------------
;; Major mode
;; -------------------------

;;;###autoload
(define-derived-mode vue-ts-mode prog-mode "Vue-ts"
  "Major mode for editing Vue templates, powered by tree-sitter."
  :group 'vue

  (unless (treesit-ready-p 'vue)
    (error "Tree-sitter grammar for Vue isn't available"))
  (unless (treesit-ready-p 'css)
    (error "Tree-sitter grammar for CSS isn't available"))
  (unless (treesit-ready-p 'typescript)
    (error "Tree-sitter grammar for TypeScript isn't available"))

  ;; Create parsers up-front so embedded ranges behave predictably.
  (treesit-parser-create 'vue)
  (treesit-parser-create 'typescript)
  (treesit-parser-create 'css)

  ;; Basic text nodes for comment/text handling
  (setq-local treesit-text-type-regexp (regexp-opt '("comment" "text")))

  ;; Indentation
  (setq-local treesit-simple-indent-rules vue-ts-mode--indent-rules
              css-indent-offset vue-ts-mode-indent-offset)

  ;; Font-lock
  (setq-local treesit-font-lock-settings vue-ts-mode--computed-font-lock-settings)
  (setq-local treesit-font-lock-feature-list
              '((vue-attr vue-definition
                 css-selector css-comment css-query css-keyword
                 typescript-comment typescript-declaration)
                (vue-string vue-directive
                            css-property css-constant css-string
                            typescript-keyword typescript-string typescript-escape-sequence)
                (vue-bracket
                 css-error css-variable css-function css-operator
                 typescript-constant typescript-expression typescript-identifier
                 typescript-number typescript-pattern typescript-operator typescript-property
                 typescript-custom-variable typescript-custom-property)))

  ;; Embedded
  (setq-local treesit-range-settings vue-ts-mode--range-settings)
  (setq-local treesit-language-at-point-function #'vue-ts-mode--treesit-language-at-point)

  (treesit-major-mode-setup))

;;;###autoload
(when (treesit-ready-p 'vue)
  (add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-ts-mode)))

(provide 'vue-ts-mode)
;;; vue-ts-mode.el ends here
