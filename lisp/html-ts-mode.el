;;; html-ts-mode.el --- tree-sitter support for HTML  -*- lexical-binding: t; -*-

;; Minimal backport-style html-ts-mode for Emacs 29+
;; - No grammar download / no treesit-language-source-alist modification.
;; - Only provides the mode; user must install the html grammar themselves.

(require 'treesit)
(require 'sgml-mode)

(declare-function treesit-parser-create "treesit.c")
(declare-function treesit-node-type "treesit.c")
(declare-function treesit-search-subtree "treesit.c")
(declare-function treesit-node-end "treesit.c")
(declare-function treesit-node-start "treesit.c")

(defgroup html-ts nil
  "Tree-sitter support for editing HTML."
  :group 'languages)

(defcustom html-ts-mode-indent-offset 2
  "Number of spaces for each indentation step in `html-ts-mode'."
  :type 'integer
  :safe 'integerp
  :group 'html-ts)

(defvar html-ts-mode--indent-rules
  `((html
     ((parent-is "fragment") column-0 0)
     ((node-is "/>") parent-bol 0)
     ((node-is ">") parent-bol 0)
     ((node-is "end_tag") parent-bol 0)
     ((parent-is "comment") prev-adaptive-prefix 0)
     ((parent-is "element") parent-bol html-ts-mode-indent-offset)
     ((parent-is "script_element") parent-bol html-ts-mode-indent-offset)
     ((parent-is "style_element") parent-bol html-ts-mode-indent-offset)
     ((parent-is "start_tag") parent-bol html-ts-mode-indent-offset)
     ((parent-is "self_closing_tag") parent-bol html-ts-mode-indent-offset)))
  "Tree-sitter indent rules.")

(defvar html-ts-mode--font-lock-settings
  (treesit-font-lock-rules
   :language 'html
   :override t
   :feature 'comment
   `((comment) @font-lock-comment-face)

   :language 'html
   :override t
   :feature 'keyword
   `("doctype" @font-lock-keyword-face)

   :language 'html
   :override t
   :feature 'definition
   `((tag_name) @font-lock-function-name-face)

   :language 'html
   :override t
   :feature 'string
   `((quoted_attribute_value) @font-lock-string-face)

   :language 'html
   :override t
   :feature 'property
   `((attribute_name) @font-lock-variable-name-face))
  "Tree-sitter font-lock settings for `html-ts-mode'.")

(defvar html-ts-mode--treesit-things-settings
  `((html
     (sexp (not (or (and named
                         ,(rx bos (or "document" "tag_name") eos))
                    (and anonymous
                         ,(rx (or "<!" "<" ">" "</"))))))
     (list ,(rx (or "doctype"
                    "element"
                    "comment")))
     (sentence ,(rx (and bos (or "tag_name" "attribute") eos)))
     (text ,(regexp-opt '("comment" "text")))))
  "Settings for `treesit-thing-settings'.")

(defvar html-ts-mode--treesit-font-lock-feature-list
  '((comment keyword definition)
    (property string)
    () ())
  "Settings for `treesit-font-lock-feature-list'.")

(defvar html-ts-mode--treesit-simple-imenu-settings
  '((nil "element" nil nil))
  "Settings for `treesit-simple-imenu'.")

(defvar html-ts-mode--treesit-defun-type-regexp
  "element"
  "Settings for `treesit-defun-type-regexp'.")

(defun html-ts-mode--defun-name (node)
  "Return the defun name of NODE. Return nil if none."
  (when (string-match-p "element" (treesit-node-type node))
    (treesit-node-text
     (treesit-search-subtree node "\\`tag_name\\'" nil nil 2)
     t)))

(defun html-ts-mode--outline-predicate (node)
  "Limit outlines to multi-line elements."
  (when (string-match-p "element" (treesit-node-type node))
    (< (save-excursion
         (goto-char (treesit-node-start node))
         (pos-bol))
       (save-excursion
         (goto-char (treesit-node-end node))
         (skip-chars-backward " \t\n")
         (pos-bol)))))

(defun html-ts-mode--grammar-available-p ()
  "Return non-nil if the HTML tree-sitter grammar is available."
  ;; Emacs 29 has `treesit-language-available-p`. Some builds also have `treesit-ready-p`.
  (cond
   ((fboundp 'treesit-ready-p)
    (treesit-ready-p 'html))
   ((fboundp 'treesit-language-available-p)
    (treesit-language-available-p 'html))
   (t
    ;; Extremely old/partial treesit build: assume not available.
    nil)))

(defun html-ts-mode--require-grammar ()
  "Signal an error if HTML tree-sitter grammar isn't available."
  (unless (html-ts-mode--grammar-available-p)
    (error
     (concat
      "Tree-sitter grammar for HTML is not available.\n\n"
      "This html-ts-mode.el does NOT download/install grammars.\n"
      "Please install the HTML grammar yourself, e.g.:\n"
      "- M-x treesit-install-language-grammar RET html RET   (if your Emacs is configured for it)\n"
      "- or install a prebuilt grammar / build it via your OS/Nix and ensure Emacs can find it.\n"))))

(defun html-ts-mode--show-paren-data ()
  "Tree-sitter-aware paren highlighting, if available.
On Emacs versions lacking `treesit-show-paren-data`, fall back to default behavior."
  (if (fboundp 'treesit-show-paren-data)
      (let ((default (treesit-show-paren-data)))
        ;; Exclude unbalanced tags when the closing tag is missing.
        (when (= (length default) 4)
          (let ((pos1 (min (nth 0 default) (nth 2 default)))
                (pos2 (max (nth 0 default) (nth 2 default))))
            (when (and (equal (treesit-node-type (treesit-node-at pos1)) "<")
                       (not (equal (treesit-node-type (treesit-node-at pos2)) "</")))
              (setq default nil))))
        default)
    ;; No treesit show-paren integration on this build.
    nil))

;;;###autoload
(define-derived-mode html-ts-mode html-mode "HTML"
  "Major mode for editing HTML, powered by tree-sitter."
  :group 'html-ts

  (html-ts-mode--require-grammar)

  ;; Parser
  (setq-local treesit-primary-parser (treesit-parser-create 'html))

  ;; Indent
  (setq-local treesit-simple-indent-rules html-ts-mode--indent-rules)

  ;; Navigation
  (setq-local treesit-defun-type-regexp html-ts-mode--treesit-defun-type-regexp)
  (setq-local treesit-defun-name-function #'html-ts-mode--defun-name)
  (setq-local treesit-thing-settings html-ts-mode--treesit-things-settings)

  ;; Font-lock
  (setq-local treesit-font-lock-settings html-ts-mode--font-lock-settings)
  (setq-local treesit-font-lock-feature-list html-ts-mode--treesit-font-lock-feature-list)

  ;; Imenu
  (setq-local treesit-simple-imenu-settings html-ts-mode--treesit-simple-imenu-settings)

  ;; Outline minor mode
  (setq-local treesit-outline-predicate #'html-ts-mode--outline-predicate)
  ;; html-mode sets regexp-based outline vars; remove them so treesit predicate works.
  (kill-local-variable 'outline-regexp)
  (kill-local-variable 'outline-heading-end-regexp)
  (kill-local-variable 'outline-level)

  (treesit-major-mode-setup)

  ;; show-paren integration (guarded for Emacs variants)
  (when (boundp 'show-paren-data-function)
    (setq-local show-paren-data-function #'html-ts-mode--show-paren-data)))

;; Emacs 29 might not have `derived-mode-add-parents` (depending on patchlevel/build).
(when (fboundp 'derived-mode-add-parents)
  (derived-mode-add-parents 'html-ts-mode '(html-mode)))

(provide 'html-ts-mode)
;;; html-ts-mode.el ends here
