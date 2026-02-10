;;; fringe-setup.el --- eglot core -*- lexical-binding: t; -*-
;; Wider fringe helps letters look like letters
(set-fringe-mode 16)

(with-eval-after-load 'flymake
  ;; --- 1) Define colored faces ONLY for fringe ---
  (defface my/flymake-fringe-error-face
    '((t :inherit fringe :foreground "#ff5f5f"))
    "Face for Flymake error indicator in fringe only.")

  (defface my/flymake-fringe-warning-face
    '((t :inherit fringe :foreground "#ffaf00"))
    "Face for Flymake warning indicator in fringe only.")

  (defface my/flymake-fringe-note-face
    '((t :inherit fringe :foreground "#5fafff"))
    "Face for Flymake note/info indicator in fringe only.")

  ;; --- 2) Your 12x16 E/W/I bitmaps (use your existing ones) ---
  ;; If you already defined these, keep them and remove this section.
  (define-fringe-bitmap 'my/flymake-fringe-E12
    [#b111111111110 #b110000000000 #b110000000000 #b110000000000
     #b111111111100 #b110000000000 #b110000000000 #b110000000000
     #b110000000000 #b111111111110 #b000000000000 #b000000000000
     #b000000000000 #b000000000000 #b000000000000 #b000000000000]
    12 16 'center)

  (define-fringe-bitmap 'my/flymake-fringe-W12
    [#b110000000011 #b110000000011 #b110000000011 #b110000000011
     #b110000110011 #b110000110011 #b110011000011 #b110011000011
     #b110110000011 #b111100000111 #b011000000110 #b000000000000
     #b000000000000 #b000000000000 #b000000000000 #b000000000000]
    12 16 'center)

  (define-fringe-bitmap 'my/flymake-fringe-I12
    [#b111111111110 #b000011110000 #b000011110000 #b000011110000
     #b000011110000 #b000011110000 #b000011110000 #b000011110000
     #b000011110000 #b111111111110 #b000000000000 #b000000000000
     #b000000000000 #b000000000000 #b000000000000 #b000000000000]
    12 16 'center)

  ;; --- 3) Tell Flymake to use our bitmaps ---
  (setq flymake-error-bitmap   'my/flymake-fringe-E12)
  (setq flymake-warning-bitmap 'my/flymake-fringe-W12)
  (setq flymake-note-bitmap    'my/flymake-fringe-I12)

  ;; --- 4) Force bitmap -> face mapping (FRIDGE ONLY) ---
  (defun my/flymake-apply-fringe-faces ()
    (set-fringe-bitmap-face 'my/flymake-fringe-E12 'my/flymake-fringe-error-face)
    (set-fringe-bitmap-face 'my/flymake-fringe-W12 'my/flymake-fringe-warning-face)
    (set-fringe-bitmap-face 'my/flymake-fringe-I12 'my/flymake-fringe-note-face))

  (my/flymake-apply-fringe-faces)

  ;; Themes sometimes reset bitmap faces, so re-apply after theme load
  (add-hook 'after-load-theme-hook #'my/flymake-apply-fringe-faces))

(provide 'fringe-setup)
;;; fringe-setup.el ends here

