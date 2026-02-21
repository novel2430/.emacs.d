(use-package move-dup :ensure t)

;; Move selected region (or current line) up/down
(define-key my/visual-block-mode-map (kbd "k")   #'move-dup-move-lines-up)
(define-key my/visual-block-mode-map (kbd "j") #'move-dup-move-lines-down)
;; Duplicate selected region (or current line) up/down
(define-key my/visual-block-mode-map (kbd "M-k")   #'move-dup-duplicate-up)
(define-key my/visual-block-mode-map (kbd "M-j") #'move-dup-duplicate-down)

(provide 'move-dup-setup)
