(use-package pdf-tools :ensure t)
(pdf-tools-install)

;; Keybind
(with-eval-after-load 'pdf-view
  ;; Resize
  (define-key pdf-view-mode-map (kbd "+") #'pdf-view-enlarge)
  (define-key pdf-view-mode-map (kbd "-") #'pdf-view-shrink)
  (define-key pdf-view-mode-map (kbd "w") #'pdf-view-fit-width-to-window)
  (define-key pdf-view-mode-map (kbd "p") #'pdf-view-fit-page-to-window)

  ;; Highlight
  (define-key pdf-view-mode-map (kbd "C-c h") #'pdf-annot-add-highlight-markup-annotation)
  (define-key pdf-view-mode-map (kbd "C-c d h") #'pdf-annot-delete)
  (define-key pdf-view-mode-map (kbd "C-c l h") #'pdf-annot-list-annotations))

(provide 'pdf-tools-setup)
