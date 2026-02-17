
(use-package consult :ensure t)

(global-set-key (kbd "C-x b") #'consult-buffer)

;; For Searching Rencent file
(recentf-mode 1)
(setq recentf-max-saved-items 200)
(setq recentf-exclude '("/tmp/" "/ssh:" "/sudo:" "\\.git/.*"))
(global-set-key (kbd "C-x C-r") #'consult-recent-file)

(provide 'consult-setup)
