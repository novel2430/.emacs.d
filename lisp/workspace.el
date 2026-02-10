;;; workspace.el --- my workspaces -*- lexical-binding: t; -*-

(tab-bar-mode 1)

(setq tab-bar-show 1
      tab-bar-new-button-show nil
      tab-bar-close-button-show nil
      tab-bar-separator " | ")

;; 基础条背景（可以 nil，让它跟主题走；也可以给个固定色）
;; 如果你想“整条”有底色，就给一个具体颜色而不是 nil
;; (set-face-attribute 'tab-bar nil :background "#1e1e1e" :foreground nil)

(set-face-attribute 'tab-bar nil
                    :inherit 'default
                    :background nil)

;; active tab（当前 tab）
(set-face-attribute 'tab-bar-tab nil
                    :inherit 'tab-bar
                    :weight 'bold
                    :background "#5e936c"
                    :foreground nil)

;; inactive tab（非当前 tab）
(set-face-attribute 'tab-bar-tab-inactive nil
                    :inherit 'tab-bar
                    :weight 'normal
                    :foreground "grey60"
                    :background nil)

;; ---- Tab name: keep default styling + prefix index ----
(setq tab-bar-tab-name-format-function
      (lambda (tab i)
        (let* ((s (tab-bar-tab-name-format-default tab i))
               (rest (if (and (stringp s)
                              (> (length s) 0)
                              (eq (aref s 0) ?\s))
                         (substring s 1)
                       s)))
          (concat (format " %d:" i) rest))))

;; ---- Jump to tab by number: C-c w 1..9 ----
(defun my/tab-bar-select-tab (n)
  (lambda ()
    (interactive)
    (tab-bar-select-tab n)))

(dotimes (i 9)
  (let ((n (1+ i)))
    (global-set-key (kbd (format "C-c w %d" n))
                    (my/tab-bar-select-tab n))))

;; tab operations
(global-set-key (kbd "C-c w n") #'tab-bar-new-tab)
(global-set-key (kbd "C-c w j") #'tab-bar-switch-to-next-tab)
(global-set-key (kbd "C-c w k") #'tab-bar-switch-to-prev-tab)
(global-set-key (kbd "C-c w e") #'tab-bar-rename-tab)
(global-set-key (kbd "C-c w d") #'tab-bar-close-tab)

(provide 'workspace)
;;; workspace.el ends here
