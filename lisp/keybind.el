;; My Keybinding

;; ==== Visual Selecting ====
(global-set-key (kbd "M-v") #'set-mark-command)
(defun my/visual-line ()
  "Select whole lines like Vim Visual Line.
If region inactive: select current line (including newline).
If region active: expand selection to whole lines, regardless of direction."
  (interactive)
  (if (not (region-active-p))
      ;; start: current line
      (progn
        (beginning-of-line)
        (set-mark (point))
        (forward-line 1)
        (activate-mark))
    ;; normalize: extend to full lines
    (let* ((beg (region-beginning))
           (end (region-end)))
      ;; make beginning at bol of first touched line
      (goto-char beg)
      (beginning-of-line)
      (set-mark (point))
      ;; make end at bol of line after last touched line
      (goto-char end)
      (unless (bolp) (end-of-line))   ; if end is mid-line, go to EOL
      (forward-line 1)                ; include newline / next line bol
      (activate-mark))))

(global-set-key (kbd "M-;") #'my/visual-line)

;; ==== Vim Like Movement
(global-set-key (kbd "M-h") #'backward-char)   ; left
(global-set-key (kbd "M-j") #'next-line)       ; down
(global-set-key (kbd "M-k") #'previous-line)   ; up
(global-set-key (kbd "M-l") #'forward-char)    ; right
(global-set-key (kbd "M-u") #'undo-only)
(global-set-key (kbd "M-a") #'mark-whole-buffer)
(defun my/vim-o-below ()
  "Like Vim 'o': open a new line below current line and indent."
  (interactive)
  (end-of-line)
  (newline-and-indent))
(defun my/vim-O-above ()
  "Like Vim 'O': open a new line above current line and indent."
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-according-to-mode))
(defun my/delete-region-if-active ()
  "Delete active region without saving to kill-ring."
  (interactive)
  (when (region-active-p)
    (delete-region (region-beginning) (region-end))
    (deactivate-mark)))
(global-set-key (kbd "M-o") #'my/vim-o-below)
(global-set-key (kbd "M-p") #'my/vim-O-above)
(global-set-key (kbd "M-d") #'my/delete-region-if-active)

;; ==== Split Window ====
(global-set-key (kbd "M-q") #'kill-buffer-and-window)

;; ==== Cycle Buffer ====
(defun my/buffer-file-p (buf)
  "Non-nil if BUF is visiting a file."
  (buffer-file-name buf))

(defun my/cycle-buffer (pred step)
  "Cycle buffers in (buffer-list) by STEP (+1 or -1) filtering by PRED."
  (let* ((bufs (buffer-list))
         (cur  (current-buffer))
         (n    (length bufs))
         (i0   0)
         (i    0)
         (k    0)
         found)
    ;; find current buffer index
    (while (and (< i0 n) (not (eq (nth i0 bufs) cur)))
      (setq i0 (1+ i0)))
    (setq i (if (< i0 n) i0 0))

    ;; search next matching buffer, at most n steps
    (setq k 0)
    (while (and (< k n) (not found))
      (setq i (mod (+ i step) n))
      (let ((b (nth i bufs)))
        (when (funcall pred b)
          (setq found b)))
      (setq k (1+ k)))

    (when (buffer-live-p found)
      (switch-to-buffer found))))

(defun my/next-file-buffer () (interactive) (my/cycle-buffer #'my/buffer-file-p  1))
(defun my/prev-file-buffer () (interactive) (my/cycle-buffer #'my/buffer-file-p -1))

(global-set-key (kbd "M-]") #'my/next-file-buffer)
(global-set-key (kbd "M-[") #'my/prev-file-buffer)



(provide 'keybind)
