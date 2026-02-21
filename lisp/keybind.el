;; My Keybinding

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c u") #'eval-buffer)))


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
  "If region is active, delete it without saving to kill-ring.
Otherwise, kill the next word (like `kill-word`)."
  (interactive)
  (if (use-region-p)
      (progn
        (delete-region (region-beginning) (region-end))
        (deactivate-mark))
    (kill-word 1)))

(global-set-key (kbd "M-o") #'my/vim-o-below)
(global-set-key (kbd "M-p") #'my/vim-O-above)
(global-set-key (kbd "M-d") #'my/delete-region-if-active)

;; ==== Split Window ====
(global-set-key (kbd "M-q") #'kill-buffer-and-window)

;; ==== Cycle Buffer ====
(require 'seq)

(defun my/buffer-file-p (buf)
  (buffer-file-name buf))

(defun my/file-buffers-sorted ()
  "Stable file buffer list sorted by file path."
  (sort
   (seq-filter #'my/buffer-file-p (buffer-list))
   (lambda (a b)
     (string-lessp (buffer-file-name a) (buffer-file-name b)))))

(defun my/cycle-file-buffer-fixed (step)
  "Cycle file buffers in a stable order (sorted by file path)."
  (let* ((bufs (my/file-buffers-sorted))
         (n    (length bufs)))
    (cond
     ((<= n 1) (message "No other file buffers"))
     (t
      (let* ((cur (current-buffer))
             (i0  (or (seq-position bufs cur) 0))
             (i   (mod (+ i0 step) n)))
        (switch-to-buffer (nth i bufs)))))))

(defun my/next-file-buffer () (interactive) (my/cycle-file-buffer-fixed  1))
(defun my/prev-file-buffer () (interactive) (my/cycle-file-buffer-fixed -1))

(global-set-key (kbd "M-]") #'my/next-file-buffer)
(global-set-key (kbd "M-[") #'my/prev-file-buffer)

;; Move Visual Line Block
;;; --- Visual line + block edit mode (jk<> move/indent while staying active) ---
(defvar my/visual-block-mode-map (make-sparse-keymap)
  "Keymap for `my/visual-block-mode'.")

(define-minor-mode my/visual-block-mode
  "A small mode for moving/indenting selected lines with j/k/</>."
  :init-value nil
  :lighter " VBlk"
  :keymap my/visual-block-mode-map)

(defun my/region-line-bounds ()
  "Return (BEG . END) as whole-line bounds of active region.
END is at beginning of line after the last selected line."
  (let* ((rb (region-beginning))
         (re (region-end))
         (beg (save-excursion (goto-char rb) (line-beginning-position)))
         (end (save-excursion
                (goto-char re)
                ;; If region ends exactly at BOL, don't include that line.
                (if (bolp) re (line-beginning-position 2)))))
    (cons beg end)))

(defun my/indent-region-right (beg end)
  "Indent region to the right and keep region active."
  (interactive "r")
  (indent-rigidly beg end 2)   ;; change 2 -> 4 if you want
  (setq deactivate-mark nil))

(defun my/indent-region-left (beg end)
  "Indent region to the left and keep region active."
  (interactive "r")
  (indent-rigidly beg end -2)  ;; change 2 -> 4 if you want
  (setq deactivate-mark nil))

(defun my/visual-line-enter-block-mode ()
  "Select current line (including newline) and enter `my/visual-block-mode'."
  (interactive)
  ;; Start a line-based region if none
  (unless (use-region-p)
    (beginning-of-line)
    (set-mark (point))
    (forward-line 1)
    (activate-mark))
  (my/visual-block-mode 1))

(defun my/visual-block-mode-quit ()
  "Quit visual block mode and deactivate region."
  (interactive)
  (my/visual-block-mode 0)
  (deactivate-mark))

;; Bind keys inside the small mode
(define-key my/visual-block-mode-map (kbd "<") #'my/indent-region-left)
(define-key my/visual-block-mode-map (kbd ">") #'my/indent-region-right)
(define-key my/visual-block-mode-map (kbd "q") #'my/visual-block-mode-quit)
(define-key my/visual-block-mode-map (kbd "<escape>") #'my/visual-block-mode-quit)

;; Entry key
(global-set-key (kbd "C-c m") #'my/visual-line-enter-block-mode)

(provide 'keybind)
