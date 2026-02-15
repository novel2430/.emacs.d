;;; mdxw-preview.el --- Markdown preview via xwidget-webkit -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'json)
(require 'xwidget nil t)

(defgroup mdxw-preview nil
  "Markdown preview via xwidget-webkit."
  :group 'tools)

(defcustom mdxw-preview-render-display t
  "If non-nil, `mdxw-preview-render-current` will display the preview side window."
  :type 'boolean)

(defcustom mdxw-preview-side 'right
  "Which side to show the preview window."
  :type '(choice (const :tag "Right" right)
                 (const :tag "Left" left)
                 (const :tag "Bottom" bottom)
                 (const :tag "Top" top)))

(defcustom mdxw-preview-window-width 0.45
  "Side window width for preview (fraction if 0 < value < 1, else columns).
Used when `mdxw-preview-side` is left/right."
  :type 'number)

(defcustom mdxw-preview-window-height 0.35
  "Side window height for preview (fraction if 0 < value < 1, else lines).
Used when `mdxw-preview-side` is top/bottom."
  :type 'number)

(defcustom mdxw-preview-debounce-seconds 0.35
  "Idle time in seconds before auto refreshing the preview after edits."
  :type 'number)

(defcustom mdxw-preview-window-slot 1
  "Slot for `display-buffer-in-side-window`."
  :type 'integer)

(defcustom mdxw-preview-no-other-window t
  "If non-nil, set side window parameter `no-other-window` to t."
  :type 'boolean)

(defcustom mdxw-preview-no-delete-other-windows t
  "If non-nil, set side window parameter `no-delete-other-windows` to t."
  :type 'boolean)

(defcustom mdxw-preview-refresh-message t
  "If non-nil, `mdxw-preview-refresh` will message \"refreshed\"."
  :type 'boolean)

(defvar-local mdxw--dirty nil
  "Non-nil means current buffer has edits not yet pushed to preview.")

(defvar-local mdxw--update-timer nil
  "Idle timer object for debounced preview update.")

(defvar-local mdxw--dirty-tick 0
  "buffer-chars-modified-tick value when we last marked dirty.")

(defvar-local mdxw--preview-buffer nil
  "Preview buffer associated with current source buffer.")

(defvar-local mdxw--last-line nil
  "Last line number synced to preview.")

(defvar mdxw--global-preview-buffer nil
  "Singleton preview buffer (webkit).")

(defvar mdxw--global-source-buffer nil
  "Current source buffer bound to the singleton preview.")

(defconst mdxw--this-file
  (or load-file-name (buffer-file-name))
  "Path of mdxw-preview.el when loaded.")

(defun mdxw--attach-preview-to-source (src-buf)
  "Bind singleton preview to SRC-BUF: disable hooks on previous source, enable on SRC-BUF."
  (when (and mdxw--global-source-buffer
             (buffer-live-p mdxw--global-source-buffer)
             (not (eq mdxw--global-source-buffer src-buf)))
    (with-current-buffer mdxw--global-source-buffer
      (mdxw--disable-scroll-sync)
      (mdxw--disable-auto-refresh)
      ;; 只是不再綁定，不必 kill preview
      (setq-local mdxw--preview-buffer nil)))

  (setq mdxw--global-source-buffer src-buf)

  (with-current-buffer src-buf
    ;; 讓現有邏輯繼續使用 mdxw--preview-buffer，但實際指向全域唯一 preview
    (setq-local mdxw--preview-buffer mdxw--global-preview-buffer)
    (mdxw--enable-auto-refresh)
    (mdxw--enable-scroll-sync)
    ;; 更新 xwidget callback 裡用到的 source buffer 指標
    (when (and mdxw--global-preview-buffer (buffer-live-p mdxw--global-preview-buffer))
      (mdxw--set-xwidget-callback mdxw--global-preview-buffer src-buf))
    ;; 切換綁定後，立刻刷新一次內容
    (mdxw-preview-refresh)))

(defun mdxw--package-root ()
  "Return the directory that contains mdxw-preview.el."
  (file-name-directory
   (or mdxw--this-file
       (locate-library "mdxw-preview")
       (user-error "mdxw-preview: cannot locate library path"))))

(defun mdxw--assets-dir ()
  "Return absolute directory path of assets/."
  (expand-file-name "assets/" (mdxw--package-root)))

(defun mdxw--assets-file (rel)
  "Return absolute file path under assets/ given REL like \"preview.html\"."
  (expand-file-name rel (mdxw--assets-dir)))

(defun mdxw--ensure-xwidget-webkit ()
  "Ensure xwidget-webkit is available."
  (unless (featurep 'xwidget)
    (error "mdxw-preview: feature 'xwidget is not available (Emacs not built with xwidgets?)"))
  (unless (or (fboundp 'xwidget-webkit-new-session)
              (fboundp 'xwidget-webkit-browse-url))
    (error "mdxw-preview: xwidget webkit functions are not available in this build")))

(defun mdxw--preview-uri ()
  "Return file:// URI to assets/preview.html (absolute, stable)."
  (let ((html (mdxw--assets-file "preview.html")))
    (unless (file-exists-p html)
      (user-error "mdxw-preview: missing %s (assets not found?)" html))
    (concat "file://" (expand-file-name html))))

(defun mdxw--display-side-window (buf)
  "Display BUF in a side window and return the window."
  (let* ((win-params `((no-other-window . ,(and mdxw-preview-no-other-window t))
                       (no-delete-other-windows . ,(and mdxw-preview-no-delete-other-windows t))))
         (params
          (append
           `((side . ,mdxw-preview-side)
             (slot . ,mdxw-preview-window-slot)
             (window-parameters . ,win-params))
           (if (memq mdxw-preview-side '(left right))
               `((window-width . ,mdxw-preview-window-width))
             `((window-height . ,mdxw-preview-window-height))))))
    (display-buffer-in-side-window buf params)))

(defun mdxw--webkit-buffer-for-uri (uri)
  "Create a fresh webkit buffer for URI without disturbing current windows.
Return the created buffer."
  (mdxw--ensure-xwidget-webkit)
  (let (buf)
    (save-window-excursion
      (save-current-buffer
        (cond
         ((fboundp 'xwidget-webkit-new-session)
          (xwidget-webkit-new-session uri)
          (setq buf (current-buffer)))
         (t
          (let ((ret (xwidget-webkit-browse-url uri t)))
            (setq buf (if (bufferp ret) ret (current-buffer))))))))
    (unless (buffer-live-p buf)
      (error "mdxw-preview: failed to create webkit buffer"))
    buf))

(defun mdxw--json-string (s)
  "Return JSON string literal for S (including quotes), safe for embedding into JS."
  (cond
   ((fboundp 'json-serialize-string) (json-serialize-string s))
   ((fboundp 'json-encode-string) (json-encode-string s))
   (t (concat "\"" (replace-regexp-in-string "\"" "\\\\\"" (or s "")) "\""))))

(defun mdxw--webkit-session (webkit-buf)
  "Return current xwidget webkit session object for WEBKIT-BUF."
  (with-current-buffer webkit-buf
    (unless (fboundp 'xwidget-webkit-current-session)
      (error "mdxw-preview: missing xwidget-webkit-current-session (xwidget.el not loaded?)"))
    (xwidget-webkit-current-session)))

(defun mdxw--webkit-exec (webkit-buf js)
  "Execute JS in WEBKIT-BUF."
  (let ((xw (mdxw--webkit-session webkit-buf)))
    (xwidget-webkit-execute-script xw js)))

(defun mdxw--set-xwidget-callback (webkit-buf src-buf)
  "Attach xwidget callback to WEBKIT-BUF; SRC-BUF is the markdown source buffer."
  (with-current-buffer webkit-buf
    (let ((xw (mdxw--webkit-session webkit-buf)))
      (xwidget-put xw 'mdxw-source-buffer src-buf)
      (xwidget-put xw 'callback #'mdxw--xwidget-callback))))

(defun mdxw--xwidget-callback (xw kind)
  "Handle xwidget events for MDXW."
  (when (eq kind 'load-changed)
    (let ((status (nth 3 last-input-event)))
      (when (and (stringp status) (string= status "load-finished"))
        (let ((src-buf (xwidget-get xw 'mdxw-source-buffer)))
          (when (buffer-live-p src-buf)
            (with-current-buffer src-buf
              (when (and mdxw--preview-buffer (buffer-live-p mdxw--preview-buffer))
                (mdxw-preview-refresh)))))))))

;;;###autoload
(defun mdxw-preview-open ()
  "Open singleton preview in a side window for current buffer."
  (interactive)
  (when (minibufferp)
    (user-error "mdxw-preview: not in minibuffer"))

  (let* ((src-win (selected-window))
         (src-buf (current-buffer)))

    ;; 1) Ensure singleton preview exists
    (unless (and mdxw--global-preview-buffer (buffer-live-p mdxw--global-preview-buffer))
      (let* ((uri (mdxw--preview-uri))
             (webkit-buf (mdxw--webkit-buffer-for-uri uri)))
        (setq mdxw--global-preview-buffer webkit-buf)
        (with-current-buffer webkit-buf
          (setq-local mode-line-format '(" mdxw-preview "))
          (setq-local cursor-type nil)
          (setq-local buffer-read-only t))))

    ;; 2) Display it (always reuse)
    (let ((side-win (mdxw--display-side-window mdxw--global-preview-buffer)))
      (when (window-live-p side-win)
        (set-window-buffer side-win mdxw--global-preview-buffer)))

    ;; 3) Restore focus to source window
    (when (window-live-p src-win)
      (set-window-buffer src-win src-buf)
      (select-window src-win))

    ;; 4) Attach this source buffer to the singleton preview
    (mdxw--attach-preview-to-source src-buf)))

;;;###autoload
(defun mdxw-preview-close ()
  "Close singleton preview side window and kill its buffer."
  (interactive)
  ;; 先解除目前 source 的 hooks
  (when (and mdxw--global-source-buffer (buffer-live-p mdxw--global-source-buffer))
    (with-current-buffer mdxw--global-source-buffer
      (mdxw--disable-scroll-sync)
      (mdxw--disable-auto-refresh)
      (setq-local mdxw--preview-buffer nil)))
  (setq mdxw--global-source-buffer nil)

  ;; 關掉 preview window + kill preview buffer
  (when (and mdxw--global-preview-buffer (buffer-live-p mdxw--global-preview-buffer))
    (let* ((pb mdxw--global-preview-buffer)
           (win (get-buffer-window pb t)))
      (when (window-live-p win)
        (delete-window win))
      (kill-buffer pb)))
  (setq mdxw--global-preview-buffer nil))

;;;###autoload
(defun mdxw-preview-toggle ()
  "Toggle singleton preview side window."
  (interactive)
  (if (and mdxw--global-preview-buffer (buffer-live-p mdxw--global-preview-buffer))
      (mdxw-preview-close)
    (mdxw-preview-open)))

;;;###autoload
(defun mdxw-preview-refresh ()
  "Send current buffer text to preview via window.updateContent(...)."
  (interactive)
  (unless (and mdxw--preview-buffer (buffer-live-p mdxw--preview-buffer))
    (user-error "mdxw-preview: preview not open; run M-x mdxw-preview-toggle first"))
  (let* ((text (buffer-substring-no-properties (point-min) (point-max)))
         (payload (mdxw--json-string text))
         (js (concat "window.updateContent(" payload ");")))
    (mdxw--webkit-exec mdxw--preview-buffer js)
    (when mdxw-preview-refresh-message
      (message "mdxw-preview: refreshed"))))

;;;###autoload
(defun mdxw-preview-render-current (&optional no-display)
  "Render current buffer into the singleton webkit preview.

With prefix arg (or when NO-DISPLAY is non-nil), do not display the preview window."
  (interactive "P")
  (when (minibufferp)
    (user-error "mdxw-preview: not in minibuffer"))

  (let* ((src-win (selected-window))
         (src-buf (current-buffer))
         (pb (mdxw--ensure-global-preview)))

    ;; 需要的話顯示 side window
    (unless (or no-display (not mdxw-preview-render-display))
      (let ((side-win (mdxw--display-side-window pb)))
        (when (window-live-p side-win)
          (set-window-buffer side-win pb))))

    ;; 恢復焦點到 source window（跟你原本 open 行為一致）
    (when (window-live-p src-win)
      (set-window-buffer src-win src-buf)
      (select-window src-win))

    ;; attach + refresh
    (mdxw--attach-preview-to-source src-buf)
    (mdxw-preview-refresh)))

(defun mdxw--cancel-update-timer ()
  (when (timerp mdxw--update-timer)
    (cancel-timer mdxw--update-timer))
  (setq-local mdxw--update-timer nil))

(defun mdxw--schedule-refresh ()
  "Schedule a debounced refresh if preview is open."
  (mdxw--cancel-update-timer)
  (setq-local mdxw--update-timer
              (run-with-idle-timer
               mdxw-preview-debounce-seconds
               nil
               #'mdxw--maybe-refresh
               (current-buffer)
               mdxw--dirty-tick)))

(defun mdxw--maybe-refresh (src-buf scheduled-tick)
  "Refresh preview if SRC-BUF is still live and hasn't changed since SCHEDULED-TICK."
  (when (buffer-live-p src-buf)
    (with-current-buffer src-buf
      (when (and (buffer-live-p mdxw--preview-buffer)
                 mdxw--dirty
                 (= scheduled-tick (buffer-chars-modified-tick)))
        (condition-case err
            (progn
              (mdxw-preview-refresh)
              (setq-local mdxw--dirty nil))
          (error
           (message "mdxw-preview: auto refresh failed: %S" err))))
      (setq-local mdxw--update-timer nil))))

(defun mdxw--after-change (_beg _end _len)
  "Mark dirty and schedule a debounced refresh."
  (when (and mdxw--preview-buffer (buffer-live-p mdxw--preview-buffer))
    (setq-local mdxw--dirty t)
    (setq-local mdxw--dirty-tick (buffer-chars-modified-tick))
    (mdxw--schedule-refresh)))

(defun mdxw--enable-auto-refresh ()
  "Enable auto refresh hooks for current buffer."
  (add-hook 'after-change-functions #'mdxw--after-change nil t)
  (add-hook 'kill-buffer-hook #'mdxw--disable-auto-refresh nil t))

(defun mdxw--disable-auto-refresh ()
  "Disable auto refresh hooks for current buffer."
  (remove-hook 'after-change-functions #'mdxw--after-change t)
  (remove-hook 'kill-buffer-hook #'mdxw--disable-auto-refresh t)
  (mdxw--cancel-update-timer)
  (setq-local mdxw--dirty nil))

(defun mdxw--sync-cursor-line ()
  "If line number changed, ask preview to scroll to that line."
  (when (and mdxw--preview-buffer (buffer-live-p mdxw--preview-buffer))
    (let ((line (line-number-at-pos)))
      (unless (equal line mdxw--last-line)
        (setq-local mdxw--last-line line)
        (condition-case err
            (mdxw--webkit-exec mdxw--preview-buffer
                              (format "window.synCursor(%d);" line))
          (error
           (message "mdxw-preview: synCursor failed: %S" err)))))))

(defun mdxw--enable-scroll-sync ()
  "Enable point->preview scroll sync for current buffer."
  (setq-local mdxw--last-line nil)
  (add-hook 'post-command-hook #'mdxw--sync-cursor-line nil t))

(defun mdxw--disable-scroll-sync ()
  "Disable point->preview scroll sync for current buffer."
  (remove-hook 'post-command-hook #'mdxw--sync-cursor-line t)
  (setq-local mdxw--last-line nil))

(defun mdxw--ensure-global-preview ()
  "Ensure singleton preview buffer exists; return it."
  (unless (and mdxw--global-preview-buffer
               (buffer-live-p mdxw--global-preview-buffer))
    (let* ((uri (mdxw--preview-uri))
           (webkit-buf (mdxw--webkit-buffer-for-uri uri)))
      (setq mdxw--global-preview-buffer webkit-buf)
      (with-current-buffer webkit-buf
        (setq-local mode-line-format '(" mdxw-preview "))
        (setq-local cursor-type nil)
        (setq-local buffer-read-only t))))
  mdxw--global-preview-buffer)

(provide 'mdxw-preview)
;;; mdxw-preview.el ends here
