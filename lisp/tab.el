;; --- Smart TAB cycling for icomplete/fido (minibuffer only) ---
;; ==== Tabs ====
(setq-default tab-width 4)
(setq-default standard-indent 4)

(defun my/icomplete--candidate-count ()
  "Return number of completion candidates for current minibuffer contents."
  (let* ((base (minibuffer-contents-no-properties))
         (table minibuffer-completion-table)
         (pred  minibuffer-completion-predicate)
         (cands (all-completions base table pred)))
    (length cands)))

(defun my/icomplete-smart-tab ()
  "If multiple candidates, cycle using icomplete UI; if single, do real completion."
  (interactive)
  (if (> (my/icomplete--candidate-count) 1)
      (icomplete-forward-completions)
    (minibuffer-complete)))

(defun my/icomplete-smart-backtab ()
  "Cycle backward in icomplete UI when multiple candidates; otherwise complete."
  (interactive)
  (if (> (my/icomplete--candidate-count) 1)
      (icomplete-backward-completions)
    (minibuffer-complete)))

(with-eval-after-load 'icomplete
  (define-key icomplete-minibuffer-map (kbd "TAB") #'my/icomplete-smart-tab)
  (define-key icomplete-minibuffer-map (kbd "M-j") #'my/icomplete-smart-tab)
  (define-key icomplete-minibuffer-map (kbd "M-k") #'my/icomplete-smart-backtab)
  (define-key icomplete-minibuffer-map (kbd "C-i") #'my/icomplete-smart-tab)       ;; terminal TAB
  (define-key icomplete-minibuffer-map (kbd "<backtab>") #'my/icomplete-smart-backtab))

(provide 'tab)
