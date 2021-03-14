;;; chn-window-nav.el --- Divide and conquer

(defun prev-window ()
  (interactive)
  (other-window -1))

;; display temporary/help messages in window "1" unless there is only
;; 1 window, then pop up another one using emacs default settings.
(defun right-edge (window) (nth 2 (window-edges window)))
(defun top-edge (window) (nth 1 (window-edges window)))

(defun chn-temp-window (buffer alist)
  (if (= (count-windows) 1)
      (display-buffer-pop-up-window buffer alist)
    (let ((desired-window (-max-by (-on '> 'right-edge) (--filter (= 0 (top-edge it)) (window-list)))))
      (set-window-buffer desired-window buffer)
      desired-window)))

(add-to-list 'display-buffer-alist '("\\*Help\\*" chn-temp-window))
(add-to-list 'display-buffer-alist '("\\*Shell Command Output\\*" chn-temp-window))
(add-to-list 'display-buffer-alist '("\\*Apropos\\*" chn-temp-window))
(add-to-list 'display-buffer-alist '("\\*Ack-and-a-half\\*" chn-temp-window))
(add-to-list 'display-buffer-alist '("\\*ag search" chn-temp-window))
(add-to-list 'display-buffer-alist '("\\*ag dired\\*" chn-temp-window))
(add-to-list 'display-buffer-alist '("\\*elm\\*" chn-temp-window))
(add-to-list 'display-buffer-alist '("\\*elm-make\\*" chn-temp-window))
(add-to-list 'display-buffer-alist '("\\*rg\\*" chn-temp-window))
(add-to-list 'display-buffer-alist '("\\*Occur\\*" chn-temp-window))
(add-to-list 'display-buffer-alist '("\\*Async Shell Command\\*" chn-temp-window))

(setq help-window-select t)

;; Sometimes emacs smart-splitting is a little too smart.
(setq split-height-threshold 90)

(defun four-window-organize ()
  "Create a window layout suitable for large screens"
  (interactive)
  (delete-other-windows (get-buffer-window (current-buffer)))
  (split-window-right)
  (other-window 1)
  (mode-line-other-buffer)
  (split-window-right)
  (other-window 1)
  (mode-line-other-buffer)
  (split-window-vertically)
  (balance-windows)
  (other-window 1)
  (mode-line-other-buffer))

;; I find this much easier than "C-x o" and friends
(global-set-key (kbd "M-]") 'other-window)
(global-set-key (kbd "M-[") 'prev-window)

(provide 'chn-window-nav)
