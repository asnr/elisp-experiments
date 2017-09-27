(defvar stopwatch-elapsed-seconds 0)

(defun update-stopwatch (stopwatch-buffer)
  (with-current-buffer stopwatch-buffer
    (erase-buffer)
    (setq stopwatch-elapsed-seconds (1+ stopwatch-elapsed-seconds))
    (insert (number-to-string stopwatch-elapsed-seconds))))

(progn
  (setq stopwatch-buffer (generate-new-buffer "stopwatch"))
  (with-current-buffer stopwatch-buffer
    (insert (number-to-string stopwatch-elapsed-seconds)))
  (setq new-window (split-window))
  (set-window-buffer new-window stopwatch-buffer)
  (run-at-time "3 sec" nil 'update-stopwatch stopwatch-buffer))
