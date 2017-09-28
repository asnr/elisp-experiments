;; -*- lexical-binding: t; -*-

(defun update-stopwatch-function (stopwatch-buffer)
  (let ((elapsed-seconds 0))
    (lambda ()
      (with-current-buffer stopwatch-buffer
        (setq elapsed-seconds (1+ elapsed-seconds))
        (insert (number-to-string elapsed-seconds))))))

(defun buffers= (buffer other-buffer)
  (if (string= (buffer-name buffer) (buffer-name other-buffer))
      t
    nil))

(progn
  (setq stopwatch-buffer (generate-new-buffer "stopwatch"))
  (with-current-buffer stopwatch-buffer
    (insert (number-to-string 0)))
  (setq new-window (split-window))
  (set-window-buffer new-window stopwatch-buffer)
  (let ((stopwatch-timer
         (run-at-time t 1 (update-stopwatch-function stopwatch-buffer))))
    (add-hook 'kill-buffer-hook
              (lambda () (when (buffers= (current-buffer) stopwatch-buffer)
                           (print (concat "Stop timer at buffer " (buffer-name)))
                           (cancel-timer stopwatch-timer))))))
