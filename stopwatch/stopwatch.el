;; -*- lexical-binding: t; -*-

(defun make-stopwatch-updater (stopwatch-buffer)
  (let ((elapsed-seconds 0))
    (lambda ()
      (with-current-buffer stopwatch-buffer
        (erase-buffer)
        (setq elapsed-seconds (1+ elapsed-seconds))
        (insert (number-to-string elapsed-seconds))))))

(defun buffers= (buffer other-buffer)
  (if (string= (buffer-name buffer) (buffer-name other-buffer))
      t
    nil))

(defun make-stopwatch-finaliser (stopwatch-buffer stopwatch-timer)
  (lambda ()
    (when (buffers= (current-buffer) stopwatch-buffer)
      (print (concat "Stop timer at buffer " (buffer-name)))
      (cancel-timer stopwatch-timer))))

(defun start-stopwatch (stopwatch-buffer)
  (let ((stopwatch-timer
         (run-at-time t 1 (make-stopwatch-updater stopwatch-buffer))))
    (add-hook 'kill-buffer-hook
              (make-stopwatch-finaliser stopwatch-buffer stopwatch-timer))))

(progn
  (setq stopwatch-buffer (generate-new-buffer "stopwatch"))
  (with-current-buffer stopwatch-buffer
    (insert (number-to-string 0)))
  (setq new-window (split-window))
  (set-window-buffer new-window stopwatch-buffer)
  (start-stopwatch stopwatch-buffer)
  (select-window new-window))
