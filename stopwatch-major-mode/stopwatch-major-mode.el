;; -*- lexical-binding: t; -*-

(defun make-stopwatch-updater (stopwatch-buffer)
  (let ((elapsed-seconds 0))
    (lambda ()
      (with-current-buffer stopwatch-buffer
        (erase-buffer)
        (setq elapsed-seconds (1+ elapsed-seconds))
        (let ((inhibit-read-only t))
          (insert (number-to-string elapsed-seconds)))))))

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

(define-derived-mode stopwatch-mode special-mode "Stopwatch"
  "Major mode for stopwatch."
  ;; Should try and make this read only for the user but then how do we update
  ;; the buffer on every tick without throwing an error?
  (read-only-mode 0))

(progn
  (setq stopwatch-buffer (generate-new-buffer "stopwatch"))
  (with-current-buffer stopwatch-buffer
    (insert (number-to-string 0))
    (stopwatch-mode))
  (setq stopwatch-window (split-window))
  (set-window-buffer stopwatch-window stopwatch-buffer)
  (start-stopwatch stopwatch-buffer)
  (select-window stopwatch-window))
