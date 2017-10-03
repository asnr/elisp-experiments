;; -*- lexical-binding: t; -*-

(define-derived-mode stopwatch-mode special-mode "Stopwatch"
  "Major mode for stopwatch."
  ;; Should try and make this read only for the user but then how do we update
  ;; The buffer on every tick without throwing an error?
  (read-only-mode 0))

(define-key stopwatch-mode-map "q" 'kill-buffer-and-window)

;; By default, the evil normal mode keymaps will take precedence over the
;; stopwatch-mode-map and "q" will resolve to evil-record-macro. Make evil use
;; the Emacs state for stopwatch mode.
(add-to-list 'evil-emacs-state-modes 'stopwatch-mode)

(defun make-stopwatch-updater (stopwatch-buffer)
  (let ((elapsed-seconds 0))
    (lambda ()
      (with-current-buffer stopwatch-buffer
        (delete-region 19 (point-max))
        (setq elapsed-seconds (1+ elapsed-seconds))
        (insert (number-to-string elapsed-seconds))))))

(defun buffers= (buffer other-buffer)
  (string= (buffer-name buffer) (buffer-name other-buffer)))

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

(defun initialise-stopwatch-buffer-and-set-mode (stopwatch-buffer)
  (with-current-buffer stopwatch-buffer
    (insert "Press 'q' to quit\n")
    (insert (number-to-string 0))
    (stopwatch-mode)))

(progn
  (setq stopwatch-buffer (generate-new-buffer "stopwatch"))
  (initialise-stopwatch-buffer-and-set-mode stopwatch-buffer)
  (setq stopwatch-window (split-window))
  (set-window-buffer stopwatch-window stopwatch-buffer)
  (start-stopwatch stopwatch-buffer)
  (select-window stopwatch-window))
