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

(defun buffers= (buffer other-buffer)
  (string= (buffer-name buffer) (buffer-name other-buffer)))

(defconst STOPWATCH-INSTRUCTIONS "Press 'q' to quit")

(defun stopwatch-construct (stopwatch-buffer)
  (list (cons 'buffer stopwatch-buffer)
        (cons 'window nil)
        (cons 'timer nil)
        (cons 'elapsed-seconds 0)))

(defun stopwatch-start (stopwatch)
  (stopwatch--init-buffer stopwatch)
  (stopwatch--start stopwatch)
  (stopwatch--open-window stopwatch)
  (stopwatch--attach-stop-hook stopwatch))

(defun stopwatch--init-buffer (stopwatch)
  (with-current-buffer (stopwatch--get-buffer stopwatch)
    (insert STOPWATCH-INSTRUCTIONS)
    (insert "\n")
    (insert (number-to-string (stopwatch--get-elapsed-seconds stopwatch)))
    (stopwatch-mode)))

(defun stopwatch--open-window (stopwatch)
  (let ((window (split-window)))
    (stopwatch--set-window stopwatch window)
    (set-window-buffer window (stopwatch--get-buffer stopwatch))
    (select-window window)))

(defun stopwatch--start (stopwatch)
  (let ((timer (run-at-time t 1 (stopwatch--make-updater stopwatch))))
    (stopwatch--set-timer stopwatch timer)))

(defun stopwatch--make-updater (stopwatch)
  (lambda ()
    (with-current-buffer (stopwatch--get-buffer stopwatch)
      (delete-region 19 (point-max))
      (let ((elapsed-seconds (1+ (stopwatch--get-elapsed-seconds stopwatch))))
        (stopwatch--set-elapsed-seconds stopwatch elapsed-seconds)
        (insert (number-to-string elapsed-seconds))))))

(defun stopwatch--attach-stop-hook (stopwatch)
  (add-hook 'kill-buffer-hook (lambda () (stopwatch--stop stopwatch))))

(defun stopwatch--stop (stopwatch)
  (when (buffers= (current-buffer) (stopwatch--get-buffer stopwatch))
    (print (concat "Stop timer at buffer " (buffer-name)))
    (cancel-timer (stopwatch--get-timer stopwatch))))

(defun stopwatch--get-buffer (stopwatch)
  (cdr (assoc 'buffer stopwatch)))

(defun stopwatch--set-window (stopwatch window)
  (setf (cdr (assoc 'window stopwatch)) window))

(defun stopwatch--get-timer (stopwatch)
  (cdr (assoc 'timer stopwatch)))

(defun stopwatch--set-timer (stopwatch timer)
  (setf (cdr (assoc 'timer stopwatch)) timer))

(defun stopwatch--get-elapsed-seconds (stopwatch)
  (cdr (assoc 'elapsed-seconds stopwatch)))

(defun stopwatch--set-elapsed-seconds (stopwatch elapsed-seconds)
  (setf (cdr (assoc 'elapsed-seconds stopwatch)) elapsed-seconds))

(progn
  (let* ((stopwatch-buffer (generate-new-buffer "stopwatch"))
         (stopwatch (stopwatch-construct stopwatch-buffer)))
    (stopwatch-start stopwatch)))
