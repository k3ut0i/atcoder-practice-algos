(defun read-line-nums (&optional (stream *standard-input*))
  (read-from-string (concatenate 'string "(" (read-line stream) ")")))

(defun main ()
  (destructuring-bind (n m) (read-line-nums)
    (let ((a (read-line-nums))
          (b (read-line-nums)))
      ;; XXX
      )))
