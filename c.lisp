#+brute-version
(defun floor-sum (n m a b)
  "Brute force/version"
  (loop :for i :from 0 :below n
        :sum (floor (/ (+ (* a i) b) m))))

#-brute-version
(defun floor-sum (n m a b)
  (let ((direct-sum (/ (+ (* n b) (/ (* n (- n 1) a) 2)) m)))
    ;; XXX:
    ))

(defun main ()
  (flet ((read-case ()
           (values (read) (read) (read) (read))))
    (let ((ncases (read)))
      (dotimes (i ncases)
        (multiple-value-bind (n m a b) (read-case)
          (format t "~A~%" (floor-sum n m a b)))))))
