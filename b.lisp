(defun read-line-nums ()
  (read-from-string (concatenate 'string "(" (read-line) ")")))

(defun deal-query-brute (a q)
  (if (= 0 (car q))
      (destructuring-bind (qtype p x) q
        (assert (= 0 qtype))
        (incf (aref a p) x))
      (destructuring-bind (qtype l r) q
        (assert (= 1 qtype))
          (format t "~D~%" (loop :for i :from l :below r :sum (aref a i))))))

(defun lsb-exp (n)
  "Return the least sum of 2 power in N.
If N = 2^i + 2^j+... written in ascending order, then return 2^i."
  (logand n (- n)))

(defstruct fenwick-tree
  array)

(defun create-fenwick-tree (a)
  "Convert the array A into a Fenwick Tree."
  (dotimes (i (array-dimension a 0) (make-fenwick-tree :array a))
    (let ((prev-nums (lsb-exp (1+ i))))
      (let ((num (loop :with n = (floor prev-nums 2)
                       :while(> n 0)
                       :sum (aref a (- i n))
                       :do (setf n (floor n 2)))))
        (incf (aref a i) num)))))

(defun ft-update (ft i x)
  "Increase the I th element of array represented by FT by X."
  (do ((n (1+ i) (+ n (lsb-exp n))))
      ((> n (array-dimension (fenwick-tree-array ft) 0)) ft)
    (incf (aref (fenwick-tree-array ft) (1- n)) x)))

(defun ft-sum (ft i)
  "Prefix sum upto I inclusive of the array represented by FT."
  (loop :with n = (1+ i)
        :while (> n 0)
        :sum (aref (fenwick-tree-array ft) (1- n))
        :do (decf n (lsb-exp n))))

(defun deal-query (ft q)
  (if (= 0 (car q))
      (destructuring-bind (qtype p x) q
        (assert (= 0 qtype))
        (ft-update ft p x))
      (destructuring-bind (qtype l r) q
        (assert (= 1 qtype))
        (format t "~D~%" (- (ft-sum ft (1- r)) (ft-sum ft (1- l)))))))

(defun main ()
  (let* ((n (read))
         (q (read))
         (a (make-array n :element-type 'fixnum :initial-element 0)))
    (dotimes (i n)
      (setf (aref a i) (read)))
    (let ((ft (create-fenwick-tree a)))
      (dotimes (i q)
        (deal-query ft (read-line-nums))))))

(main)

