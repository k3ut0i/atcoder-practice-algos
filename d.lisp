(defun array-indices-from-row-major (ad i)
  "Given a row-major-index I for an array of dimensions AD,
return the indices which represent I."
  (flet ((f (d acc)
           (let ((idx (car acc)))
             (cons (floor idx d) (cons (mod idx d) (cdr acc))))))
    (let ((ans (reduce #'f ad :from-end t :initial-value (cons i nil))))
      (if (= 0 (car ans))
          (cdr ans)
          (error "The row-major-index is out of bounds: ~S" i)))))

(defun main ()
  (let ((n (read))
        (m (read)))
    (let ((a (make-array (list n m)
                         :element-type 'standard-char
                         :initial-contents
                         (loop :for i :from 0 :below n :collect (read-line)))))
      (print-char-array (max-flow a)))))

(defun print-char-array (a)
  (declare (type (array standard-char *) a))
  (destructuring-bind
      (nrows ncols)
      (array-dimensions a)
    (dotimes (i nrows)
      (dotimes (j ncols)
        (format t "~C" (aref a i j)))
      (format t "~%"))))

(defun max-flow (a)
  (declare (type (array standard-char *) a))
  (destructuring-bind
      (nrows ncols)
      (array-dimensions a)
    (dotimes (i nrows)
      (dotimes (j ncols)
        (format t "~A~%" (valid-neighbours a i j)))))
  a)

(defun opposite-color (c)
  (declare (type standard-char c))
  (case c
    (#\b #\w)
    (#\w #\b)))

(defun make-bipartite (a)
  (declare (type (array standard-char* ) a))
  (let ((b (make-array (array-dimensions a) :element-type 'standard-char
                                            :initial-element #\u)))
    (do ((i 0))
        ((= i (array-total-size b)))
      (if (not (eq (row-major-aref b i) #\u))
          (loop :while (and (< i (array-total-size b))
                            (not (eq (row-major-aref b i) #\u)))
                :do (incf i))
          (progn
            (setf (row-major-aref b i) #\b)
            (do ((q (list (array-indices-from-row-major (array-dimensions b)
                                                        i))))
                ((null q))
              (let* ((current (car q))
                     (current-color (apply #'aref b current))
                     (cneighbours (apply #'valid-neighbours a current)))
                )))))))

(defun valid-neighbours (a i j)
  (remove-if-not (lambda (p)
                   (destructuring-bind (in jn) p
                     (and (array-in-bounds-p a in jn)
                          (eq (aref a in jn) #\.))))
                 (list (list (1+ i) j)
                       (list i (1+ j))
                       (list i (1- j))
                       (list (1- i) j))))
#-swank (main)
