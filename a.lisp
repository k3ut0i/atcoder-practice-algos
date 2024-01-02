(defun read-line-nums ()
  (read-from-string (concatenate 'string "(" (read-line) ")")))

(defstruct dsu
  "Simple array to keep track of parent pointer trees.
Ideally I should also store the rank or size of each tree indexed by root."
  array)

(defun dsu-new (n)
    "Make a DSU with elements numbered 1..N"
  (let ((a (make-array (1+ n) :element-type 'fixnum :initial-element 0)))
    (dotimes (i (1+ n) (make-dsu :array a))
      (setf (aref a i) 0))))

(defun dsu-find! (s e)
  "Find the root of the element E in Disjoin-set union S.
Also modifies S for effective find."
  (declare (type dsu s))
  (let* ((a (dsu-array s))
         (root (do ((r e (aref a r)))
                   ((= (aref a r) 0) r))))
    (do ((r e))
        ((= (aref a r) 0) root)
      (let ((parent (aref a r)))
        (setf (aref a r) root
              r parent)))))


(defun dsu-union! (s e1 e2)
  (declare (type dsu s))
  "In S Merge the sets which contain E1 and E2."
  (let ((r1 (dsu-find! s e1))
        (r2 (dsu-find! s e2))
        (a (dsu-array s)))
    (cond ((= r1 r2) :equal)
          ((< r1 r2) (setf (aref a r2) r1) :left)
          ((> r1 r2) (setf (aref a r1) r2) :right))))

(defun main ()
  (let* ((n (read))
         (q (read))
         (s (dsu-new n)))
    (dotimes (i q)
      (destructuring-bind (query-type v1t v2t) (read-line-nums)
        (let ((v1 (1+ v1t))  ;; Nodes are numbered 0..N-1. So I am shifting them by1.
              (v2 (1+ v2t)))
          (if (= 0 query-type)
              (dsu-union! s v1 v2)
              (format t "~D~%" (if (= (dsu-find! s v1) (dsu-find! s v2)) 1 0))))))))
(main)
