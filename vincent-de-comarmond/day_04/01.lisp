(defun cons-sum (a b)
  (cons (+ (car a) (car b)) (+ (cdr a) (cdr b))))

(defun get-gridpoints (loc dir)
  (let ((cur loc) (gridpoints '()))
    (dotimes (n 3)
      (setf cur (cons-sum cur dir))
      (push cur gridpoints))
    (nreverse gridpoints)))

(let ((grid (make-hash-table :test 'equal))
      (total 0))

  (with-open-file (stream "./input.txt")
    (loop for line = (read-line stream nil)
	  for r from 0
	  while line do
	    (loop for c from 0 to (1- (length line)) do
	      (setf (gethash (cons r c) grid) (aref line c)))))

  (maphash (lambda (key val)
	     (if (eq val #\X)
		 (loop for dir in '((-1 . 0) (-1 . 1) (0 . 1) (1 . 1)
				    (1 . 0) (1 . -1) (0 . -1) (-1 . -1)) do
		   (if (equalp
			(mapcar (lambda (d) (gethash d grid)) (get-gridpoints key dir))
			'(#\M #\A #\S))
		       (setf total (1+ total))))))
	   grid)
  (print total))

