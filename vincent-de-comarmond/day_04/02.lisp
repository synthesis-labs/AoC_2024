(defun cons-sum (a b)
  (cons (+ (car a) (car b)) (+ (cdr a) (cdr b))))

(let ((grid (make-hash-table :test 'equal))
      (total 0))
  
  (with-open-file (stream "./input.txt")
    (loop for line = (read-line stream nil)
	  for r from 0
	  while line do
	    (loop for c from 0 to (1- (length line)) do
	      (setf (gethash (cons r c) grid) (aref line c)))))
  
  (maphash (lambda (key val)
	     (if (eq val #\A)
		 (let ((diag-intersect
			 (apply 'intersection
				(loop for direction in '(((-1 . -1) (1 . 1))
							 ((-1 . 1) (1 . -1)))
				      collect (mapcar
					       (lambda (x) (gethash (cons-sum key x) grid))
					       direction)))))
		   (if (null (set-exclusive-or diag-intersect '(#\M #\S))) (incf total)))))
	   grid)

  (print total))
