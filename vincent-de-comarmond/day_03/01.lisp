(ql:quickload "str")

(defun digit-or-comma (input-char)
  (or (digit-char-p input-char) (eql #\, input-char)))

(defun digit-or-close (input-char)
  (or (digit-char-p input-char) (eql #\) input-char)))

(defun parse-stream (input-stream expectation-func
				  &optional (accumulator 0) (num-buf "") (num-1 nil))
  (let ((char (read-char input-stream nil))
	(stream input-stream)
	(accum accumulator))
    (cond
      ;; Return the accumulated value if we hit the end of the stream
      ((null char) accumulator)

      ;; reset if we don't read the expected character
      ((not (funcall expectation-func char))
       (parse-stream stream (lambda (x) (eql x #\m)) accum))

      ((eql char #\m)
       (parse-stream stream (lambda (x) (eql x #\u)) accum))

      ((eql char #\u)
       (parse-stream stream (lambda (x) (eql x #\l)) accum))

      ((eql char #\l)
       (parse-stream stream (lambda (x) (eql x #\()) accum))

      ((eql char #\()
       (parse-stream stream (lambda (x) (digit-char-p x)) accum))

      ((and (digit-char-p char) (null num-1))
       (parse-stream stream 'digit-or-comma accum (str:concat num-buf (string char))))

      ((eql char #\,)
       (parse-stream stream (lambda (x) (digit-char-p x)) accum "" (parse-integer num-buf)))

      ((and (digit-char-p char) (not (null num-1)))
       (parse-stream stream 'digit-or-close accum (str:concat num-buf (string char)) num-1))

      ((eql char #\))
       ;; (format t "        Adding ~d x ~d to ~d:~%" num-1 num-buf accum)
       (parse-stream stream (lambda (x) (eql x #\m)) (+ accum (* num-1 (parse-integer num-buf))) ""))

      (t (parse-stream stream (lambda (x) (eql x #\m)) accum))))) ;; default - i.e. reset if anything else happens


(with-open-file (stream "./input.txt")
    (format t "~d~%" (parse-stream stream (lambda (c) (eql #\m c)))))
