(ql:quickload "str")

(defun expect-m (input-char)
  (eql input-char #\m))

(defun digit-or-comma (input-char)
  (or (digit-char-p input-char) (eql #\,)))

(defun parse-stream (input-stream expectation-func
				  &optional (accumulator 0) (num-buf-1 "") (num-buf-2 ""))
  (let ((char (read-char stream nil))
	(stream input-stream)
	(accum accumulator))
    (cond
      ((null char) accumulator)
      ;; reset if we don't read the expected character
      ((not (funcall expectation-func char)) 
       (parse-stream stream (lambda (x) (eql x #\m)) accum))
      ;; We've read the expected character
      ((eql char #\m)
       (parse-stream stream (lambda (x) (eql x #\u)) accum))

      ((eql char #\u)
       (parse-stream stream (lambda (x) (eql x #\l)) accum))

      ((eql char #\l)
       (parse-stream stream (lambda (x) (eql x #\()) accum))

      ((eql char #\()
       (parse-stream stream (lambda (x) (digit-char-p x)) accum))

      ((and (digit-char-p char) (= 0 (length num-buf 1)))
       (parse-stream stream digit-or-comma accum (string char)))

      ((and (digit-char-p char) (= 0 (length num-buf 1)))
       (parse-stream stream digit-or-comma accum (string char)))

      
      
     )))


(with-open-file (stream "./input.txt")
  (do ((char (read-char stream nil) (read-char stream nil)))
      ((not char)) ;; not end of stream
    (format t "~a~%" char)))


		
  ;; (let* ((list1 '())
  ;; 	 (list2 '()))
  ;;   (do ((line (read-line stream nil) (read-line stream nil)))
  ;; 	((not line)) ;; not nil ... i.e. a line has been read from the stream
  ;;     (let ((tmp (str:split #\Space line :omit-nulls 'true)))
  ;; 	(push (parse-integer (first tmp)) list1)
  ;; 	(push (parse-integer (second tmp)) list2)))
  ;;   (let ((list1 (sort list1 #'<))
  ;; 	  (list2 (sort list2 #'<)))
  ;;     (print (reduce '+ (mapcar 'abs (mapcar '- list1 list2)))))))
