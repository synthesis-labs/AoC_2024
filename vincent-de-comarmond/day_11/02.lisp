(ql:quickload "str")

(defun blink (input-list)
  (nreverse
   (let ((output-list '()))
   (dolist (item input-list output-list)
     (let* ((str-rep (write-to-string item))
	    (str-len (length str-rep)))
       (cond
	 ((= 0 item) (push 1 output-list))
	 ((= 0 (mod str-len 2))
	  (push (parse-integer (subseq str-rep 0 (/ str-len 2))) output-list)
	  (push (parse-integer (subseq str-rep (/ str-len 2))) output-list))
	 (t (push (* item 2024) output-list))))))))

(with-open-file (stream "./input.txt")
  (let ((line (mapcar 'parse-integer (str:split #\Space (read-line stream nil)))))
    (dotimes (i 75)
      (format t "Blinking ~d times.~%" (1+ i))
      (setf line (blink line)))
    (format t "~d~%" (length line))))
