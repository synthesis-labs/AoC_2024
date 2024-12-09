(ql:quickload "str")

(defun parse-stream (input-stream num-buf-1 num-buf-2)



  )


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
