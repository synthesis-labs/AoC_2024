(defun digit-or-comma (input-char)
  (or (digit-char-p input-char) (eql #\, input-char)))

(defun digit-or-close (input-char)
  (or (digit-char-p input-char) (eql #\) input-char)))

(defun str-char+ (str-in char-in)
  (concatenate 'string str-in (string char-in)))

(defun digit-1 (char-in number-1)
  (and (digit-char-p char-in) (not (null number-1))))

(defun digit-not-1 (char-in number-1)
  (and (digit-char-p char-in) (null number-1)))

(defmacro expect (c)
  `(lambda (x) (eql x ,c)))

(defun parse-nums (char expect-func num-buf num-1)
  (cond
    ((not (funcall expect-func char))
     (lambda (x) (parse-nums x (expect #\m) "" nil)))

    ((eql char #\m)
     (lambda (x) (parse-nums x (expect #\u) "" nil)))

    ((eql char #\u)
     (lambda (x) (parse-nums x (expect #\l) "" nil)))
    
    ((eql char #\l)
     (lambda (x) (parse-nums x (expect #\() "" nil)))

    ((eql char #\()
     (lambda (x) (parse-nums x 'digit-char-p "" nil)))

    ((digit-not-1 char num-1)
     (lambda (x) (parse-nums x 'digit-or-comma (str-char+ num-buf char) nil)))

    ((eql char #\,)
     (lambda (x) (parse-nums x 'digit-char-p "" (parse-integer num-buf))))

    ((digit-1 char num-1)
     (lambda (x) (parse-nums x 'digit-or-close (str-char+ num-buf char) num-1)))

    ((eql char #\)) (* num-1 (parse-integer num-buf)))

    (t (lambda (x) (parse-nums x (expect #\m) "" nil)))))


(with-open-file (stream "./input.txt")
  (let* ((total 0)
	 (num-parse (lambda (x) (parse-nums x (expect #\m) "" nil))))

    (do ((char (read-char stream nil) (read-char stream nil)))
	((not char))
      (let ((tmp-func (funcall num-parse char)))
	(if (numberp tmp-func) (setf total (+ total tmp-func)))
	(if (functionp tmp-func)
	    (setf num-parse tmp-func)
	    (setf num-parse (lambda (x) (parse-nums x (expect #\m) "" nil))))))
    (format t "~d~%" total)))
