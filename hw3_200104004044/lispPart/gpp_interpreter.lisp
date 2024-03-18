;;
;; Atakan Kurt 200104004044
;;

(load "gpp_lexer.lisp")
(gppinterpreter "input.txt")
(defvar tokenType (list))
(defvar valueList (list))
(setq tokenType (mapcar #'(lambda (token)
                           (if (string= token "VALUEF")
                               (pop valueList)
                               token))
                       tokenType))


(defun parse-fraction (str)
  (loop for char across str
        for digit = (- (char-code char) (char-code #\0))
        for result = (if result (+ (* result 10) digit) digit)
        finally (return result)))

(defun plus-operation (fraction1 fraction2)
  (let* ((numerator1 (parse-fraction (subseq fraction1 0 (position #\b fraction1))))
         (denominator1 (parse-fraction (subseq fraction1 (1+ (position #\b fraction1)))))
         (numerator2 (parse-fraction (subseq fraction2 0 (position #\b fraction2))))
         (denominator2 (parse-fraction (subseq fraction2 (1+ (position #\b fraction2)))))
         (common-denominator (lcm denominator1 denominator2)))
    (format nil "~db~d" (+ (* numerator1 (/ common-denominator denominator1))
                            (* numerator2 (/ common-denominator denominator2)))
            common-denominator)))

(defun sub-operation (fraction1 fraction2)
  (let* ((sign1 (if (string= (subseq fraction1 0 1) "-") -1 1))
         (sign2 (if (string= (subseq fraction2 0 1) "-") -1 1))
         (numerator1 (parse-fraction (subseq fraction1 (if (= sign1 -1) 1 0) (position #\b fraction1))))
         (denominator1 (parse-fraction (subseq fraction1 (1+ (position #\b fraction1)))))
         (numerator2 (parse-fraction (subseq fraction2 (if (= sign2 -1) 1 0) (position #\b fraction2))))
         (denominator2 (parse-fraction (subseq fraction2 (1+ (position #\b fraction2)))))
         (common-denominator (lcm denominator1 denominator2)))
    (format nil "~db~d" (- (* sign1 numerator1 (/ common-denominator denominator1))
                            (* sign2 numerator2 (/ common-denominator denominator2)))
            common-denominator)))

(defun divide-operation (fraction1 fraction2)
  (let* ((numerator1 (parse-fraction (subseq fraction1 0 (position #\b fraction1))))
         (denominator1 (parse-fraction (subseq fraction1 (1+ (position #\b fraction1)))))
         (numerator2 (parse-fraction (subseq fraction2 0 (position #\b fraction2))))
         (denominator2 (parse-fraction (subseq fraction2 (1+ (position #\b fraction2)))))
         (result-numerator (* numerator1 denominator2))
         (result-denominator (* denominator1 numerator2)))
    (format nil "~db~d" result-numerator result-denominator)))

(defun multiply-operation (fraction1 fraction2)
  (let* ((numerator1 (parse-fraction (subseq fraction1 0 (position #\b fraction1))))
         (denominator1 (parse-fraction (subseq fraction1 (1+ (position #\b fraction1)))))
         (numerator2 (parse-fraction (subseq fraction2 0 (position #\b fraction2))))
         (denominator2 (parse-fraction (subseq fraction2 (1+ (position #\b fraction2)))))
         (result-numerator (* numerator1 numerator2))
         (result-denominator (* denominator1 denominator2)))
    (format nil "~db~d" result-numerator result-denominator)))
    
(defun evaluate_expression (expression)
  (destructuring-bind (operand arg1 arg2) expression
    (let ((value1 (if (listp arg1) (evaluate_expression arg1) arg1))
          (value2 (if (listp arg2) (evaluate_expression arg2) arg2)))
      (case (intern operand)
        ((|OP_PLUS|) (plus-operation value1 value2))
        ((|OP_MINUS|) (sub-operation value1 value2))
        ((|OP_DIV|) (divide-operation value1 value2))
        ((|OP_MULT|) (multiply-operation value1 value2))
        (t (error "Unsupported operand"))))))

(defun split-keywords (kw-list)
  (labels ((split-helper (remaining acc)
             (cond
               ((null remaining) (values (reverse acc) nil))
               ((string= (car remaining) "OP_OP")
                (multiple-value-bind (values remaining-tail)
                    (split-helper (cdr remaining) nil)
                  (split-helper remaining-tail (cons values acc))))
               ((string= (car remaining) "OP_CP")
                (values (reverse acc) (cdr remaining)))
               (t
                (split-helper (cdr remaining) (cons (car remaining) acc))))))
    (multiple-value-bind (operands remaining-tail) (split-helper kw-list nil)
      (values (cons (car operands) (cadr operands)) remaining-tail))))

;; Example usage:
(let* ((result (split-keywords tokenType))
       (expression (car result))
       (remaining-tail (cdr result)))
  (format t "~a~%" (evaluate_expression expression)))