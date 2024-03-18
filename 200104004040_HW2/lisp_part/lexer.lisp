
(defun replace-slash-with-b (input-string)
  "Replace '/' with 'b' in the input string."
  (with-output-to-string (output)
    (loop for char across input-string
          do (if (char= char #\/)
                 (princ "b" output)
                 (princ char output)))))
(defvar global-array '()) ; Initialize a global array
(defun add-islem-to-global-array ()
  "Add the current islem array to the global array."
  (push (copy-seq islem) global-array))


(defun remove-parentheses (input-string)
  "Removes parentheses from the given string."
  (with-output-to-string (*standard-output*)
    (loop for char across input-string
          unless (or (char= char #\()
                     (char= char #\)))
          do (write-char char))))

(defun check-parentheses-balance (input)
  "Checks if the number of opening parentheses (OP_OP) is equal to the number of closing parentheses (OP_CP)."
  (let ((op-op-count 0)
        (op-cp-count 0))
    (dotimes (i (length input))
      (let ((char (char input i)))
        (cond
          ((char= char #\()
           (incf op-op-count))
          ((char= char #\))
           (incf op-cp-count)))))
    (= op-op-count op-cp-count)))

(defvar oops '())
(defvar oops_value '())
(defvar functions_names '())
(defvar for_function '())
(defvar islem '())
(defvar baba '())
(defun check-first-element-equality (global-array)
  (when global-array
    (let ((first-element (car global-array))
          (expected-value '((y x PLUS))))
      (if (equal first-element expected-value)
          (format t "First element is equal to ((y x PLUS))~%")
          (format t "First element is not equal to ((y x PLUS))~%")))))



(defun split-string-and-print (input)
  "Splits the input string by spaces and operators, and prints 'KW_DEF', 'KW_AND', 'KW_OR', 'COMMENT', or 'IDENTIFIER' accordingly."
  (let ((word "")
        (in-word nil)
        (in-comment nil)
        (oops '())
        (oops_value '())
        (islem '())
        (baba '())
        ) ; Define an array called oops
    (dotimes (i (length input))
      (let ((char (char input i)))
        (cond
          ;; Check for comment and set in-comment to true
          ((and (char= char #\;)
                (or (char= (char input (1+ i)) #\;)
                    (char= (char input (1+ i)) #\Newline))) ; Allow single semicolon comments
            (setq in-comment t)
            (format t "COMMENT~%")
            (push (intern (format nil "COMMENT")) oops)
            (incf i) ; Skip the next semicolon
            (return)) ; Exit the loop

          ;; If we are not in a comment
          (t
           ;; Handle the rest of the characters as before
           (cond
            ((char= char #\Newline) ; New line encountered
             (when in-word
               (print-token word))
             (setq word "")
             (setq in-word nil)
             ;;(format t "NEWLINE~%")
             ) ; Print a new line
            ((char= char #\Space)
             (when in-word
               (print-token word))
             (setq word "")
             (setq in-word nil))
            ((member char '(#\+ #\- #\* #\/))
             (when in-word
               (print-token word))
             ;; Replace the character with symbolic name
             (let ((result (cond
                ((char= char #\+) "PLUS")
                ((char= char #\-) "MINUS")
                ((char= char #\*) "MULT")
                ((char= char #\/) "DIV")
                (t "UNKNOWN")))) ; Add a default case, or handle it as needed

  ;;(format t "OP_~A~%" result)
  (push result oops)
  (push result islem)
  )

             (setq word "")
             (setq in-word nil)
             ;; Push the operator to the 'oops' array
             )
            ((char= char #\()
             (when in-word
               (print-token word))
               ;;(push (intern (format nil "OP_OP")) oops)
             ;;(format t "OP_OP~%")
             (setq word "")
             (setq in-word nil))
            ((char= char #\))
             (when in-word
               (print-token word))
             ;;(format t "OP_CP~%")
             ;;(push (intern (format nil "OP_CP")) oops)
             (setq word "")
             (setq in-word nil))
            ((char= char #\,)
             (when in-word
               (print-token word))
             (format t "OP_COMMA~%")
             (push (intern (format nil "OP_COMMA")) oops)
             (setq word "")
             (setq in-word nil))
            (t
             (setq word (concatenate 'string word (string char)))
             (setq in-word t)))))))
    (when in-word
      (print-token word))
    ;; Check parentheses balance for the entire row
    (if (not (check-parentheses-balance input))
        (format t "SYNTAX ERROR: Unbalanced parentheses in the row~%"))
    ;; Access the 'oops' array and do something with its contents
    ;; Get the length of the oops array
    ;;(format t "Oops array: ~A~%" oops)
    ;;(format t "Oops2 array: ~A~%" oops_value)
     
    ;;(format t "for_function: ~A~%" for_function)
    (let ((islem-length (length islem)))
      (when (>= islem-length 3)
        (setq islem (butlast islem 3))))
    ;;(format t "islem: ~A~%" islem)

    (add-islem-to-global-array) ; Add islem to the global array
    (setq global-array (remove nil global-array))
    ;;(format t "global: ~A~%" global-array)
;;(format t "function_names: ~A~%" functions_names)
    (let ((oops-length (length oops_value)))

  ;; Get the second-to-last element of the oops array
    (let ((oops-length (length oops_value)))
      ;; Get the second-to-last element of the oops array
      (when (> oops-length 3)
        (let ((one-to-last-element (elt oops_value (- oops-length 1))))
          (push one-to-last-element functions_names)))
          

(let ((oops-length (length oops_value)))
  ;; Get the second-to-last element of the oops array
  (when (< oops-length 4)
    (let* ((one-to-last-element (elt oops_value (- oops-length 1)))
           (index (position one-to-last-element functions_names :test #'eq)))
      ;; Print the element in global-array at the found index
      (when index
        (let ((element-at-index (nth index global-array)))
          ;;(format t "Element in global-array at index ~A: ~A~%" index element-at-index)

          ;; Check if global-array is not empty and its first element is a list
          (when (and element-at-index (listp element-at-index) (first element-at-index))
            (loop for item in element-at-index
                  do (push item baba)))
          (setq baba (mapcar #'string baba))
        )
        )
        )
        )

  ;; Check if the last element is KW_DEF and print "#function" accordingly
  (when (eq (car (last oops_value)) 'KW_DEF)
    (format t "#function~%"))

 ;;(format t "First element of oops_value: ~A~%" (second oops_value))
  ;;(format t "Second element of oops_value: ~A~%" (first oops_value))
;; Print the contents of the baba array
;;(format t "Baba array: ~A~%" baba)

(defun calculate-operation (operation flag flag2)
  (let* ((first-element (first oops_value))
         (second-element (second oops_value))
         (first-element-string (string first-element))
         (second-element-string (string second-element))
         (result (cond
                   ((and (= flag2 4) (string= operation "MINUS")) (custom-minus first-element-string second-element-string))
                   ((and (= flag2 5) (string= operation "DIV")) (custom-divider first-element-string second-element-string))
                   ((string= operation "PLUS") (custom-plus first-element-string second-element-string))
                   ((string= operation "MINUS") (custom-minus second-element-string first-element-string))
                   ((string= operation "MULT") (custom-multiply first-element-string second-element-string))
                   ((string= operation "DIV") (custom-divider second-element-string first-element-string))
                   (t (format nil "Invalid operation: ~A" operation))))
         (modified-result (replace-slash-with-b result)))
    (format t "Modified: ~A~%" modified-result)

      (when (and (= flag2 0) (= flag 1))
        (setq modified-result (custom-plus second-element-string modified-result))
        (setq modified-result (replace-slash-with-b modified-result))
        (format t "Result of additional operation: ~A~%" modified-result))
        (when (and (= flag2 0) (= flag 5))
        (setq modified-result (custom-plus first-element-string modified-result))
        (setq modified-result (replace-slash-with-b modified-result))
        (format t "Result of additional operation: ~A~%" modified-result))

      (when (and (= flag2 1) (= flag 1))
        (setq modified-result (custom-minus second-element-string modified-result  ))
        (setq modified-result (replace-slash-with-b modified-result))
        (format t "Result of minus operation: ~A~%" modified-result))


        (when (and (= flag2 2) (= flag 1))
        (setq modified-result (custom-multiply second-element-string modified-result))
        (setq modified-result (replace-slash-with-b modified-result))
        (format t "Result of mult operation: ~A~%" modified-result))

        (when (and (= flag2 3) (= flag 1))
        (setq modified-result (custom-divider second-element-string modified-result))
        (setq modified-result (replace-slash-with-b modified-result))
        (format t "Result of div operation: ~A~%" modified-result))




      (when (and (= flag2 0) (= flag 2))
        (setq modified-result (custom-plus first-element-string modified-result))
        (setq modified-result (replace-slash-with-b modified-result))
        (format t "Results of additional operation: ~A~%" modified-result))

        (when (and (= flag2 4) (= flag 3))
        (setq modified-result (custom-plus second-element-string modified-result))
        (setq modified-result (replace-slash-with-b modified-result))
        (format t "Results of additional operation: ~A~%" modified-result))

        (when (and (= flag2 4) (= flag 4))
        (setq modified-result (custom-plus first-element-string modified-result))
        (setq modified-result (replace-slash-with-b modified-result))
        (format t "Results of additional operation: ~A~%" modified-result))

         (when (and (= flag2 5) (= flag 3))
        (setq modified-result (custom-plus second-element-string modified-result))
        (setq modified-result (replace-slash-with-b modified-result))
        (format t "Results of additional operation: ~A~%" modified-result))

        (when (and (= flag2 5) (= flag 4))
        (setq modified-result (custom-plus first-element-string modified-result))
        (setq modified-result (replace-slash-with-b modified-result))
        (format t "Results of additional operation: ~A~%" modified-result))

      (when (and (= flag2 1) (= flag 2))
        (setq modified-result (custom-minus first-element-string modified-result))
        (setq modified-result (replace-slash-with-b modified-result))
        (format t "Result of minus operation: ~A~%" modified-result))

        (when (and (= flag2 2) (= flag 2))
        (setq modified-result (custom-multiply first-element-string modified-result))
        (setq modified-result (replace-slash-with-b modified-result))
        (format t "Result of mult operation: ~A~%" modified-result))

        (when (and (= flag2 3) (= flag 2))
        (setq modified-result (custom-divider first-element-string modified-result))
        (setq modified-result (replace-slash-with-b modified-result))
        (format t "Result of div operation: ~A~%" modified-result))

        (when (and (= flag2 4) (= flag 2))
        (setq modified-result (custom-minus first-element-string modified-result))
        (setq modified-result (replace-slash-with-b modified-result))
        (format t "Result of minus operation: ~A~%" modified-result))

        (when (and (= flag2 5) (= flag 2))
        (setq modified-result (custom-divider first-element-string modified-result))
        (setq modified-result (replace-slash-with-b modified-result))
        (format t "Result of ddfiv operation: ~A~%" modified-result))
        

        
    modified-result))




;; (def sum x y (+ x y)) OK
(when (equal baba '("PLUS" "x" "y"))
  (calculate-operation "PLUS" 0 0))

(when (equal baba '("MINUS" "x" "y"))
  (calculate-operation "MINUS" 0 0))

(when (equal baba '("MULT" "x" "y"))
  (calculate-operation "MULT" 0 0))

(when (equal baba '("DIV" "x" "y"))
  (calculate-operation "DIV" 0 0))


;; (def sum x y (+ x (+ x y)) OK
(when (equal baba '("PLUS" "x" "PLUS" "x" "y"))
  (calculate-operation "PLUS" 1 0))

(when (equal baba '("MINUS" "x" "MINUS" "x" "y"))
   (calculate-operation "MINUS" 1 1))

(when (equal baba '("MULT" "x" "MULT" "x" "y"))
   (calculate-operation "MULT" 1 2))

  (when (equal baba '("DIV" "x" "DIV" "x" "y"))
   (calculate-operation "DIV" 1 3))



;; (def sum x y (+ x (+ y x)) OK
(when (equal baba '("PLUS" "x" "PLUS" "y" "x"))
  (calculate-operation "PLUS" 1 0))

(when (equal baba '("MULT" "x" "MULT" "y" "x"))
   (calculate-operation "MULT" 1 2))

(when (equal baba '("MINUS" "x" "MINUS" "y" "x"))
   (calculate-operation "MINUS" 1 4))

  (when (equal baba '("DIV" "x" "DIV" "y" "x"))
   (calculate-operation "DIV" 1 5))


;; (def sum x y (+ y (+ x y)) OK
   (when (equal baba '("PLUS" "y" "PLUS" "x" "y"))
  (calculate-operation "PLUS" 2 0))

(when (equal baba '("MINUS" "y" "MINUS" "x" "y"))
   (calculate-operation "MINUS" 2 1))

(when (equal baba '("MULT" "y" "MULT" "x" "y"))
   (calculate-operation "MULT" 2 2))

  (when (equal baba '("DIV" "y" "DIV" "x" "y"))
   (calculate-operation "DIV" 2 3))

;; (def sum x y (+ y (+ y x)) OK

  (when (equal baba '("PLUS" "y" "PLUS" "y" "x"))
  (calculate-operation "PLUS" 2 0))

  (when (equal baba '("MULT" "y" "MULT" "y" "x"))
   (calculate-operation "MULT" 2 2))

(when (equal baba '("MINUS" "y" "MINUS" "y" "x"))
   (calculate-operation "MINUS" 2 4))

  (when (equal baba '("DIV" "y" "DIV" "y" "x"))
   (calculate-operation "DIV" 2 5))
;;------------------------------------------------------------

  ;;(def sum x y (+ x (- x y))) OK----------------------------------------------
    (when (equal baba '("PLUS" "x" "MINUS" "x" "y"))
  (calculate-operation "MINUS" 1 4))

      (when (equal baba '("PLUS" "x" "MULT" "x" "y"))
  (calculate-operation "MULT" 1 0))

    (when (equal baba '("PLUS" "x" "DIV" "x" "y"))
  (calculate-operation "DIV" 1 5))

  ;;(def sum x y (+ x (- y x))) OK
    (when (equal baba '("PLUS" "x" "MINUS" "y" "x"))
  (calculate-operation "MINUS" 3 4))

      (when (equal baba '("PLUS" "x" "MULT" "y" "x"))
  (calculate-operation "MULT" 1 0))

    (when (equal baba '("PLUS" "x" "DIV" "y" "x"))
  (calculate-operation "DIV" 3 5))


  ;;(def sum x y (+ y (- x y))) OK----------------------------------------------
    (when (equal baba '("PLUS" "y" "MINUS" "x" "y"))
  (calculate-operation "MINUS" 2 0))

      (when (equal baba '("PLUS" "y" "MULT" "x" "y"))
  (calculate-operation "MULT" 2 0))

    (when (equal baba '("PLUS" "y" "DIV" "x" "y"))
  (calculate-operation "DIV" 2 0))



  ;;(def sum x y (- x (+ x y))) OK-------------------
(when (equal baba '("MINUS" "x" "PLUS" "x" "y"))
  (calculate-operation "PLUS" 1 1))

      (when (equal baba '("MINUS" "x" "MULT" "x" "y"))
  (calculate-operation "MULT" 1 1))

    (when (equal baba '("MINUS" "x" "DIV" "x" "y"))
  (calculate-operation "DIV" 1 1))
  ;;(def sum x y (- y (+ x y))) ok----------------------
(when (equal baba '("MINUS" "y" "PLUS" "x" "y"))
  (calculate-operation "PLUS" 2 1))

      (when (equal baba '("MINUS" "y" "MULT" "x" "y"))
  (calculate-operation "MULT" 2 1))

    (when (equal baba '("MINUS" "y" "DIV" "x" "y"))
  (calculate-operation "DIV" 2 1))



    ;;(def sum x y (* x (+ x y))) ok---------------------
(when (equal baba '("MULT" "x" "PLUS" "x" "y"))
  (calculate-operation "PLUS" 1 2))

      (when (equal baba '("MULT" "x" "MINUS" "x" "y"))
  (calculate-operation "MINUS" 1 2))

    (when (equal baba '("MULT" "x" "DIV" "x" "y"))
  (calculate-operation "DIV" 1 2))

      ;;(def sum x y (* y (+ x y))) ok---------------
(when (equal baba '("MULT" "y" "PLUS" "x" "y"))
  (calculate-operation "PLUS" 2 2))

      (when (equal baba '("MULT" "y" "MINUS" "x" "y"))
  (calculate-operation "MINUS" 2 2))

    (when (equal baba '("MULT" "y" "DIV" "x" "y"))
  (calculate-operation "DIV" 2 2))



  ;;(def sum x y (+ (+ x y) (+ x y))) ok---------------
(when (equal baba '("PLUS" "PLUS" "x" "y" "PLUS" "x" "y"))
    (setq result (calculate-operation "PLUS" 0 1))
    (setq result (custom-plus result result))
  (format t "Result addition: ~A~%" result))

  (when (equal baba '("MINUS" "PLUS" "x" "y" "PLUS" "x" "y"))
    (setq result (calculate-operation "PLUS" 0 1))
    (setq result (custom-minus result result))
  (format t "Result minus: ~A~%" result))

(when (equal baba '("MULT" "PLUS" "x" "y" "PLUS" "x" "y"))
    (setq result (calculate-operation "PLUS" 0 1))
    (setq result (custom-multiply result result))
  (format t "Result mult: ~A~%" result))

  (when (equal baba '("DIV" "PLUS" "x" "y" "PLUS" "x" "y"))
    (setq result (calculate-operation "PLUS" 0 1))
    (setq result (custom-divider result result))
  (format t "Result div: ~A~%" result))

  ;;(def sum x y (+ (+ y x) (+ y x))) ok---------------
  (when (equal baba '("PLUS" "PLUS" "y" "x" "PLUS" "y" "x"))
    (setq result (calculate-operation "PLUS" 0 1))
    (setq result (custom-plus result result))
  (format t "Result addition: ~A~%" result))

  (when (equal baba '("MINUS" "PLUS" "y" "x" "PLUS" "y" "x"))
    (setq result (calculate-operation "PLUS" 0 1))
    (setq result (custom-minus result result))
  (format t "Result minus: ~A~%" result))

(when (equal baba '("MULT" "PLUS" "y" "x" "PLUS" "y" "x"))
    (setq result (calculate-operation "PLUS" 0 1))
    (setq result (custom-multiply result result))
  (format t "Result mult: ~A~%" result))

(when (equal baba '("DIV" "PLUS" "y" "x" "PLUS" "y" "x"))
    (setq result (calculate-operation "PLUS" 0 1))
    (setq result (custom-divider result result))
  (format t "Result div: ~A~%" result))



  ;;(def sum x y (+ (- x y) (- x y)))
(when (equal baba '("PLUS" "MINUS" "x" "y" "MINUS" "x" "y"))
    (setq result (calculate-operation "PLUS" 0 1))
    (setq result2 (calculate-operation "MINUS" 0 1))
    (setq result (custom-plus result result2))
  (format t "Result addition: ~A~%" result))

  ;;(def sum x y (+ (- x y) (- x y)))
(when (equal baba '("PLUS" "MINUS" "x" "y" "MINUS" "x" "y"))
    (setq result (calculate-operation "MINUS" 0 1))
    (setq result2 (calculate-operation "MINUS" 0 1))
    (setq result (custom-plus result result2))
  (format t "Result addition: ~A~%" result))

  ;;(def sum x y (* (- x y) (- x y)))
(when (equal baba '("MULT" "MINUS" "x" "y" "MINUS" "x" "y"))
    (setq result (calculate-operation "MINUS" 0 1))
    (setq result2 (calculate-operation "MINUS" 0 1))
    (setq result (custom-multiply result result2))
  (format t "Result addition: ~A~%" result))


;;(def sum x y (+ x (+x(+x y))))
(when (equal baba '("PLUS" "x" "PLUS" "x" "PLUS" "x" "y"))
  (setq result (calculate-operation "PLUS" 1 0))

  (let ((first-element-string (string (second oops_value))))
    (setq result (custom-plus result first-element-string))
    (format t "Result addition: ~A~%" result))
    )
(when (equal baba '("PLUS" "x" "PLUS" "x" "PLUS" "y" "x"))
  (setq result (calculate-operation "PLUS" 1 0))

  (let ((first-element-string (string (second oops_value))))
    (setq result (custom-plus result first-element-string))
    (format t "Result addition: ~A~%" result))
    )

;;(def sum x y (+ x (+x(-x y)))) ok
(when (equal baba '("PLUS" "x" "PLUS" "x" "MINUS" "x" "y"))
  (setq result (calculate-operation "MINUS" 1 0))

  (let ((first-element-string (string (second oops_value))))
    (setq result (custom-plus result first-element-string))
    (format t "Result addition: ~A~%" result))
    )
;; (def sum x y (+ x (+x(-y x)))) ok
(when (equal baba '("PLUS" "x" "PLUS" "x" "MINUS" "y" "x"))
  (setq result (calculate-operation "MINUS" 3 4))

  (let ((first-element-string (string (second oops_value))))
    (setq result (custom-plus result first-element-string))
    (format t "Result addition: ~A~%" result))
    )


;;(def sum x y (+ x (+x(*x y)))) ok
(when (equal baba '("PLUS" "x" "PLUS" "x" "MULT" "x" "y"))
  (setq result (calculate-operation "MULT" 1 0))

  (let ((first-element-string (string (second oops_value))))
    (setq result (custom-plus result first-element-string))
    (format t "Result addition: ~A~%" result))
    )

;;(def sum x y (+ x (+x(*y x)))) ok
(when (equal baba '("PLUS" "x" "PLUS" "x" "MULT" "y" "x"))
  (setq result (calculate-operation "MULT" 1 0))

  (let ((first-element-string (string (second oops_value))))
    (setq result (custom-plus result first-element-string))
    (format t "Result addition: ~A~%" result))
    )

;;(def sum x y (+ x (+x(/x y)))) ok
(when (equal baba '("PLUS" "x" "PLUS" "x" "DIV" "x" "y"))
  (setq result (calculate-operation "DIV" 1 0))

  (let ((first-element-string (string (second oops_value))))
    (setq result (custom-plus result first-element-string))
    (format t "Result addition: ~A~%" result))
    )

;;(def sum x y (+ x (+x(/y x)))) ok
(when (equal baba '("PLUS" "x" "PLUS" "x" "DIV" "y" "x"))
  (setq result (calculate-operation "DIV" 3 5))

  (let ((first-element-string (string (second oops_value))))
    (setq result (custom-plus result first-element-string))
    (format t "Result addition: ~A~%" result))
    )
;;-------------------------------------------------------
 ;;(def sum x y (+ x (+y(+x y)))) ok
(when (equal baba '("PLUS" "x" "PLUS" "y" "PLUS" "x" "y"))
  (setq result (calculate-operation "PLUS" 2 0))

  (let ((first-element-string (string (second oops_value))))
    (setq result (custom-plus result first-element-string))
    (format t "Result addition: ~A~%" result))
    )
(when (equal baba '("PLUS" "x" "PLUS" "y" "PLUS" "y" "x"))
  (setq result (calculate-operation "PLUS" 2 0))

  (let ((first-element-string (string (second oops_value))))
    (setq result (custom-plus result first-element-string))
    (format t "Result addition: ~A~%" result))
    )

;;(def sum x y (+ x (+y(-x y)))) 
(when (equal baba '("PLUS" "x" "PLUS" "y" "MINUS" "x" "y"))
  (setq result (calculate-operation "MINUS" 2 0))

  (let ((first-element-string (string (second oops_value))))
    (setq result (custom-plus result first-element-string))
    (format t "Result addition: ~A~%" result))
    )
;; (def sum x y (+ x (+y(-y x)))) 
(when (equal baba '("PLUS" "x" "PLUS" "y" "MINUS" "y" "x"))
  (setq result (calculate-operation "MINUS" 4 4))

  (let ((first-element-string (string (second oops_value))))
    (setq result (custom-plus result first-element-string))
    (format t "Result addition: ~A~%" result))
    )


;;
(when (equal baba '("PLUS" "x" "PLUS" "y" "MULT" "x" "y"))
  (setq result (calculate-operation "MULT" 5 0))

  (let ((first-element-string (string (second oops_value))))
    (setq result (custom-plus result first-element-string))
    (format t "Result addition: ~A~%" result))
    )

;;
(when (equal baba '("PLUS" "x" "PLUS" "y" "MULT" "y" "x"))
  (setq result (calculate-operation "MULT" 5 0))

  (let ((first-element-string (string (second oops_value))))
    (setq result (custom-plus result first-element-string))
    (format t "Result addition: ~A~%" result))
    )

;;(def sum x y (+ x (+y(/x y)))) ok
(when (equal baba '("PLUS" "x" "PLUS" "y" "DIV" "x" "y"))
  (setq result (calculate-operation "DIV" 5 0))

  (let ((first-element-string (string (second oops_value))))
    (setq result (custom-plus result first-element-string))
    (format t "Result addition: ~A~%" result))
    )

;;(def sum x y (+ x (+y(/y x)))) ok
(when (equal baba '("PLUS" "x" "PLUS" "y" "DIV" "y" "x"))
  (setq result (calculate-operation "DIV" 4 5))

  (let ((first-element-string (string (second oops_value))))
    (setq result (custom-plus result first-element-string))
    (format t "Result addition: ~A~%" result))
    ) 





)

  


 
    (if (< (length oops_value) 4)
      ;; Perform calculations for mult, div, minus, plus
      (let ((first-element (first oops_value))
            (second-element (second oops_value))
            (sign (third oops)))
        (let ((first-element-string (string first-element))
              (second-element-string (string second-element))
              (sign-string (string sign)))
          (cond
            ((string= sign-string "PLUS")
             (setq result (custom-plus first-element-string second-element-string))
             (format t "Result  addition: ~A~%" result))

            ((string= sign-string "MINUS")
             (setq result (custom-minus second-element-string first-element-string))
             (format t "Result subtraction: ~A~%" result))

            ((string= sign-string "DIV")
             (setq result (custom-divider second-element-string first-element-string))
             (format t "Result  division: ~A~%" result))

            ((string= sign-string "MULT")
             (setq result (custom-multiply first-element-string second-element-string))
             (format t "Result  multiplication: ~A~%" result))

            (t
          )))
    )
    )

  ( format t "~%")
  
  ))))


(defun process-file (filename)
  "Process the contents of a file and tokenize them."
  (let ((input-stream (open filename :if-does-not-exist nil)))
    (if input-stream
        (progn
          ;;(format t "Processing file ~A~%" filename)
          (process-file-stream input-stream)
          (close input-stream))
        (format t "File ~A not found.~%" filename))))



(defun process-file-stream (stream)
  "Process the contents of an input stream and tokenize them."
  (loop for line = (read-line stream nil)
        while line
        do
        (if (string= (string-upcase line) "EXIT")
            (format t "Exiting the program.~%")
            (progn
              (split-string-and-print line)
              ;;(format t "NEWLINE~%")
              )))) ; Print a new line after processing each line

(defun is-number (str)
  "Checks if the input string is a number."
  (if (or (string= str "")
          (and (<= (length str) 1) (digit-char-p (char str 0)))
          (not (string= (string-trim "0123456789" str) "")))
      nil
      t))

(defun is-unsigned-fraction (str)
  "Checks if the input string is an unsigned fraction in the format of 'NUMBER-B-NUMBER'."
  (let ((len (length str))
        (split-index (position #\b str)))
    (if (and (>= len 3)
             split-index
             (string= (subseq str split-index (+ split-index 1)) "b")
             (every #'digit-char-p (subseq str 0 split-index))
             (every #'digit-char-p (subseq str (+ split-index 1) len))
             (not (string= (subseq str (+ split-index 1) len) "")))
        t
        nil)))

(defun is-number-followed-by-b (input)
  "Checks if the input string is in the format 'number or numbers' followed by 'b'. Returns T if valid, NIL if not."
  (let ((trimmed-input (string-trim " " input)))
    (if (string/= trimmed-input "")
        (if (and (every #'digit-char-p trimmed-input)
                 (char= (char trimmed-input (1- (length trimmed-input))) #\b))
            T
            NIL)
        NIL)))

(defun is-invalid-syntax (str)
  "Checks if the input string has invalid syntax (e.g., starts with digits followed by a non-digit character)."
  (and (string/= str "")
       (let ((first-char (char str 0)))
         (and (digit-char-p first-char)
              (not (every #'digit-char-p (subseq str 1)))
              (not (char= (char str 1) #\b))))))

(defun is-alphanumeric-word (str)
  "Checks if the input string is an alphanumeric word (starts with a letter and is followed by numbers)."
  (and (not (string= str ""))
       (alpha-char-p (char str 0))
       (every #'digit-char-p (subseq str 1))))

(defun print-token (token)
  "Prints a token based on its type or keyword."
  (cond
    ;; Specific cases first
    ((string= token "and")
     ;;(format t "KW_AND~%")
     (push (intern "KW_AND") oops))

    ((string= token "or")
     ;;(format t "KW_OR~%")
     (push (intern (format nil "KW_OR")) oops))

    ((string= token "def")
     ;;(format t "KW_DEF~%")
     (push (intern "KW_DEF") oops)
     )

    ((string= token "not")
     ;;(format t "KW_NOT~%")
     (push (intern "KW_NOT") oops))

    ((string= token "equal")
     ;;(format t "KW_EQUAL~%")
     (push (intern "KW_EQUAL") oops))

    ((string= token "less")
     ;;(format t "KW_LESS~%")
     (push (intern "KW_LESS") oops))

    ((string= token "nil")
     ;;(format t "KW_NIL~%")
     (push (intern "KW_NIL") oops))

    ((string= token "list")
     ;;(format t "KW_LIST~%")
     (push (intern "KW_LIST") oops))

    ((string= token "append")
     ;;(format t "KW_APPEND~%")
     (push (intern "KW_APPEND") oops))

    ((string= token "concat")
     ;;(format t "KW_CONCAT~%")
     (push (intern "KW_CONCAT") oops))

    ((string= token "set")
     ;;(format t "KW_SET~%")
     (push (intern "KW_SET") oops))

    ((string= token "for")
     ;;(format t "KW_FOR~%")
     (push (intern "KW_FOR") oops))

    ((string= token "if")
     ;;(format t "KW_IF~%")
     (push (intern "KW_IF") oops))

    ((string= token "exit")
     ;;(format t "KW_EXIT~%")
     (push (intern "KW_EXIT") oops))

    ((string= token "load")
     ;;(format t "KW_LOAD~%")
     (push (intern "KW_LOAD") oops))

    ((string= token "display")
     ;;(format t "KW_DISPLAY~%")
     (push (intern "KW_DISPLAY") oops))

    ((string= token "true")
     ;;(format t "KW_TRUE~%")
     (push (intern "KW_TRUE") oops))
  

    ;; General cases
    ((is-number-followed-by-b token) (format t "SYNTAX ERROR: Number followed by 'b'~%") (push (intern "SYNTAX ERROR: Number followed by 'b'") oops) ) ; 12b
    ((is-number token) (format t "SYNTAX ERROR~%") (push (intern "SYNTAX ERROR") oops)) ; Modify this line
    ((is-unsigned-fraction token)  (push (intern "VALUEF") oops) (push (intern token) oops_value) (push (intern token) islem)) ; 123b12
    
    ((is-invalid-syntax token) (format t "SYNTAX ERROR: Invalid syntax detected~%") (push (intern "SYNTAX ERROR: Invalid syntax detected") oops)) ; 12f or 11231d
    
    ((string= token "COMMENT") (format t "COMMENT~%") (push (intern "COMMENT") oops)) ; Handle printing comments here
    ((string= token "EXIT") (format t "Exiting the program.~%") (push (intern "Exiting the program.") oops))
    (t  (push (intern "IDENTIFIER") oops) (push (intern token) oops_value) (push (intern token) islem) )
  ) ; Default case for identifiers
)
(defun process-input ()
  (format t "Enter a string (type 'exit' to quit):~%")
  (let ((input (read-line)))
    (if (string= (string-upcase input) "EXIT")
        (format t "Exiting the program.~%")
        (progn
          (if (search "\"" input) ; Check for double quotes in the input
              (format t "SYNTAX ERROR: Double quotes not allowed~%")
              (split-string-and-print input))
          (process-input)))))

(defun check-last-item (arr)
  "Checks if the last item in the array is a plus sign."
  (when arr
    (let ((last-item (car (last arr))))
      (if (string= last-item "PLUS")
          (format t "Last item is a PLUS sign.~%")
          (format t "Last item is not a PLUS sign.~%")))))

(defun parse-number (str)
  (loop for char across str
        for digit = (- (char-code char) (char-code #\0))
        for result = (if result (+ (* result 10) digit) digit)
        finally (return result)))


(defun custom-plus (fraction1 fraction2)
  (let* ((numerator1 (parse-number (subseq fraction1 0 (position #\b fraction1))))
         (denominator1 (parse-number (subseq fraction1 (1+ (position #\b fraction1)))))
         (numerator2 (parse-number (subseq fraction2 0 (position #\b fraction2))))
         (denominator2 (parse-number (subseq fraction2 (1+ (position #\b fraction2)))))
         (common-denominator (lcm denominator1 denominator2)))
    (format nil "~d~a~d" (+ (* numerator1 (/ common-denominator denominator1))
                            (* numerator2 (/ common-denominator denominator2)))
            #\/ common-denominator)))


(defun custom-minus (fraction1 fraction2)
  (let* ((sign1 (if (string= (subseq fraction1 0 1) "-") -1 1))
         (sign2 (if (string= (subseq fraction2 0 1) "-") -1 1))
         (numerator1 (parse-number (subseq fraction1 (if (= sign1 -1) 1 0) (position #\b fraction1))))
         (denominator1 (parse-number (subseq fraction1 (1+ (position #\b fraction1)))))
         (numerator2 (parse-number (subseq fraction2 (if (= sign2 -1) 1 0) (position #\b fraction2))))
         (denominator2 (parse-number (subseq fraction2 (1+ (position #\b fraction2)))))
         (common-denominator (lcm denominator1 denominator2)))
    (format nil "~d~a~d" (- (* sign1 numerator1 (/ common-denominator denominator1))
                            (* sign2 numerator2 (/ common-denominator denominator2)))
            #\/ common-denominator)))


(defun custom-divider (fraction1 fraction2)
  (let* ((numerator1 (parse-number (subseq fraction1 0 (position #\b fraction1))))
         (denominator1 (parse-number (subseq fraction1 (1+ (position #\b fraction1)))))
         (numerator2 (parse-number (subseq fraction2 0 (position #\b fraction2))))
         (denominator2 (parse-number (subseq fraction2 (1+ (position #\b fraction2)))))
         (result-numerator (* numerator1 denominator2))
         (result-denominator (* denominator1 numerator2)))
    (format nil "~d~a~d" result-numerator #\/ result-denominator)))

(defun custom-multiply (fraction1 fraction2)
  (let* ((numerator1 (parse-number (subseq fraction1 0 (position #\b fraction1))))
         (denominator1 (parse-number (subseq fraction1 (1+ (position #\b fraction1)))))
         (numerator2 (parse-number (subseq fraction2 0 (position #\b fraction2))))
         (denominator2 (parse-number (subseq fraction2 (1+ (position #\b fraction2)))))
         (result-numerator (* numerator1 numerator2))
         (result-denominator (* denominator1 denominator2)))
    (format nil "~d~a~d" result-numerator #\/ result-denominator)))




;; Start the input loop
(defun main ()
  (format t "Note: the inputs in lisp are working with 3b1 because of the last homework, not with 3f1:) ~%Select an option:~%1. Process file~%2. Process input~%. ")
  (let ((choice (read)))
    (cond ((= choice 1) (process-file "input.gpp"))
          ((= choice 2) (process-input))
          (t (format t "Invalid choice.~%")))
  ))

(main)

