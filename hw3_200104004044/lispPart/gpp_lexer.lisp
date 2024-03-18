;;============================
;; ATAKAN KURT - 200104004044
;; G++ Lexer in Common Lisp
;;============================

(setq KeyWordList '("and" "or" "not" "equal" "less" "nil" "list" "append" "concat" "set" "def" "for" "if" "exit" "load" "display" "true" "false"))
(setq KW '("KW_AND" "KW_OR" "KW_NOT" "KW_EQUAL" "KW_LESS" "KW_NIL" "KW_LIST" "KW_APPEND" "KW_CONCAT" "KW_SET" "KW_DEF" "KW_FOR" "KW_IF" "KW_EXIT" "KW_LOAD" "KW_DISP" "KW_TRUE" "KW_FALSE"))
(setq OperatorList '("+" "-" "/" "*" "(" ")" ","))
(setq OP '("OP_PLUS" "OP_MINUS" "OP_DIV" "OP_MULT" "OP_OP" "OP_CP" "OP_COMMA"))

(setq COMMENT ";;")
(setq Space '("\n" "\t" " "))

(defvar tokenType (list)) ; keep token list to use in interpreter
(defvar valueList (list)) ; keep value list to use in interpreter


(defun get-line ()
  (with-open-file (file "input.txt" :direction :input)
    (loop for line = (read-line file nil)
          while line
          do (format t "~a~%" line))))


(defun split_string(input-string)
  (let ((result '())
        (start-pos nil))

    (dotimes (i (length input-string))
      (let ((char (char input-string i)))
        (cond
          ((or (char= char #\space) (char= char #\tab) (char= char #\newline)) ;; if its just word
           (when start-pos
             (push (subseq input-string start-pos i) result)
             (setq start-pos nil)))

          ((char= char #\() ;; if it is oppenning bracket
           (when start-pos
             (push (subseq input-string start-pos i) result))
           (push (string char) result)
           (setq start-pos nil))

          ((char= char #\)) ;; if it is closing bracket
           (when start-pos
             (push (subseq input-string start-pos i) result))
           (push (string char) result)
           (setq start-pos nil))

          ((char= char #\,) ;; if it is a comma
           (when start-pos
             (push (subseq input-string start-pos i) result))
           (push (string char) result)
           (setq start-pos nil))

          ;====================================================================================================================
          ;; UNCOMMENT THIS PART IF YOU WANT TO SEPARATE + - / * FROM IDENTIFIERS. FOR EXEMPLE (A+B -> WILL RETURN ID COMMA ID)
          ;====================================================================================================================
          ; ((char= char #\+) ;; if it is a plus
          ;   (when start-pos
          ;     (push (subseq input-string start-pos i) result))
          ;   (push (string char) result)
          ;   (setq start-pos nil))

          ; ((char= char #\-) ;; if it is a minus
          ;   (when start-pos
          ;     (push (subseq input-string start-pos i) result))
          ;   (push (string char) result)
          ;   (setq start-pos nil))
          
          ; ((char= char #\/) ;; if it is a div
          ;   (when start-pos
          ;     (push (subseq input-string start-pos i) result))
          ;   (push (string char) result)
          ;   (setq start-pos nil))

          ; ((char= char #\*) ;; if it is a multip
          ;   (when start-pos
          ;     (push (subseq input-string start-pos i) result))
          ;   (push (string char) result)
          ;   (setq start-pos nil))

          (t (unless start-pos
               (setq start-pos i))))))

    (when start-pos
      (push (subseq input-string start-pos) result))

    (reverse result)))


(defun is-identifier (text)
  "Check if the input text matches the pattern [a-zA-Z][a-zA-Z0-9]*."
  (if (and (> (length text) 0)
           (alpha-char-p (char text 0)) ; Check if the first character is a letter
           (every #'alphanumericp (subseq text 1))) ; Check if the remaining characters are alphabetic or numberic
      t  ; It's an identifier
      nil ; It's not an identifier
  ))

(defun is-valuef (input-string)
  "Check if the input is unsigned fraction."
  (let* ((b-position (position #\b input-string))
         (before-b (if b-position
                       (subseq input-string 0 b-position)
                       ""))
         (after-b (if b-position
                      (subseq input-string (1+ b-position))
                      input-string)))
    (and (not (zerop (length before-b))) ; Check if before-b is not empty
         (not (zerop (length after-b)))  ; Check if after-b is not empty
         (every #'digit-char-p before-b)
         (every #'digit-char-p after-b))))


(defun process-file-for-keywords-operators (file-path)
  "Reads a file line by line, identifies KEYWORDS, OPERATORS, VALUEF, SYNTAX ERROR, COMMENT and prints corresponding tokens."
  (with-open-file (file file-path :direction :input)
    (loop for line = (read-line file nil)
          while line
          do
          (process-line-for-keywords-operators line)
          )
    )
  )

(defun addToListTail (item list)
    "Adds to the given list"
    (setq list (append list (list item)))
    list
)
(defun process-line-for-keywords-operators (line)
  "Processes a line of text, identifies KEYWORDS, OPERATORS, VALUEF, SYNTAX ERROR, COMMENT, and adds corresponding tokens to lists."
  (let ((words (split_string line))) ;; Divide line into words and store them in the words list.
    (dolist (word words) ;; iterate words list
      (let ((kw-index (position word KeyWordList :test #'string=))
            (op-index (position word OperatorList :test #'string=)))
        (cond
          ((string= word ";;")    ;; COMMENT CHECK
           (setf tokenType (addToListTail "COMMENT" tokenType))
           (return))             ;; if comment break from inner loop
          ((not (null kw-index))  ;; KEYWORD CHECK
           (setf tokenType (addToListTail (nth kw-index KW) tokenType)))
          ((not (null op-index))  ;; OPERATOR CHECK
           (setf tokenType (addToListTail (nth op-index OP) tokenType)))
          ((is-valuef word)    ;; VALUEF CHECK
           (setf tokenType (addToListTail "VALUEF" tokenType))
           (setf valueList (addToListTail word valueList)))
          (t                      ;; Rest of the tokens are considered syntax error.
           (setf tokenType (addToListTail "SYNTAX_ERROR" tokenType))
           (setf valueList (addToListTail (format nil "~a cannot be tokenized" word) valueList))
           (return)))
        )
      )
    )
  )

(defun gppinterpreter (&optional file-path)
  (if file-path
      (process-file-for-keywords-operators file-path)
      (loop
        (format t "Enter input from the console (type 'KILL' to exit): ")
        (let ((user-input (read-line)))
          (when (string= (string-upcase user-input) "KILL")
            (return))
          (process-line-for-keywords-operators user-input)))))


(defun print-token-lists ()
  "Prints the contents of tokenType and valueList lists for testing purposes."
  (format t "Token Types: ~a~%" tokenType)
  (format t "Values: ~a~%" valueList))
