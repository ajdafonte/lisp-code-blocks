;;;-----------------------------------------------------------------------------
;;;IS.VALID.PATHNAME.P
;;;Description
;;;	Indicates if \arg{str} is a valid pathname or not.
;;;
;;;	\args
;;;		\arg{str} is a \emph{string}.
;;;
;;;	\return-types
;;;		A \emph{boolean}. If \arg{str} is a valid pathname returns \emph{t}.
;;;		Otherwise, \emph{nil}.
;;;
;;;	\example
;;;		;; valid - minimum:
;;;		(is.valid.pathname.p "\\\\a\\b")
;;;		(is.valid.pathname.p "c:\\")
;;;		(is.valid.pathname.p "/")
;;;		(is.valid.pathname.p "a:/")
;;;
;;;		;; valid - bigger:
;;;		(is.valid.pathname.p "\\\\server\\share\\something")
;;;		(is.valid.pathname.p "c:\\temp")
;;;		(is.valid.pathname.p "/folder/file")
;;;		(is.valid.pathname.p "server:/folder")
;;;
;;;		;; invalid
;;;		(is.valid.pathname.p "a//")
;;;		(is.valid.pathname.p "c:\\a:\b")
;;;		(is.valid.pathname.p "c:\\a*\b")
;;;		(is.valid.pathname.p "\\\\server\\share\\\\cenas")
;;;		(is.valid.pathname.p NIL)
;;;		(is.valid.pathname.p "")
;;;		(is.valid.pathname.p " ")
;;;		(is.valid.pathname.p ":")  ;; probe-file fails
;;;		(is.valid.pathname.p "//")  ;; probe-file fails
;;;		(is.valid.pathname.p "C:/temp//a")  ;; probe-file fails
;;;
;;;-----------------------------------------------------------------------------
(defun is.valid.pathname.p (str)
  (let ((windows.invalid.path.chars (remove-if #'(lambda (char)
						   (or (char= char #\\)
						       (char= char #\/)))
					       (ilegal.filename.chars))))
    (labels ((ensure.slashes (str)
	       (substitute #\\ #\/ str))
	     (char.after.pos? (str char &optional pos)
	       (find char str :start (or (and pos (1+ pos))
					 0)))
	     (chars.after.pos? (str str1 &optional pos)
	       (search str1 str :start2 (or (and pos (1+ pos))
					    0)))
	     (is.unc.path? (str)
	       ;; \\\\a\\b
	       (let ((str (ensure.slashes str)))
		 (and (>= (length str) 5)
		      (char= (elt str 0) #\\)
		      (char= (elt str 1) #\\)
		      (find #\\ str :start 3)
		      (every #'(lambda (char)
				 (not (char.after.pos? str char)))
			     windows.invalid.path.chars)
		      (not (chars.after.pos? str "\\\\" 1)))))
	     (is.windows.path? (str)
	       ;; c:\\
	       (let ((str (ensure.slashes str)))
		 (and (>= (length str) 3)
		      (let ((first.char.code (char-code (elt str 0))))
			(or (<= 65 first.char.code 90)
			    (<= 97 first.char.code 122)))
		      (char= (elt str 1) #\:)
		      (char= (elt str 2) #\\)
		      (every #'(lambda (char)
				 (not (char.after.pos? str char 1)))
			     windows.invalid.path.chars)
		      (not (chars.after.pos? str "\\\\")))))
	     (is.unix.path? (str)
	       ;; / 
	       (and (>= (length str) 1)
		    (char= (elt str 0) #\/)
		    (not (chars.after.pos? str "//"))))
	     (is.nfs.path? (str)
	       ;; a:/ 
	       (and (>= (length str) 3)
		    (not (chars.after.pos? str "//"))
		    (let ((colon.pos (position #\: str)))
		      (and colon.pos
			   (> (length str) (1+ colon.pos))
			   (char= (elt str (1+ colon.pos)) #\/))))))
      
      (or (is.unc.path? str)
	  (is.windows.path? str)
	  (is.unix.path? str)
	  (is.nfs.path? str))
      )))


;;;-----------------------------------------------------------------------------
;;;ENSURE-SAFE-BIND-PARAMETER-VALUES
;;;Description
;;;	Considering the \arg{parameter-original-types}, allows to ensure the safe
;;;	parameters values to use in the execution of the prepared operation.
;;;
;;;	\args
;;;		\arg{parameter-values} is a \emph{list} with the values to use in
;;;		the execution of the prepared operation.
;;;
;;;		\arg{parameter-original-types} is the original \emph{list} of keywords
;;;		that represent the data type for each bind parameter.
;;;
;;;	\return-types
;;;		A \emph{list} with the safe parameters values to use in the execution
;;;		of the prepared operation.
;;;
;;;	\example
;;;		> (let ((parameter-values '(10 nil 20 :ac 30))
;;;			(parameter-original-types '(:int :int :int :varchar :int)))
;;;		    (ensure-safe-bind-parameter-values parameter-values parameter-original-types))
;;;		  ("10" NIL "20" :ac "30")
;;;
;;;		> (let ((parameter-values '(10.0d0 nil 20.0d0 :ac 30.0d0))
;;;			(parameter-original-types '(:double :double :double :varchar :double)))
;;;		    (ensure-safe-bind-parameter-values parameter-values parameter-original-types))
;;;		  ("10.000000000000000" NIL "20.000000000000000" :AC "30.000000000000000")
;;;
;;;-----------------------------------------------------------------------------
(defun ensure-safe-bind-parameter-values (parameter-values parameter-original-types)
  (flet ((ensure-value (value type)
           (if (or (null value)
                   (symbolp value))
               value
               (cond ((member type '(:int :long :float :double))
                      (translate-literal-to-infix value))
                     (t ;; :varchar :char
                      value)))))

    (let ((result nil))
      (loop
        for parameter-val in parameter-values
        for parameter-type in parameter-original-types
        do (ul:npush (ensure-value parameter-val parameter-type) result))
      result)))
