(defpackage :with-fields(:use :cl)
  (:export
    ;;;; main api
    #:with-fields
    #:dofields
    #:do-stream-fields
    #:for-each-line
    ;;;; special variable
    #:*line*
    ))
(in-package :with-fields)

(define-condition arg-error(simple-type-error)())

(macrolet((!(n form)
	    `(handler-case ,form
	       (error(c)
		 (if(or (not(typep c 'simple-condition))
			(typep c 'arg-error))
		   (error c)
		   (error 'arg-error
			  :format-control
			  ,(ecase n
			     (1 "~S: Separator must character, but ~S")
			     (2 "~S: Second arguments must string, but ~S")
			     (4 "~S: Indexes must non negative integer, but ~S")
			     )
			  :format-arguments
			  (list 'nth-fields ,(ecase n
					       (1 'separator)
					       (2 'string)
					       ((3 4) 'indice)
					       ))))))))

  (defun nth-fields(string separator &rest indice)
    (! 2(check-type string string))
    (when(string= "" string)
      (return-from nth-fields nil))
    (flet((trim(string)
	    (string-trim '(#\space) string)))
      (when(! 1(char= #\space separator))
	(setf string (trim string)))
      (do((max(length string))
	  (sorted(! 4(sort(copy-list indice)#'<))) ; keep order.
	  (field-address 0)
	  (start 0)
	  (current 0 (1+ current))
	  (table(make-hash-table)))
	((or (null sorted) ; all specified fields are collected.
	     (when(= current max) ; exhaust string.
	       (if(= field-address(! 4 (the integer(car sorted))))
		 (setf (gethash(car sorted)table)(trim(subseq string start)))
		 T)))
	 (loop :for key :in indice
	       :collect(gethash key table)))
	(if(char= #\\ (schar string current))
	  (incf current)
	  (when(and (char= separator (schar string current))
		    (not (and (char= #\space separator)
			      (char= #\space (schar string (1- current))))))
	    (if(= field-address (car sorted))
	      (setf (gethash(car sorted)table)(trim(subseq string start current))
		    sorted (cdr sorted)
		    field-address (1+ field-address)
		    start (1+ current))
	      (setf field-address (1+ field-address)
		    start (1+ current)))))))))

(defmacro with-fields(binds init &body body)
  (check-binds binds)
  (multiple-value-bind(separator declares body)(parse-body body)
    (let((string(gensym"STRING"))
	 (list(gensym "LIST")))
      `(LET*((,string (THE STRING ,init))
	     (,list(NTH-FIELDS ,string ,separator ,@(mapcar #'cadr binds))))
	 (WHEN,list
	   (DESTRUCTURING-BIND,(mapcar #'car binds),list
	     (LET,(rebinds binds)
	       ,@declares
	       ,@body)))))))

(defun rebinds(binds)
  (loop :for (var nil . params) :in binds
	:collect `(,var ,(if params
			   `(funcall ,(getf params :key),var)
			   var))))

(define-compiler-macro with-fields(&whole whole binds init &body body)
  (if(not(stringp init))
    whole
    (multiple-value-bind(separator declares body)(parse-body body)
      (check-binds binds)
      `(let,(mapcar (lambda(bind string)
		      (let((key(getf bind :key)))
			(if key
			  `(,(car bind),(if(constantp key)
					    (funcall key string)
					    `(funcall ,key ,string)))
			  `(,(car bind) ,string))))
		    binds
		    (apply #'nth-fields init separator (mapcar #'cadr binds)))
	 ,@declares
	 ,@body))))

(defun check-binds(binds)
  (loop :for (key index . option) :in binds
	:unless (symbolp key)
	:do (error "VAR must symbol but ~S"key)
	:unless (typep index '(integer 0 *))
	:do (error "Index must be non negative integer, but ~S"index)
	:when  (and option
		    (or (atom option)
			(not(eq :key (car option)))))
	:do (error "Must :key but ~S" option)))

(defun parse-body(body)
  (if(atom(car body))
    (values #\Space nil body)
    (if(not(eq 'declare (caar body)))
      (values #\Space nil body)
      (multiple-value-call #'values
	(separator(cdar body))
	(body body)))))

(defun separator(forms)
  (loop :for (key . rest) :in forms
	:when (string= 'separator key)
	:do (return(car rest))
	:finally (return #\space)))

(defun body(body)
  (multiple-value-bind(declare body)(parse body)
    (values (remove-separator declare)
	    body)))

(defun parse(body)
  (loop :for form :in body
	:when (typep form '(cons (eql declare) T))
	:collect form :into declare
	:else :collect form :into bodies
	:finally (return (values declare bodies))))

(defun remove-separator(declares)
  (loop :for declare :in declares
	:for temp = (loop :for option :in (cdr declare)
			  :unless (string= 'separator (car option))
			  :collect option)
	:when temp :collect `(DECLARE ,@temp)))

(defvar *line*)

(defmacro do-stream-fields((&rest bind*) input &body body)
  (check-binds bind*)
  (multiple-value-bind(separator declares body)(parse-body body)
    (let((in(gensym "IN"))
	 (list(gensym "LIST")))
      `(LET((,in ,input))
	 (LOOP :FOR *LINE* = (READ-LINE ,in NIL NIL)
	       :WHILE *LINE*
	       :DO (LET((,list(NTH-FIELDS *LINE* ,separator ,@(mapcar #'cadr bind*))))
		     (WHEN,list
		       (DESTRUCTURING-BIND,(mapcar #'car bind*),list
			 (LET,(rebinds bind*)
			   ,@declares
			   (TAGBODY 
			     ,@body))))))))))

(defmacro dofields((&rest bind*)input &body body)
  (let((var(gensym "VAR")))
    `(LET((,var ,input))
       (IF (STREAMP ,var)
	 (DO-STREAM-FIELDS ,bind* ,var ,@body)
	 (WITH-OPEN-FILE(,var ,var)
	   (DO-STREAM-FIELDS ,bind* ,var ,@body))))))

#+design
(for-each-line-fields((var 0))#P"pathname"
  :when (string= "hoge" var)
  :count :it)

(defmacro for-each-line((&rest bind*)input &body body)
  (check-binds bind*)
  (multiple-value-bind(separator declares body)(parse-body body)
    (when declares (error "DECLARE is valid only SEPARATOR. but ~S" declares))
    (let((first(gensym "FIRST"))
	 (list(gensym "LIST")))
      `(WITH-OPEN-FILE(*STANDARD-INPUT* ,input)
	 (LET((,first(READ-LINE NIL NIL)))
	   (WHEN ,first
	     (LOOP :FOR *LINE* = ,first :THEN (OR (READ-LINE NIL NIL)
						  (LOOP-FINISH))
		   ,@(loop :for (var) :in bind*
			   :nconc `(:WITH ,var))
		   :FOR ,list = (NTH-FIELDS *LINE* ,separator ,@(mapcar #'cadr bind*))
		   :WHEN ,list
		   :DO (SETF ,@(loop :for (var nil . params) :in bind*
				     :collect var
				     :when params
				     :collect `(FUNCALL,(getf params :key)(POP ,list))
				     :else :collect `(POP ,list)))
		   ,@body)))))))

#++
(for-each-line((v 0))"/proc/cpuinfo"
  (declare(separator #\:))
  :when (uiop:string-prefix-p "processor" v)
  :count :it)
