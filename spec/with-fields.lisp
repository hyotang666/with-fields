(defpackage :with-fields.spec
  (:use :cl :jingoh :with-fields)
  (:import-from :with-fields #:nth-fields)
  )
(in-package :with-fields.spec)
(setup :with-fields)

(requirements-about WITH-FIELDS)

;;;; Description:
; binds var by character separated value string's specified index,
; then evaluate body.
#?(with-fields((s 1))"foo bar bazz"
    s)
=> "bar"
,:test string=

#+syntax
(WITH-FIELDS (&rest bind*) init &body body) ; => result

;;;; Arguments and Values:

; bind* := (var index &key key)
; var := symbol, otherwise error.  not evaluated.
#?(with-fields(("s" 1))"foo bar bazz"
    "s")
:signals error
,:ignore-signals warning

#?(with-fields(((intern "VAR")1))"foo bar bazz"
    var)
:signals error
,:ignore-signals warning

; var bound by string by default.
#?(with-fields((v 1))"1 2 3 4"
    v)
:be-the string

; index := non negative integer, otherwise error. not evaluated.
#?(with-fields((v -1))"1 2 3 4"
    v)
:signals error
,:ignore-signals warning

#?(with-fields((v (1+ 1)))"1 2 3 4"
    v)
:signals error
,:ignore-signals warning

; key := function generate form. evaluated.
; when specified, var is bound by return value of such function.
#?(with-fields((v 1 :key #'read-from-string))"1 2 3 4"
    v)
=> 2

#?(with-fields((v 1 :key read-from-string))"1 2 3 4"
    v)
:signals (or error
	     warning ; for ccl
	     )

; init := string generate form. evaluated.
#?(with-fields((v 0))(format nil "~{~A~^ ~}"'(a s d f))
    v)
=> "A"
,:test string=

; when init does not generate string, an error is signaled.
#?(with-fields((v 0))'("foo" "bar")
    v)
:signals error
,:ignore-signals warning

; body := implicit progn.
; CL:DECLARE is valid, when it appear top of BODY.
#?(with-fields((v 0 :key #'parse-integer))"1 2 3"
    (declare(type fixnum v))
    v)
=> 1

; result := return value of BODY.

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:
; The default separator is #\space.
; When you need to specify another separator, you can use declare in top of body.
#?(with-fields((v 0))"foo:bar:bazz"
    v)
=> "foo:bar:bazz"
,:test string=
#?(with-fields((v 0))"foo:bar:bazz"
    (declare(separator #\:))
    v)
=> "foo"
,:test string=

; When index over the length of fields, var bound by nil.
#?(with-fields((v 1))"foo"
    v)
=> unspecified ; <--- NIL, but is error better? or empty string?

; INIT is string generate form, not `LINE` generate form.
; #\Newline is included as field value.
#?(with-fields((v 2))"zero one two
three"
    v)
=> "two
three"
,:test string=

;;;; Exceptional-Situations:

(requirements-about DO-STREAM-FIELDS
		    :around
		    (with-input-from-string(*standard-input* (format nil "~{~A ~A~%~}"'(foo 1 bar 2)))
		      (call-body)))

;;;; Description:
; same like with-fields but accepts stream,
; and iterate lines.
#?(do-stream-fields((num 1))*standard-input*
    (princ num))
:outputs "12"

#+syntax
(DO-STREAM-FIELDS (&rest bind*) input &body body) ; => result

;;;; Arguments and Values:

; bind* := (var index &key key)
; var := symbol, otherwise error.
#?(do-stream-fields(("VAR" 0))*standard-input*
    var)
:signals error

; not evaluated.
#?(do-stream-fields(((intern "VAR")0))*standard-input*
    var)
:signals error

; bound by string
#?(do-stream-fields((s 0))*standard-input*
    (princ(stringp s)))
:outputs "TT"

; index := non negative integer, otherwise error.
#?(do-stream-fields((s -1))*standard-input*
    s)
:signals error

#?(do-stream-fields((s zero))*standard-input*
    s)
:signals error

; not evaluated.
#?(do-stream-fields((s (1+ 1)))*standard-input*
    s)
:signals error

; key := function which applyed string.
#?(do-stream-fields((num 1 :key #'read-from-string))*standard-input*
    (princ num))
:outputs "12"

; evaluated.
#?(do-stream-fields((num 1 :key read-from-string))*standard-input*
    (princ num))
:signals (or error
	     warning ; for ccl
	     )

; input := input stream generate form, evaluated.
; otherwise error.
#?(do-stream-fields((num 1))"foo 0"
    (princ num))
:signals error
,:ignore-signals warning

; body := implicit progn

; result := NIL
#?(do-stream-fields((num 1 :key #'parse-integer))*standard-input*
    (1+ num))
=> NIL

;;;; Affected By:
; none

;;;; Side-Effects:
; Bounds `*LINE*` with current line.
#?(do-stream-fields((num 1 :key #'parse-integer))*standard-input*
    (when(= 2 num)
      (write-string *line*)))
:outputs "BAR 2"

;;;; Notes:
; When index over the length of fields, unspecified.
#?(do-stream-fields((nothing 5))*standard-input*
    nothing)
=> unspecified ; <--- currently NIL, is error better?

; return can work
#?(do-stream-fields((num 1 :key #'parse-integer))*standard-input*
    (return num))
=> 1

; go can work.
#?(do-stream-fields((num 1 :key #'parse-integer))*standard-input*
    (when(evenp num)
      (go :end))
    (princ num)
    :end)
:outputs "1"

; declare can work.
#?(do-stream-fields((num 1 :key #'parse-integer))*standard-input*
    (declare(type integer num))
    (princ num))
:outputs "12"

; When need to specify separator, you can use declare.
#?(with-input-from-string(in "foo:bar:bazz")
    (do-stream-fields((foo 0))in
      (declare(separator #\:))
      (princ foo)))
:outputs "foo"

;;;; Exceptional-Situations:

(requirements-about DOFIELDS)

;;;; Description:
; accept input line generator, then bound fields to var,
; then iterate body in such context.
#?(with-input-from-string(in "foo bar bazz")
    (dofields((bar 1))in
      (princ bar)))
:outputs "bar"

#+syntax
(DOFIELDS (&rest bind*) input &body body) ; => result

;;;; Arguments and Values:

; bind* := (var index &key key)
; var := symbol, otherwise error.
#?(dofields(("VAR" 0))"emp.data"
    var)
:signals error

; not evaluated.
#?(dofields(((intern "VAR")0))"emp.data"
    var)
:signals error

; index := non negative integer, otherwise error.
#?(dofields((v -1))"emp.data"
    v)
:signals error

; not evaluated.
#?(dofields((v (1+ 1)))"emp.data"
    v)
:signals error

; key := function applied field string of line.
#?(with-input-from-string(in "foo bar bazz")
    (dofields((sym 0 :key #'read-from-string))in
      (princ sym)))
:outputs "FOO"

; evaluated.
#?(dofields((v 0 :key read-from-string))"emp.data"
    v)
:signals error
,:ignore-signals warning

; input := input stream or pathname designator.
#?(defvar *file*(merge-pathnames (asdf:system-source-directory
				   (asdf:find-system :with-fields.test))
				 "emp.data"))
=> *FILE*
,:lazy NIL

#?(dofields((name 0))*file*
    (print name))
:outputs
"
\"Beth\" 
\"Dan\" 
\"Kathy\" 
\"Mark\" 
\"Mary\" 
\"Suzie\" "

; body := implicit progn

; result := NIL
#?(dofields((name 0))*file*
    (print name))
=> NIL
,:stream nil

;;;; Affected By:
; none

;;;; Side-Effects:
; consume stream contents.

;;;; Notes:

;;;; Exceptional-Situations:

;;;; Tests from clawk.
; Chapter 1, page 1, example 1
; Print all employees that have worked this week
;
; $3 > 0 { print $1 $2 $3 }
;
#?(dofields((name 0)(payrate 1)(hrsworked 2 :key #'parse-integer))*file*
    (when(< 0 hrsworked)
      (format t "~A~A~A~&" name payrate hrsworked)))
:outputs
"Kathy4.0010
Mark5.0020
Mary5.5022
Suzie4.2518
"

#?(dofields((hrsworked 2 :key #'parse-integer))*file*
    (when(zerop hrsworked)
      (write-line *line*)))
:outputs
"Beth    4.00    0
Dan     3.75    0
"

#?(dofields((name 0)
	    (payrate 1 :key #'read-from-string)
	    (hrsworked 2 :key #'read-from-string))
    *file*
    (format t "~&total pay for ~A is ~D"
	    name (float(* payrate hrsworked))))
:outputs
"total pay for Beth is 0.0
total pay for Dan is 0.0
total pay for Kathy is 40.0
total pay for Mark is 100.0
total pay for Mary is 121.0
total pay for Suzie is 76.5"

#?(let((count 0)
       (pay 0))
    (dofields((payrate 1 :key #'read-from-string)
	      (hrsworked 2 :key #'read-from-string))
      *file*
      (incf count)
      (incf pay (* payrate hrsworked)))
    (format t "~D employees~&total pay is ~D~&average pay is ~D"
	    count
	    pay
	    (/ pay count)))
:outputs
"6 employees
total pay is 337.5
average pay is 56.25"

(requirements-about NTH-FIELDS :test equal)

;;;; NOTE!
; This is internal, behavior may be changed.
; Especially...
; * When string is empty, NIL vs Signaling condition.
; * When there is no fields, NIL vs empty string.

;;;; Description:
; Collect nth fields of string.

#+syntax
(NTH-FIELDS string separator &rest indexies) ; => result

#?(nth-fields "foo bar bazz" #\space 0 2)
=> ("foo" "bazz")

;;;; Arguments and Values:

; string := string which separated by CHARACTER.
; When not string, an error is signaled.
#?(nth-fields () #\space 0) :signals error
; When STRING is empty, NIL is returned.
#?(nth-fields "" #\space 2) => NIL

; separator := character which separates STRING.
; When separator imedeately follows separator, it is interpreted as empty field.
#?(nth-fields "foo::bar" #\: 0 1 2)
=> ("foo" "" "bar")
; #\Space is special. It is reduced.
#?(nth-fields "foo  bar" #\space 0 1 2)
=> ("foo" "bar" nil)
; When separator be at first, it means first field is empty.
#?(nth-fields ":foo:bar" #\: 0 1 2)
=> ("" "foo" "bar")
; When separator is escaped, such separator is ignored as separator.
#?(nth-fields "foo\\:bar:bazz" #\: 0 1 2)
=> ("foo\\:bar" "bazz" nil)

; index := (INTEGER 0 *)
; When specified index's fields is not exist, such field's value becomes NIL.
#?(nth-fields "foo bar" #\space 5) => (NIL)
; When end without separator, it is interpretted no more fields.
#?(nth-fields "foo:bar" #\: 0 1 2) => ("foo" "bar" nil)
; When end with separator, it is interpretted one more fields.
#?(nth-fields "foo:bar:" #\: 0 1 2) => ("foo" "bar" "")
; NOTE! #\Space is treated as special. (Trimmed at first.)
#?(nth-fields "  foo bar  " #\space 0 1 2) => ("foo" "bar" nil)

; result := list which include specified index's sub string of STRING.
; Sub string's #\space is trimmed.
#?(nth-fields "foo  :  bar: bazz  :" #\: 0 1 2 3)
=> ("foo" "bar" "bazz" "")

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about FOR-EACH-LINE)

;;;; Description:
; Almost same with DOFIELDS.
; DOFIELDS's syntax is based on CL:DO family, but FOR-EACH-LINE's syntax is based on CL:LOOP facility.
; In the body, you can use CL:LOOP macro keywords, but INITIALLY is invalid, and FOR, and WITH are implementation dependent (especially CLISP).

#+syntax
(FOR-EACH-LINE (&rest bind*) input &body body) ; => result

;;;; Arguments and Values:

; bind* := (var index)
; var := symbol
; index := (integer 0 *)

; input := pathname-designator

; body := implicit-progn

; result := NIL (the default).

;;;; Affected By:
; none

;;;; Side-Effects:
; Reading contents from specified input.
; Binds `*line*`.

;;;; Notes:

;;;; Exceptional-Situations:

;;;; Examples:
#?(let((path(probe-file "/proc/cpuinfo")))
    (when path
      (for-each-line((tag 0))path
	(declare(separator #\:))
	:when (uiop:string-prefix-p "processor" tag)
	:count :it)))
=> implementation-dependent ; <--- In particular, environment dependent, the number of cpu cores.

#?(for-each-line((hrsworked 2 :key #'parse-integer))*file*
    :when(zerop hrsworked)
    :do (write-line *line*))
:outputs
"Beth    4.00    0
Dan     3.75    0
"

#?(for-each-line((payrate 1 :key #'read-from-string)
		 (hrsworked 2 :key #'read-from-string))
    *file*
    :count :it :into count
    :sum (* payrate hrsworked) :into pay
    :finally (format t "~D employees~&total pay is ~D~&average pay is ~D"
		     count pay (/ pay count)))
:outputs
"6 employees
total pay is 337.5
average pay is 56.25"

(requirements-about *LINE*)

;;;; Description:
; Bound with processing current line.
#?*line* :signals unbound-variable

#?(with-input-from-string(s "foo bar")
    (dofields((v 0))s
      (declare(ignore v))
      (write-string *line*)))
:outputs "foo bar"

;;;; Value type is UNBOUND

;;;; Affected By:
; DO-STREAM-FIELDS DOFIELDS FOR-EACH-LINE

;;;; Notes:

