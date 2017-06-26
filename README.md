# WITH-FIELDS 0.0.0
## What is this?
Utility for line oriented string data.
### Current lisp world
We have CLAWK.
### Issues
It is too much awky.
And too much strong.
It exports 173 symbols.
### Proposal
With-fields proveds more lispy one.
Less power, but easy to lean.
## Usage
```lisp
;; cl-awk test-1-1-1
(for-file-lines("emp.data")
  (with-fields((name payrate hrsworked))
    (when ($> hrsworked 0)
      ($print name payrate hrsworked))))

;; cl-awk test-1-1-2 ; alternatively like this
(for-file-fields("emp.data"(name payrate hrsworked))
  (when($> hrsworked 0)
    ($print name payrate hrsworked)))

;; cl-awk test-1-1-3 ; or this
(for-file-fields("emp.data")
  (when($> $3 0)($print $1 $2 $3)))

;; cl-awk test-1-1-4 ; or even like this
(defawk test-1-1-4()
  (($> $3 0)($print $1 $2 $3)))

;; dofields
(dofields((name 0)
          (payrate 1)
	  (hrsworked 2 :key #'parse-integer))
    "emp.data"
  (when(> hrsworked 0)
    (format t "~A ~A ~A"name payrate hrsworked)))

;; for-each-line
(for-each-line((name 0)
               (payrate 1)
	       (hrsworked 2 :key #'parse-integer))
    "emp.data"
  :when (> hrsworked 0)
  :do (format t "~A ~A ~A"name payrate hrsworked))
```

```lisp
;; clawk
(defun test-1-12-1 (&optional (filename "emp.data") &aux (emp 0))
  (for-file-lines (filename)
    (with-fields ((name payrate hrsworked))
      (declare (ignore name payrate))
      (when ($> hrsworked 15)
        (incf emp))))
  ($print emp "employees worked more than 15 hours"))

;; dofields
(let((emp 0))
  (dofields((hrsworked 2 :key #'parse-integer))"emp.data"
    (when(> hrsworked 15)
      (incf emp)))
  (format t "~D employees worked more than 15 hours" emp))

;; for-each-line
(format t "~D employeers worked more than 15 hours"
        (for-each-line((hrsworked 2 :key #'parse-integer))"emp.data"
	  :when (> hrsworked 15)
	  :count :it))
```

## From developer

### Product's goal

### License
MIT
### Developed with
CLISP/2.49
### Tested with
SBCL/1.3.17
CCL/1.11
ECL/16.1.3
## Installation

