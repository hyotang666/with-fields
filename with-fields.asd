; vim: ft=lisp et
(in-package :asdf)
(defsystem :with-fields
  :author "Shinichi Sato"
  :description "Utility for line oriented string data."
  :long-description #.(read-file-string(subpathname *load-pathname*
                                                    "README.md"))
  :license "MIT"
  :components ((:file "with-fields")))

(defmethod component-depends-on ((o test-op) (c (eql (find-system "with-fields"))))
  (append (call-next-method) '((test-op "with-fields.test"))))
