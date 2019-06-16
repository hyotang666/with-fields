; vim: ft=lisp et
(in-package :asdf)
(defsystem :with-fields
  :version "0.0.1"
  :author "Shinichi Sato"
  :description "Utility for line oriented string data."
  :long-description #.(read-file-string(subpathname *load-pathname*
                                                    "README.md"))
  :license "MIT"
  :components ((:file "with-fields")))

(defmethod component-depends-on ((o test-op) (c (eql (find-system "with-fields"))))
  (append (call-next-method) '((test-op "with-fields.test"))))
(defmethod operate :around((o test-op)(c (eql (find-system "with-fields")))
                           &key ((:compile-print *compile-print*))
                           ((:compile-verbose *compile-verbose*))
                           &allow-other-keys)
  (call-next-method))
