; vim: ft=lisp et
(in-package :asdf)
(defsystem :with-fields
  :components ((:file "with-fields")))
;; Perform method below is added by JINGOH.GENERATOR.
(defmethod perform ((o test-op) (c (eql (find-system "with-fields"))))
  (test-system :with-fields.test))