; vim: ft=lisp et
(in-package :asdf)
(defsystem :with-fields
  :author "Shinichi Sato"
  :description "Utility for line oriented string data."
  :long-description #.(read-file-string(subpathname *load-pathname*
                                                    "README.md"))
  :license "MIT"
  :components ((:file "with-fields")))
;; Perform method below is added by JINGOH.GENERATOR.
(defmethod perform ((o test-op) (c (eql (find-system "with-fields"))))
  (test-system :with-fields.test))
