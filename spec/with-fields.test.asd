; vim: ft=lisp et
(in-package :asdf)
(defsystem :with-fields.test :depends-on (:jingoh "with-fields")
 :components ((:file "with-fields")) :perform
 (test-op (o c) (symbol-call :jingoh :examine :with-fields)))