; vim: ft=lisp et
(in-package :asdf)
(defsystem :with-package.test
 :version "0.1.0"
 :depends-on (:jingoh "with-package" :alexandria)
 :components ((:file "with-package")) :perform
 (test-op (o c) (symbol-call :jingoh :examine :with-package)))
