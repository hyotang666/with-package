; vim: ft=lisp et
(in-package :asdf)
(defsystem :with-package.test
 :version "0.0.2"
 :depends-on (:jingoh "with-package" :alexandria)
 :components ((:file "with-package")) :perform
 (test-op (o c) (symbol-call :jingoh :examine :with-package)))
