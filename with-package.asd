;; vim: ft=lisp et
(in-package :asdf)
(defsystem :with-package
  :version "0.0.1"
  :description "Tiny library for using external package locally."
  :long-description #.(uiop:read-file-string
                        (uiop:subpathname *load-pathname* "README.md"))
  :author "Shinichi Sato"
  :license "MIT"
  :depends-on
  (
   "trestrul"           ; utilities for tree structured list.
   "named-readtables"   ; manage readtables.
   )
  :components ((:file "with-package")))

(defmethod component-depends-on ((o test-op) (c (eql (find-system "with-package"))))
  (append (call-next-method)'((test-op "with-package.test"))))
(defmethod operate :around ((o test-op)(c (eql (find-system "with-package")))
                            &key ((:compile-print *compile-print*))
                            ((:compile-verbose *compile-verbose*))
                            &allow-other-keys)
  (call-next-method))
