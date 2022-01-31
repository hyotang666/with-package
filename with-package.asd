;; vim: ft=lisp et
(in-package :asdf)
(defsystem :with-package
  :version "1.0.3"
  :description "Tiny library for using external package locally."
  :author "SATO Shinichi"
  :source-control (:git "git@github.com:hyotang666/with-package")
  :bug-tracker "https://github.com/hyotang666/with-package/issues"
  :license "MIT"
  :depends-on
  (
   "trestrul"           ; Utilities for tree structured list.
   "uiop"               ; Utilities implicitly depend on via asdf.
   "named-readtables"   ; Manage readtables.
   )
  :components ((:file "with-package")))

(defmethod component-depends-on ((o test-op) (c (eql (find-system "with-package"))))
  (append (call-next-method)'((test-op "with-package.test"))))
(defmethod operate :around ((o test-op)(c (eql (find-system "with-package")))
                            &key ((:compile-print *compile-print*))
                            ((:compile-verbose *compile-verbose*))
                            &allow-other-keys)
  (call-next-method))
