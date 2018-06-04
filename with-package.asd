;; vim: ft=lisp et
(in-package :asdf)
(defsystem :with-package
  :description "Tiny library for using external package locally."
  :long-description #.(uiop:read-file-string
                        (uiop:subpathname *load-pathname* "README.md"))
  :author "Shinichi Sato"
  :license "MIT"
  :depends-on(:trestrul :named-readtables)
  :components ((:file "with-package")))

(defmethod component-depends-on ((o test-op) (c (eql (find-system "with-package"))))
  (append (call-next-method)'((test-op "with-package.test"))))
