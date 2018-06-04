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
;; Perform method below is added by JINGOH.GENERATOR.
(defmethod perform ((o test-op) (c (eql (find-system "with-package"))))
 (test-system :with-package.test))
