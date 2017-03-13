;; vim: ft=lisp et
(in-package :asdf)
(defsystem :with-package
  :description "Tiny library for using external package locally."
  :long-description #.(uiop:read-file-string
                        (uiop:subpathname "README.md" *load-pathname*))
  :author "Shinichi Sato"
  :depends-on(:trestrul :named-readtables)
  :components ((:file "with-package")))
