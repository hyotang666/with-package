(in-package :cl-user)
(defpackage :with-package (:use :cl :named-readtables)
  (:import-from :trestrul #:mapleaf)
  (:export
    ;;;; main api
    #:with-import
    #:with-use-package
    ;;;; readtable-control
    #:|#@-reader|
    #:enable
    #:syntax ; readtable name.
    ;;;; condition
    #:package-missing
    ))
(in-package :with-package)

(define-condition package-missing(error)
  ((api :initarg :api :reader api)
   (name :initarg :name :reader name))
  (:report(lambda(condition *standard-output*)
	    (format t "~A: Package ~S is not found.~%"
		    (api condition)(name condition)))))

(defun package-missing(api package-name)
  (error(make-condition 'package-missing
			:api api
			:name package-name)))

(defmacro with-import((package &rest symbols)&body body)
  (let((*package*(or (find-package package)
		     (package-missing 'with-import package))))
    `(PROGN ,@(treatment symbols body))))

(defun treatment(symbols body)
  (labels((TREAT(leaf)
	    (cond
	      #+sbcl
	      ((sb-int:comma-p leaf)
	       (let((expr(sb-int:comma-expr leaf)))
		 (sb-int:unquote (if(consp expr)
				   (treatment symbols expr)
				   (TREAT expr))
				 (sb-int:comma-kind leaf))))
	      ((not(target-symbolp leaf))
	       leaf)
	      ((member leaf symbols :test #'string=)
	       (intern(string leaf)))
	      (t leaf))))
    (mapleaf #'TREAT body)))

(defun target-symbolp(arg)
  (and (typep arg '(and symbol (not (or keyword boolean))))
       (symbol-package arg)))

(defmacro with-use-package((package &key except with-internal)&body body)
  (let((*package*(or (find-package package)
		     (package-missing 'with-use-package package))))
    `(PROGN
       ,@(treatment 
	   (symbols-but-except package except with-internal)
	   body))))

(defun symbols-but-except(package except with-internal)
  (loop :for symbol :being :each :external-symbol :in package
	:collect symbol :into black-list
	:finally (return(nconc (uiop:ensure-list with-internal)
			       (set-exclusive-or black-list
						 (uiop:ensure-list except)
						 :test #'string=)))))

(defun |#@-reader|(stream character number)
  (declare(ignore character number))
  (let((package(read stream t t t)))
    (if(atom package)
      `(WITH-USE-PACKAGE(,package),(read stream t t t))
      `(WITH-IMPORT ,package ,(read stream t t t)))))

(defmacro enable()
  `(EVAL-WHEN(:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
     (SET-DISPATCH-MACRO-CHARACTER #\# #\@ #'|#@-reader|)))

(defreadtable syntax
  (:merge :standard)
  (:dispatch-macro-char #\# #\@ #'|#@-reader|))
