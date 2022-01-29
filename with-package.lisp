(in-package :cl-user)

(defpackage :with-package
  (:use :cl :named-readtables)
  (:import-from :trestrul #:mapleaf)
  (:export ;;;; main api
           #:with-import
           #:with-use-package
           ;;;; readtable-control
           #:|#@-reader|
           #:enable
           #:syntax ; readtable name.
           ;;;; condition
           #:package-missing))

(in-package :with-package)

(declaim (optimize speed))

(define-condition package-missing (error)
  ((api :initarg :api :reader api)
   (name :initarg :name :reader name))
  (:report
   (lambda (condition *standard-output*)
     (format t "~A: Package ~S is not found.~%" (api condition)
             (name condition)))))

(defun package-missing (api package-name)
  (error (make-condition 'package-missing :api api :name package-name)))

(defun target-symbolp (arg)
  (and (typep arg '(and symbol (not (or keyword boolean))))
       (symbol-package arg)))

(defun treatment (symbols body)
  (labels ((treat (leaf)
             (cond #+sbcl
                   ((sb-int:comma-p leaf)
                    (let ((expr (sb-int:comma-expr leaf)))
                      (sb-int:unquote
                        (if (consp expr)
                            (treatment symbols expr)
                            (treat expr))
                        (sb-int:comma-kind leaf))))
                   ((not (target-symbolp leaf)) leaf)
                   ((member leaf symbols :test #'string=)
                    (intern (symbol-name leaf)))
                   (t leaf))))
    (mapleaf #'treat body)))

(defmacro with-import ((package &rest symbols) &body body)
  "(with-import (<package> symbol+) { body }*)

  <package> := package designator. Not be evaluated.

  In the WITH-IMPORT body, the specified SYMBOLs are treated as if it is
  imported from the <PACKAGE>.

  NOTE!: In fact, WITH-IMPORT does not import the symbols actually.
  Emulating it by replacing the symbols instead.
  So you can not access the symbols that have the same name even if explicitly
  specify another package prefix.

  NOTE!: If the SYMBOLs is also used as a variable and such variable is bound at
  the outer scope of the WITH-IMPORT you can not access such a variable because
  the outer variable is current-package::var but the inner variable becomes
  <package>::var.
  So use WITH-IMPORT in outermost scope is strongly recommended."
  (let ((*package*
         (or (find-package package) (package-missing 'with-import package))))
    `(progn ,@(treatment symbols body))))

(defun symbols-but-except (package except with-internal)
  (loop :for symbol :being :each :external-symbol :in package
        :collect symbol :into black-list
        :finally (return
                  (nconc (uiop:ensure-list with-internal)
                         (set-exclusive-or black-list (uiop:ensure-list except)
                                           :test #'string=)))))

(defmacro with-use-package ((package &key except with-internal) &body body)
  "(with-use-package (<package> { :except symbol* } { :with-internal symbol* }) { body }*)

  <package> := package designator. Not be evaluated.

  In the WITH-USE-PACKAGE body, all symbols that are exported from the <PACKAGE>
  can be accessed as is the <PACKAGE> is used.
  If :EXCEPT is specified such symbols are not able to be accessed.
  If :WITH-INTERNAL is specified such symbols can be accessed.

  NOTE!: In fact, WITH-USE-PACKAGE does not use the <PACKAGE> actually.
  Emulating it by replacing the symbols instead.
  So you can not access the symbols that have the same name even if explicitly
  specify another package prefix.

  NOTE!: If the symbols that are exported from the <PACKAGE> is also used as
  a variable and such variable is bound at the outer scope of the
  WITH-USE-PACKAGE you can not access such a variable because the outer variable
  is current-package::var but the inner variable becomes <package>::var.
  So use WITH-USE-PACKAGE in outermost scope is strongly recommended."
  (let ((*package*
         (or (find-package package)
             (package-missing 'with-use-package package))))
    `(progn
      ,@(treatment (symbols-but-except package except with-internal) body))))

(defun |#@-reader| (stream character number)
  (declare (ignore character number))
  (let ((package (read stream t t t)))
    (if (atom package)
        `(with-use-package (,package)
           ,(read stream t t t))
        `(with-import ,package
           ,(read stream t t t)))))

(defmacro enable ()
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (set-dispatch-macro-character #\# #\@ #'|#@-reader|)))

(locally ; Out of our responsibility.
 #+sbcl
 (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
 (defreadtable syntax
   (:merge :standard)
   (:dispatch-macro-char #\# #\@ #'|#@-reader|)))