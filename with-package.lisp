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
           ))

(in-package :with-package)

(declaim (optimize speed))

(defun target-symbolp (arg)
  (and (typep arg '(and symbol (not (or keyword boolean))))
       (symbol-package arg)))

(declaim (ftype (function (list list) (values list &optional)) replace-symbols))

(defun replace-symbols (symbols body)
  (labels ((treat (leaf)
             (cond #+sbcl
                   ((sb-int:comma-p leaf)
                    (let ((expr (sb-int:comma-expr leaf)))
                      (sb-int:unquote
                        (if (consp expr)
                            (replace-symbols symbols expr)
                            (treat expr))
                        (sb-int:comma-kind leaf))))
                   ((not (target-symbolp leaf)) leaf)
                   ((locally
                     #+sbcl
                     (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
                     (find leaf symbols :test #'string=)))
                   (t leaf))))
    (mapleaf #'treat body)))

(defmacro with-import ((package &rest symbols) &body body)
  "(with-import (<package> symbol+) { body }*)

  <package> := package designator. Not be evaluated.

  In the WITH-IMPORT body, the specified SYMBOLs are treated as if it is
  imported from the <PACKAGE>.

  NOTE!: In fact, WITH-IMPORT does not import the symbols actually.
  Emulating it by replacing the symbols instead.
  Every symbol that has the same name as specified SYMBOLs are replaced.

  NOTE!: If the SYMBOLs is also used as a variable and such variable is bound at
  the outer scope of the WITH-IMPORT you can not access such a variable because
  the outer variable is current-package::var but the inner variable becomes
  <package>::var.
  So use WITH-IMPORT in outermost scope is strongly recommended."
  `(progn
    ,@(replace-symbols
        (mapcar (lambda (symbol) (uiop:find-symbol* symbol package)) symbols)
        body)))

(defun symbols-but-except (package except with-internal)
  (labels ((external-symbols (package)
             (loop :for symbol :being :each :external-symbol :in package
                   :collect symbol))
           (find-symbols-in (package symbols)
             (mapcar (lambda (symbol) (uiop:find-symbol* symbol package))
                     symbols)))
    (nconc (find-symbols-in package (uiop:ensure-list with-internal))
           (set-exclusive-or (external-symbols package)
                             (find-symbols-in package
                                              (uiop:ensure-list except))))))

(defmacro with-use-package ((package &key except with-internal) &body body)
  "(with-use-package (<package> { :except symbol* } { :with-internal symbol* }) { body }*)

  <package> := package designator. Not be evaluated.

  In the WITH-USE-PACKAGE body, all symbols that are exported from the <PACKAGE>
  can be accessed as is the <PACKAGE> is used.
  If :EXCEPT is specified such symbols are not able to be accessed.
  If :WITH-INTERNAL is specified such symbols can be accessed.

  NOTE!: In fact, WITH-USE-PACKAGE does not use the <PACKAGE> actually.
  Emulating it by replacing the symbols instead.
  Every symbol that has the same name as specified SYMBOLs are replaced.

  NOTE!: If the specified symbols is also used as a variable and such variable
  is bound at the outer scope of the WITH-USE-PACKAGE you can not access such a
  variable because the outer variable is current-package::var but the inner
  variable becomes <package>::var.
  So use WITH-USE-PACKAGE in outermost scope is strongly recommended."
  `(progn
    ,@(replace-symbols (symbols-but-except package except with-internal) body)))

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