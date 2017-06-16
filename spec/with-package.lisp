(defpackage :with-package.spec (:use :cl :jingoh :with-package)
  (:shadowing-import-from :with-package #:enable #:syntax))
(in-package :with-package.spec)
(setup :with-package)

(requirements-about WITH-IMPORT)

;;;; Description:
; Import some symbols temporarily.
#?(multiple-value-call #'values
    (ignore-errors (iota 5)) ; iota function is not defined.
     (with-import(:alexandria #:iota)
       (iota 5)) ; can call alexandria:iota.
    (ignore-errors (iota 5))) ; alexandria:iota is not imported.
:multiple-value-satisfies
#`(& (null $1)
     (typep $2 'condition)
     (equal $3 '(0 1 2 3 4))
     (null $4)
     (typep $5 'condition))

; Internal symbols also can be imported temporarily.
#?(nth-value 1 (find-symbol "EXTRACT-FUNCTION-NAME" :alexandria))
=> :INTERNAL
#?(with-import(:alexandria #:extract-function-name)
    (extract-function-name '#'car))
=> CAR

#+syntax
(WITH-IMPORT (package &rest symbol*) &body body) ; => result

;;;; Arguments and Values:

; package := package-name (i.e. symbol or string), not evaluated.
#?(with-import(alexandria :iota)
    (iota 5))
=> (0 1 2 3 4)
,:test equal
#?(with-import(#:alexandria :iota)
    (iota 5))
=> (0 1 2 3 4)
,:test equal
#?(with-import("ALEXANDRIA" :iota)
    (iota 5))
=> (0 1 2 3 4)
,:test equal
#?(with-import("alexandria" :iota)
    (iota 5))
:signals package-missing
,:lazy T
#?(with-import((find-package :alexandria) :iota)
    (iota 5))
:signals error

; symbol := symbol which is imported temporarily.
; Only symbol name is used. (i.e. keyword, uninterned symbol, and string are valid.)
#?(with-import(:alexandria :iota)
    (iota 5))
=> (0 1 2 3 4)
,:test equal
#?(with-import(:alexandria iota)
    (iota 5))
=> (0 1 2 3 4)
,:test equal
#?(with-import(:alexandria "IOTA")
    (iota 5))
=> (0 1 2 3 4)
,:test equal
#?(with-import(:alexandria "iota")
    (iota 5))
:signals error
; not evaluated.
#?(with-import(:alexandria (concatenate 'string "IO" "TA"))
    (iota 5))
:signals error

; body := implicit progn.

; result := return value of BODY.

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:
; When symbol conflicts, strong shadowing is occur.
#?(defun iota (arg)(princ arg))
=> IOTA
#?(iota 5) :outputs "5"
#?(with-import(:alexandria #:iota)
    (iota 5))
=> (0 1 2 3 4)
,:test equal

; Even if package prefix is specified, it is shadowed.
#?(with-import(:alexandria #:iota)
    (with-package.spec::iota 5))
=> (0 1 2 3 4)
,:test equal

; When with-import is nested, most inner one is picked.
#?(with-import(:alexandria #:iota)
    (with-import(:with-package.spec #:iota)
      (iota 5)))
:outputs "5"
,:after (fmakunbound 'iota)

;;;; Exceptional-Situations:
; When package does not exist, an error is signaled.
#?(with-import(:no-such-package :hoge)
    (hoge))
:signals package-missing
,:lazy T

(requirements-about WITH-USE-PACKAGE)

;;;; Description:
; Use package temporarily.
#?(with-use-package(:alexandria)
    (iota 5))
=> (0 1 2 3 4)
,:test equal

; can access only external symbols.
#?(with-use-package(:alexandria)
    (extract-function-name '#'car))
:signals error

#+syntax
(WITH-USE-PACKAGE (package &key except with-internal) &body body) ; => result

;;;; Arguments and Values:

; package := package name (i.e. symbol or string). Not evaluated.

; except := symbol name or list which includes symbol names.
; When specified, such symbols are not accessable.
; not evaluated.
#?(with-use-package(:alexandria :except iota)
    (iota 5))
:signals error

; with-internal := symbol name or list which includes symbol names.
; When specified, such symbols are accessable.
; Not evaluated.
#?(with-use-package(:alexandria :with-internal #:extract-function-name)
    (extract-function-name '#'car))
=> CAR

; body := implicit progn

; result := return value of BODY.

;;;; Affected By:
; none

;;;; Side-Effects:
; noen

;;;; Notes:
; When symbol conflicts, strong shadowing is occur.
#?(defun iota (arg)(princ arg))
=> IOTA
#?(with-use-package(:alexandria)
    (with-package.spec::iota 5))
=> (0 1 2 3 4)
,:test equal
,:after (fmakunbound 'iota)

;;;; Exceptional-Situations:
; When package is not found, an error is signaled.
#?(with-use-package(:no-such-package)
    :hoge)
:signals package-missing
,:lazy T

(requirements-about |#@-reader|)

;;;; Description:
; 1. Read from stream.
; 2. if read expression is symbol, then generate with-use-package form.
; Otherwise generate with-import form.

#+syntax
(|#@-reader| stream character number) ; => result

;;;; Arguments and Values:

; stream := file input stream.

; character := associated dispatch character.

; number := (or null (integer 0 *))

; result := form

;;;; Affected By:
; none

;;;; Side-Effects:
; Consume stream contents.

;;;; Notes:
; NUMBER is ignored.

;;;; Exceptional-Situations:

(requirements-about ENABLE
		    :around (let((*readtable*(copy-readtable nil)))
			      (call-body)))

;;;; Description:
; Set dispatch macro `#@` to `*readtable*`
#?(let((exist? (get-dispatch-macro-character #\# #\@)))
    (enable)
    (values exist? (get-dispatch-macro-character #\# #\@)))
:multiple-value-satisfies
#`(& (null $1)
     (not(null $2)))

#+syntax
(ENABLE) ; => result

;;;; Arguments and Values:

; result := implementation dependent.
#?(enable) => implementation-dependent

;;;; Affected By:

;;;; Side-Effects:
; Modify `*readtable*`

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about PACKAGE-MISSING)

;;;; Description:
; When specified package is not found, this condition is signaled.

;;;; Class Precedence List: (case in CLISP)
; package-missing error serious-condition condition standard-object t

;;;; Effective Slots:

; API [Type] T
; [READER] api

; NAME [Type] T
; [READER] name

;;;; Notes:

(requirements-about PACKAGE-MISSING)

;;;; Description:
; Short hand for error.

#+syntax
(PACKAGE-MISSING api package-name) ; => result

;;;; Arguments and Values:

; api := symbol represents which api signals.

; package-name := expected pacakge name.

; result := error

;;;; Affected By:
; Dynamic condition handlers.

;;;; Side-Effects:
; Signals an error.

;;;; Notes:

;;;; Exceptional-Situations:

