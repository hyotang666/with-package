(in-package :with-package)

(defun treatment(fn-list list)
  "This is helper function for with-package-library.
  treatment fn-list list => list with side-effect!
  FN-LIST is list each element must be string represents exported symbol.
  LIST is list as SExpressions.
  Retruns list looks same with LIST, but some symbol element may
  be interned in *PACKAGE*."
  (mapleaf(lambda(leaf)
	    (cond
	      ((symbolp leaf)
	       (if(member(ignore-errors(string leaf))
		    fn-list :test #'string=)
		 (intern(string leaf)*package*)
		 leaf))
	      (t leaf)))
    list))

(defmacro with-import((package &rest commands)&body body)
  "with-import (package &rest commands) &body body

  PACKAGE is keyword symbol represents external package.
  COMMANDS is keyword symbol represents symbol interned by PACKAGE.
  BODY is some S-Expressions.

  Returns last S-Expression's retrun value.(Work as implicit PROGN)

  WITH-IMPORT allows you to use external package's symbol (include 
  special-symbols functions methods macros) without prefix.
  If current package have same name with one of COMMANDS,
  it will be shadowed.
  e.g. Let's say there is CL-USER::FOO and BAR::FOO.
  If you import BAR::FOO, inside scope of WITH-IMPORT 
  you never acsess to CL-USER::FOO, even if you write \"cl-user::foo\".
  It will be reason of complex and search hard bugs.
  But if you does not forget to be care, this will be help for your cord.
  After through out from WITH-IMPORT's scope,
  you will be able to access CL-USER::FOO again."
  (let((*package*(find-package package)))
    `(progn ,@(treatment (mapcar #'string commands) body))))

(defmacro with-use-package((package)&body body)
  "with-use-package (package) &body body
  
  PACKAGE is keyword symbol represents external package.
  BODY is some S-Expressions.
  
  Returns last of S-Expression's return value(work as implicit PROGN).
  
  WITH-USE-PACKAGE allows you to use external package locally.
  Like USE-PACKAGE, but all of external package's external symbol is
  never interned in current package.
  If current package have same name with one of PACKAGE's external symbols,
  it will be shadowed.
  e.g. Let's say there is CL-USER::FOO and BAR::FOO.
  If you use package BAR, inside scope of WITH-IMPORT 
  you never acsess to CL-USER::FOO, even if you write \"cl-user::foo\".
  It will be reason of complex and search hard bugs.
  But if you does not forget to be care, this will be help for your cord.
  After through out from WITH-USE-PACKAGE's scope,
  you will be able to access CL-USER::FOO again."
  (let((*package*(find-package package)))
    `(progn ,@(treatment 
		(loop for symbol being each external-symbols of package
		      collect (string symbol))
		body))))

