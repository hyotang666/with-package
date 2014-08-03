(in-package :with-package)

(defun treatment(fn-list list)
  "This is helper function for with-package-library.

  treatment fn-list list => list with side-effect!

  FN-LIST is list each element must be string represents exported symbol.
  LIST is list as S-Expressions.

  Retruns list looks same with LIST, but some symbol element may
  be interned in *PACKAGE*."
  (mapleaf(lambda(leaf)
	    (cond
	      ((and(not(keywordp leaf))(symbolp leaf))
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

  WITH-IMPORT allows you to use external package's symbol (e.g.
  special-symbols, functions, methods, macros) without prefix locally.
  If current package have same name with one of COMMANDS,
  it will be shadowed.
  e.g. Let's say there is CL-USER::FOO and BAR::FOO.
  If you import BAR::FOO, inside scope of WITH-IMPORT 
  you never acsess to CL-USER::FOO, even if you wrote \"cl-user::foo\".
  It will be reason of complex and search hard bugs.
  But if you did not forget to be careful, 
  this will be great help for your cord writing.
  After through out from WITH-IMPORT's scope,
  you will be able to access CL-USER::FOO again."
  (let((*package*(let((temp(find-package package)))
		   (if temp
		     temp
		     (error "WITH-PACKAGE:with-import signaled~&~
			    Can not find package ~S."package)))))
    `(progn ,@(treatment (mapcar #'string commands) body))))

(defmacro with-use-package((package &key (except nil))&body body)
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
  If you used package BAR inside scope of WITH-USE-PACKAGE, 
  you never accsess to CL-USER::FOO, even if you wrote \"cl-user::foo\".
  It will be reason of complex and search hard bugs.
  But if you does not forget to be careful, 
  this will be great help for your cord writing.
  After through out from WITH-USE-PACKAGE's scope,
  you will be able to access CL-USER::FOO again."
  (let((*package*(let((temp(find-package package)))
		   (if temp
		     temp
		     (error "WITH-PACKAGE:with-use-package signaled!~&~
			    Can not find package ~S"package)))))
    `(progn ,@(treatment 
		(loop for symbol being each external-symbol of package
		      collect (string symbol)into result
		      finally (return(set-exclusive-or 
				       result
				       (mapcar #'string (if(listp except)
							  except
							  (list except))))))
		body))))

(defun dangerous-use-package (package)
  "dangerous-use-package package => side-effect!

  PACKAGE is keyword symbol represents external package name.

  If name conflicts occured, that symbol is ignored.
  Otherwise imported.

  Returns 2 values.
  First is generalized boolean.
  When some symbol is ignored, returns such symbols list.
  Second is fixnum represents how many symbol is imported.
  If first value is NIL, 
  it means all external symbol of PACKAGE is imported successfully." 
  (loop for symbol being each external-symbol in package
	if (find-symbol(string symbol)) collect it into ignored
	else count symbol into import and do (import symbol) 
	finally (return (values ignored import))))

(defun most-dangerous-use-package (package)
  "most-dangerous-use-package package => side-effect!

  PACKAGE is keyword symbol represents external package name.

  If name conflicts occured, that symbol is shadowing-imported.

  Returns generalized boolean.
  When some symbol is shadowed, returns such symbols list.
  If return value is NIL, 
  it means all external symbol of PACKAGE is imported successfully." 
  (loop for symbol being each external-symbol in package
	if (find-symbol(string symbol)) collect it into shadowed
	else do (shadowing-import symbol) 
	finally (return shadowed)))

(defun find-conflict(package)
  "find-conflict package => list

  PACKAGE is keyword symbol represents external package name.

  If name is conflicted, collect such names then return it.
  Otherwise nil."
  (loop for symbol being each external-symbol in package
	if (find-symbol(string symbol))
	collect(let((sym(intern(string symbol))))
		 (cons sym (symbol-package sym)))))
