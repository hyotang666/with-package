(in-package :with-package)

(defun pairp(arg)
  "This is helper function for mapleaf.
  pairp arg => boolean
  ARG is any lisp expression.
  Return True if ARG is just one cons and its CDR is not NIL.
  Otherwise nil."
  (and(consp arg)
    (atom(cdr arg))
    (not(null(cdr arg)))))

(defun mapleaf (fn tree);depend-on DLIST-P
  "mapleaf fn tree => tree
  FN is function to be called for each leaf of TREE.
  TREE is list as tree structure.
  Like mapcar, but for tree structured list."
  (cond
    ((null tree) nil)
    ((atom tree)(funcall fn tree))
    ((pairp tree)(cons(funcall fn (car tree))
		        (funcall fn (cdr tree))))
    (t (cons(mapleaf fn (car tree))
	    (mapleaf fn (cdr tree))))))

