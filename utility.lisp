(in-package :with-package)

(defun dlist-p(arg)
  "This is helper function for mapleaf.
  dlist-p arg => boolean
  ARG is any lisp expression.
  Return True if ARG is dot-list.
  Otherwise nil."
  (and(consp arg)
    (atom(car arg))
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
    ((dlist-p tree)(cons(funcall fn (car tree))
		        (funcall fn (cdr tree))))
    (t (cons(mapleaf fn (car tree))
	    (mapleaf fn (cdr tree))))))

