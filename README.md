# WITH-PACKAGE 1.1.2

## Tiny library for using external package locally.

### Current lisp world
CL has strong package system.

### Issues
Although there is no way to use package temporarily.

### Propose
WITH-PACKAGE allows you to do it.

## Usage
```lisp
(named-readtables:in-readtable with-package:syntax)

(iota 5)
=> error

#@alexandria
(iota 5)
=> (0 1 2 3 4)

(iota 5)
=> error
```

## From developer

* Product's goal - already?
* License - MIT
### Tested with
SBCL/1.5.6
CCL/1.11.5

