# WITH-PACKAGE 1.1.2

## Tiny library for using external package locally.

## Alternatives.

* [advanced-readtabl](https://github.com/Kalimehtar/advanced-readtable)
* [trivial-package-local-nicknames](https://github.com/phoe/trivial-package-local-nicknames)

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
* SBCL/2.0.5
* CCL/1.12
* ECL/20.4.24
* CLISP/2.49

