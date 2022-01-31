# WITH-PACKAGE 1.1.2

## Tiny library for using external package locally.

## Alternatives.

* [advanced-readtable]
* [trivial-package-local-nicknames]

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

### Product's goal.
Already?

### License
MIT

## Tested with

* SBCL/2.0.5
* CCL/1.12.1
* ECL/21.2.1
* CLISP/2.49
* allegro/10.1
* CMUCL/21D
* abcl/1.8.0

<!-- Links -->

[advanced-readtable]:https://github.com/Kalimehtar/advanced-readtable
[trivial-package-local-nicknames]:https://github.com/phoe/trivial-package-local-nicknames

