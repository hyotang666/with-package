WITH-PACKAGE - Common Lisp utility library for using external package locally.
==============

Abstraction
--------------

WITH-PACKAGE is a propose of one way to decrease possibility of occurring name confliction.
Please note WITH-PACKAGE never solve the name conflict problems, but just decrease it with few restrictions.
WITH-PACKAGE allows you to use external package locally.
Of course current package's namespace is never polluted.

Additionally, WITH-PACKAGE provides some useful API.

Quick start, or for person who doesn't have enough time.
--------------

Look examples below.
No need to learn about special knowledge.

    EXAMPLE[1]> (ql:quickload :alexandria)
    To load "alexandria":
      Load 1 ASDF system:
        alexandria
    ; Loading "alexandria"
    
    (:ALEXANDRIA)
    EXAMPLE[2]> (iota 3)
    
    *** - EVAL: undefined function IOTA
    EXAMPLE[3]> (with-package:with-use-package(:alexandria)(iota 3))
    (0 1 2)
    EXAMPLE[4]> (iota 3)
    
    *** - EVAL: undefined function IOTA

What you should know is ...

   1. Command name is WITH-USE-PACKAGE.
   2. WITH-USE-PACKAGE can accept only one package.
     If you need to use some external packages, you need to nest command.
   3. You should pass parameter as keyword symbol.

That's all.

When you met a problem, see chapters below.

API or for person who has enough time.
--------------

[macro]
with-use-package (package &key :except :with-internal)&body body => result

PACKAGE is keyword symbol represents external package to use.
Keyword parameter EXCEPT is list include keyword symbols represents exceptional symbols.
If exceptional symbols is specified, such symbols is not accessible inside scope of WITH-USE-PACKAGE without prefix.
This keyword parameter is useful when few symbols conflicts with current package's symbols, but others.
Keyword parameter WITH-INTERNAL is list include keyword symbols which is internal symbol of PACKAGE needed to be accessed.
Note, these keyword parameters should not be quoted, otherwise error will be signaled.

[macro]
with-import (package &rest symbols)&body body => result

PACKAGE is keyword symbol represents external package.
Rest parameter SYMBOLS is also keyword symbols represents symbols in PACKAGE to be used.
WITH-IMPORT is useful when some symbols conflicts with current package's symbols, but your interests few symbols.
In fact, WITH-IMPORT is recommended rather than using WITH-USE-PACKAGE, because WITH-IMPORT can be meta-data for your code too.
After writing the code, in the maintainace phase, it is very helpful to be able to recognize where the symbol came from.
Additionaly, WITH-IMPORT is safer than WITH-USE-PACKAGE.
For detail, see next chapter.

[function]
find-conflict package => generalized boolean

PACKAGE is keyword symbol represents external package.
FIND-CONFLICT check the external symbols of PACKAGE, then if name confliction is occured with current package, returns alist include such symbol and its package.(This is useful when current package already eval USE-PACKAGE sometimes.You can understand where name conflicted symbol was inherited from.)
Otherwise nil (means no name confliction occurs, so you can eval USE-PACKAGE safely).

[function]
dangerous-use-package package => side-effect!

PACKAGE is keyword symbol represents external package.
DANGEROUS-USE-PACKAGE tries to use PAKCAGE, then if name confliction is occured with currect package, only such symbols are ignored.
This is useful when you are in develop phase, and you know name confliction will be occured, and you want to ignore such external symbols.

[function]
most-dangerous-use-package package => side-effect!

PACKAGE is keyword symbol represents external package.
MOST-DANGEROUS-USE-PACKAGE tries to use PAKCAGE, then if name confliction is occured with current package, such symbols are SHADOWING-IMPORTed.
This is useful when you are in develop phase, and you know name confliction will be occured, and you want to ignore such current package's symbols.

Rationale. Or for person who want to gaze into deep lisp darkness...
--------------

Basic approach is below.

    (let((*package*(find-package :alexandria)))
      (iota 3))

Yes, very simple.
But problem is not so simple.
This code signaled an error.
Because lisp do REPL.
At first lisp do READ, and all symbols are interned in current package in this time.
Let's say current package is EXAMPLE, and EXAMPLE uses COMMON-LISP package.
The code above is, in fact ...

    (example::let((example::*package*(example::find-package keyword:alexandria)))
     (example::iota 3))

after READ.
In the EVAL time, at first, LET binds *package* to alexandria package dynamically.
Then, lisp calls example::iota function.
Of course such function is not defined in package EXAMPLE.
So error was signaled.

WITH-PACKAGE chose to modify S-Expression before EVAL.
Yes, it's macro's job.

With WITH-PACKAGE, lisp binds current package to new package to use.
Then traverse S-Expression.
When met symbol, lisp check the BLACK-LIST.
If symbol is found in BLACK-LIST, it will be interned in current package.(of course now current package is binded by new package to use)
Else do nothing, traversing will go on.

So code above becomes below

    (example::let((example::*package*(example::find-package keyword:alexandria)))
     (alexandria::iota 3))

after macro expansion.

This algorithm brings some restrictions.

   * **Only one package can use.**

Maybe this is not the issue.
You can nest it.

Unfortunately, small restriction is above only.
Others are very buggy.

   * **Too much strong shadowing.**

Similar with FLET, WITH-PACKAGE has very strong shadowing.

Let's say here is the code below

    (flet((car(arg)
            "Which do you like cl:car or me?"
            (declare(ignore arg))
            (princ "You chose me! I love you!")))
      (cl:car '(a b c)))

You chose cl:car explicitly, but flet:car never cares.

    => You chose me! I love you!
    "You chose me! I love you!"

It may depends on implementation, but atleast GNU CLISP and CCL do this behavior.

Like that, WITH-PACKAGE do similar behavior.

    (defun iota(arg)
      (declare(ignore arg))
      (princ "I am iota."))
    
    (with-package:with-use-package(:alexandria)
      (example::iota 3))
    => (0 1 2)

SBCL has function STRING-TO-OCTETS.
Library BABEL and FLEXI-STREAMS has same name function too.

If you eval code below in sbcl...

    (with-package:with-use-package(:babel)
      (with-package:with-use-package(:flexi-streams)
        (babel:string-to-octets "foo")
        (sb-ext:string-to-octets "bar")
        (string-to-octets "bazz")))

FLEXI-STREAMS'S STRING-TO-OCTETS shall be called three times.

   * **Never cares helper.**

If you want to use WITH-PACKAGE inside DEFMACRO, you need to be more carefully.
Because WITH-PACKAGE never cares helper command's return value.

    (defun helper(num)
      `(iota ,num))
    
    (defmacro foo(num)
      (with-package:with-use-pacage(:alexandria)
        `(,@(helper num))))
    
    (foo 3)
    *** - EVAL: undefined function IOTA

Because WITH-PACKAGE cares only its parameter body.

Macro call (foo 3) becomes...

    (example::iota 3)

To avoid this, you need to let command be inside of backquote.

    (defmacro foo(num)
      `(with-package:with-use-package(:alexandria)
         (,@(helper num))))
    
    (foo 3)
    =>(0 1 2)

   * **Name separation occurs against parameter.**

Example code below signaled an error.

    (defun foo (png)
      (with-package:with-use-package(:zpng)
        (start-png png)
        (write-png png)
        (finish-png png)))

Because package ZPNG export symbol PNG as class name.
So code above is equivalant with below.

    (example::defun example::foo (example::png)
      (zpng:start-png zpng:png)
      (zpng:write-png zpng:png)
      (zpng:finish-png zpng:png))

Unfortunately, definition is valid (although it depends on implementation. SBCL will signal style warning.).
Error is signaled in run time.

    (foo 3)
    *** - FOO: variable ZPNG:PNG has no value

If symbol ZPNG:PNG has its own value, it is worst situation.
Even if it is run time, error may not signaled, but FOO's return value is not your expect because ZPNG:PNG value is used instead of parameter.

To avoid this, you have 3 ways.

1:Let command be outside of DEFUN.

    (with-package:with-use-package(:zpng)
      (defun foo (png)
        (start-png png)
        (write-png png)
        (finish-png png)))

Above code becomes

    (example::defun example::foo (zpng:png)
      (zpng:start-png zpng:png)
      (zpng:write-png zpng:png)
      (zpng:finish-png zpng:png)))

after macro expansion.
Name separation does not occur.

2:Use :EXCEPT key word parameter.

    (defun foo(png)
      (with-package:with-use-package(:zpng :except (:png))
        (start-png png)
        (write-png png)
        (finish-png png)))

3:Use WITH-IMPORT alternatively.

If your interests symbol is few, WITH-IMPORT is better.
In this example, ZPNG:PNG is not your interest symbol.

    (defun foo(png)
      (with-package:with-import(:zpng :start-png :write-png :finish-png))
        (start-png png)
        (write-png png)
        (finish-png png)))

finally
--------------

Do you feel WITH-PACKAGE has too many restrictions?
Probably you are right.
It is reason why I said "WITH-PACKAGE is a propose of one way to decrease possibility of occurring name confliction".
(What a timid it is!)
If you are not satisfied WITH-PACKAGE, please try to find other approach.
Until someone(of course it may me) find more useful approach, I use WITH-PACKAGE:)
I hope you like it.
