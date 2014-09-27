WITH-PACKAGE - 外部パッケージを局所的に使用するためのCommon Lispユーティリティライブラリ。
================

Abstraction
----------------

WITH-PACKAGEは名前衝突が起こる可能性を軽減させる一つの提案です。
WITH-PACKAGEはけして名前衝突問題を解決するものではない点、要注意して欲しい。
少々の制限を犠牲に軽減させるだけです。
WITH-PACKAGEを使うことで外部パッケージを局所的に使うことができるようになる。
もちろんカレントパッケージの名前空間はけして汚染されない。

さらにWITH-PACKAGEはいくつかの便利なAPIを提供する。

クイックスタート、もしくは充分な時間の無い方のために。
----------------

以下の例を見て欲しい。
特別な知識を学ぶ必要性はなにもない。

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

知っておくべきことは以下の通り。

   1. コマンド名はWITH-USE-PACKAGEである。
   2. WITH-USE-PACKAGEで使えるのは１つのパッケージのみ。
   　　もし複数の外部パッケージを使いたいならコマンドをネストさせる必要がある。
   3. 引数はキーワードシンボルで指定する。

以上。

何か問題にぶつかったら、その時は以下の章に目を通してみてほしい。

API、もしくは充分な時間のある方のために。
----------------

[macro]
with-use-package (package &key :except :with-internal)&body body => result

PACKAGEはキーワードシンボルで、使用されるべき外部パッケージを指す。
キーワード引数EXCEPTはリストで、エクスターナルシンボルを指すキーワードシンボルを内包する。
もし例外シンボルが指定されたら、そのシンボルはWITH-USE-PACKAGEのスコープ内といえどもプリフィックス無しでアクセスすることはできない。
このキーワード引数は、極少数のシンボルがカレントパッケージのシンボルと名前衝突を起こすような外部パッケージを使いたいときに便利です。
キーワード引数WITH-INTERNALはリストで、アクセスしたいPACKAGEのインターナルシンボルを指すキーワードシンボルを内包する。
要注意、これらのキーワード引数はクォートしてはならず、さもなくばエラーが発せられる。

[macro]
with-import (package &rest symbols)&body body => result

PACKAGEはキーワードシンボルで、使用されるべき外部パッケージを指す。
rest引数SYMBOLSもまたキーワードシンボルで、PACKAGE内の使われるべきシンボルを指す。
WITH-IMPORTは複数のシンボルがカレントパッケージのシンボルと名前衝突を起こすが、興味あるシンボルに限ってはそうでない場合に便利です。
実は、WITH-IMPORTはWITH-USE-PACKAGEより使用を推奨されるもので、というのもWITH-IMPORTはコードのメタデータともなりうるから。
コードを書き終え、保守をする段階に入ると、何処からシンボルが来たのかを区別できるのは非常に有用です。
更に、WITH-IMPORTはWITH-USE-PACKAGEより安全です。
詳細については次章を参照して欲しい。

[function]
find-conflict package => generalized boolean

PACKAGEはキーワードシンボルで、外部パッケージを指す。
FIND-CONFLICTはPACKAGEのエクスターナルシンボルをチェックし、もしカレントパッケージと名前衝突が起きるようなら、そのシンボルとパッケージとを連想リストにして返す。（これは既にカレントパッケージが何度かUSE-PACKAGEしているときに便利です。名前衝突を起こしたシンボルがどのパッケージからカレントパッケージに継承されてきたものか知ることができる）
さもなくばNILを返す（つまり名前衝突は起こらず、安全にUSE-PACKAGEできる）。

[function]
dangerous-use-package package => side-effect!

PACKAGEはキーワードシンボルで、外部パッケージを指す。
DANGEROUS-USE-PACKAGEはPACKAGEをUSEしようとし、もしカレントパッケージと名前衝突が起こるようなら、そのようなシンボルのみ無視する。
これは開発段階において、名前衝突が起きることを既に承知しており、そのようなエクスターナルシンボルを無視してしまいたいときに便利です。

[function]
most-dangerous-use-package package => side-effect!

PACKAGEはキーワードシンボルで、外部パッケージを指す。
MOST-DANGEROUS-USE-PACKAGEはPACKAGEをUSEしようとし、もしカレントパッケージと名前衝突が起こるようなら、そのようなシンボルをSHADOWING-IMPORTする。
これは開発段階において、名前衝突が起きることを既に承知しており、そのようなカレントパッケージのシンボルを無視してしまいたいときに便利です。

論理的根拠、もしくはLispの深淵を覗き込みたい方のために。
----------------

基本的な攻略法は以下の通り。

    (let((*package*(find-package :alexandria)))
      (iota 3))

非常にシンプルです。
でも問題はそんなにシンプルじゃない。
このコードはエラーを発する。
というのもLispはREPLを行うからです。
先ず最初にLispはREADを行い、そのときに全シンボルはカレントパッケージにインターンされてしまう。
カレントパッケージがEXAMPLEだとして、EXAMPLEはCOMMON-LISPパッケージをUSEしているものとしよう。
とすると上記のコードは実は以下のようなものであるといえる。

    (example::let((example::*package*(example::find-package keyword:alexandria)))
     (example::iota 3))

EVAL時に、Lispはまず最初にカレントパッケージをalexandriaパッケージに動的に束縛する。
そしてexample::iota関数を呼び出そうとする。
もちろんそんな関数はEXAMPLEパッケージに定義されてはいない。
だからエラーが発せられる。

WITH-PACKAGEはEVAL前にS式に手を加える方法を選んだ。
その通り,　それはマクロの仕事です。

WITH-PACKAGEを使うと、Lispはカレントパッケージを、使うべき新しいパッケージに束縛する。
それからS式を見て回っていく。
シンボルに出会うと、Lispはブラックリストをチェックする。
もしそのシンボルがブラックリストに載っていたなら、そのシンボルはカレントパッケージにインターンされる。（勿論その時点でカレントパッケージは、使うべき新しいパッケージに束縛されている）。
さもなくば何もせず、見回りを続けていく。

ゆえに上記のコードはマクロ展開後以下のようになる。

    (example::let((example::*package*(example::find-package keyword:alexandria)))
     (alexandria::iota 3))

このアルゴリズムはいくつかの制約を引き換えとする。

   * **使えるパッケージは一つのみ。**

これはたぶん問題とならないと思う。
ネストすればいいからです。

残念ながら、小さな制約はこれのみです。
他の制約は結構バグを生みやすい。

   * **強すぎるシャドウイング。**

FLETと似ていて、WITH-PACKAGEはとても強いシャドウイングを行う。

以下のようなコードがあるとしよう。

    (flet((car(arg)
            "Which do you like cl:car or me?"
            (declare(ignore arg))
            (princ "You chose me! I love you!")))
      (cl:car '(a b c)))

CL:CARを明示的に呼び出しているのに、FLETはお構い無しです。

    => You chose me! I love you!
    "You chose me! I love you!"

処理系にも依存するけど、少なくともGNU CLISPとCCLはこの振る舞いをする。

同様の振る舞いをWITH-PACKAGEも行う。

    (defun iota(arg)
      (declare(ignore arg))
      (princ "I am iota."))
    
    (with-package:with-use-package(:alexandria)
      (example::iota 3))
    => (0 1 2)

SBCLにはSTRING-TO-OCTETSという関数がある。
BABELやFLEXI-STREAMSといったライブラリにも同名の関数がある。

もし以下のコードをSBCLで評価したなら、、、

    (with-package:with-use-package(:babel)
      (with-package:with-use-package(:flexi-streams)
        (babel:string-to-octets "foo")
        (sb-ext:string-to-octets "bar")
        (string-to-octets "bazz")))

FLEXI-STREAMSのSTRING-TO-OCTETSが３回呼び出されることとなる。

   * **ヘルパー関数には関与しない。**

もしWITH-PACKAGEをDEFMACRO内で使いたいなら、さらに気をつけなくてはならない。
というのもWITH-PACKAGEはヘルパーコマンドの返り値には関与しないからです。

    (defun helper(num)
      `(iota ,num))
    
    (defmacro foo(num)
      (with-package:with-use-pacage(:alexandria)
        `(,@(helper num))))
    
    (foo 3)
    *** - EVAL: undefined function IOTA

WITH-PACKAGEが関与するのは引数として受け取ったBODYのみです。

(foo 3)というマクロ呼び出しは結局、、

(example::iota 3)へと展開される。

これを避けるためにはコマンドをバッククォートの中に入れてしまう必要がある。

    (defmacro foo(num)
      `(with-package:with-use-package(:alexandria)
         (,@(helper num))))
    
    (foo 3)
    =>(0 1 2)

   * **引数に対し名前分割が起きる。**

以下のサンプルコードはエラーを発する。

    (defun foo (png)
      (with-package:with-use-package(:zpng)
        (start-png png)
        (write-png png)
        (finish-png png)))

というのもパッケージZPNGはクラス名としてシンボルPNGをエクスポートしているからです。
ゆえに、上記のコードは以下のものと等価です

    (example::defun example::foo (example::png)
      (zpng:start-png zpng:png)
      (zpng:write-png zpng:png)
      (zpng:finish-png zpng:png))

不幸にも、この定義は合法となる（もっとも処理系に依存するのだけれど。sbclならスタイルウォーニングを発するだろう）
エラーは実行時に発せられる。

    (foo 3)
    *** - FOO: variable ZPNG:PNG has no value

もしシンボルZPNG:PNGが値を持っていたなら最悪だ。
実行時でもエラーは発せられないかもしれず、それでいてfooの返り値は、引数の変わりにZPNG:PNGの値が使われるため期待とことなるものとなってしまう。

これを避けるには３つの方法がある。

１：コマンドをDEFUNの外側に出す。

    (with-package:with-use-package(:zpng)
      (defun foo (png)
        (start-png png)
        (write-png png)
        (finish-png png)))

上記のコードは以下コードに展開される。

    (example::defun example::foo (zpng:png)
      (zpng:start-png zpng:png)
      (zpng:write-png zpng:png)
      (zpng:finish-png zpng:png)))

名前分割は起きない。

２：:EXCEPTキーワード引数を使う。

    (defun foo(png)
      (with-package:with-use-package(:zpng :except (:png))
        (start-png png)
        (write-png png)
        (finish-png png)))

３：WITH-IMPORTを代わりに使う。

使いたいシンボルが極少数ならWITH-IMPORTのほうがベターです。
この例に於いて、ZPNG:PNGはべつに興味深いシンボルではないはずです。

最後に。
----------------

WITH-PACKAGEには制約が多すぎると感じられたんじゃないでしょうか？
たぶんそれは正しい。
まさにそれが、僕が冒頭で”WITH-PACKAGEは名前衝突が起こる可能性を軽減させる一つの提案だ。"と書いた理由です。
（なんとも弱気ですね！）
もしWITH-PACKAGEに不満があるなら、他の方法を探してみて欲しい。
誰かが（その誰かには勿論僕自身も含まれているけれど）より便利な方法を見つけるまで、とりあえず僕はWITH-PACKAGEを使うことにします:)
あなたにとっても便利なものであったらうれしい。