# 数当てゲームのプログラミング

> [!NOTE]
>
> [The Rust Programming Language](https://doc.rust-jp.rs/book-ja/)
> の第２章「数当てゲームのプログラミング」を元に作成しています。
> また、書籍「[入門 Haskell プログラミング](https://www.shoeisha.co.jp/book/detail/9784798161280)」のリンクが記載されていることがあります。

ハンズオン形式のプロジェクトに一緒に取り組むことで、Haskell の世界に飛び込んでみましょう！

プログラミング初心者向けの定番問題である「数当てゲーム」を実装してみましょう。
これは次のように動作します。
プログラムは 1 から 100 までのランダムな整数を生成します。
そして、プレーヤーに予想（した数字）を入力するように促します。
予想が入力されると、プログラムはその予想が小さすぎるか大きすぎるかを表示します。
予想が当たっているなら、お祝いのメッセージを表示し、ゲームを終了します。

## 環境構築

以下の手順で Haskell プロジェクトを管理するための stack ツールと Haskell をインストールします。

### 1. GHCup による Haskell のインストール

Haskell.org というコミュニティが運営する [haskell.org のダウンロードページ](https://www.haskell.org/downloads/)に行くと、[GHCup](https://www.haskell.org/ghcup/)により Haskell およびその周辺ツールをインストールすることが推奨されています。

### 2. [VSCode](https://code.visualstudio.com/) のインストール（任意）

### 3. VSCode の拡張機能のインストール（任意）

[Haskell](https://marketplace.visualstudio.com/items?itemName=haskell.haskell) がおすすめです。

## 新規プロジェクトの立ち上げ

新しいプロジェクトを立ち上げましょう。
プロジェクト管理ツールである Stack を使って新規プロジェクトを作成します。

```sh
stack new guessing-game-with-haskell
cd guessing-game-with-haskell
```

> [!WARNING]
>
> `guessing-game-with-haskell` までのパスに日本語を用いたディレクトリ名が無いようにしましょう。周辺ツールがうまく動作しない場合があります。

最初のコマンド `stack new` は、第 1 引数としてプロジェクト名 ( `guessing-game-with-haskell` ) を取ります。 2 番目のコマンドは新規プロジェクトのディレクトリに移動します。

生成された **package.yaml** ファイルを見てみましょう。

ファイル名： package.yaml

```yml
name: guessing-game-with-haskell
version: 0.1.0.0
github: "githubuser/guessing-game-with-haskell"
license: BSD-3-Clause
author: "Author name here"
maintainer: "example@example.com"
copyright: "2023 Author name here"

extra-source-files:
  - README.md
  - CHANGELOG.md

# Metadata used when publishing your package
# synopsis:            Short description of your package
# category:            Web

# To avoid duplicated efforts in documentation and dealing with the
# complications of embedding Haddock markup inside cabal files, it is
# common to point users to the README.md file.
description: >
  Please see the README
  on GitHub at <https://github.com/githubuser/guessing-game-with-haskell#readme>

dependencies:
  - base >= 4.7 && < 5
# 以下略...
```

`stack new` は"someFunc"と出力するプログラムを生成してくれます。
**src/Lib.hs** ファイルをチェックしてみましょう。

ファイル名： src/Lib.hs

```hs
module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"
```

`putStrLn` は引数の文字列を出力する"関数"です。
（Haskell において「関数」である条件として「値を返す」という条件があるので、正確には「関数」ではないです。ここでは関数のようなものという意味で"関数"と呼びます。）
`putStrLn` は文字列 String を受け取り、 `IO ()` 型を返却します。
`IO` は別の「型」を引数に取ります。
ここで`IO` が引数に取っているのは `()` です。
`()` は空のタプルであり、他の言語（C 言語や JavaScript）における void のようなものです。
`IO` はユーザー入力の読み取りやファイルの読み取りの結果としての値を格納するための"箱"のようなものです。
ユーザー入力の読み取りやファイルの読み取りの結果など、結果の予想ができないものを安全に取り扱うときに用いる型です。
`puStrLn` が行うのは出力であり、String を受け取る訳ではないので、 void のような意味の型である `IO ()` を返却します。反対にユーザーの入力を受け取る関数 `getLine` (のちに登場します) は `IO String` を返却します。

`someFunc :: IO ()` は型の定義です。
`::` は型の宣言時に利用し、 左側に変数（関数）名、右側に型を記載します。

`someFunc = putStrLn "someFunc"` は"関数"の定義です。

`module Lib ( someFunc ) where` でモジュールの名前が `Lib` で、`someFunc` "関数"をエクスポートすることを宣言しています。
Haskell の規約では、 モジュールはそのモジュールと同じ名前のファイルに配置されることになっています。

次に **app/Main.hs** ファイルをチェックしてみましょう。

ファイル名： app/Main.hs

```hs
module Main (main) where

import Lib

main :: IO ()
main = someFunc
```

`module Main (main) where` でモジュールの名前が `Main` で、`main` "関数"をエクスポートすることを宣言しています。

`import Lib` で **src/Lib.hs** の Lib module を import しています。

`main = someFunc` で import した someFunc "関数" を main に代入しています。
`main :: IO ()` と宣言されていることから main の型は `IO ()` です。
someFunc の型も `IO ()` なので someFunc を main に代入することができます。

さて、 `stack run` コマンドを使って、この「someFunc」プログラムのコンパイルと実行を一気に行いましょう。

```console
Building all executables for guessing-game-with-haskell once. After a successful build
of all of them, only specified executables will be rebuilt.
guessing-game-with-haskell> configure (lib + exe)
Configuring guessing-game-with-haskell-0.1.0.0...
guessing-game-with-haskell> build (lib + exe)
Preprocessing library for guessing-game-with-haskell-0.1.0.0..
Building library for guessing-game-with-haskell-0.1.0.0..
[1 of 2] Compiling Lib
[2 of 2] Compiling Paths_guessing_game_with_haskell
ld: warning: -single_module is obsolete
Preprocessing executable 'guessing-game-with-haskell-exe' for guessing-game-with-haskell-0.1.0.0..
Building executable 'guessing-game-with-haskell-exe' for guessing-game-with-haskell-0.1.0.0..
[1 of 2] Compiling Main
[2 of 2] Compiling Paths_guessing_game_with_haskell
[3 of 3] Linking .stack-work/dist/aarch64-osx/ghc-9.6.3/build/guessing-game-with-haskell-exe/guessing-game-with-haskell-exe
ld: warning: ignoring duplicate libraries: '-lm'
guessing-game-with-haskell> copy/register
Installing library in /guessing-game-with-haskell/.stack-work/install/aarch64-osx/be2558e1a57eda9f909bd5a2758b3200be8a73bc81d573e68b55955fef427fab/9.6.3/lib/aarch64-osx-ghc-9.6.3/guessing-game-with-haskell-0.1.0.0-Kl0dIzrGiVrJhNqo02c9ZC
Installing executable guessing-game-with-haskell-exe in /guessing-game-with-haskell/.stack-work/install/aarch64-osx/be2558e1a57eda9f909bd5a2758b3200be8a73bc81d573e68b55955fef427fab/9.6.3/bin
Registering library for guessing-game-with-haskell-0.1.0.0..
someFunc
```

`stack run` はコンパイルも行うため、初めて実行する場合は、上記のようにコンパイル時に出力されるメッセージが出力されれます。

このゲーム（の開発）では各イテレーションを素早くテストしてから、次のイテレーションに移ります。 run コマンドは、今回のようにプロジェクトのイテレーションを素早く回したいときに便利です。

> 訳注：ここでのイテレーションは、アジャイルな開発手法で用いられている用語にあたります。
>
> イテレーションとは開発工程の「一回のサイクル」のことで、サイクルには、設計、実装、テスト、改善（リリース後の振り返り）が含まれます。
> アジャイル開発ではイテレーションを数週間の短いスパンで一通り回し、それを繰り返すことで開発を進めていきます。
>
> この章では「実装」→「テスト」のごく短いサイクルを繰り返すことで、プログラムに少しずつ機能を追加していきます。

**src/Main.hs** ファイルを開きましょう。
このファイルにすべてのコードを書いていきます。

## 予想を処理する

数当てゲームプログラムの最初の部分は、ユーザに入力を求め、その入力を処理し、期待した形式になっていることを確認することです。
手始めに、プレーヤーが予想を入力できるようにしましょう。
リスト 2-1 のコードを **app/Main.hs** に入力してください。

ファイル名：app/Main.hs

```hs
main :: IO ()
main = do
  putStrLn "Guess the number!"

  putStrLn "Please input your guess."

  guess <- getLine

  putStrLn ("You guessed: " ++ guess)
```

> リスト 2-1：ユーザに予想を入力してもらい、それを出力するコード

このコードには多くの情報が詰め込まれています。
行ごとに見ていきましょう。

main 関数がプログラムへのエントリーポイント（訳注：スタート地点）になります。
通常、 main 関数は Main モジュールに含まれます。

```hs
main :: IO ()
```

ここでは、`main` が `IO ()` 型であることを示しています。
`IO` は「型の箱」のようなものです。 Haskell には様々な"箱"があり、それぞれの"箱"はそれぞれの機能・意味を持っています。`IO` はその箱の中身がユーザー入力の読み取りやファイルの読み取りの結果としての値であることを示します。
Haskell の型には別の型を引数にとる型があり、`IO` は１つの型を引数に取ります。ここで`IO` が引数に取っているのは `()` です。先述の通り、`()` は空のタプルであり、他の言語における `void` のようなものです。

```hs
main = do
```

`do` は特別なキーワードで、後ろには式を並べて記載するブロックを書きます。
Haskell では python と同様、ブロックを波括弧`{}`ではなく、インデントで表現します。
なので、 `do` より下の行はインデントを１レベル上げる必要があります。
`do` の後ろに記載するブロックを do ブロックと言います。

```hs
  putStrLn "Guess the number!"

  putStrLn "Please input your guess."
```

このコードはゲームの内容などを示すプロンプトを表示し、ユーザに入力を求めています。
`putStrLn`は組み込み関数であり、画面に文字列を表示します。
組み込みの関数や型は標準モジュールに含まれており、自動的にインポートされます。
標準モジュールは **Prelude**（プレリュード）と呼ばれ、 [Hackage](https://hackage.haskell.org/package/base-4.19.0.0/docs/Prelude.html) でその中のすべてを見ることができます。

使いたい関数・型が Prelude にない場合は、`import`文で明示的にスコープに入れる必要があります。

### ユーザの入力を受け取る

```hs
  guess <- getLine
```

`getLine` は実行されると、ユーザから入力を受け取る組み込み"関数"です。
ユーザから文字列が入力されると、`IO String` 型を返します。
`<-` 記号は do ブロック内で利用でき、`IO` などの値の"箱"から値を取り出します。
`<-` の右辺には値の"箱"を書き、左辺には右辺の"箱"から取り出した値を格納する変数を書きます。
`guess <- getLine`では、右辺が `IO String` 型であり、左辺が`IO`の"箱"から取り出された`String`型の値を格納する変数となります。
ここで`String`の値となるのはユーザが入力した文字列です。

### ++演算で文字列を結合する

ここまでのコードで説明するのは残り 1 行だけです。

```hs
  putStrLn ("You guessed: " ++ guess)
```

この行はユーザの入力を現在保持している文字列を表示します。
++演算子は文字列を結合します。
Haskell では関数は常に演算子よりも優先されます。
そのため、 `putStrLn` 関数が "You guessed" を引数に取り実行される前に、括弧`()`を用いて、++演算子を先に処理させます。

### 最初の部分をテストする

数当てゲームの最初の部分をテストしてみましょう。
`stack run`で実行してください。

```console
$ stack run
(略)
Guess the number!
Please input your guess.
6
You guessed: 6
```

これで、キーボードからの入力を得て、それを表示するという、ゲームの最初の部分は完成になります。

## 秘密の数字を生成する

次にユーザが数当てに挑戦する秘密の数字を生成する必要があります。
この数字を毎回変えることで何度やっても楽しいゲームになります。
ゲームが難しくなりすぎないように 1 から 100 までの乱数を使用しましょう。

ここでは外部モジュール[System.Random](https://hackage.haskell.org/package/random-1.2.1.1/docs/System-Random.html) をインポートし、 randomRIO を利用します。
randomRIO は、最小値と最大値を表すタプルを受け取り、その範囲の乱数を生成します。

外部モジュールを利用するには **package.yaml** を編集する必要があります。
**package.yaml** ファイルを開いてください。
`dependencies`はプロジェクトが依存する外部モジュールと必要とするバージョンを stack に伝えます。
今回は`random`を追加します。

```yml
dependencies:
  - base >= 4.7 && < 5
  - random
```

`System.Random` モジュールを使って予想する数字を生成しましょう。
次のステップは **app/Main.hs** ファイルをリスト 2-3 のように更新することです。

ファイル名：app/Main.hs

```hs
module Main (main) where

import System.Random

main :: IO ()
main = do
  putStrLn "Guess the number!"

  secretNumber <- randomRIO (1, 100) :: IO Int

  putStrLn ("The secret number is: " ++ show secretNumber)

  putStrLn "Please input your guess."

  guess <- getLine

  putStrLn ("You guessed: " ++ guess)
```

リスト 2-3：乱数を生成するコードの追加

まず`import System.Random`という行を追加します。
`System.Random`モジュールには乱数生成関数が定義されており、それらの関数を使用するには、このモジュールを import しければなりません。

次に、`secretNumber <- randomRIO (1, 100) :: IO Int` を追加しています。
`randomRIO`"関数"にタプル `(1, 100)` を渡しています。
`::IO Int` は型注釈（type annotation）です。
`randomRIO`"関数"の返却値は、関数がどのように使用されているかに基づいてコンパイル時に決定される仕様（ポリモーフィック）になっています。
ここでは、Haskell が型を理解できる文脈がないため、型注釈を利用する必要があります。
`randomRIO (1, 100) :: IO Int` は `IO Int` 型であり、ここから `Int` の値を取り出すために `<-` を用いて `secretNumber` 変数に格納しています。

コードに追加した `putStrLn ("The secret number is: " ++ show secretNumber)` は秘密の数字を表示します。
これはプログラムを開発している間のテストに便利ですが、最終版からは削除する予定です。
プログラムが始まってすぐに答えが表示されたらゲームになりませんからね！
`show` 関数は様々な型を文字列`String`に変換する関数です。

試しにプログラムを何回か走らせてみてください。

```console
$ stack run
Guess the number!
The secret number is: 99
Please input your guess.
100
You guessed: 100

$ stack run
Guess the number!
The secret number is: 72
Please input your guess.
1
You guessed: 1
```

毎回異なる乱数を取得し、それらはすべて 1 から 100 の範囲内の数字になるはずです。
よくやりました！

## 予想と秘密の数字を比較する

さて、ユーザ入力と乱数が揃ったので両者を比較してみましょう。
このステップをリスト 2-4 に示します。

ファイル名：app/Main.hs

```hs
module Main (main) where

import System.Random

main :: IO ()
main = do
  putStrLn "Guess the number!"

  secretNumber <- randomRIO (1, 100) :: IO Int

  putStrLn ("The secret number is: " ++ show secretNumber)

  putStrLn "Please input your guess."

  guess <- getLine

  putStrLn ("You guessed: " ++ guess)

  let guessedNumber = read guess :: Int
  let ordering = compare guessedNumber secretNumber
  let message = case ordering of
        LT -> "Too small!"
        GT -> "Too big!"
        EQ -> "You win!"

  putStrLn message

```

リスト 2-4：二つの数値を比較したときに返される可能性のある値を処理する

`let guessedNumber = read guess` は `String`型の `guess` を `Int`型に変換し、変換した`Int`の値を`guessedNumber`変数に格納しています。
`read`関数はポリモーフィックであるため、その返却値は、関数がどのように使用されているかに基づいてコンパイル時に決定されます。ここでは、後続の文脈で Haskell が型を推論できるので、型注釈を利用する必要がありません。
また、do ブロック内で IO 型ではない変数を作成するには、let 文を使用します。

`let ordering = compare guessedNumber secretNumber` はユーザが予想した数字と秘密の数字を比較しています。
`compare`関数は「比較可能であるように実装されている型」を２つ引数に取り、`Ordering`型を返します。
ここでは、`Int` 型の引数 guessedNumber secretNumber を引数に取っています。
`Ordering` 型は`LT`（less than）, `EQ` (equal), `GT` (greater than)のいづれかであり、以下のように定義されています。

```hs
data Ordering = LT | EQ | GT
```

`data` はキーワードであり、新しい型を定義します。
`LT`, `EQ`, `GT` はデータコンストラクタであり、型の具体的なインスタンスを作成するために使用されます。
データコンストラクタを垂直バー（`|`） で区切ると 、「Ordering 型のインスタンスは LT または EQ または GT のどれかになる 」 と 宣言することになります。
Haskell の Bool 型は、 同様の方法で定義されています。

```hs
data Bool = True | False
```

それから`Ordering`型を使用する新しい 4 行をいちばん下に追加してしています。

```hs
  let message = case ordering of
        LT -> "Too small!"
        GT -> "Too big!"
        EQ -> "You win!"
```

ここでは`case`式を使用して`ordering`が LT の時の値、GT の時の値、 EQ の時の値をそれぞれ定義しています。
`case`式で`Ordering`のパターンを全て網羅した実装になっていない場合、コンパイラがワーニングを出力してくれます。

`read`関数は数値に変換できる文字にしか使えないので、エラーになる可能性があります。
たとえばユーザが文字列`abc`を入力したらエラーが発生します。

```console
$ stack run
Guess the number!
The secret number is: 8
Please input your guess.
abc
You guessed: abc
guessing-game-haskell-exe: Prelude.read: no parse
```

そこで変換に失敗した場合をハンドリング可能にする`readMaybe`関数を利用しましょう。

ファイル名：app/Main.hs

```hs
module Main (main) where

import System.Random
import Text.Read (readMaybe)

main :: IO ()
main = do
  --snip--

  putStrLn ("You guessed: " ++ guess)

  let maybeGuessedNumber = readMaybe guess
  let guessedNumber = case maybeGuessedNumber of
        Just parsedInput -> parsedInput
        Nothing -> error "Please type a number!"
  let ordering = compare guessedNumber secretNumber
  --snip--
```

リスト 2-4(改)： ユーザの入力が数値に変換できない場合を考慮する

`import Text.Read (readMaybe)` で `readMaybe` 関数を import します。
import した module の右型に括弧を書き、括弧内に利用する関数を書くことで、module 全体ではなく、その関数のみを import することができます。

`let maybeGuessedNumber = readMaybe guess` で `String` の guess を `Maybe Int` に変換し、 maybeGuessedNumber に格納しています。
`readMaybe` 関数は `Maybe` 型を返却します。
`Maybe` はいわば値の"箱"であり、その中には欠損する可能性のある値を入れます。
他のプログラミング言語では、欠損していることを null で表現することが多いですが、 Haskell では Maybe を利用します。
(Rust では Haskell と同様に `Result`型を用いて欠損の可能性を考慮しますが、Rust の Result 型に該当するのは Haskell では `Either`型です。ここでは Either 型の説明は割愛します。)
欠損する可能性のある値を Maybe の"箱"に入れて返却するように関数を実装することで、その関数を利用するコードでは常に Maybe を正しく扱うことを強制され、これによりプログラムが安全になります。
`Maybe`の"箱" は Haskell の外の世界とやり取りする場合に利用する `IO` の"箱"と同様の仕組みを有しています。
Haskell はこの"箱"(モナド)を取り扱う仕組みを言語レベルで備えています。

追加（修正）したコードのうち、最後の３行を見てみましょう。

```hs
  let guessedNumber = case maybeGuessedNumber of
        Just parsedInput -> parsedInput
        Nothing -> error "Please type a number!"
```

Maybe 型は data キーワードを利用して以下のように定義されています。

```hs
data Maybe a = Nothing | Just a
```

この型の宣言が以前の `Ordering`型と異なる点は `Maybe` が `a` という型の引数（型パラメータ）を持っている点です。
Maybe は何らかの型 `a` を受け取り別の型となります。
例えば、 `a` が `Int` であれば、 `Maybe Int`型になり、`a` が `String` であれば `Maybe String`型になります。
`Ordering`型が `LT` または `EQ` または `GT` であることを表すように、
`Maybe Int`型 は `Nothing` または `Just Int` であることを表します。
なので、`case`式ではこの２つのパターンに対応する値を記載します。

`Just parsedInput -> parsedInput` というコードは `Just Int` から `Int` の値を取り出しています。
`maybeGuessedNumber` が `Just 88` だった場合、 case 式は 88 に評価され、`guessedNumber` には 88 が格納されます。
`case`式を用いるとこのようにして"箱"から値を取り出すことができます。

`Nothing -> error "Please type a number!"` というコードでは、`maybeGuessedNumber` が `Nothing` だった場合、式 `error "Please type a number!"` が評価するようにしています。
`error` は Haskell における error の throw 方法です。

さあ、プログラムを実行し、変換に失敗する値を入力してみましょう！

```console
$ stack run
Guess the number!
The secret number is: 57
Please input your guess.
abc
You guessed: abc
guessing-game-haskell-exe: Please type a number!
CallStack (from HasCallStack):
  error, called at app/Main.hs:23:20 in main:Main
```

Please type a number! というエラーメッセージが表示されるようになりました！
このプログラムを何回か走らせ、数字を正しく言い当てたり、大きすぎる数字や小さすぎる数字を予想したりといった、異なる種類の入力に対する動作の違いを検証してください。

現在、ゲームの大半は動作していますが、まだユーザは 1 回しか予想できません。
ループを追加して、その部分を変更しましょう！

## 再帰で複数回の予想を可能にする

Haskell では loop, for, while, until といった状態の変化に依存するループ処理を利用できません。
Haskell では変数の状態を変化することを許容していないためです。
Haskell ではループの代わりに、再帰を利用します。

まず、再帰を利用するために main 関数から別の関数（`compareNumbers`とします）へコードを切り出します。
切り出すコードは繰り返し実行したいコードです。
`compareNumbers` 関数の最後で再起的に `compareNumbers` を呼び出します。

ファイル名：app/Main.hs

```hs
  --snip--
  putStrLn ("The secret number is: " ++ show secretNumber)

  compareNumbers secretNumber

compareNumbers :: Int -> IO ()
compareNumbers secretNumber = do
  putStrLn "Please input your guess."

  guess <- getLine

  putStrLn ("You guessed: " ++ guess)

  let maybeGuessedNumber = readMaybe guess
  let guessedNumber = case maybeGuessedNumber of
        Just parsedInput -> parsedInput
        Nothing -> error "Please type a number!"
  let ordering = compare guessedNumber secretNumber
  let message = case ordering of
        LT -> "Too small!"
        GT -> "Too big!"
        EQ -> "You win!"

  putStrLn message

  compareNumbers secretNumber
```

追加した `compareNumbers secretNumber` というコードでは追加した関数 `compareNumbers` に引数 `secretNumber` を渡しています。

`compareNumbers :: Int -> IO ()` では追加する関数のシグネチャ（関数の型）を宣言しています。
Haskell では１つの Int 型を引数にとり、Int 型を返却する関数の型は以下のように宣言します。
`someFunc :: Int -> Int`
また、２つの Int 型を引数にとり、Int 型を返却する関数の型は以下のように宣言します。
`someFunc :: Int -> Int -> Int`
ここでは １つの Int 型を引数にとり、`IO ()`型を返却する関数を作成したいので、`compareNumbers :: Int -> IO ()`のように宣言します。

`compareNumbers secretNumber = do` は `main = do` とほとんど同じですが、引数 `secretNumber` を用意しています。
引数は関数名と`=` の間に並べて書きます。
引数が２つの場合は `compareNumbers xNumber yNumber = do` のようになります。

main 関数の予想入力のプロンプト以降のコードを compareNumbers 関数に移動してます。
compareNumbers 関数の最後に `compareNumbers secretNumber` を追加し、
compareNumbers 関数が再帰的に呼び出されるようにしています。

プログラムはいつまでも推測を求めるようになりましたが、これが新たな問題を引き起こしています。
ユーザがゲームを終了できません！

ユーザはキーボードショートカットの ctrl-c を使えば、いつでもプログラムを中断させられます。
しかし中断方法はもう一つあります。
ユーザが数字以外の答えを入力すればプログラムはクラッシュします。
それを利用して以下のようにすれば終了できます。

```console
$ stack run
Guess the number!
The secret number is: 72
Please input your guess.
71
You guessed: 71
Too small!
Please input your guess.
73
You guessed: 73
Too big!
Please input your guess.
72
You guessed: 72
You win!
Please input your guess.
quit
You guessed: quit
guessing-game-haskell-exe: Please type a number!
CallStack (from HasCallStack):
  error, called at app/Main.hs:27:20 in main:Main
```

数字以外、例えば`quit`と入力すればゲームが終了します。
これは好ましい仕様ではありません。
正しい数字が予想されたときにゲームが停止するようにしたいですね。

### 正しい予想をした後に終了する

ユーザが勝ったらゲームが終了するようにプログラムしましょう。

```hs
  --snip--
  let (message, shouldContinue) = case ordering of
        LT -> ("Too small!", True)
        GT -> ("Too big!", True)
        EQ -> ("You win!", False)

  putStrLn message
  if shouldContinue then compareNumbers secretNumber else return ()
```

case 式で message だけでなく、shouldContinue フラグを返すようにします。
shouldContinue が True の場合は再帰呼出しを行い、False の場合は何もせずに `IO ()` を返却します。
`compareNumbers :: Int -> IO ()` と宣言している通り、compareNumbers 関数は `IO ()` を返却する必要があります。そのためには、do ブロックの最後の行は`IO ()`でなければなりません。
そのため、if 式の else の部分には`IO ()`を返却するように`return ()`を書きます。`return` は引数の値を"箱"に入れて、その"箱"を返却します。
ここでは、空のタブル`()` を`IO` の"箱"に入れて`IO ()` を返却しています。

ユーザが秘密の数字を正確に予想したときにプログラムがループを抜けるようになりました。

### 不正な入力を処理する

このゲームの動作をさらに洗練させるために、ユーザが数値以外を入力したときにプログラムをクラッシュさせるのではなく、数値以外を無視してユーザが数当てを続けられるようにしましょう。

ファイル名：app/Main.hs

```hs
module Main (main) where

import System.Random
import Text.Read (readMaybe)

main :: IO ()
main = do
  putStrLn "Guess the number!"

  secretNumber <- randomRIO (1, 100) :: IO Int

  putStrLn ("The secret number is: " ++ show secretNumber)

  compareNumbers secretNumber

compareNumbers :: Int -> IO ()
compareNumbers secretNumber = do
  putStrLn "Please input your guess."

  guess <- getLine

  let maybeGuessedNumber = readMaybe guess
  case maybeGuessedNumber of
    Nothing -> compareNumbers secretNumber
    Just guessedNumber -> do
      putStrLn ("You guessed: " ++ guess)

      let ordering = compare guessedNumber secretNumber
      let (message, shouldContinue) = case ordering of
            LT -> ("Too small!", True)
            GT -> ("Too big!", True)
            EQ -> ("You win!", False)

      putStrLn message
      if shouldContinue then compareNumbers secretNumber else return ()
```

リスト 2-5：数値以外の予想を無視し、プログラムをクラッシュさせるのではなく、もう 1 回予想してもらう

`maybeGuessedNumber` が `Nothing` の時、 error を throw するのではなく、 compareNumbers を再帰呼出しするようにしています。
`maybeGuessedNumber` が `Just Int` だった場合、`->` の右型の式を do 表記にしています。
do ブロック内に、ユーザーが正しい入力を行った場合の処理を記載します。

これでプログラム内のすべてが期待通りに動作するはずです。
試してみましょう。

```console
$ stack run
Guess the number!
The secret number is: 11
Please input your guess.
10
You guessed: 10
Too small!
Please input your guess.
99
You guessed: 99
Too big!
Please input your guess.
foo
Please input your guess.
11
You guessed: 11
You win!
```

素晴らしい！
最後にほんの少し手を加えれば数当てゲームは完成です。
このプログラムはまだ秘密の数字を表示していることを思い出してください。
テストには便利でしたが、これではゲームが台無しです。
秘密の数字を表示している`putStrLn`を削除しましょう。
最終的なコードをリスト 2-6 に示します。

ファイル名：app/Main.hs

```hs
module Main (main) where

import System.Random
import Text.Read (readMaybe)

main :: IO ()
main = do
  putStrLn "Guess the number!"

  secretNumber <- randomRIO (1, 100) :: IO Int

  compareNumbers secretNumber

compareNumbers :: Int -> IO ()
compareNumbers secretNumber = do
  putStrLn "Please input your guess."

  guess <- getLine

  let maybeGuessedNumber = readMaybe guess
  case maybeGuessedNumber of
    Nothing -> compareNumbers secretNumber
    Just guessedNumber -> do
      putStrLn ("You guessed: " ++ guess)

      let ordering = compare guessedNumber secretNumber
      let (message, shouldContinue) = case ordering of
            LT -> ("Too small!", True)
            GT -> ("Too big!", True)
            EQ -> ("You win!", False)

      putStrLn message
      if shouldContinue then compareNumbers secretNumber else return ()

```

リスト 2-6：数当てゲームの完全なコード

## まとめ

数当てゲームを無事に作り上げることができました。
おめでとうございます！
