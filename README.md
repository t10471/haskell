
#メモ

## プログラムの説明

---------

- Lens1.hs  
  UserとProjectを定義し、setOwnerNameをつかった単純な値の変更
- Lens2.hs  
  NaiveLensにviewとsetを定義。レコード構文の説明
- Lens3.hs  
  NaiveLensにviewとoverを定義。constの説明
- Lens4.hs  
  Lensの完成。
- Lens5.hs  
  Traversableを使った繰り返しのあるLens
- Lens6.hs  
  Lens5の本当のライブラリを使用したversion
- Foldable.hs  
  Data.Foldableの説明
- Traversable.hs  
  Data.Traversableの説明
- NewType.hs  
  Pair1、Pair2を使って、タプルを持つnewTypeの説明

## プログラムの実行

-------

```
runghc Lens1.hs
```

## ghciのコマンド

------

- :l ファイル  
    ファイルのロード
- :! command...   
    シェルのコマンドcommandを実行する。
- :i  
    便利
- :type expression  
    expressionの型を推論し、印字する。多相型には明示的な全称量化が加えられる。推論に際して、単相性制限は適用されない。
- :kind type   
    typeの類を推論し、印字する。typeは任意の型式で、Either Intのような型構築子の部分適用であっても構わない。

## いろろなコマンド

------

```
ghc-pkg list
ghc-pkg describe persistent-1.3.3
apt-get install libghc-pcre-light-dev libpcrecpp0 libpcre3-dev
cabal list --installed
ghc-pkg unregister persistent-2.1
```

