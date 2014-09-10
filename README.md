
#メモ

## プログラムの説明

---------

- A1.hs  
  UserとProjectを定義し、setOwnerNameをつかった単純な値の変更
- A2.hs  
  NaiveLensにviewとsetを定義。レコード構文の説明
- A3.hs  
  NaiveLensにviewとoverを定義。constの説明
- A4.hs  
  Lensの完成。
- NewType.hs  
  Pair1、Pair2を使って、タプルを持つnewTypeの説明

## プログラムの実行

-------

```
runghc A1.hs
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

