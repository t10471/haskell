module MaxOccurs where

maxOccurs :: [Int] -> (Int, Int)
maxOccurs a =
    (m, n)
  where
    m = maximum a
    n = length (filter (==m) a)


{-
http://two-wrongs.com/how-laziness-works

ghc -fforce-recomp -O0 MaxOccurs.hs -ddump-simpl to get the Core code,
ghc -fforce-recomp -O0 MaxOccurs.hs -ddump-stg to get the STG code,
ghc -fforce-recomp -O0 MaxOccurs.hs -ddump-cmm to get the C-- code, and
ghc -fforce-recomp -O0 MaxOccurs.hs -ddump-asm to get the assembly code,


These two occurrences correspond to the two places in your Haskell code where m is used! 
それら2つの出現場所はHaskellのコードのmが使われている2つの個所に対応する!
You might have picked up on the pattern already, but in case you haven't: this is the set-up of two new thunks.
このパターンはすでにとりあげたと思うかもしれないが、このケースは違う。2つの新しいサンクを準備している
The top three lines are how the thunk that represents n is constructed, 
and the bottom three lines creates the thunk that represents the return value of (m, n). 
上の3行はnの構築の表現についてのサンクであり、下の3行は戻り値(m,n)の値の表現するサンクを作成している
(You can spot the latter quite easily, 
because the thunk points to code referenced by (,)_con which presumably stands for "(,) constructor".)
後者は簡単に見つけることができる。なぜなら、サンクはおそらく、(,)コンストラクターを表現であろう(,)_conによって参照されているコードを示している
Something that might surprise if you're not used to lazy evaluation is the way your maxOccurs function ends. 
maxOccurs関数の終わりの方が遅延評価を使っていないことに驚くかもしれません
It ends not with a bang, but with a whimper:
バングで終わらず、弱弱しく終わる

call (P64[Sp])(R1) args: 8, res: 0, upd: 8;

This means, "return the tuple thunk". That's it. Nothing has been computed. 
これはタプルのサンクを返すということであり。それだけであり。計算はしない。
All the function does is set up a bunch of thunks and then return the outermost of them. 
全ての関数はサンクの準備を行い一番外側のサンクを返す
We've just painstakingly set up an elaborate contraption, and we have no guarantee that it will ever be sprung. 

If the caller doesn't do something with the tuple, none of the code we set up will get executed.
もし呼び出し元が何もタプルにしないなら、準備したコードを実行するものはいない
However, if the calling code uses the tuple such that it forces either side of it, eventually, 
the code for the m thunk will be called. 
しかし、タプルのどちらかを使ったコードが呼ばれれば、段々、mのサンクは呼ばれていく

That is, this code, except the real code has more error handling and meta data:
実際のコードからエラーハンドリングやメタデータを除いたのがこれである

m_s18n_entry() { //  [R1]
    _s18n::P64 = R1;
    I64[Sp - 16] = stg_upd_frame_info;
    P64[Sp - 8] 　= _s18n::P64;
    _s18m::P64 　　= P64[_s18n::P64 + 16];
    R2 = Data.Foldable.$fFoldable[]_closure;
    I64[Sp - 40] = stg_ap_pp_info;
    P64[Sp - 32] = GHC.Classes.$fOrdInt_closure;
    P64[Sp - 24] = _s18m::P64;
    Sp = Sp - 40;
    call Data.Foldable.maximum_info(R2) args: 48, res: 0, upd: 24;
}

This procedure sets up two further procedure calls, of which one is exciting and the other is the regular maximum call.
このプロシージャはさらに2つのプロシージャを準備する 1つはエキサイティングで一つは普通のmaximumの呼び出しである
One thing should be said about how STG is translated to imperative code: 
それは、どうやってSTGが重要なコードを翻訳しているかについて言及している
an STG function never really returns back to its caller. 
STG関数は呼び出し元には決して戻さない
The caller decides where the function should return when it's done, 
by setting up the "return stack".
呼び出し元は準備されたリターンスタックによって処理が終わったときに関数がどこに戻すかを決める
Here's how the return stack gets set up in our m thunk:
ここにどうやってリターンスタックが準備されたmのサンクのから取得するか記す
I64[Sp - 16] = stg_upd_frame_info;
P64[Sp - 8] = _s18n::P64;

This means that when maximum has computed an Int, it will not return back to the m thunk 
– it will "return" into the stg_upd_frame procedure, 
which is also informed of the address of the current thunk (_s18n). 
The stg_upd_frame procedure has the responsibility of taking 
the computed value and putting it into the specified address 
– effectively overwriting the thunk code that's already there. 
mazimumがIntの計算をおこなったときmのサンクに戻さないという意味である
- それは現在のサンク(_s18n)のアドレスを教えるstg_upd_frameの中に戻るだろう
stg_upd_frameは計算結果の値取得と指定されたアドレスにそれを設置する責任を持つ
事実上すでにそこにあるサンクのコードは上書きされる
This ensures the value will never be computed again, but instead fetched directly from where the thunk used to be.
これは、値は決して再度計算されないことを保証する サンクが使われたところから直接取得するのではない

Final Words
最後の言葉

Well... that's that. 
これで全てだ
The C-- language that's currently in use in GHC seems very different from the C-- language the report specifies, 
which means I've been guessing at a bunch of the things I've talked about in this comment. 
GHCで使われているC--は規定されているC--とはだいぶ違うように使われているように思える
このコメントの中でいうと推測する
If someone knows of an up-to-date documentation for C--, please send me an email.
C--のドキュメントが更新されていることを知っている人は連絡をくれ
I also should say as a disclaimer that I haven't finished reading the STG paper yet, 
but I'm probably doing that later this week!

-}
