{-# LANGUAGE MultiParamTypeClasses  #-}
import Data.Tuple (swap)

-- MultiParamTypeClassesクラスを使用したパターン
-- MultiParamTypeClassesは型推論がうまくいかないケースがおおい

-- ポケモンのタイプと名前
data Fire = Charmander | Charmeleon | Charizard deriving Show
data Water = Squirtle | Wartortle | Blastoise deriving Show
data Grass = Bulbasaur | Ivysaur | Venusaur deriving Show
-- タイプと技
data FireMove = Ember | FlameThrower | FireBlast deriving Show
data WaterMove = Bubble | WaterGun deriving Show
data GrassMove = VineWhip deriving Show

-- ポケモンと技の組み合わせ
class (Show pokemon, Show move) => Pokemon pokemon move where
  pickMove :: pokemon -> move
-- print $ pickMove Charmander はエラー
-- print $ pickMove Charmander :: FireMove しなければいけない
-- これを解決するのにデータ族が必要 pokemon3.hsで解決
instance Pokemon Fire FireMove where
  pickMove Charmander = Ember
  pickMove Charmeleon = FlameThrower
  pickMove Charizard = FireBlast

instance Pokemon Water WaterMove where
  pickMove Squirtle = Bubble
  pickMove _ = WaterGun

instance Pokemon Grass GrassMove where
  pickMove _ = VineWhip

-- 戦闘の表示
printBattle :: String -> String -> String -> String -> String -> IO ()
printBattle pokemonOne moveOne pokemonTwo moveTwo winner = do
  putStrLn $ pokemonOne ++ " used " ++ moveOne
  putStrLn $ pokemonTwo ++ " used " ++ moveTwo
  putStrLn $ "Winner is: " ++ winner ++ "\n"

-- 各組み合わせの戦闘
-- battle は本来値を返す必要がないが
-- 型推論がうまくいかないためにしかたなく値を返し
-- 型を指定している
class (Pokemon pokemon move, Pokemon foe foeMove)
  => Battle pokemon move foe foeMove where
  battle :: pokemon -> foe -> IO (move, foeMove)
  battle pokemon foe = do
    printBattle (show pokemon) (show move) (show foe) (show foeMove) (show pokemon)
    return (move, foeMove)
   where
    foeMove = pickMove foe
    move = pickMove pokemon
  
instance Battle Water WaterMove Fire FireMove
instance Battle Fire FireMove Water WaterMove where
  battle a b = fmap swap $ flip battle a b

instance Battle Grass GrassMove Water WaterMove
instance Battle Water WaterMove Grass GrassMove where
  battle a b = fmap swap $ flip battle a b
  
instance Battle Fire FireMove Grass GrassMove
instance Battle Grass GrassMove Fire FireMove where
  battle a b = fmap swap $ flip battle a b

main :: IO ()
main = do
  -- 型の補助がないとエラーになる
  battle Squirtle Charmander :: IO (WaterMove, FireMove)
  battle Charmeleon Wartortle :: IO (FireMove, WaterMove)
  battle Bulbasaur Blastoise :: IO (GrassMove, WaterMove)
  battle Wartortle Ivysaur :: IO (WaterMove, GrassMove)
  battle Charmeleon Ivysaur :: IO (FireMove, GrassMove)
  battle Venusaur Charizard :: IO (GrassMove, FireMove)
  putStrLn "Done Fighting"
