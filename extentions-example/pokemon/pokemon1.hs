
-- 何の工夫もなく実装したパターン

-- ポケモンのタイプと名前
data Fire = Charmander | Charmeleon | Charizard deriving Show -- These are actual Pokemon names
data Water = Squirtle | Wartortle | Blastoise deriving Show
data Grass = Bulbasaur | Ivysaur | Venusaur deriving Show
-- タイプと技
data FireMove = Ember | FlameThrower | FireBlast deriving Show -- These are actual Pokemon moves
data WaterMove = Bubble | WaterGun deriving Show
data GrassMove = VineWhip deriving Show
-- ポケモンと技の組み合わせ
pickFireMove :: Fire -> FireMove
pickFireMove Charmander = Ember
pickFireMove Charmeleon = FlameThrower
pickFireMove Charizard = FireBlast

pickWaterMove :: Water -> WaterMove
pickWaterMove Squirtle = Bubble
pickWaterMove _ = WaterGun

pickGrassMove :: Grass -> GrassMove
pickGrassMove _ = VineWhip

-- 戦闘の表示
printBattle :: String -> String -> String -> String -> String -> IO ()
printBattle pokemonOne moveOne pokemonTwo moveTwo winner = do
  putStrLn $ pokemonOne ++ " used " ++ moveOne
  putStrLn $ pokemonTwo ++ " used " ++ moveTwo
  putStrLn $ "Winner is: " ++ winner ++ "\n"

-- 各組み合わせの戦闘
battleWaterVsFire :: Water -> Fire -> IO ()
battleWaterVsFire water fire = do
  printBattle (show water) moveOne (show fire) moveTwo (show water)
 where
  moveOne = show $ pickWaterMove water
  moveTwo = show $ pickFireMove fire

battleFireVsWater = flip battleWaterVsFire

battleGrassVsWater :: Grass -> Water -> IO ()
battleGrassVsWater grass water = do
  printBattle (show grass) moveOne (show water) moveTwo (show grass)
 where
  moveOne = show $ pickGrassMove grass
  moveTwo = show $ pickWaterMove water

battleWaterVsGrass = flip battleGrassVsWater

battleFireVsGrass :: Fire -> Grass -> IO ()
battleFireVsGrass fire grass = do
  printBattle (show fire) moveOne (show grass) moveTwo (show fire)
 where
  moveOne = show $ pickFireMove fire
  moveTwo = show $ pickGrassMove grass

battleGrassVsFire = flip battleFireVsGrass

main :: IO ()
main = do
  printBattle "Water Pokemon" "Water Attack" "Fire Pokemon" "Fire Attack" "Water Pokemon"
  battleWaterVsFire Squirtle Charmander
  battleFireVsWater Charmeleon Wartortle
  battleGrassVsWater Bulbasaur Blastoise 
  battleWaterVsGrass Wartortle Ivysaur
  battleFireVsGrass Charmeleon Ivysaur
  battleGrassVsFire Venusaur Charizard
