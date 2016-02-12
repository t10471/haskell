{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeOperators              #-}

module PanGalacticDivision where

import           Control.Arrow (second, (&&&))
import           Data.List     (find, findIndex, transpose)
import           Data.Maybe
import           Data.Proxy

-- Standard unary natural number type
data Nat :: * where
  Z   :: Nat
  Suc :: Nat -> Nat

type One      = Suc Z
type Two      = Suc One
type Three    = Suc Two
type Four     = Suc Three
type Six      = Suc (Suc Four)
type Eight    = Suc (Suc Six)
type Ten      = Suc (Suc Eight)
type Thirteen = Suc (Suc (Suc Ten))

-- Singleton Nat-indexed natural numbers, to connect value-level and
-- type-level Nats
data SNat :: Nat -> * where
  SZ :: SNat Z
  SS :: Natural n => SNat n -> SNat (Suc n)

-- A class for converting type-level nats to value-level ones
class Natural n where
  toSNat :: SNat n

instance Natural Z where
  toSNat = SZ

instance Natural n => Natural (Suc n) where
  toSNat = SS toSNat

-- A function for turning explicit nat evidence into implicit
natty :: SNat n -> (Natural n => r) -> r
natty SZ     r = r
natty (SS n) r = natty n r

-- The usual canonical finite type.  Fin n has exactly n
-- (non-bottom) values.
data Fin :: Nat -> * where
  FZ :: Fin (Suc n)
  FS :: Fin n -> Fin (Suc n)

finToInt :: Fin n -> Int
finToInt FZ     = 0
finToInt (FS n) = 1 + finToInt n

deriving instance Show Nat
deriving instance Show (SNat a)
deriving instance Show (Fin a)

deriving instance Eq (Fin n)


class Eq a => Finite a where
  universe :: [a]

instance Natural n => Finite (Fin n) where
  universe = fins toSNat

fins :: SNat n -> [Fin n]
fins SZ     = []
fins (SS n) = FZ : map FS (fins n)

-- The product of two finite types is finite.
instance (Finite a, Finite b) => Finite (a,b) where
  universe = [(a,b) | a <- universe, b <- universe]

panGalacticDivision
  :: forall a b n. (Finite a, Eq b)
  => SNat n -> ((a, Fin (Suc n)) -> (b, Fin (Suc n))) -> (a -> b)
panGalacticDivision SZ      f = \a -> fst (f (a, FZ))
panGalacticDivision (SS n') f = panGalacticDivision n' (panGalacticPred n' f)

panGalacticPred
  :: (Finite a, Eq b, Natural n)
  => SNat n
  -> ((a, Fin (Suc (Suc n))) -> (b, Fin (Suc (Suc n))))
  -> ((a, Fin (Suc n)) -> (b, Fin (Suc n)))
panGalacticPred n f = \(a,i) -> second unFS (f' (a, FS i))
  where
    unFS :: Fin (Suc n) -> Fin n
    unFS FZ     = error "impossible!"
    unFS (FS i) = i
    oneRound = natty n $ shipOut . shapeUp
    -- iterate 'oneRound' beginning with the original function...
    fs = iterate oneRound f
    -- ... and stop when we reach a fixed point.
    f' = fst . head . dropWhile (uncurry (=/=)) $ zip fs (tail fs)
    f1 =/= f2 = all (\x -> f1 x == f2 x) universe

type Card v s = (v, s)
type PlayerSpot p s = (p, s)
type Hand v s = s -> Card v s
--              s -> (v, s)
type Game p v s = PlayerSpot p s -> Card v s
--                        (p, s) -> (v, s)


value :: Card v s -> v
value = fst

suit :: Card v s -> s
suit = snd

hand :: p -> Game p v s -> Hand v s
hand p g = \s -> g (p, s)

swap :: (Eq s, Eq v) => Card v s -> Card v s -> (Card v s -> Card v s)
swap c1 c2 = f
  where
    f c
      | c == c1   = c2
      | c == c2   = c1
      | otherwise = c

leftmost :: Finite s => s -> Hand v s -> Maybe s
leftmost targetSuit h = find (\s -> suit (h s) == targetSuit) universe

-- xx :: Finite s => s -> [t]
-- xx = universe

playRound :: (Finite s, Finite p, Eq v) => (Hand v s -> Card v s -> Card v s) -> Game p v s -> Game p v s
playRound withHand g = foldr (.) id swaps . g
  where
    swaps   = map (withHand . flip hand g) players
    players = universe

shapeUp :: (Finite s, Finite p, Eq v) => Game p v s -> Game p v s
shapeUp = playRound shapeUp1
  where
    badSuit          = head universe
    shapeUp1 theHand = case leftmost badSuit theHand of
        Nothing      -> id
        Just badSpot -> swap (theHand badSuit) (theHand badSpot)

shipOut :: (Finite s, Finite p, Eq v) => Game p v s -> Game p v s
shipOut = playRound shipOutHand
  where
    badSuit             = head universe
    spots               = universe
    shipOutHand theHand = foldr (.) id swaps
      where
        swaps            = map (shipOut1 . (theHand &&& id)) (drop 1 spots)
        shipOut1 ((_,s), spot)
          | s == badSuit = swap (theHand spot) (value (theHand badSuit), spot)
          | otherwise    = id

type Suit   = Fin
type Rank   = Fin
type Player = Fin

readRank :: SNat n -> Char -> Rank n
readRank n c = fins n !! (fromJust $ findIndex (==c) "A23456789TJQK")

readSuit :: SNat n -> Char -> Suit n
readSuit (SS _) 'S'                = FZ
readSuit (SS (SS _)) 'H'           = FS FZ
readSuit (SS (SS (SS _))) 'D'      = FS (FS FZ)
readSuit (SS (SS (SS (SS _)))) 'C' = FS (FS (FS FZ))

readGame :: SNat a -> SNat b -> SNat n -> String -> Game (Player a) (Rank b) (Suit n)
readGame a b n str = \(p, s) -> table !! finToInt p !! finToInt s
  where
    table          = transpose . map (map readCard . words) . lines $ str
    readCard [r,s] = (readRank b r, readSuit n s)

-- Example game from Doyle & Qiu
exampleGameStr :: String
exampleGameStr = unlines
  [ "4D 6H QD 8D 9H QS 4C AD 6C 4S"
  , "JH AH 9C 8H AS TC TD 5H QC JS"
  , "KC 6S 4H 6D TS 9S JC KD 8S 8C"
  , "5C 5D KS 5S TH JD AC QH 9D KH"
  ]

exampleGame :: Game (Player Ten) (Rank Thirteen) (Suit Four)
exampleGame = readGame toSNat toSNat toSNat exampleGameStr

suitSymbol :: Suit n -> String
suitSymbol = (:[]) . ("♠♥♦♣"!!) . finToInt  -- Huzzah for Unicode

rankStr :: Rank n -> String
rankStr n = rankStr' (finToInt n + 1)
  where
    rankStr' 1              = "A"
    rankStr' i | i <= 10    = show i
               | otherwise  = ["JQK" !! (i - 11)]


xx :: SNat Four
xx = toSNat

yy :: Finite s => s -> [s]
yy s = universe

zz :: Fin Four
zz = undefined

