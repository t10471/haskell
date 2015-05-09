{-# LANGUAGE PatternGuards   #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns    #-}
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

-- No language extensions
zipZag :: Seq a -> Seq b -> Seq (a, b)
zipZag xxs yys =
  case Seq.viewl xxs of
    x Seq.:< xs -> case Seq.viewr yys of
      ys Seq.:> y -> (x, y) Seq.<| zipZag xs ys
      _           -> Seq.empty
    _          -> Seq.empty

-- Pattern guards
zipZag' :: Seq a -> Seq b -> Seq (a, b)
zipZag' xxs yys
  | x Seq.:< xs <- Seq.viewl xxs
  , ys Seq.:> y <- Seq.viewr yys = (x, y) Seq.<| zipZag' xs ys
  | otherwise = Seq.empty

-- View patterns
zipZag'' :: Seq a -> Seq b -> Seq (a, b)
zipZag'' (Seq.viewl -> x Seq.:< xs) (Seq.viewr -> ys Seq.:> y) =
  (x, y) Seq.<| zipZag'' xs ys
zipZag'' _ _ = Seq.empty 

-- Unidirectional pattern synonyms + view patterns
pattern Empty <- (Seq.viewl -> Seq.EmptyL)
pattern x :< xs <- (Seq.viewl -> x Seq.:< xs)
pattern xs :> x <- (Seq.viewr -> xs Seq.:> x)

zipZag''' :: Seq a -> Seq b -> Seq (a, b)
zipZag''' (x :< xs) (ys :> y) = (x, y) Seq.<| zipZag''' xs ys
zipZag''' _ _                 = Seq.empty
