{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies, ConstraintKinds #-}

import Control.Lens
import GHC.Exts -- 制約種 (constraint kind) を使うため

newtype ReadWrite = ReadWrite String
newtype ReadOnly  = ReadOnly String

       -- Functor f => LensLike' f ReadWrite String
rwName :: Lens' ReadWrite String
rwName f (ReadWrite s) = ReadWrite `fmap` f s

       -- Contravariant f, Functor f => LensLike' f ReadOnly String
roName :: Getter ReadOnly String
roName = to (\(ReadOnly a) -> a)

class Named a where
    type NameConstraint a (f :: * -> *) :: Constraint
    name :: NameConstraint a f => LensLike' f a String
--
instance Named ReadWrite where
    type NameConstraint ReadWrite f = Functor f
    name = rwName

instance Named ReadOnly where
    type NameConstraint ReadOnly f = (Contravariant f, Functor f)
    name = roName

main :: IO ()
main = do
  putStrLn "end"
