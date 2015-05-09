{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies           #-}

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Word


-- 標準的なクラス定義
-- Listのインスタンス定義でEq制約が必要
-- Setのインスタンス定義でOrdが必要
-- インスタンス定義のたびに制約が増える・・・
class Collects_TC c where
  empty_TC  :: c e
  insert_TC :: Ord e => e -> c e -> c e
  member_TC :: (Eq e, Ord e) => e -> c e -> Bool

instance Collects_TC [] where
  empty_TC  = []
  insert_TC = (:)
  member_TC = elem

instance Collects_TC Set where
  empty_TC  = Set.empty
  insert_TC = Set.insert
  member_TC = Set.member

-- MPTCs(multi-parameter type classes)
-- 型変数を複数指定するにはMultiParamTypeClassesが必要
-- 制約付きの型変数をクラスに指定することで上記の問題を解決しようとするが・・・
-- 色々と型推論ができない問題がある
-- empty_MPTC :: Collects_MPTC e c => c
-- にはeが出てこないので推論できず使おうとするがエラーになる

class Collects_MPTC e c where
  empty_MPTC  :: c
  insert_MPTC :: e -> c -> c
  member_MPTC :: e -> c -> Bool

instance Eq e => Collects_MPTC e [e] where
  empty_MPTC  = []
  insert_MPTC = (:)
  member_MPTC = elem

instance (Eq e, Ord e) => Collects_MPTC e (Set e) where
  empty_MPTC  = Set.empty
  insert_MPTC = Set.insert
  member_MPTC = Set.member

-- e -> e1 -> s -> s
-- 型推論がうまくいかない
f_MPTC x y c = insert_MPTC x $ insert_MPTC y c

-- MPTC + Constructor Classes
-- c eとすることで曖昧さをなくす
-- ただし * -> * カインドしか使用できない問題がある
-- ByteStringはインスタンス化できない
-- [] :: * -> *
-- Set :: * -> *
-- ByteString :: *
class Collects_CC e c where
  empty_CC  :: c e
  insert_CC :: e -> c e -> c e
  member_CC :: e -> c e -> Bool

instance Eq e => Collects_CC e [] where
  empty_CC  = []
  insert_CC = (:)
  member_CC = elem

instance (Eq e, Ord e) => Collects_CC e Set where
  empty_CC  = Set.empty
  insert_CC = Set.insert
  member_CC = Set.member

-- e -> e -> c e -> c e うまく推論できる
f_CC x y c = insert_CC x $ insert_CC y c


-- Functional Dependencies
-- |c -> e は型cが決まればeが決まるという
-- FunctionalDependencies拡張
-- MultiParamTypeClassesは不要
class Collects_FD e c | c -> e where
  empty_FD  :: c
  insert_FD :: e -> c -> c
  member_FD :: e -> c -> Bool

instance Eq e => Collects_FD e [e] where
  empty_FD  = []
  insert_FD = (:)
  member_FD = elem

instance (Eq e, Ord e) => Collects_FD e (Set e) where
  empty_FD  = Set.empty
  insert_FD = Set.insert
  member_FD = Set.member

instance Collects_FD Word8 ByteString where
  empty_FD  = BS.empty
  insert_FD = BS.cons
  member_FD = BS.elem

-- うまく推論できる
f_FD x y c = insert_FD x $ insert_FD y c

-- Type Families
-- TypeFamilies拡張が必要
type family Elem c
type instance Elem [e] = e
type instance Elem (Set e) = e
type instance Elem ByteString = Word8

class Collects_TF c where
  empty_TF  :: c
  insert_TF :: Elem c -> c -> c
  member_TF :: Elem c -> c -> Bool

instance Eq c => Collects_TF [c] where
  empty_TF  = []
  insert_TF = (:)
  member_TF = elem

instance (Eq e, Ord e) => Collects_TF (Set e) where
  empty_TF  = Set.empty
  insert_TF = Set.insert
  member_TF = Set.member

instance Collects_TF ByteString where
  empty_TF  = BS.empty
  insert_TF = BS.cons
  member_TF = BS.elem


main :: IO ()
main = do
  putStrLn "end"

