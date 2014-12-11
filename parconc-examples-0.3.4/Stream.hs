{-# LANGUAGE BangPatterns, CPP #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fwarn-unused-imports #-}
-- -Wall 

-- A module for stream processing built on top of Control.Monad.Par

-- (In the future may want to look into the stream interface used by
--  the stream fusion framework.)

-- パイプライン並列を実現する
-- 生産者->変換器->消費者などのステージを想定
-- ちなみにParBufferはデータ並列

module Stream
 ( 
   Stream, streamFromList, streamMap, streamFold, streamFilter
 ) where

import Control.Monad.Par.Scheds.Trace as P
import Control.DeepSeq

--------------------------------------------------------------------------------
-- Types

-- <<IList
data IList a
  = Nil
  | Cons a (IVar (IList a))

type Stream a = IVar (IList a)
-- >>

instance NFData a => NFData (IList a) where
--  rnf Nil = r0
  rnf Nil = ()
  rnf (Cons a b) = rnf a `seq` rnf b

-- -----------------------------------------------------------------------------
-- Stream operators

-- <<streamFromList
streamFromList :: NFData a => [a] -> Par (Stream a)
streamFromList xs = do
  var <- new                            -- <1>
  fork $ loop xs var                    -- <2>
  -- この時点でに並列に作り始めいる
  return var                            -- <3>
    where
  loop [] var = put var Nil             -- <4>
  loop (x:xs) var = do                  -- <5>
    tail <- new                         -- <6>
    -- putは正格なのでxはこの時点で評価されている
    put var (Cons x tail)               -- <7>
    loop xs tail                        -- <8>
-- >>

-- <<streamMap
streamMap :: NFData b => (a -> b) -> Stream a -> Par (Stream b)
streamMap fn instrm = do
  outstrm <- new
  fork $ loop instrm outstrm
  return outstrm
 where
  loop instrm outstrm = do
    ilst <- get instrm
    case ilst of
      Nil -> put outstrm Nil
      Cons h t -> do
        newtl <- new
        put outstrm (Cons (fn h) newtl)
        loop t newtl
-- >>


-- | Reduce a stream to a single value.  This function will not return
--   until it reaches the end-of-stream.
-- <<streamFold
streamFold :: (a -> b -> a) -> a -> Stream b -> Par a
streamFold fn !acc instrm = do
  -- 生産者に追いついたらget呼び出しで次の要素が来るまでブロックされる
  ilst <- get instrm
  case ilst of
    Nil      -> return acc
    Cons h t -> streamFold fn (fn acc h) t
-- >>

streamFilter :: NFData a => (a -> Bool) -> Stream a -> Par (Stream a)
streamFilter p instr = do
    outstr <- new
    fork $ loop instr outstr
    return outstr
  where
    loop instr outstr = do
      v <- get instr
      case v of
        Nil -> put outstr Nil
        Cons x instr'
          | p x -> do
             tail <- new
             put_ outstr (Cons x tail)
             loop instr' tail
          | otherwise -> do
             loop instr' outstr

