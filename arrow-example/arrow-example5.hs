{-# LANGUAGE Arrows #-}

module Main where

import Control.Arrow
import qualified Control.Category as Cat

-- http://d.hatena.ne.jp/haxis_fx/20110726/1311657175
-- loop のサンプル

class ArrowLoop a => ArrowCircuit a where
  delay :: b -> a b b

counter = proc reset -> do
            rec output <- returnA -< if reset then 0 else next
                next   <- delay 0 -< output + 1
            returnA -< output

type Seq a = [a]
newtype SeqMap b c = SM { runSM :: Seq b -> Seq c }

instance Cat.Category SeqMap where
  id = Cat.id
  SM g . SM f = SM ((Cat..) g f)

mapSeq = map
zipSeq (a, b) = zip a b
unzipSeq = unzip

instance Arrow SeqMap where
  arr f = SM (mapSeq f)
  first (SM f) = SM ( zipSeq . first f . unzipSeq  )

instance ArrowLoop SeqMap where
  loop (SM f) = SM (\as -> 
            let (bs, cs) = unzipSeq . f . zipSeq $ (as, (stream cs)) in bs)
    where stream ~(x:xs) = x:stream xs
-- this example code in the thesis will cause infinite loop
-- loop (SM f) = SM (loop (unzipSeq . f . zipSeq))

instance ArrowCircuit SeqMap where
  delay x = SM ((:) x)

main = print $ runSM counter (map b "ffffffffttfftt")
  where b 't' = True
        b 'f' = False
