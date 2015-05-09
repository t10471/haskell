{-# LANGUAGE Arrows #-}

import Control.Monad.Reader
import Control.Arrow
import Control.Arrow.Abort
import Control.Arrow.Reader

-- atl の説明のつもりが
-- 型族などのややこしいのが必要なので保留

main :: IO ()
main = do
    putStrLn "end"
