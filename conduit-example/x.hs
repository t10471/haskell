{-# LANGUAGE OverloadedStrings #-}
import Data.Conduit
import qualified Data.ByteString as BS
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import Control.Monad.Trans.Resource

takeWhile' :: Monad m => (a -> Bool) -> Conduit a m a
takeWhile' f = do
  mx <- await
  case mx of
    Nothing -> return ()
    Just x
      | f x -> yield x >> takeWhile' f
      | otherwise -> return ()

main = do
  ss <-
    runResourceT $
    CB.sourceFile "hogehoge"
    $= CB.lines
    $= takeWhile' (/= "END")
    $$ CL.consume
  print ss
