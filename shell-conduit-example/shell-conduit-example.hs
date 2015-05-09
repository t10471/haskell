{-# LANGUAGE ExtendedDefaultRules #-}
import Control.Monad.IO.Class
import Data.Conduit.Shell
import System.Directory
import Data.Char (ord, chr, toUpper, toLower)
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Binary as CB
import qualified Data.ByteString.Char8 as S8

main :: IO ()
main = do
  run (ls $| grep ".*" $| shell "cat" $| conduit (CL.map (S8.map toUpper)))
