--
-- Adapted from the program "infer", believed to have been originally
-- authored by Philip Wadler, and used in the nofib benchmark suite
-- since at least the late 90s.
--

module Type
       (TVarId, TConId,
        MonoType (TVar, TCon), arrow, intType,
        PolyType (All),
        freeTVarMono, freeTVarPoly)
       where

import Shows
import MyList
import Data.List(nub)--1.3
import Control.DeepSeq

type  TVarId          =  String
type  TConId          =  String
data  MonoType        =  TVar TVarId
                      |  TCon TConId [MonoType]

instance NFData MonoType where
  rnf (TVar s) = head s `seq` ()
  rnf (TCon c ts) = rnf ts `seq` head c `seq` ()
-- 多相型
data  PolyType        =  All [TVarId] MonoType
u `arrow` v           =  TCon "->" [u,v]
infixr 5 `arrow`

intType               = TCon "Int" []

freeTVarMono                  :: MonoType -> [TVarId]
freeTVarMono (TVar x)         =  [x]
freeTVarMono (TCon k ts)      =  concat (map freeTVarMono ts)
freeTVarPoly                  :: PolyType -> [TVarId]
freeTVarPoly (All xs t)       =  nub (freeTVarMono t) `minus` xs

-- WDP: too bad deriving doesn't work
instance Eq MonoType where
    (TVar tv1)       == (TVar tv2)	 = tv1 == tv2
    (TCon tc1 args1) == (TCon tc2 args2) = tc1 == tc2 && (args1 == args2)
    other1	     == other2		 = False
-- end of too bad

instance  Show MonoType  where
      showsPrec d     =  showsMono d

instance NFData PolyType where
  rnf (All tvs t) = rnf tvs `seq` rnf t

showsMono             :: Int -> Shows MonoType
showsMono d (TVar xx)
      =  showsString xx
showsMono d (TCon "->" [uu,vv])
      =  showsParenIf (d>1)
         (showsMono 2 uu . showsString " -> " . showsMono 1 vv)
showsMono d (TCon kk tts)
      =  showsParenIf (d>9)
         (showsString kk .
          showsStar (\tt -> showsString " " . showsMono 10 tt) tts)

instance  Show PolyType  where
      showsPrec d (All xs t)  =  showsString "All " . showsString (unwords xs) .
                                 showsString ". " . showsMono 0 t
polyFromMono          :: MonoType -> PolyType
polyFromMono t        =  All (nub (freeTVarMono t)) t
main :: IO()
main = do
  let tvarid1 = "tvarid1" :: TVarId
  let tvarid2 = "tvarid2" :: TVarId
  let tvarid3 = "tvarid3" :: TVarId
  let tvarid4 = "tvarid4" :: TVarId
  let tConid1 = "tConid1" :: TConId
  let tConid2 = "tConid2" :: TConId
  let tConid3 = "tConid3" :: TConId
  let tConid4 = "tConid4" :: TConId
  let monotypeV1 = TVar tvarid1
  let monotypeV2 = TVar tvarid2
  let monotypeV3 = TVar tvarid3
  let monotypeV4 = TVar tvarid4
  let monotypeC1 = TCon tConid1 [monotypeV2]
  let monotypeC2 = TCon "->" [monotypeV3,monotypeV4]
  let monotypeC3 = TCon tConid3 [monotypeC1]
  let monotypeC4 = TCon tConid4 [monotypeC3]
  print $ (++) "monotypeC4 = "  $ show $ monotypeC4
  -- tConid4 (tConid3 (tConid1 tvarid2))
  let a1 = TVar "a1"
  let a2 = TVar "a2"
  let a4 = TVar "a4"
  let m1 =  TCon "->" [a1, a2]
  let m2 =  TCon "->" [m1, a4]
  let m3 =  TCon "->" [a1, a2, a4]
  print $ (++) "m2 = " $ show $ m2
  -- "m2 = (a1 -> a2) -> a4"
  print $ (++) "m3 = " $ show $ m3

