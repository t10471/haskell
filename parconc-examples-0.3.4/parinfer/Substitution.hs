--
-- Adapted from the program "infer", believed to have been originally
-- authored by Philip Wadler, and used in the nofib benchmark suite
-- since at least the late 90s.
--

module Substitution   (Sub, applySub, lookupSub, emptySub, extendSub,
                       makeSub, thenSub, domSub, unifySub)
                      where

import Type
import FiniteMap
import MaybeM
import Shows
import MyDebug

data  Sub  =  MkSub (FM TVarId MonoType)
rep                           ::  Sub -> FM TVarId MonoType
rep (MkSub f)                 =   f
applySub                      ::  Sub -> MonoType -> MonoType
applySub s (TVar x)           =   lookupSub s x
applySub s (TCon k ts)        =   TCon k (map (applySub s) ts)
lookupSub                     ::  Sub -> TVarId -> MonoType
lookupSub s x                 =   lookupElseFM (TVar x) (rep s) x
unitSub                       ::  TVarId -> MonoType -> Sub
unitSub x t                   =   MkSub (makeFM [(x,t)])
emptySub                      ::  Sub
emptySub                      =   MkSub emptyFM
makeSub                       ::  [(TVarId, MonoType)] -> Sub
makeSub xts                   =   MkSub (makeFM xts)
extendSub                     ::  Sub -> TVarId -> MonoType -> Sub
extendSub s x t               =   s `thenSub` unitSub x (applySub s t)
thenSub                       ::  Sub -> Sub -> Sub
r `thenSub` s                 =   MkSub (mapFM (applySub s) (rep r) `thenFM` rep s)
domSub                        ::  Sub -> [TVarId]
domSub s                      =   domFM (rep s)
unifySub                              =  unify
unify                                 :: MonoType -> MonoType -> Sub -> Maybe Sub
unify (TVar x) u s                    =  unifyTVar (debug "unifyTVar1 x = " x) (debug "unifyTVar1 u = " u) (debug "unifyTVar1 s = " s)
unify t (TVar y) s                    =  unifyTVar (debug "unifyTVar2 y = " y) (debug "unifyTVar2 t = " t) (debug "unifyTVar2 s = " s)
unify (TCon j ts) (TCon k us) s       =  (j == k) `guardM` unifies (debug "unify unifies ts = " ts) (debug "unify unifies us = " us) (debug "unify unifies s = " s)
unifies                               :: [MonoType] -> [MonoType] -> Sub -> Maybe Sub
unifies [] [] s                       =  returnM (debug "unifies returnM = " (s))
unifies (t:ts) (u:us) s               =  unify t u s `thenM` (\s' -> unifies (debug "unifies ts = " (ts)) (debug "unifies us = " (us)) (debug "unifies s' = " (s')))
unifyTVar                             :: TVarId -> MonoType -> Sub -> Maybe Sub
unifyTVar x t s | x `elem` domSub s     =  unify (debug "lookupSub s x = " ((lookupSub s x))) (debug "unifyTVar unify t = " (t)) (debug "unifyTVar unify s = " (s))
                | TVar x == t         =  returnM s
                | x `elem` freeVars t   =  failM
                | otherwise           =  returnM (debug "extendSub s x t = " ((extendSub (debug "extendSub s = " (s)) (debug "extendSub x = " (x)) (debug "extendSub t = " (t)))))    
freeVars                              =  freeTVarMono

instance Show Sub where
      showsPrec d  =  showsSub
showsSub              :: Shows Sub
showsSub              = shows . rep 

main :: IO()
main = do
  let tvarid1 = "tvarid1" :: TVarId
  let tvarid2 = "tvarid2" :: TVarId
  let tvarid3 = "tvarid3" :: TVarId
  let tvarid4 = "tvarid4" :: TVarId
  let tConid1 = "tConid1" :: TConId
  let tConid2 = "tConid2" :: TConId
  let monotypeV1 = TVar tvarid1
  let monotypeV2 = TVar tvarid2
  let monotypeV3 = TVar tvarid3
  let monotypeV4 = TVar tvarid4
  let monotypeC1 = TCon tConid1 [monotypeV2]
  let monotypeC2 = TCon "->" [monotypeV3,monotypeV4]
  let fm1 = makeFM [(tvarid1,monotypeV1),(tConid1, monotypeC1)]
  let sub1 = makeSub ([(tvarid1,monotypeV1),(tConid1, monotypeC1)])
  let sub2 = makeSub ([(tvarid1,monotypeV2)])
  print $ (++) "unitSub tvarid1 monotypeV1 = "                             $ show $ unitSub   tvarid1                          monotypeV1
  -- "unitSub tvarid1 monotypeV1 = [\"tvarid1\" : tvarid1]"
  print $ (++) "makeSub ([(tvarid1,monotypeV1),(tConid1, monotypeC1)]) = " $ show $ makeSub   ([(tvarid1,monotypeV1),(tConid1, monotypeC1)])
  -- "makeSub ([(tvarid1,monotypeV1),(tConid1, monotypeC1)]) = [\"tvarid1\" : tvarid1,\n \"tConid1\" : tConid1 tvarid2]"
  print $ (++) "extendSub sub1 tvarid2 monotypeV2 = "                      $ show $ extendSub sub1                             tvarid2 monotypeV2
  -- "extendSub sub1 tvarid2 monotypeV2 = [\"tvarid2\" : tvarid2,\n \"tvarid1\" : tvarid1,\n \"tConid1\" : tConid1 tvarid2]"
  print $ (++) "lookupSub sub1 tvarid1 = "                                 $ show $ lookupSub sub1                             tvarid1
  -- "lookupSub sub1 tvarid1 = tvarid1"
  print $ (++) "lookupSub sub1 tConid1 = "                                 $ show $ lookupSub sub1                             tConid1
  -- "lookupSub sub1 tConid1 = tConid1 tvarid2"
  print $ (++) "applySub sub1 monotypeV1 = "                               $ show $ applySub  sub1                             monotypeV1
  -- "applySub sub1 monotypeV1 = tvarid1"
  print $ (++) "applySub sub1 monotypeC1 = "                               $ show $ applySub  sub1                             monotypeC1
  -- "applySub sub1 monotypeC1 = tConid1 tvarid2"
  print $ (++) "domSub sub1 = "                                            $ show $ domSub    sub1
  -- "domSub sub1 = [\"tvarid1\",\"tConid1\"]"
  print $ (++) "thenSub sub1 sub2 = "                                      $ show $ thenSub   sub1                             sub2
  -- "thenSub sub1 sub2 = [\"tvarid1\" : tvarid2,\n \"tvarid1\" : tvarid2,\n \"tConid1\" : tConid1 tvarid2]"
  print $ (++) "freeVars monotypeV1 = "  $ show $ freeVars monotypeV1
  -- ["tvarid1"]
  print $ (++) "freeVars monotypeC1 = "  $ show $ freeVars monotypeC1
  -- ["tvarid2"]
  print $ (++) "freeVars monotypeC2 = "  $ show $ freeVars monotypeC2
  -- ["tvarid3","tvarid4"]
  print $ (++) "domSub sub1 = "  $ show $ domSub sub1
  -- "domSub sub1 = [\"tvarid1\",\"tConid1\"]"
  print $ (++) "domSub sub2 = "  $ show $ domSub sub2
  -- "domSub sub2 = [\"tvarid1\"]"
-- unify                                 :: MonoType -> MonoType -> Sub -> Maybe Sub
-- unify (TVar x) u s                    =  unifyTVar x u s
-- unify t (TVar y) s                    =  unifyTVar y t s
-- unify (TCon j ts) (TCon k us) s       =  (j == k) `guardM` unifies ts us s
-- unifyTVar                             :: TVarId -> MonoType -> Sub -> Maybe Sub
-- unifyTVar x t s | x `elem` domSub s     =  unify (lookupSub s x) t s
--                 | TVar x == t         =  returnM s
--                 | x `elem` freeVars t   =  failM
--                 | otherwise           =  returnM (extendSub s x t)    
  -- print $ unifySub monotypeV1 monotypeV2 sub1
  -- unifyTvar tvarid1 monotypeV2 sub1
  -- lookupSub sub1 tvarid1
  print $ unifySub monotypeV3 monotypeV4 sub1
