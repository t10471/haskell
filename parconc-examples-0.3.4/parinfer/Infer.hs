--
-- Adapted from the program "infer", believed to have been originally
-- authored by Philip Wadler, and used in the nofib benchmark suite
-- since at least the late 90s.
--

module Infer (inferTerm,inferTop) where

import Data.List(nub)

import  MyList                  (minus)
import  Type                  (TVarId, MonoType (..), PolyType (All),
                               arrow, intType, freeTVarMono)
import  Term
import  Substitution          (Sub, applySub, lookupSub, makeSub)
import  Environment
import  InferMonad
import  Control.Monad.Par.Scheds.Trace
import  qualified Data.Set as Set
import  qualified Data.Map as Map
import  Data.Map (Map)
import  Data.Maybe
import Control.Monad
import Debug.Trace
import MaybeM
import StateX         (ddd, StateX, returnSX, eachSX, thenSX, toSX, putSX, getSX, useSX)
import Substitution
import MyDebug

zipp                          :: (Show a, Show b) => [a] -> [b] -> [(a,b)]
zipp x y                      = zip (debug "zipp x = " x) (debug "zipp y = " y) 

-- xxs は [TVarId], tt は MonoType
-- 型の番号を振りなおし、かぶらないようにする
specialiseI                   :: PolyType -> Infer MonoType
specialiseI (All xxs tt)      =  freshesI (length xxs) `thenI` (\yys ->
                                 returnI (
                                  (debug "specialiseI applySubs::MonoType  = " (applySubs xxs yys 
                                  (debug "specialiseI tt::MonoType = " tt)))
                                 ))

-- Sub は TVarId と MonoType のマップ
applySubs                     :: [TVarId] -> [MonoType] -> MonoType -> MonoType
applySubs xxs yys tt          =  applySub (makeSub (zipp xxs yys)) tt

generaliseI                   :: Env -> MonoType -> Infer PolyType
generaliseI aa tt             =  getSubI `thenI` (\s ->
 				 let aaVars = nub (freeTVarSubEnv s aa) in
				 let ttVars = nub (freeTVarMono tt) in
				 let xxs    = ttVars `minus` aaVars in
                                 returnI ((debug "result generaliseI::PolyType = " (All xxs tt)))
                                 )
freeTVarSubEnv                :: Sub -> Env -> [TVarId]
freeTVarSubEnv s aa           =  concat (map (freeTVarMono . lookupSub s)
                                             (freeTVarEnv aa))

inferTerm  ::  Env -> Term -> Infer MonoType
inferTerm _  (Int _)  = returnI intType
-- 変数
inferTerm aa (Var x)  =
  ((debug "inferTerm Var x::VarId " x) 
      `elem` 
  (debug "inferTerm Var domEnv aa::[VarId] " (domEnv aa)))             `guardI` (
      let ss = lookupEnv aa x in
      -- 変数の番号を振りなおししてかぶらないようにする
      specialiseI (debug "inferTerm Var specialiseI ss::PolyType " ss) `thenI`  (\tt ->
      -- Envに置換できるのがあれば置換する
      substituteI (debug "inferTerm Var substituteI tt::MonoType " tt) `thenI`  (\uu  ->
      returnI  
        (debug "inferTerm Var returnI uu::MonoType " uu)
      )))
-- 関数
inferTerm aa (Abs x v)  =
      freshI                                                            `thenI` (\xx ->
      inferTerm 
        (extendLocal
          (debug "inferTerm Abs extendLocal aa::Env " aa)
          (debug "inferTerm Abs extendLocal x::VarId " x)
          (debug "inferTerm Abs extendLocal xx::MonoType " xx)
        ) 
        (debug "inferTerm Abs v::Term " v)                              `thenI` (\vv ->
      substituteI 
        (debug "inferTerm Abs substituteI xx::MonoType " xx)            `thenI` (\uu ->
      returnI (
        (debug "inferTerm Abs returnI uu::MonoType " uu) 
        `arrow` 
        (debug "inferTerm Abs returnI vv::MonoType " vv)
      ))))
-- 式
inferTerm aa (App t u)  =
      inferTerm 
        (debug "inferTerm App inferTerm aa::Env " aa)
        (debug "inferTerm App inferTerm t::Term " t)                    `thenI` (\tt ->
      inferTerm 
        (debug "inferTerm App inferTerm aa::Env " aa)
        (debug "inferTerm App inferTerm t::Term " u)                    `thenI` (\uu ->
      freshI                                                            `thenI` (\xx ->
      -- 2つのinfertermを結合
      unifyI 
        (debug "inferTerm App unifyI tt::MonoType " tt) 
        (
          (debug "inferTerm App unifyI uu::MonoType " uu) 
          `arrow` 
          (debug "inferTerm App unifyI xx::MonoType " xx) 
        )                                                               `thenI` (\() ->
      substituteI 
        (debug "inferTerm App substituteI xx::MonoType " xx)            `thenI` (\vv ->
      returnI 
        (debug "inferTerm App returnI vv::MonoType " vv) 
      )))))
inferTerm aa (Let x u v)  = do
    -- ss <- inferRhs aa u
    -- inferTerm (extendGlobal aa x ss) v
    ss <- inferRhs' aa u
    inferTerm (e aa x ss) (debug "inferTerm Let extendGlobal v::Term " v) 

inferRhs :: Env -> Term -> Infer PolyType
inferRhs aa u = do
    uu <- inferTerm aa u
    generaliseI aa uu

inferRhs' :: Env -> Term -> Infer PolyType
inferRhs' aa u = inferRhs 
                (debug "inferTerm Let inferRhs aa::Env " aa)
                (debug "inferTerm Let inferRhs u::Term " u)

-- extendGlobal
inferTopRhs :: Env -> Term -> PolyType
inferTopRhs aa u = useI (error "type error") $ do
    uu <- inferTerm aa u
    generaliseI (debug "aa " aa) (debug "uu " uu)

e :: Env -> VarId -> PolyType -> Env
e aa x ss = extendGlobal
    (debug "inferTerm Let extendGlobal aa::Env " aa) 
    (debug "inferTerm Let extendGlobal x::VarId " x)
    (debug "inferTerm Let extendGlobal ss::PolyType " ss)

-- 並列型推論器用の環境、計算途中の並列時に使用する
type TopEnv = Map VarId (IVar PolyType)

-- <<inferTop
inferTop :: TopEnv -> [(VarId,Term)] -> Par [(VarId,PolyType)]
-- binds は字句解析結果
inferTop topenv0 binds = do
  topenv1 <- foldM inferBind topenv0 binds                          -- <1>
  -- 全変数の簡約待ち、結果を返す
  mapM (\(v,i) -> do t <- get i; return (v,t)) (Map.toList topenv1) -- <2>
-- >>

-- <<inferBind
inferBind :: TopEnv -> (VarId,Term) -> Par TopEnv
inferBind topenv (x,u) = do
  -- IVarを作成
  vu <- new                                                     -- <1>
  fork $ do                                                     -- <2>
    let fu = Set.toList (freeVars (debug "inferBind u " u))                            -- <3>
    -- topEnvからfuを取得
    tfu <- mapM (get . fromJust . flip Map.lookup topenv) fu    -- <4>
    -- Envに自由変数と型を紐づける
    let aa = makeEnv (zip (debug "inferBind fu " fu) (debug "inferBind tfu " tfu))                               -- <5>
    put vu (inferTopRhs aa u)                                   -- <6>
  return (Map.insert (debug "inferBind x " x) vu topenv)                               -- <7>
-- >>


maina :: IO()
maina = do
  let m0 = TVar "a0"
  let p0 = All [] m0
  let s0 = specialiseI p0
  let a0 = TVar "a0"
  let a1 = TVar "a1"
  let a2 = TVar "a2"
  let a3 = TVar "a3"
  let a4 = TVar "a4"
  let p  = (\x -> useM ((TVar "faild", emptySub), 0) (ddd(ddd(mmm x) emptySub) 0))
  let pp = (\x -> useM (((), emptySub), 0)           (ddd(ddd(mmm x) emptySub) 0))
  -- print $ (++) "s0 = "  $ show $ p s0
  -- print $ (++) "applySub m0 = "  $ show $ applySub emptySub m0
  let u1 = unifyI a0 (a1 `arrow` a3) 
  -- print $ (++) "u1 = " $ show $ pp u1 
  -- u1 = (((),[\"a0\" : a1 -> a3]),0)
  -- let s1 = u1 >>= substituteI 
  let vv =  a1 `arrow` a2 `arrow` a4
  let ret = returnI (vv `arrow` a4)
  -- print $ (++) "ret = "  $ show $ p ret
  print "maina end"

mainb :: IO()
mainb = do
  let a0 = TVar "a0"
  let a1 = TVar "a1"
  let a2 = TVar "a2"
  let a3 = TVar "a3"
  let a4 = TVar "a4"
  let p  = (\x -> useM ((TVar "faild", emptySub), 0) (ddd(ddd(mmm x) emptySub) 0))
  let pp = (\x -> useM (((), emptySub), 0)           (ddd(ddd(mmm x) emptySub) 0))
  let x = "x"
  let y = "y"
  let z = "z"
  let vx = Var x
  let vy = Var y
  let vz = Var z
  let app1 = App vx vy
  let app2 = App app1 vz
  let abs1 = Abs z app2
  let abs2 = Abs y abs1
  let abs3 = Abs x abs2
  -- print $ (++) "abs3 = " $ show $ abs3
  -- t::Term App Var x Var y
  -- aa::Env [x : All . a0,y : All . a1,z : All . a2]
  let env0 = emptyEnv
  let env1 = extendGlobal env0 "x" (All [] a0)
  let env2 = extendGlobal env1 "y" (All [] a1)
  let env3 = extendGlobal env2 "z" (All [] a2)
  let im0 = inferTerm env3 app1
  let im1 = inferTerm env3 app2
  let im2 = inferTerm env2 abs1
  let im3 = inferTerm env1 abs2
  let im4 = inferTerm env0 abs3
  -- print $ (++) (show app1 ++ " = ") $ show $ p $
  --   freshI           >>= (\x0  ->
  --   freshI           >>= (\x1  ->
  --   freshI           >>= (\x2  ->
  --   im0
  --   )))
  -- print $ (++) (show app2 ++ " = ") $ show $ p $
  --   freshI           >>= (\x0  ->
  --   freshI           >>= (\x1  ->
  --   freshI           >>= (\x2  ->
  --   im1
  --   )))
  -- print $ (++) (show abs1 ++ " = ") $ show $ p $
  --   freshI           >>= (\x0  ->
  --   freshI           >>= (\x1  ->
  --   im2
  --   ))
  -- print $ (++) (show abs2 ++ " = ") $ show $ p $
  --   freshI           >>= (\x0  ->
  --   im3
  --   )
  -- print $ (++) (show abs3 ++ " = ") $ show $ p $
  --       im4
  print "mainb end"

mainc :: IO()
mainc = do
  let p  = (\x -> useM ((TVar "faild", emptySub), 0) (ddd(ddd(mmm x) emptySub) 0))
  let ia0 = "a0"
  let ia1 = "a1"
  let ia2 = "a2"
  let ia3 = "a3"
  let ia4 = "a4"
  let a0 = TVar ia0
  let a1 = TVar ia1
  let a2 = TVar ia2
  let a3 = TVar ia3
  let a4 = TVar ia4
  -- App App App Var a Var b Var c Int 1
  -- aa::Env [a : All a1 a2 a4. (a1 -> a2 -> a4) -> a1 -> a2 -> a4,
  --          b : All a1 a2. (a1 -> a2) -> a1 -> a2,
  --          c : All a0. a0 -> a0]
  let aa2 = a1 `arrow` a2 
  let aa4 = a1 `arrow` a2 `arrow` a4
  let ma = TCon "->" [aa4, aa4]
  let mb = TCon "->" [aa2, aa2]
  let mc = TCon "->" [a0, a0]
  let a = All [ia1, ia2, ia4] ma
  let b = All [ia1, ia2] mb
  let c = All [ia0] mc
  let env0 = emptyEnv
  let env1 = extendGlobal env0 "a" a
  let env2 = extendGlobal env1 "b" b
  let env3 = extendGlobal env2 "c" c
  let va = Var "a"
  let vb = Var "b"
  let vc = Var "c"
  let i = Int 1
  let app1 = App va vb 
  let app2 = App app1 vc 
  let app3 = App app2 i 
  let im1 = inferTerm env3 app1
  let im2 = inferTerm env3 app2
  let im3 = inferTerm env3 app3
  print $ (++) (show app1 ++ " = ") $ show $ p $
    im1
  print $ (++) (show app2 ++ " = ") $ show $ p $
    im2
  print $ (++) (show app3 ++ " = ") $ show $ p $
    im3
  print "mainc end"


main :: IO()
main = do
  maina
  mainb
  mainc
