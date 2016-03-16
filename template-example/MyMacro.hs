{-# LANGUAGE TemplateHaskell #-}

module MyMacro where

import Language.Haskell.TH
import Web.Users.Types
import Control.Monad (replicateM)
import Control.Monad.Trans (liftIO)

bigBadMathProblem :: Int -> Double
bigBadMathProblem i = (fromIntegral i :: Double) * 2.0

intToPat :: Int -> Pat
intToPat = LitP . IntegerL . toInteger

precomputeInteger :: Int -> Exp
precomputeInteger = LitE . RationalL . toRational . bigBadMathProblem
-- precomputeInteger = LitE . DoublePrimL . toRational . bigBadMathProblem

precompute :: [Int] -> DecsQ
precompute xs = do
  let name       = mkName "lookupTable"
      patterns   = map intToPat xs
      fnBodies   = map precomputeInteger xs
      precomputedClauses 
                 = zipWith (\body pattern -> Clause [pattern] (NormalB body) []) fnBodies patterns
      x'         = mkName "x"
      lastClause = [Clause [VarP x'] (NormalB appBody) []]
      appBody    = AppE (VarE (mkName "bigBadMathProblem")) (VarE x')
      clauses    = precomputedClauses ++ lastClause
  return [FunD name clauses]

deriveReader :: Name -> DecsQ
deriveReader rd =
  mapM (decForFunc rd) 
    [ 'destroyUserBackend
    , 'housekeepBackend
    , 'getUserIdByName
    , 'getUserById
    , 'listUsers
    , 'countUsers
    , 'createUser
    , 'updateUser
    , 'authUser
    , 'deleteUser
    ]

decForFunc :: Name -> Name -> Q Dec
decForFunc reader fn = do
  info <- reify fn
  arity <- maybe (reportError "Unable to get arity of name" >> return 0)
        (return . functionLevels) 
        (getType info)
  varNames <- replicateM (arity - 1) (newName "arg")
  b <- newName "b"
  let fnName     = mkName . nameBase $ fn
      bound      = AppE (VarE '(>>=)) (VarE reader)
      binder     = AppE bound . LamE [VarP b]
      varExprs   = map VarE (b : varNames)
      fullExpr   = foldl AppE (VarE fn) varExprs
      liftedExpr = AppE (VarE 'liftIO) fullExpr
      final      = binder liftedExpr
      varPat     = map VarP varNames
  return $ FunD fnName [Clause varPat (NormalB final) []]

functionLevels :: Type -> Int
functionLevels = go 0
  where
    go :: Int -> Type -> Int
    go n (AppT (AppT ArrowT _) rest) =
      go (n+1) rest
    go n (ForallT _ _ rest) =
      go n rest
    go n _ =
      n

getType :: Info -> Maybe Type
getType (ClassOpI _ t _ _) = Just t
getType (DataConI _ t _ _) = Just t
getType (VarI     _ t _ _) = Just t
getType (TyVarI   _ t    ) = Just t
getType _                  = Nothing

