{-# LANGUAGE TemplateHaskell, Rank2Types, ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}


import Control.Lens
import Control.Applicative
import Control.Monad.State
import Control.Monad.IO.Class
import Data.IORef
import Control.Lens.Internal.Zoom 
import Data.Profunctor.Unsafe

-- http://qiita.com/fumieval/items/57cd7b3040d4268642d1
-- クラスはデータ型、メソッドはインスタンスの状態を変更するStateモナド、インスタンスはIORefに対応している

class (Zoomed m ~ Zoomed n, MonadState s m, MonadState t n) => Zoom' m n s t | m -> s, n -> t, m t -> n, n s -> m where
  zoom' :: LensLike' (Zoomed m c) t s -> m c -> n c

instance Monad z => Zoom' (StateT s z) (StateT t z) s t where
  zoom' l (StateT m) = StateT $ unfocusing #. l (Focusing #. m)

infix 9 .-
infix 9 .!

data Vec2 = Vec2 Float Float deriving Show

(&+) :: Vec2 -> Vec2 -> Vec2
Vec2 x0 y0 &+ Vec2 x1 y1 = Vec2 (x0 + x1) (y0 + y1)

newtype Default a = Default { getDefault :: a } deriving (Show)

ofDefault :: Iso (Default a) (Default b) a b
ofDefault = iso getDefault Default

-- 指定されたLensに対するメソッドの呼び出し
(.-) :: MonadState s m => Lens' s c -> StateT c m a -> m a
l .- m = do
  s <- get
  -- LensLike' (Control.Lens.Internal.Zoom.Zoomed (StateT c m) a) s c ->
  -- StateT c m a -> StateT s m a
  -- lの中のStateTにmを合成し、合成したStateTを返す
  (a, s') <- zoom l m `runStateT` s
  put s'
  return a

-- IORefに対するメソッドの呼び出し
(.!) :: MonadIO m => IORef c -> StateT c m a -> m a
ref .! m = do
  s <- liftIO $ readIORef ref
  (a, s') <- runStateT m s
  liftIO $ writeIORef ref s'
  return a

data Obj = Obj -- 基底クラスのメンバー変数
    { _position :: Vec2
    , _velocity :: Vec2
    , _name     :: String
    } deriving Show

makeClassy ''Obj

instance HasObj a => HasObj (Default a) where 
  obj = ofDefault . obj

class Objs c where -- 基底クラスのメソッド
  update :: MonadState c m => m ()
  draw   :: (MonadState c m, MonadIO m) => m ()

instance HasObj c => Objs (Default c) where -- メソッドの実装
  update = do
    p <- use position
    v <- use velocity
    position .= v &+ p
  draw = liftIO . print =<< use position 

data ObjA = ObjA -- クラスの継承(メンバー変数の拡張)
    { _superObjA :: Default Obj
    , _accel :: Vec2
    }

makeLenses ''ObjA

instance HasObj ObjA where 
  obj = superObjA . ofDefault

instance Objs ObjA where
  update = do
    a <- use accel
    v <- use velocity
    velocity .= a &+ v
    superObjA .- update -- スーパークラスのメソッドの呼び出し

  draw = superObjA .- draw

class Objs c => ObjsA c where -- クラスの継承(メソッドの拡張)
  reset :: MonadState c m => m ()

instance ObjsA ObjA where
  reset = superObjA . velocity .= Vec2 0 0

data WrapObjs = forall a. (Objs a, HasObj a) => WrapObjs (IORef a)

test = do
  a <- newIORef $ Default $ Obj { _position = Vec2 0 0, _velocity = Vec2 1 1 , _name = "Hoge" }
  b <- newIORef $ ObjA
    { _superObjA = Default $ Obj { _position = Vec2 10 0, _velocity = Vec2 (-1) (-4), _name = "Fuga" }
    , _accel = Vec2 0 0.5 }
  let objs = [WrapObjs a, WrapObjs b]
  forM_ [1..10] $ \_ -> do 
    forM_ objs $ \(WrapObjs i) -> do
      putStr =<< i .! use name
      putStr " "
      i .! update
      i .! draw

main :: IO ()
main = do
  putStrLn "end"
