{-# LANGUAGE FlexibleContexts, BangPatterns #-}

import Data.Array.Repa
import Data.Array.Repa.IO.DevIL
import System.Environment
import Data.Array.Repa.Repr.ForeignPtr
import Data.Word

-- <<main
main :: IO ()
main = do
    -- 回転する角度, in file, out file
    [n, f1,f2] <- getArgs
    runIL $ do
      (RGB v) <- readImage f1                                            -- <1>
      rotated <- computeP $ rotate (read n) v :: IL (Array F DIM3 Word8) -- <2>
      writeImage f2 (RGB rotated)                                        -- <3>
-- >>

-- <<rotate
rotate :: Double -> Array F DIM3 Word8 -> Array D DIM3 Word8
rotate deg g = fromFunction (Z :. y :. x :. k) f      -- <1>
    where
        sh@(Z :. y :. x :. k)   = extent g
        -- ラジアンに変換
        !theta = pi/180 * deg                         -- <2>
        -- 回転の式
        -- x' = x cos theta - y sin theta
        -- y' = z sin theta + y cos theta
        !st = sin theta                               -- <3>
        !ct = cos theta
        -- 中心を求める
        !cy = fromIntegral y / 2 :: Double            -- <4>
        !cx = fromIntegral x / 2 :: Double

        f (Z :. i :. j :. k)                          -- <5>
          -- 回転後の画像位置が元画像サイズ内ならそれ値を返し
          -- 飛び出していたら黒(0)を返す
          | inShape sh old = g ! old                  -- <6>
          | otherwise      = 0                        -- <7>
          where
            fi = fromIntegral i - cy                  -- <8>
            fj = fromIntegral j - cx

            i' = round (st * fj + ct * fi + cy)       -- <9>
            j' = round (ct * fj - st * fi + cx)
            -- 回転後の位置
            old = Z :. i' :. j' :. k                  -- <10>
-- >>

