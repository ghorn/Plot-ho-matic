{-# OPTIONS_GHC -Wall #-}

module DrawLine ( displayGraph ) where

import Control.Concurrent ( readMVar )
import Control.Monad ( zipWithM_ )
import qualified Data.Sequence as Seq
import qualified Data.Foldable as F
import Graphics.Rendering.OpenGL as GL

import Quotes ( PContainer(..), VarInfo(..) )

linspace :: Fractional b => b -> b -> Int -> [b]
linspace x0 x1 n =
  map ((+ x0) . (((x1-x0)/((realToFrac n)-1)) *) . realToFrac) [0..(n-1)]

drawLine :: (Fractional a, Real a) => Seq.Seq a -> IO ()
drawLine seq' = do
  let maxval = F.foldl' (\acc x -> max acc (abs x)) (1e-12) seq'
      num = Seq.length seq'
      xs = linspace (-1) 1 num
      ys = map ((/ (realToFrac maxval)) . realToFrac) $ F.toList seq'

--  putStrLn $ "\nxs: " ++ show xs
--  putStrLn $ "ys: " ++ show ys
  renderPrimitive LineStrip $ do
    zipWithM_ (\x y -> vertex (Vertex3 x y 0.0 :: Vertex3 GLfloat)) xs ys

-- Draw the OpenGL polygon.
displayGraph :: [VarInfo] -> IO ()
displayGraph infos = do
--  let printLog = mapM_ printVarInfo infos
--  printLog
  loadIdentity
  color (Color3 1 1 1 :: Color3 GLfloat)
  -- Instead of glBegin ... glEnd there is renderPrimitive.
  let drawOne (VarInfo name (PCDouble mv)) = do
        s <- readMVar mv
        putStrLn $ "trying to draw " ++ name
        drawLine s
  mapM_ drawOne infos
--  renderPrimitive Polygon $ do
--    vertex (Vertex3 0.25 0.25 0.0 :: Vertex3 GLfloat)
--    vertex (Vertex3 0.75 0.25 0.0 :: Vertex3 GLfloat)
--    vertex (Vertex3 0.75 0.75 0.0 :: Vertex3 GLfloat)
--    vertex (Vertex3 0.25 0.75 0.0 :: Vertex3 GLfloat)
