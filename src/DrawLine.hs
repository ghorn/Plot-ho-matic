{-# OPTIONS_GHC -Wall #-}

module DrawLine ( drawLine ) where

import Control.Monad ( zipWithM_ )
import qualified Data.Sequence as Seq
import qualified Data.Foldable as F
import Graphics.Rendering.OpenGL as GL

linspace :: Fractional a => a -> a -> Int -> [a]
linspace x0 x1 n =
  map ((+ x0) . (((x1-x0)/((realToFrac n)-1)) *) . realToFrac) [0..(n-1)]

drawLine :: (Fractional a, Real a) => Seq.Seq a -> IO ()
drawLine seq' = do
  let maxval = F.foldl' (\acc x -> max acc (abs x)) (1e-12) seq'
      num = Seq.length seq'
      xs = linspace (-1) 1 num
      ys = map ((/ (realToFrac maxval)) . realToFrac) $ F.toList seq'
--      ys = map realToFrac $ F.toList seq'

  renderPrimitive LineStrip $ do
    zipWithM_ (\x y -> vertex (Vertex3 x y 0.0 :: Vertex3 GLfloat)) xs ys
