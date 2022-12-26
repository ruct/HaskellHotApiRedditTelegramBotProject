module Common.FlipCoin (flipCoin) where

import System.Random

flipCoin :: IO Int
flipCoin = do fst . (randomR (0, 1) :: StdGen -> (Int, StdGen)) <$> newStdGen