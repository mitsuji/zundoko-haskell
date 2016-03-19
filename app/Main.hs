module Main where

import qualified System.Random as R
import Control.Concurrent(threadDelay)

data Call = Zun | Doko deriving(Show,Eq,Bounded)

instance R.Random Call where
  randomR (a,b) g = 
    case (R.randomR (call2Int a, call2Int b) g) of
      (x, g') -> (int2Call x, g')
    where
      call2Int :: Call -> Int
      call2Int Zun  = 0
      call2Int Doko = 1
      
      int2Call :: Int -> Call
      int2Call 0 = Zun
      int2Call _ = Doko
  
  random g = R.randomR (minBound,maxBound) g


push :: [Call] -> Call -> [Call]
push xs x = take 5 $ x:xs


check :: [Call] -> Bool
check xs = xs == [Doko, Zun, Zun, Zun, Zun]


main :: IO()
main = do
  g <- R.newStdGen
  loop [] g
  where
    loop :: R.RandomGen g => [Call] -> g -> IO ()
    loop xs g = do
      let (x,g') = R.random g 
      putStrLn $ show x
      threadDelay (500 * 1000)
      let xs' = push xs x
      if check xs'
        then putStrLn "Kiyoshi!!"
        else loop xs' g'
             
