module Main where

import qualified System.Random as R
import Control.Concurrent(threadDelay)

data Call = Zun | Doko
          deriving(Show,Eq,Bounded,Enum)

instance R.Random Call where
  randomR (a,b) g = 
    case (R.randomR (fromEnum a, fromEnum b) g) of
      (x, g') -> (toEnum x, g')
  
  random = R.randomR (minBound,maxBound)


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
             
