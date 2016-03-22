module Main where

import qualified System.Random as R
import Control.Concurrent(threadDelay)


data Call = Zun | Doko
          deriving(Eq,Bounded,Enum)

instance Show Call where
  show Zun = "Zun!" 
  show Doko = "Doko!" 

instance R.Random Call where
  randomR (a,b) g = 
    case (R.randomR (fromEnum a, fromEnum b) g) of
      (x, g') -> (toEnum x, g')
  
  random = R.randomR (minBound,maxBound)


push :: [Call] -> Call -> [Call]
push xs x = x : take 4 xs

check :: [Call] -> Bool
check xs = xs == [Doko, Zun, Zun, Zun, Zun]


main :: IO()
main = do
  loop []
  where
    loop :: [Call] -> IO ()
    loop xs = do
      x <- R.randomIO
      putStrLn (show x) >> threadDelay (500 * 1000)
      let xs' = push xs x
      if check xs'
        then putStrLn "Kiyoshi!!"
        else loop xs'
             
