import System.Environment
import Data.List 

rotate [] = []
rotate (x:xs) = xs ++ [x]

allRotations xs = take (length xs) (iterate rotate xs)

main = do
  [file] <- getArgs
  contents <- readFile file
  let l = lines contents
      lw = map words l
      possibleLines = map unwords $ concatMap allRotations lw
  putStrLn $ unlines $ sort possibleLines
