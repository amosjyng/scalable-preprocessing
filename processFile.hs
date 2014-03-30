import System.IO
import System.Environment
import Data.List.Split
import Data.List

parse :: String -> [[String]]
parse s = map (splitOn " ") $ lines s

processLine :: [String] -> [String]
processLine l = [head l] ++ (init . init . init . tail . tail) l

process :: [[String]] -> String
process x = unlines $ map (concat . intersperse " ") $ map processLine x

main = do
	args <- getArgs
	file <- readFile $ args !! 0
	putStr . process . parse $ file
