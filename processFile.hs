import System.IO
import System.Environment
import Data.List.Split
import Data.List

processLine :: [String] -> [String]
processLine l = [head l] ++ (init . init . init . tail . tail) l

process :: [[String]] -> [[String]]
process x = map processLine x

main = do
	args <- getArgs
	file <- readFile $ args !! 0
	let x = map (splitOn " ") (lines file)
		in mapM (putStrLn . concat . intersperse " ") $ process x
