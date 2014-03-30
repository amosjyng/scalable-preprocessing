import System.IO
import System.Environment
import Data.List.Split
import Data.List

process :: [String] -> [String]
process x = [head x] ++ (init . init . init . tail . tail) x

main = do
	args <- getArgs
	file <- readFile $ args !! 0
	let x = map (splitOn " ") (lines file)
		in mapM (putStrLn . concat . intersperse " ") $ map process x
