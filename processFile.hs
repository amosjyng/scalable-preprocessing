import System.IO
import System.Environment
import Data.List
import Data.List.Split
import qualified Data.Map as Map

-- Define data structure and accessors
type Document = (Int,Int,Map.Map String Double)

docType :: Document -> Int
docType (x, _, _) = x

docQuery :: Document -> Int
docQuery (_, x, _) = x

docAttr :: Document -> Map.Map String Double
docAttr (_, _, x) = x

-- Read in data
parseLine :: String -> Document
parseLine l =
	let
		ws       = init . init . init $ words l
		strAttrs = map (\s -> splitOn ":" s) $ tail ws
		attrs    = map (\l -> (head l, read $ last l :: Double)) strAttrs
	in (read $ head ws, round . snd . head $ attrs,
		Map.fromList . tail $ attrs)

parse :: String -> [Document]
parse s = map parseLine $ lines s

-- Process data into desired form
processDoc :: Document -> Document
processDoc l = l

doc2Str :: Document -> String
doc2Str d =
	let
		attrList = Map.toList . docAttr $ d
		strList  = map (\t -> fst t ++ ":" ++ (show . snd) t) attrList
	in unwords $ [show . docType $ d] ++ strList

process :: [Document] -> String
process x = unlines $ map (doc2Str . processDoc) x

-- Execute main program
main = do
	args <- getArgs
	file <- readFile $ args !! 0
	putStr . process . parse $ file
