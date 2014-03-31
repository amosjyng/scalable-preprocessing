import System.IO
import System.Environment
import Data.List
import Data.List.Split
import Data.Maybe
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

subtractDoc :: Document -> Document -> Maybe Document
subtractDoc relevant irrelevant
	| docQuery relevant == docQuery irrelevant =
		Just (1, -1, Map.unionWith (-) (docAttr relevant) (docAttr irrelevant))
	| otherwise = Nothing

subtractDocs :: [Document] -> Document -> [Document]
subtractDocs relevants irrelevant =
	let subtractionResults = map (\d -> subtractDoc d irrelevant) relevants
	in [fromJust r | r <- subtractionResults, isJust r]

subtractAll :: [Document] -> [Document] -> [Document]
subtractAll relevant irrelevant =
	concat $ map (subtractDocs relevant) irrelevant

process :: [Document] -> String
process x =
	let
		relevantDocs   = [doc | doc <- x, docType doc == 1]
		irrelevantDocs = [doc | doc <- x, docType doc == 0]
	in unlines . map doc2Str $ subtractAll relevantDocs irrelevantDocs

-- Execute main program
main = do
	args <- getArgs
	file <- readFile $ args !! 0
	putStr . process . parse $ file
