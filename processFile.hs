import System.IO
import System.Environment
import Data.List
import Data.List.Split
import Data.Maybe

-- Define data structure and accessors
type Document = (Int,Int,[Double])

docType :: Document -> Int
docType (x, _, _) = x

docQuery :: Document -> Int
docQuery (_, x, _) = x

docAttr :: Document -> [Double]
docAttr (_, _, x) = x

-- Read in data
parseLine :: String -> Document
parseLine l =
	let
		ws    = init . init . init $ words l
		attrs = map (\s -> read . last . splitOn ":" $ s) $ tail ws
	in (read $ head ws, round . head $ attrs, tail $ attrs)

parse :: String -> [Document]
parse s = map parseLine $ lines s

-- Process data into desired form
processDoc :: Document -> Document
processDoc l = l

doc2Str :: Document -> String
doc2Str d =
	let strList = zipWith (\a b -> (show a) ++ ":" ++ (show b))
						  [1..] $ docAttr d
	in unwords $ [show . docType $ d] ++ strList

normalize :: Document -> Document
normalize d =
	(docType d, docQuery d, map (/ l2) $ docAttr d)
	where l2 = sqrt . sum . map (^2) $ docAttr d

subtractDoc :: Document -> Document -> Maybe Document
subtractDoc relevant irrelevant
	| docQuery relevant == docQuery irrelevant =
		Just (1, -1, zipWith (-) (docAttr relevant) (docAttr irrelevant))
	| otherwise = Nothing

subtractDocs :: [Document] -> Document -> [Document]
subtractDocs relevants irrelevant =
	[fromJust doc | doc <- subtractedDocs, isJust doc]
	where subtractedDocs = map (`subtractDoc` irrelevant) relevants

subtractAll :: [Document] -> [Document] -> [Document]
subtractAll relevants irrelevants =
	concat . map (subtractDocs relevants) $ irrelevants

process :: [Document] -> String
process x =
	let
		relevantDocs   = [doc | doc <- x, docType doc == 1]
		irrelevantDocs = [doc | doc <- x, docType doc == 0]
	in unlines . map (doc2Str . normalize) $
		subtractAll relevantDocs irrelevantDocs

-- Execute main program
main = do
	args <- getArgs
	file <- readFile $ args !! 0
	putStr . process . parse $ file
