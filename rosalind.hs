module Main where

import Data.List (intercalate)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as TO

emitChar :: Char -> M.Map Char Int
emitChar c = M.singleton c 1

dna_nt_count :: T.Text -> String
dna_nt_count content = intercalate " " $ map (show . snd) $ M.toAscList $ T.foldr (\c acc -> M.unionWith (+) acc (emitChar c)) M.empty content

main :: IO ()
main = do
	let fn = "count-dna-nt.txt"
	let fxn = dna_nt_count

	TO.readFile fn >>= putStrLn . fxn 
	
	