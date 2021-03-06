{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as TO

emitChar :: Char -> M.Map Char Int
emitChar c = M.singleton c 1

dna_nt_count :: T.Text -> T.Text
dna_nt_count content = T.intercalate " " $ map (T.pack . show . snd) $ M.toAscList reducedValue
	where reducedValue = T.foldr (\c acc -> M.unionWith (+) acc (emitChar c)) M.empty content

dna2rna :: T.Text -> T.Text
dna2rna = T.replace "T" "U"
	
compliment :: T.Text -> T.Text
compliment c 
	| c == "A" = "T"
	| c == "T" = "A"
	| c == "C" = "G"
	| c == "G" = "C"
	
dna_compliment :: T.Text -> T.Text
dna_compliment c = [compliment x | x <- c]
	
main :: IO ()
main = do
	let fn = "data.txt"
	let fxn = dna_compliment

	TO.readFile fn >>= TO.putStrLn . fxn 
	
	