{-# LANGUAGE OverloadedStrings #-}
	
import Data.Monoid

data Hangman = More (Char -> Hangman) String | Won Int String | Lost String

maxFail = 5

instance Monoid Hangman where
  mempty = Won 0 []
  (More h1 w) `mappend` (More h2 w2) = More (\g -> h1 g <> h2 g) (w <> w2)
  (Won x1 w1) `mappend` (Won x2 w2) = Won (min x1 x2) (w1 <> w2)
  (Lost w1) `mappend` (Lost w2) = Lost (w1 <> w2)
  (More h1 w1) `mappend` w@(Won _ w2) = More (\g -> h1 g <> w) (w1 <> w2)
  w@(Won _ w1) `mappend` m@(More h1 w2) = More (\g -> w <> h1 g) (w1 <> w2)
  l@(Lost {}) `mappend` _ = l
  _ `mappend` l@(Lost {}) = l


move :: Char -> Hangman -> Hangman
move _ w@(Won {}) = w
move _ l@(Lost {}) = l
move c (More nxt word) = nxt c

singleton c = More guess word
  where
    guess c2 = if c == c2 then Won 0 word else nextGuess 1 c word
    word = [c]

nextGuess n c w
          | n < maxFail = More (\g -> if g == c
                                         then Won n w
                                         else nextGuess (1 + n) c w) w
          | otherwise = Lost w

instance Show Hangman where
    show (More h w) = "More <fn> " ++ w
    show (Won n w) = "Won " ++ show n ++ " " ++ w
    show (Lost w) = "Lost " ++ w	