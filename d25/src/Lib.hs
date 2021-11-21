module Lib
    ( someFunc
    , part1
    ) where
import Data.List (find)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

newtype SubjectNr = SubjectNr Int
newtype PubKey = PubKey { rawKey :: Int }

-- takes the _subject number_ and computes a single step of the encryption loop
loopStep :: SubjectNr -> Int -> Int
loopStep (SubjectNr s) value = (value * s) `rem` 20201227

-- test
-- cardPubKey = PubKey { rawKey = 5764801 } -- 5290733
-- doorPubKey = PubKey { rawKey = 17807724 } -- 15231938

-- input
cardPubKey = PubKey { rawKey = 5290733 }
doorPubKey = PubKey { rawKey = 15231938 }

-- Finds the loop size from a public key
findLoopSize :: PubKey -> SubjectNr -> Int
findLoopSize (PubKey pub) s = go 0 1
  where
    go :: Int -> Int -> Int
    go n prev
      | prev == pub = n
      | otherwise = go (n + 1) (loopStep s prev)

cardLoopSize = findLoopSize cardPubKey (SubjectNr 7)
doorLoopSize = findLoopSize doorPubKey (SubjectNr 7)

encrypt :: Int -> SubjectNr -> Int
encrypt ls value = foldl (\x f -> f x) 1 (replicate ls (loopStep value))

part1 = do
    print $ encrypt cardLoopSize (SubjectNr (rawKey doorPubKey))
