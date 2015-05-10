-- Konrad Lisiecki 291649

module WordFreqAscii where

import Prelude hiding(lookup)
import TreeDict (Dict, empty, insert, lookup, fromList, toList, fastInsert)

import Data.Char
import System.IO  
import System.Environment

main = do
        filename <- getArgs
        if (null filename) || (length filename == 0)
            then putStrLn " Use: runhaskell program.hs nazwapliku"
            else do
                str <- readFile $ head filename
                putStrLn . unlines . map showDetails $ take 10 $ quicksort $ alpha $ toList $ tree (words str) empty

showDetails (word, count) = word ++ (spaces  (30 - length word)  []) ++ show count 


spaces 0 acc = acc
spaces n acc = spaces (n-1) acc++" "

tree [] d = d
tree (x:xs) d 
        | length x < 3 = tree xs d
        | otherwise = tree xs $ fastInsert x (\v -> case v of { Nothing -> 1; (Just v) -> v + 1 }) d

alpha [] = []
alpha x = alpha1 x [] 

alpha1 [] acc = acc
alpha1 ((x, y):xs) acc
    | onlyletters x = alpha1 xs acc++[(x,y)]
    | otherwise = alpha1 xs acc

onlyletters [] = True
onlyletters (x:xs)
    | isAlpha x = onlyletters xs
    | otherwise = False

quicksort [] = [] 
quicksort ((x, y):xs) = 
    quicksort [(e,f) | (e,f) <- xs, f >= y] ++ [(x,y)] ++ quicksort [(g,h)| (g,h) <- xs, h < y]