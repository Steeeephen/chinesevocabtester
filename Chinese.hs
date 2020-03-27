module Chinese where

import Control.Concurrent --delay
import Control.Monad --forever
import Data.Char --toLower
import System.Exit --break main
import System.IO --reading the file
import System.Random --getting a random index of the file

main = do
    --These 2 lines take in the file Dictionary.txt and cast the 
    --entire thing into 'dictionary'
    dictionary <- openFile "ChineseDictionary.txt" ReadMode
    contents <- hGetContents dictionary
    
    putStrLn "Would you like Chinese or English?"
    linput <- getLine
    let lang = changelanguage linput
    
    forever $ do
    
    --This creates a new random seed every time main is called so
    --that a new word is selected each iteration
    gen <- newStdGen

    --Splitting the file
    let splitDictionary = splitUp $ contents

    --Taking a random word from the list, Haskell being lazy means only 
    --one random number is taken, regardless of the fact that I'm 
    --technically making an infinite list
    let randomVar = take 1 $ randomRs (52, length splitDictionary - 1) gen

    --output the word to be tested
    putStrLn . repl 1 $ word splitDictionary randomVar lang

    -- take in the answer
    input <- getLine
    --check if it's right, 'quit' cancels the program
    if input == "quit" 
        then do
            putStrLn "Thanks for Playing!"
            --Added a delay to make it more user-friendly 
            threadDelay 50000
            exitSuccess
        else do
            putStrLn $ check (word splitDictionary randomVar (abs $ lang - 1)) (repl 0 input)
            threadDelay 50000
            putStrLn "---------------------------------------------------------------------"

--Simple string check using guards, typing '?' will provide the answer,
--should you be stuck
check :: String -> String -> String
check x y
        |map toLower y == map toLower x = "Correct!"
        |y == "?"  = repl 1 x
        |otherwise = "False!" ++ "\n" ++ (repl 1 x)

changelanguage :: String -> Int
changelanguage s
    | map toLower s ==  "chinese" = 0
    | map toLower s == "english" = 1

-- Replaces spaces with underscore if n is 0, and vice versa if n is 1, used to allow 
-- for multiple worded entries
repl :: Int -> [Char] -> [Char]
repl _ [] = []
repl n (x:xs)
    | n == 1 && x == '_'  = ' ' : repl 1 xs
    | n == 0 && x == ' '  = '_' : repl 0 xs
    | otherwise = x : repl n xs

-- Splits the contents file into each entry, then splits those into 
-- the chinese and english versions, all in one list
splitUp :: String -> [[String]]
splitUp x = map words $ lines x

-- returns the nth index of the yth entry
word :: [[String]] -> [Int] -> Int -> String
word x y n = (x !! (y !! 0)) !! n