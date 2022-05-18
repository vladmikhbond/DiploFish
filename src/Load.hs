module Load
    ( loadData, loadPerson, loadTemplate, 
      Key, Val
    ) where

import System.IO

type Key = String
type Val = String


loadData :: IO [(Key, [Val])]
loadData = do
    lines <- readCleanLines "data/dataF.txt"
    return $ convert lines []
 where
    convert :: [String] -> [(Key, [Val])] -> [(Key, [Val])]
    convert [] res = res
    convert (line : lines) res
        | take 2 line == "[[" = convert lines  ((line, []) : res)
        | otherwise = let (k, vals) = head res
            in convert lines ((k, line : vals) : tail res)


loadPerson :: FilePath -> IO [(Key, Val)]
loadPerson path = do
    lines <- readCleanLines path
    return $ convert lines []
 where
    convert :: [String] -> [(Key, Val)] -> [(Key, Val)]
    convert [] res = res
    convert (line : lines) res
        | head line == '[' = convert lines  ((line, []) : res)
        | otherwise = let (k, val) = head res
            in convert lines ((k, val ++ line) : tail res)


loadTemplate :: IO String
loadTemplate = readFile "data/templateF.txt"



------------ utils ----------------
readCleanLines path = do
    content <- readFile path
    let ls = lines content
    let ls' = map (dropWhile (==' ')) ls
    return $ filter (/="") ls'














