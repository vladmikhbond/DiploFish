module Loads
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
        | head line == '@' = convert lines  ((tail line, []) : res)

        | otherwise        = let (key, vals) = head res
            in reverse $ convert lines ((key, line : vals) : tail res)


loadPerson :: FilePath -> IO [(Key, Val)]
loadPerson path = do
    lines <- readCleanLines path
    return $ convert lines []
 where
    convert :: [String] -> [(Key, Val)] -> [(Key, Val)]
    convert [] res = res
    convert (line : lines) res
        | head line == '[' = convert lines  ((line, []) : res)
        | otherwise        = let (key, val) = head res
            in reverse $ convert lines ((key, val ++ line) : tail res)


loadTemplate :: IO String
loadTemplate = readFile "data/templateF.txt"



------------ utils ----------------
readCleanLines path = do
    content <- readFile path
    let ls = lines content
    let ls' = map (dropWhile (==' ')) ls
    let res  = filter (/="") ls'
    return res














