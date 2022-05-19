module Loads
    ( loadData, loadTemplate, 
      Key, Val
    ) where


type Key = String
type Val = String

readCleanLines :: FilePath -> IO [String]
readCleanLines path = do
    content <- readFile path
    let ls = lines content
    let ls1 = map (dropWhile (==' ')) ls
    let ls2  = filter (/="") ls1
    return ls2

loadData :: FilePath -> IO [(Key, [Val])]
loadData path = do
    lines <- readCleanLines path
    return $ convert lines []
 where
    convert :: [String] -> [(Key, [Val])] -> [(Key, [Val])]
    convert [] res = res
    convert (line : lines) res
        | head line == '@' = convert lines  ((tail line, []) : res)

        | otherwise        = let (key, vals) = head res
            in reverse $ convert lines ((key, line : vals) : tail res)

loadTemplate = readFile   














