module Lib
    ( 
    ) where

import System.IO
import Load
import Data.Text   

m surname = do
    doc <- pack <$> loadTemplate
    dat <- loadData
    let n = fromEnum (Prelude.head surname)  
    let doc' = substOneTo (dat!!3) doc n  
    --
    let s = unpack doc'
    writeFile "000.txt" s
    --return ()

substTo :: [(Key, [Val])] -> Text -> Text
substTo = undefined

substOneTo :: (Key, [String]) -> Text -> Int -> Text
substOneTo (_, []) doc _ = doc
substOneTo (key, vals) doc n =  let
    keyT = pack key
    len = Prelude.length vals
    valT = pack $ vals !! (n `mod` len)
 in
    replace keyT valT doc




