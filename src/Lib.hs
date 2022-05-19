module Lib
    (
    ) where


import Loads ( Val, Key, loadData, loadPerson, loadTemplate )
import Data.Text ( Text, pack, replace, unpack )
import Debug.Trace (trace)

mainF surname = do
    doc <- pack <$> loadTemplate
    dats <- loadData
    pers <- loadPerson ("data/" ++ surname ++ ".txt")
    let doc' = doc `subst` dats

    writeFile ("data/" ++ surname ++ "-F.txt") (unpack doc')


subst :: Text -> [(Key, [Val])] -> Text
subst = foldl substOne

substOne :: Text -> (Key, [String]) -> Text
substOne doc (_, []) = doc
substOne  doc (key, vals)=  let
    keyT = pack key
    len = length vals
    valT = pack $ head vals

 in replace keyT valT doc




