module Lib
    (
        run
    ) where

import Data.Text ( Text, pack, replace, unpack, length )
import System.Random ( initStdGen, uniformR, StdGen )
import Control.Monad (foldM)
import Loads ( Val, Key, loadData, loadTemplate )


templateName x = "data/template" ++ x : ".txt"
dataName x = "data/data" ++ x : ".txt"
persName surname x  = "data/" ++ surname ++ ".txt"
resultName surname 'F' =  surname ++ "-feedback.txt"
resultName surname 'R' =  surname ++ "-review.txt"
resultName surname  _  =  error surname


run  :: String -> Char -> IO ()
run surname x = do
    doc <- pack <$> loadTemplate (templateName x)
    dats <- loadData (dataName x)
    pers <- loadData (persName surname x)
    doc1 <- foldM doOneSubst doc dats  
    doc2 <- foldM doOneSubst doc1 pers
    writeFile (resultName surname x) (unpack doc2)


doOneSubst :: Text -> (Key, [Val]) -> IO Text
doOneSubst doc (_, []) = return doc
doOneSubst  doc (key, vals) = do
    let keyT = pack key
    let len = Prelude.length vals
    g0 <- initStdGen
    let (n, g1) = uniformR (0, len-1) g0 :: (Int, StdGen)
    let valT = pack $ vals !! n
    let doc' = replace keyT valT doc
    return doc'


    



