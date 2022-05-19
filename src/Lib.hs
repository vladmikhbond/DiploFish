module Lib
    (
        mainF
    ) where

import Data.Text ( Text, pack, replace, unpack )
--import Debug.Trace (trace)
import System.Random ( initStdGen, uniformR, StdGen )
import Control.Monad (foldM)
import Loads ( Val, Key, loadData, loadTemplate )

mainF :: [Char] -> IO ()
mainF surname = do
    doc <- pack <$> loadTemplate
    dats <- loadData "data/dataF.txt"
    pers <- loadData ("data/" ++ surname ++ ".txt")
    doc1 <- foldM doOneSubst doc dats    
    doc2 <- foldM doOneSubst doc1 pers
    writeFile ("data/" ++ surname ++ "-F.txt") (unpack doc2)


doOneSubst :: Text -> (Key, [Val]) -> IO Text
doOneSubst doc (_, []) = return doc
doOneSubst  doc (key, vals) = do
    let keyT = pack key
    let len = length vals
    g0 <- initStdGen
    let (n, g1) = uniformR (0, len-1) g0 :: (Int, StdGen)
    let valT = pack $ vals !! n
    let doc' = replace keyT valT doc
    return doc'


    



