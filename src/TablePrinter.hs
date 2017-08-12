module TablePrinter where

import qualified Data.Map.Strict as Map
import Control.Arrow (second)

import Data.List (sortBy)
import Data.Ord (Down(..), comparing)


type TableBorder = String
type TableTitle  = String

data SortOrder = Asc | Desc


traverseTransform :: Ord c => Map.Map a b -> (b -> c) -> SortOrder -> [(a, c)]
traverseTransform m f order = case order of
        Asc  -> sortBy (comparing snd) mlist
        Desc -> sortBy (comparing (Down . snd)) mlist
    where mlist = map (second f) $ Map.assocs m


printTable :: TableBorder -> TableTitle -> IO () -> IO ()
printTable border title tableContent = header >> tableContent >> putStrLn border
    where header = putStrLn border >> putStrLn title >> putStrLn border
