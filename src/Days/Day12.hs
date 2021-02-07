module Days.Day12 where
import           Data.Char
import           Data.List.Split

import Util.Util

import qualified Program.RunDay  as R (runDay)

runDay :: String -> IO (Maybe Double, Maybe Double)
runDay = R.runDay parser part1 part2

type Input = String

type Output1 = Int
type Output2 = Int

parser :: String -> Input
parser = id

part1 :: Input -> Output1
part1 = sum . map read . filter (/= "") . splitWhen (\c -> not (isDigit c || c == '-'))

part2 :: Input -> Output2
part2 = undefined
