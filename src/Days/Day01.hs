module Days.Day01 where
import Data.List
import Data.Maybe

import qualified Program.RunDay as R (runDay)

runDay :: String -> IO (Maybe Double, Maybe Double)
runDay = R.runDay parser part1 part2

type Input = String

type Output1 = Int
type Output2 = Int

parser :: String -> Input
parser = id

part1 :: Input -> Output1
part1 = foldl changeFloor 0

changeFloor :: Int -> Char -> Int
changeFloor n '(' = n + 1
changeFloor n ')' = n - 1

part2 :: Input -> Output2
part2 = fromJust . findIndex (<0) . scanl changeFloor 0
