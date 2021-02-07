module Days.Day10 where
import Data.List

import qualified Program.RunDay as R (runDay)

runDay :: String -> IO (Maybe Double, Maybe Double)
runDay = R.runDay parser part1 part2

type Input = String

type Output1 = Int
type Output2 = Int

parser :: String -> Input
parser = id

part1 :: Input -> Output1
part1 = length . (!! 40) . iterate lookAndSay

lookAndSay :: String -> String
lookAndSay s = concatMap (\x -> show (length x) ++ [head x]) $ group s

part2 :: Input -> Output2
part2 = length . (!! 50) . iterate lookAndSay
