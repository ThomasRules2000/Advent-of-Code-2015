module Days.Day02 where
import Data.List
import Data.List.Split

import qualified Program.RunDay as R (runDay)

runDay :: String -> IO (Maybe Double, Maybe Double)
runDay = R.runDay parser part1 part2

type Input = [[Int]]

type Output1 = Int
type Output2 = Int

parser :: String -> Input
parser = map (map read . splitOn "x") . lines

part1 :: Input -> Output1
part1 = sum . map wrappingPaper

wrappingPaper :: [Int] -> Int
wrappingPaper [l, w, h] = minimum sides + 2 * sum sides
    where sides = [l*w, w*h, h*l]

part2 :: Input -> Output2
part2 = sum . map ribbon

ribbon :: [Int] -> Int
ribbon xs = product xs + 2 * (sum . take 2 . sort) xs
