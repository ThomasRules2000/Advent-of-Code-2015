module Days.Day11 where
import           Data.List
import           Data.Maybe
import           Util.NoQuotes

import qualified Program.RunDay as R (runDay)

runDay :: String -> IO (Maybe Double, Maybe Double)
runDay = R.runDay parser part1 part2

type Input = String

type Output1 = NoQuotes
type Output2 = NoQuotes

parser :: String -> Input
parser = id

part1 :: Input -> Output1
part1 = NoQuotes . fromJust . find isValid . iterate incrementPassword
    where
        isValid :: String -> Bool
        isValid s = hasIncreasingSeq s && nPairs s >= 2 && not (any (`elem` s) ['i', 'o', 'l'])

incrementPassword :: String -> String
incrementPassword = reverse . go . reverse
    where
        go :: String -> String
        go [] = []
        go (x:xs)
            | x == 'z' = 'a' : go xs
            | otherwise = succ x : xs

hasIncreasingSeq :: String -> Bool
hasIncreasingSeq (x:y:z:rest) = y == succ x && z == succ y || hasIncreasingSeq (y:z:rest)
hasIncreasingSeq _ = False

nPairs :: String -> Int
nPairs [] = 0
nPairs [x] = 0
nPairs (x:y:rest)
    | x == y = 1 + nPairs rest
    | otherwise = nPairs (y:rest)

part2 :: Input -> Output2
part2 s = part1 $ incrementPassword p1Ans
    where NoQuotes p1Ans = part1 s
