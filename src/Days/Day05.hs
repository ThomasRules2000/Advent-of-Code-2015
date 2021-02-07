module Days.Day05 where
import Data.List

import qualified Program.RunDay as R (runDay)

runDay :: String -> IO (Maybe Double, Maybe Double)
runDay = R.runDay parser part1 part2

type Input = [String]

type Output1 = Int
type Output2 = Int

parser :: String -> Input
parser = lines

part1 :: Input -> Output1
part1 = length . filter isNice
    where
        isNice :: String -> Bool
        isNice word = length (filter (`elem` "aeiou") word) >=3
                    && containsDuplicate word
                    && not (any (`isInfixOf` word) ["ab", "cd", "pq", "xy"])

containsDuplicate :: (Eq a) => [a] -> Bool
containsDuplicate [] = False
containsDuplicate [x] = False
containsDuplicate (x:y:xs) = x == y || containsDuplicate (y:xs)

part2 :: Input -> Output2
part2 = length . filter (\word -> repeatedPair word && repeatInbetween word)

repeatedPair :: String -> Bool
repeatedPair [] = False
repeatedPair [x] = False
repeatedPair (x:y:xs) = ([x,y] `isInfixOf` xs) || repeatedPair (y:xs)

repeatInbetween :: String -> Bool
repeatInbetween [] = False
repeatInbetween [x] = False
repeatInbetween [x,y] = False 
repeatInbetween (x:y:z:xs) = x == z || repeatInbetween (y:z:xs)
