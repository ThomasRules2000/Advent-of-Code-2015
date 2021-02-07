module Days.Day06 where
import           Control.Monad
import           Data.Bifunctor
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set        (Set)
import qualified Data.Set        as Set

import qualified Program.RunDay  as R (runDay)

runDay :: String -> IO (Maybe Double, Maybe Double)
runDay = R.runDay parser part1 part2

data Instruction = TurnOn (Int, Int) (Int, Int)
                 | TurnOff (Int, Int) (Int, Int)
                 | Toggle (Int, Int) (Int, Int)

type Input = [Instruction]

type Output1 = Int
type Output2 = Int

parser :: String -> Input
parser = map (getInstruction . words) . lines

getInstruction :: [String] -> Instruction
getInstruction ("toggle":coords)     = uncurry Toggle $ getCoords coords
getInstruction ("turn":"on":coords)  = uncurry TurnOn $ getCoords coords
getInstruction ("turn":"off":coords) = uncurry TurnOff $ getCoords coords

getCoords :: [String] -> ((Int, Int), (Int, Int))
getCoords [c1, _, c2] = join bimap (join bimap ((+1) . read) . fmap tail . break (==',')) (c1, c2)

part1 :: Input -> Output1
part1 = Set.size . foldl (flip performInstruction) Set.empty

performInstruction :: Instruction -> Set (Int, Int) -> Set (Int, Int)
performInstruction (TurnOn (x1, y1) (x2, y2)) = flip (foldr Set.insert) [(x,y) | x <- [x1..x2], y <- [y1..y2]]
performInstruction (TurnOff (x1, y1) (x2, y2)) = flip (foldr Set.delete) [(x,y) | x <- [x1..x2], y <- [y1..y2]]
performInstruction (Toggle (x1, y1) (x2, y2)) = flip (foldr toggle) [(x,y) | x <- [x1..x2], y <- [y1..y2]]

toggle :: (Ord a) => a -> Set a -> Set a
toggle x s
    | Set.member x s = Set.delete x s
    | otherwise = Set.insert x s

part2 :: Input -> Output2
part2 = sum . Map.elems . foldl (flip performInstruction2) Map.empty 

performInstruction2 :: Instruction -> Map (Int, Int) Int -> Map (Int, Int) Int
performInstruction2 (TurnOn (x1, y1) (x2, y2)) = flip (foldr (flip (Map.insertWith (+)) 1)) [(x,y) | x <- [x1..x2], y <- [y1..y2]]
performInstruction2 (TurnOff (x1, y1) (x2, y2)) = flip (foldr (flip (Map.insertWith (\x y -> max 0 $ y - 1)) 0)) [(x,y) | x <- [x1..x2], y <- [y1..y2]]
performInstruction2 (Toggle (x1, y1) (x2, y2)) = flip (foldr (flip (Map.insertWith (+)) 2)) [(x,y) | x <- [x1..x2], y <- [y1..y2]]
