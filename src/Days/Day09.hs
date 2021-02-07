module Days.Day09 where
import           Control.Monad
import           Data.Bifunctor
import           Data.List.Split
import           Data.Map.Strict  (Map)
import qualified Data.Map.Strict  as Map
import           Data.Maybe
import           Data.Set         (Set)
import qualified Data.Set         as Set
import           Data.Tuple.Extra (swap)
import           Util.Util

import qualified Program.RunDay   as R (runDay)

import           Debug.Trace

runDay :: String -> IO (Maybe Double, Maybe Double)
runDay = R.runDay parser part1 part2

type Input = Map (String, String) Int

type Output1 = Int
type Output2 = Int

parser :: String -> Input
parser = Map.fromList . concatMap ((\(x, n) -> [(x, n), (swap x, n)]) . bimap (listToTuple . splitOn " to ") read . listToTuple . splitOn " = ") . lines

part1 :: Input -> Output1
part1 distances = minimum
                $ (!!(length locations - 1))
                $ iterate dynamicProgramming
                $ Map.fromList (zip (map (\x -> (x, Set.singleton x)) locations) $ repeat 0)
    where
        locations = Set.toList $ uncurry Set.union $ join bimap Set.fromList $ unzip $ Map.keys distances

        dynamicProgramming :: Map (String, Set String) Int -> Map (String, Set String) Int
        dynamicProgramming = Map.foldrWithKey (\k n m -> Map.unionWith min m $ addNodes distances k n locations) Map.empty

addNodes :: Map (String, String) Int -> (String, Set String) -> Int -> [String] -> Map (String, Set String) Int
addNodes distances key cost = Map.fromList . mapMaybe (addNode distances key cost)

addNode :: Map (String, String) Int -> (String, Set String) -> Int -> String -> Maybe ((String, Set String), Int)
addNode distances (curr, prev) cost n
    | n `Set.notMember` prev = (\d -> ((n, Set.insert n prev), cost + d)) <$> Map.lookup (n, curr) distances
    | otherwise = Nothing

part2 :: Input -> Output2
part2 distances = maximum
                $ (!!(length locations - 1))
                $ iterate dynamicProgramming
                $ Map.fromList (zip (map (\x -> (x, Set.singleton x)) locations) $ repeat 0)
    where
        locations = Set.toList $ uncurry Set.union $ join bimap Set.fromList $ unzip $ Map.keys distances

        dynamicProgramming :: Map (String, Set String) Int -> Map (String, Set String) Int
        dynamicProgramming = Map.foldrWithKey (\k n m -> Map.unionWith max m $ addNodes distances k n locations) Map.empty
