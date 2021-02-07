module Days.Day03 where
import           Data.Set       (Set)
import qualified Data.Set       as Set

import qualified Program.RunDay as R (runDay)

runDay :: String -> IO (Maybe Double, Maybe Double)
runDay = R.runDay parser part1 part2

data Dir = U | D | L | R deriving (Eq, Show)

type Input = [Dir]

type Output1 = Int
type Output2 = Int

parser :: String -> Input
parser = map toDir

toDir :: Char -> Dir
toDir '^' = U
toDir 'v' = D
toDir '<' = L
toDir '>' = R

part1 :: Input -> Output1
part1 = Set.size . snd . foldr go ((0,0), Set.singleton (0,0))
    where
        go :: Dir -> ((Int, Int), Set (Int, Int)) -> ((Int, Int), Set (Int, Int))
        go dir ((x,y), visited) = case dir of
            U -> let coords = (x, y + 1) in (coords, Set.insert coords visited)
            D -> let coords = (x, y - 1) in (coords, Set.insert coords visited)
            L -> let coords = (x - 1, y) in (coords, Set.insert coords visited)
            R -> let coords = (x + 1, y) in (coords, Set.insert coords visited)

part2 :: Input -> Output2
part2 = Set.size . go (0,0) (0,0)
    where
        go :: (Int, Int) -> (Int, Int) -> [Dir] -> Set (Int, Int)
        go s r [] = Set.fromList [s, r]
        go s r [d] = Set.insert s $ go (moveDir d s) r []
        go s r (sd:rd:rest) = Set.insert s $ Set.insert r $ go (moveDir sd s) (moveDir rd r) rest

        moveDir :: Dir -> (Int, Int) -> (Int, Int)
        moveDir U (x, y) = (x, y + 1)
        moveDir D (x, y) = (x, y - 1)
        moveDir L (x, y) = (x - 1, y)
        moveDir R (x, y) = (x + 1, y)

