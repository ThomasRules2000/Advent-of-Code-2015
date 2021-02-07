module Days.Day04 where
import Data.Hash.MD5
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
part1 secret = (+1) $ fromJust $ findIndex (isPrefixOf "00000") $ map (md5s . Str . (secret++) . show) [1..]

part2 :: Input -> Output2
part2 secret = (+1) $ fromJust $ findIndex (isPrefixOf "000000") $ map (md5s . Str . (secret++) . show) [1..]
