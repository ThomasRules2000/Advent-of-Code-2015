module Days.Day07 where
import           Data.List.Split
import           Data.Map.Strict  (Map)
import qualified Data.Map.Strict  as Map
import Data.Word
import Data.Bits
import           Util.Util
import Data.Tuple.Extra
import Text.Read

import Debug.Trace

import qualified Program.RunDay   as R (runDay)

runDay :: String -> IO (Maybe Double, Maybe Double)
runDay = R.runDay parser part1 part2

data Op = Val Word16
        | And String String
        | Or String String
        | Shift String Int
        | Not String
        | Copy String
        deriving (Eq, Show)

type Input = Map String Op

type Output1 = Word16
type Output2 = Word16

parser :: String -> Input
parser = Map.fromList . map (fmap (getOp . words) . swap . listToTuple . splitOn " -> ") . lines

getOp :: [String] -> Op
getOp [n] = maybe (Copy n) Val $ readMaybe n
getOp ["NOT", var] = Not var
getOp [a, op, b] = case op of
    "AND" -> And a b
    "OR" -> Or a b
    "LSHIFT" -> Shift a $ read b
    "RSHIFT" -> Shift a $ -read b

part1 :: Input -> Output1
part1 = fst . calcVal "a"

calcVal :: String -> Map String Op -> (Word16, Map String Op)
calcVal s m = case Map.findWithDefault (Val $ read s) s m of
    Val n -> (n, m)
    And l r -> (res, Map.insert s (Val res) m'')
        where (lVal, m') = calcVal l m 
              (rVal, m'') = calcVal r m'
              res = lVal .&. rVal
    Or l r -> (res, Map.insert s (Val res) m'')
        where (lVal, m') = calcVal l m 
              (rVal, m'') = calcVal r m'
              res = lVal .|. rVal
    Shift l r -> (res, Map.insert s (Val res) m')
        where (lVal, m') = calcVal l m 
              res = shift lVal r
    Not e -> (res, Map.insert s (Val res) m')
        where (val, m') = calcVal e m 
              res = complement val
    Copy e -> (val, Map.insert s (Val val) m')
        where (val, m') = calcVal e m 
    

part2 :: Input -> Output2
part2 m = fst $ calcVal "a" $ Map.insert "b" (Val $ part1 m) m
