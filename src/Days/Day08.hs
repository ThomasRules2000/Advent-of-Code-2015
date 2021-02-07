module Days.Day08 where
import qualified Program.RunDay as R (runDay)

runDay :: String -> IO (Maybe Double, Maybe Double)
runDay = R.runDay parser part1 part2

type Input = [String]

type Output1 = Int
type Output2 = Int

parser :: String -> Input
parser = lines

part1 :: Input -> Output1
part1 = sum . map (\x -> length x - getMemStringLength x + 2)

getMemStringLength :: String -> Int
getMemStringLength ('\\':'\\':rest) = 1 + getMemStringLength rest
getMemStringLength ('\\':'\"':rest) = 1 + getMemStringLength rest
getMemStringLength ('\\':'x':rest)  = 1 + getMemStringLength (drop 2 rest)
getMemStringLength []               = 0
getMemStringLength (x:xs)           = 1 + getMemStringLength xs

part2 :: Input -> Output2
part2 = sum . map (\x -> length (show x) - length x)
