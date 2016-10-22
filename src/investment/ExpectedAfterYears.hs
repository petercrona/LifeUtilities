import System.Environment (getArgs)
import Text.Read (readMaybe)

data Config = Config {
  currentSavings :: Double,
  monthlySaving :: Double,
  yearlyIncrease :: Double,
  numberOfYears :: Int
  } deriving (Show)

readConfig :: [String] -> Maybe Config
readConfig args
  | length args >= 4 = do
      currentSavings' <- readMaybe (head args)
      monthlySaving'  <- readMaybe (args !! 1)
      yearlyDiff'     <- readMaybe (args !! 2)
      numberOfYears'  <- readMaybe (args !! 3)
      Just Config { currentSavings = currentSavings'
                  , monthlySaving  = monthlySaving'
                  , yearlyIncrease     = yearlyDiff'
                  , numberOfYears  = numberOfYears' }
  | otherwise = Nothing

calculateExpectedBalance :: Config -> Double
calculateExpectedBalance c = foldr (addMonthlyIncrease monthlySaving' yearlyDiff')
                                   currentSavings'
                                   [1..numberOfYears'*12]
  where currentSavings' = currentSavings c
        monthlySaving' = monthlySaving c
        yearlyDiff' = yearlyIncrease c
        numberOfYears' = numberOfYears c

addMonthlyIncrease :: Double -> Double -> Int -> Double -> Double
addMonthlyIncrease monthlySaving' yearlyDiff' _ acc = acc
                                                    * (1 + yearlyDiff' / 12)
                                                    + monthlySaving'

printHelp :: IO ()
printHelp = putStrLn $ "== Please provide arguments:\n"
                    ++ "<currentSavings> <monthlySaving> <yearlyIncrease> <numberOfYears>\n"
                    ++ "eg. 10000 300 0.1 5"

main :: IO ()
main = do
  args <- getArgs
  maybe printHelp
        (print . round . calculateExpectedBalance)
        (readConfig args)
