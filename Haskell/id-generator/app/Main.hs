import Data.Char (digitToInt, isAlpha, isDigit)
import System.IO (hFlush, stdout)
import System.Process (readProcess)

computeSHA512 :: String -> IO String
computeSHA512 input = do
  hashResult <- readProcess "sha512sum" [] input
  return $ take 128 hashResult

main :: IO ()
main = do
  putStrLn "id-generator: Generating hashed id\n"
  putStr "Typing original id: "
  hFlush stdout
  input <- getLine
  putStrLn $ "The original id is \"" ++ input ++ "\" ✔\n"
  putStrLn "Caculating hashed id...\n"
  hashedInput <- computeSHA512 input
  putStrLn $ "SHA512: " ++ hashedInput ++ "\n↓"
  let
    alphaInHashedInput :: String
    alphaInHashedInput = filter isAlpha hashedInput
    digitInHashedInput :: [Int]
    digitInHashedInput = map digitToInt (filter isDigit hashedInput)
    hashedInputFactor :: Double
    hashedInputFactor = fromIntegral (sum digitInHashedInput) / fromIntegral (length digitInHashedInput)
    outputPrefix = take 2 (drop (round hashedInputFactor - 1) alphaInHashedInput)
    output = take 6 (drop (round $ hashedInputFactor * 128 / 10 - 1) hashedInput)
  putStrLn $ "Alpha in SHA512 is: " ++ alphaInHashedInput ++ "\n↓"
  putStrLn $ "Digit in SHA512 is: " ++ filter isDigit hashedInput ++ "\n↓"
  putStrLn $ "Factor is: " ++ show hashedInputFactor ++ "\n↓"
  putStrLn $ "Hashed id is: " ++ outputPrefix ++ output