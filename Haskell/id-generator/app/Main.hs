import Control.Monad (when)
import Crypto.Hash (SHA512 (SHA512), hashWith)
import Data.ByteString.Char8 (pack)
import Data.Char (digitToInt, isAlpha)
import System.Directory (createDirectoryIfMissing, getHomeDirectory)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hFlush, stdout)
import System.IO.Error (tryIOError)
import System.Info (arch)

computeSHA512 :: String -> String
computeSHA512 input = show $ hashWith SHA512 (pack input)

getSSHPrivateKey :: IO String
getSSHPrivateKey = do
  homeDirectory <- getHomeDirectory
  sshPrivateKey <- tryIOError (readFile (homeDirectory ++ "/.ssh/id_rsa"))
  case sshPrivateKey of
    Left _ -> do
      putStrLn $ "\n\n" ++ homeDirectory ++ "/.ssh/id_rsa cannot be found ❌"
      exitFailure
    Right [] -> do
      putStrLn $ "\n\n" ++ homeDirectory ++ "/.ssh/id_rsa is empty ❌"
      exitFailure
    Right x -> do
      putStrLn " ✔\n"
      return $ concat (drop 1 (init (lines x)))

main :: IO ()
main = do
  putStrLn "id-generator: Hashing your id"
  args <- getArgs
  input <- case args of
    [] -> do
      putStr "\nTyping original id: "
      hFlush stdout
      getLine
    (x : _) -> return x
  putStrLn $ "\nThe original id is \"" ++ input ++ "\" ✔\n"
  putStr "Reading SSH private key..."
  homeDirectory <- getHomeDirectory
  sshPrivateKey <- getSSHPrivateKey
  putStrLn "Caculating hashed id...\n"
  let hashedInput = computeSHA512 (input ++ sshPrivateKey)
  putStrLn $ "SHA512: " ++ hashedInput ++ "\n ↓"
  let
    hashedInputToAlpha :: String
    hashedInputToAlpha = filter isAlpha hashedInput
    hashedInputToDigit :: [Int]
    hashedInputToDigit = map digitToInt hashedInput
    hashedInputFactor :: Double
    hashedInputFactor = fromIntegral (sum hashedInputToDigit) / fromIntegral (length hashedInputToDigit) / 16
    hashedID = prefix ++ body
     where
      prefix = take 2 (drop (round $ hashedInputFactor * fromIntegral (length hashedInputToAlpha) - 1) hashedInputToAlpha)
      body = take 6 (drop (round $ hashedInputFactor * 128 - 1) hashedInput)
    hashedIDOutputPath =
      if arch == "x86_64"
        then homeDirectory ++ "/Documents/id-list.txt"
        else homeDirectory ++ "/id-list.txt"
  putStrLn $ "Alpha in SHA512 is: " ++ hashedInputToAlpha ++ "\n ↓"
  putStrLn $ "Digit in SHA512 is: " ++ show hashedInputToDigit ++ "\n ↓"
  putStrLn $ "Factor is: " ++ show hashedInputFactor ++ "\n ↓"
  putStrLn $ "Hashed id is: " ++ hashedID ++ "\n"
  putStrLn $ input ++ " -> " ++ hashedID ++ "\n"
  when (arch == "x86_64") (createDirectoryIfMissing True (homeDirectory ++ "/Documents"))
  isAppendSuccess <- tryIOError (appendFile hashedIDOutputPath (input ++ " -> " ++ hashedID ++ "\n"))
  case isAppendSuccess of
    Left _ -> do
      putStrLn $ "Cannot append result in" ++ hashedIDOutputPath ++ " ❌"
      exitFailure
    Right _ -> do
      putStrLn $ "The result is appended to " ++ hashedIDOutputPath ++ " ✔"
      exitSuccess
