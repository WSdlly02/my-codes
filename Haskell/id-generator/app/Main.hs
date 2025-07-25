import Control.Exception (IOException, try)
import Control.Monad (when)
import Crypto.Hash (SHA512 (SHA512), hashWith)
import Data.ByteString.Char8 (pack)
import Data.Char (digitToInt, isAlpha, isDigit)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs, lookupEnv)
import System.Exit (exitFailure)
import System.IO (hFlush, stdout)
import System.Info (os)

computeSHA512 :: String -> String
computeSHA512 input = show $ hashWith SHA512 (pack input)

getHomeDirectory :: IO String
getHomeDirectory = case os of
  x | x == "linux" || x == "linux-android" -> do
    homeVar <- lookupEnv "HOME"
    case homeVar of
      Nothing -> do
        putStrLn "\n\n$HOME variable missing! ❌"
        exitFailure
      Just "" -> do
        putStrLn "\n\n$HOME variable missing! ❌"
        exitFailure
      Just y -> return y
  "mingw32" -> do
    homeVar <- lookupEnv "USERPROFILE"
    case homeVar of
      Nothing -> do
        putStrLn "\n\n%USERPROFILE% variable missing! ❌"
        exitFailure
      Just "" -> do
        putStrLn "\n\n%USERPROFILE%  variable missing! ❌"
        exitFailure
      Just x -> do
        return x
  unknownOS -> do
    putStrLn $ "\n\nUnknown OS: " ++ unknownOS ++ " ❌"
    exitFailure

getSSHPrivateKey :: IO String
getSSHPrivateKey = do
  homeDirectory <- getHomeDirectory
  sshPrivateKey <- try (readFile (homeDirectory ++ "/.ssh/id_rsa")) :: IO (Either IOException String)
  case sshPrivateKey of
    Left _ -> do
      putStrLn "\n\nThe id_rsa ssh key cannot be found in the .ssh directory under your home directory ❌"
      exitFailure
    Right x -> do
      putStrLn " ✔\n"
      return $ concat (drop 1 (init (lines x)))

ensureIDListPathExists :: IO ()
ensureIDListPathExists = do
  homeDirectory <- getHomeDirectory
  when (os /= "linux-android") (createDirectoryIfMissing True (homeDirectory ++ "/Documents"))

main :: IO ()
main = do
  putStrLn "id-generator: Generating hashed id"
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
    alphaInHashedInput :: String
    alphaInHashedInput = filter isAlpha hashedInput
    digitInHashedInput :: [Int]
    digitInHashedInput = map digitToInt (filter isDigit hashedInput)
    hashedInputFactor :: Double
    hashedInputFactor = fromIntegral (sum digitInHashedInput) / fromIntegral (length digitInHashedInput) / 10
    outputHead = take 2 (drop (round $ hashedInputFactor * fromIntegral (length alphaInHashedInput) - 1) alphaInHashedInput)
    output = take 6 (drop (round $ hashedInputFactor * 128 - 1) hashedInput)
    idListPath =
      if os == "linux-android"
        then homeDirectory ++ "/id-list.txt"
        else homeDirectory ++ "/Documents/id-list.txt"
  putStrLn $ "Alpha in SHA512 is: " ++ alphaInHashedInput ++ "\n ↓"
  putStrLn $ "Digit in SHA512 is: " ++ filter isDigit hashedInput ++ "\n ↓"
  putStrLn $ "Factor is: " ++ show hashedInputFactor ++ "\n ↓"
  putStrLn $ "Hashed id is: " ++ outputHead ++ output ++ "\n"
  putStrLn $ input ++ " -> " ++ outputHead ++ output ++ "\n"
  ensureIDListPathExists
  putStrLn $ "The result is appeded to " ++ idListPath
  appendFile idListPath (input ++ " -> " ++ outputHead ++ output ++ "\n")
