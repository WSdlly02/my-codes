import System.Exit (ExitCode (ExitFailure), exitFailure, exitWith)

main :: IO ()
main = do
  exitWith (ExitFailure 42)