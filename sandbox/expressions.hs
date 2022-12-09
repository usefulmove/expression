import System.Environment
import Data.List

main = do
    args <- getArgs -- IO [String]
    progName <- getProgName -- IO String
    -- putStrLn "  comp 0.0.1 ( Haskell )"
    mapM putStrLn args