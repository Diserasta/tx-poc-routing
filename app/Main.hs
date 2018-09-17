module Main where

--import Lib
import qualified Mapper as M
import qualified Data.ByteString as B
import System.IO
import System.Exit
import Data.Text (Text,splitOn,pack,unpack)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Set as S



main :: IO ()
main = do
  putStrLn "TX POC Routing Program"
  putStrLn "Copright Samouil Vassilev 2018"
  tmp <- gen1Prompt
  putStrLn tmp



-- cmdLoop = do
--   cmd <- mainPrompt
--   putStrLn cmd
--   cmdLoop


-- mainPrompt :: IO String
-- mainPrompt = do
--   putStr $ "Please enter a command - h for help - q to quit\n"
--   hFlush stdout
--   str <-getLine
--   case str of
--     --"a"       -> addPrompt
--     "g"       -> genPrompt
--     --"h"       -> return mainHelpText
--     --"purge"   -> return ""
--     "q"       -> exitWith ExitSuccess
--     "q!"      -> exitWith (ExitFailure 05)
--     _         -> do
--       putStrLn "Invalid Input."
--   mainPrompt

splitToDistance :: [Text] -> (M.Node,M.Node,Double)
splitToDistance i
  | i == [] = error "splitToDistance: empty list"
  | otherwise = (read (unpack (i!!0)) :: M.Node, read (unpack (i!!1)) :: M.Node, read (unpack (i!!2)) :: Double)


gen1Prompt = do
  putStrLn "Please enter the name of the distance matrix file (in UTF-8 CSV format)"
  file <- getLine
  f <- decodeUtf8 <$> B.readFile file
  let tmp = map (splitOn (pack ",")) (splitOn (pack "\r\n") f)
  let tx = map splitToDistance tmp --Now should be [(Node,Node,Double)]
  --putStrLn $ show tx
  let avail = map (\k -> k !! 1) tmp
  let avail' = (S.toList . S.fromList) avail --Remove duplicates in n.logn time
  let avail'' = map unpack avail' --unpack the text to [Char]
  let availnum = map (read :: String -> Int) avail'' --Convert to num
  putStrLn "Please enter the source"
  src <- getLine
  let srcn = (read :: String -> M.Node) src
  putStrLn "Please enter the destination"
  dst <- getLine
  let dstn = (read :: String -> M.Node) dst
  putStrLn "Put in the desired max ring size"
  len <- getLine
  let lenn = (read :: String -> Int) len
  putStrLn "Beginning Ring Creation"
  hFlush stdout
  rings <- return $ M.genRing1Impl (srcn, dstn) tx availnum lenn [[]]
  return (show rings)