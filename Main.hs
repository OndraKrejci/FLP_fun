
import qualified System.Environment as Environment

import Algorithm (removeSimpleRules, createCNF)
import Input (readInput, parseGrammar)

main :: IO ()
main = do
  args <- Environment.getArgs
  let (mode, fpath) = parseArgs args
  if mode == "h"
    then do
      putStr help
      return ()
    else do
      contents <- readInput fpath
      let grammar = parseGrammar (lines contents)
      case mode of
        "i" -> print grammar
        "1" -> print (removeSimpleRules grammar)
        "2" -> print (createCNF (removeSimpleRules grammar))
        _ -> error ("Invalid mode [" ++ mode ++ "]")
      return ()

parseArgs :: [String] -> (String, Maybe String)
parseArgs [] = ("h", Nothing)
parseArgs [mode] = (parseMode mode, Nothing)
parseArgs [mode, path] = (parseMode mode, Just path)
parseArgs _ = error ("Error: Supplied more than two arguments\n" ++ help)

parseMode :: String -> String
parseMode str = case str of
  "-i" -> "i"
  "-1" -> "1"
  "-2" -> "2"
  _ -> error ("Error: Invalid argument [" ++ str ++ "]\n" ++ help)

help :: String
help =
  "Usage:\n" ++
  "./flp-fun <mode> [path]\n\n" ++
  "mode:\n" ++
  " -i: print loaded grammar\n" ++
  " -1: print grammar without simple rules\n" ++
  " -2: print grammar without cnf\n"
