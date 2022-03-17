
import qualified System.Environment as Environment

import Algorithm (removeSimpleRules, createCNF)
import Input (readInput, parseGrammar)

data Mode = Print | RemoveSimple | Chomsky | Help
  deriving (Eq, Show)

-- Main function, loads the CFG from input a prints it in the requested form
main :: IO ()
main = do
  args <- Environment.getArgs
  let (mode, fpath) = parseArgs args
  if mode == Help
    then do
      putStr help
    else do
      contents <- readInput fpath
      let grammar = parseGrammar (lines contents)
      case mode of
        Print -> print grammar
        RemoveSimple -> print (removeSimpleRules grammar)
        Chomsky -> print (createCNF (removeSimpleRules grammar))
        _ -> error ("Invalid mode [" ++ show mode ++ "]")

-- Parses the given list of arguments into a mode and a file path
-- Mode is required, file path is optional, more than two arguments are not allowed
-- Additionally returns the Help mode for an empty list of arguments
parseArgs :: [String] -> (Mode, Maybe String)
parseArgs [] = (Help, Nothing)
parseArgs [mode] = (parseMode mode, Nothing)
parseArgs [mode, path] = (parseMode mode, Just path)
parseArgs _ = error ("Supplied more than two arguments\n" ++ help)

-- Parses the mode from an argument
parseMode :: String -> Mode
parseMode str = case str of
  "-i" -> Print
  "-1" -> RemoveSimple
  "-2" -> Chomsky
  _ -> error ("Invalid argument [" ++ str ++ "]\n" ++ help)

-- Help text for the program
help :: String
help =
  "Transform a context-free grammar without simple rules into Chomsky normal form\n" ++
  "Usage:\n" ++
  "./flp-fun <mode> [path]\n\n" ++
  "mode:\n" ++
  " -i: print loaded grammar\n" ++
  " -1: print grammar without simple rules\n" ++
  " -2: print grammar in CNF\n"
