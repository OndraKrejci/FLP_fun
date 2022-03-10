
import qualified System.IO as IO
import qualified System.Environment as Environment
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Maybe as Maybe

import Grammar (CFG(CFG), Rule(Rule))
import Algorithm (removeSimpleRules, createCNF)

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

readInput :: Maybe String -> IO String -- TODO
readInput fpath = IO.readFile (Maybe.fromJust fpath)

parseGrammar :: [String] -> CFG
parseGrammar ls
  | length ls < 4 = error "Grammar definition shorter than 4 lines"
  | otherwise =
    let
      nonterms = parseNonterms (head ls)
      terms = parseTerms (ls!!1)
      rules = parseRules (drop 3 ls) nonterms terms
      start = parseStartSymbol (ls!!2) nonterms
    in CFG nonterms terms rules start

parseNonterms :: String -> Set.Set String
parseNonterms line =
  let
    symbols = words line
    validNonterms = Set.fromList (map (:[]) ['A'..'Z'])
    fn symbol acc =
      if Set.member symbol validNonterms
        then Set.insert symbol acc
        else error ("Invalid nonterminal symbol [" ++ symbol ++ "]")
  in foldr fn Set.empty symbols

parseTerms :: String -> Set.Set String
parseTerms line =
  let
    symbols = words line
    -- validTerms = Set.fromList (map (:[]) ['a'..'z']) -- TODO
    validTerms = Set.union (Set.fromList (map (:[]) ['a'..'z'])) (Set.fromList ["+", "-", "*", "/", "(", ")", "[", "]"])
    fn symbol acc =
      if Set.member symbol validTerms
        then Set.insert symbol acc
        else error ("Invalid terminal symbol [" ++ symbol ++ "]")
  in foldr fn Set.empty symbols

parseStartSymbol :: String -> Set.Set String -> String
parseStartSymbol start nonterms =
  if Set.member start nonterms
    then start
    else error ("Start symbol [" ++ start ++ "] is not a nonterminal symbol")

parseRules :: [String] -> Set.Set String -> Set.Set String -> Set.Set Rule
parseRules ls nonterms terms =
  foldr (\line acc -> Set.insert (parseRule line nonterms terms) acc) Set.empty ls

parseRule :: String -> Set.Set String -> Set.Set String -> Rule
parseRule line nonterms terms =
  let
    parts = splitStr "->" line
    handleLeft left =
        if Set.member left nonterms
          then left
          else error ("Unknown nonterminal symbol [" ++ left ++ " ]" ++ "in rule: " ++ line)
    handleRight right =
      let invalid = foldr (\x acc -> if Set.member x terms || Set.member x nonterms then acc else x:acc) [] right
      in
        if null invalid
          then right
          else error ("Unknown symbols [" ++ List.intercalate ", " invalid ++ "] in rule: " ++ line)
    makeRule left rightStr =
      let right = map (:[]) rightStr
      in Rule (handleLeft left) (handleRight right)
  in
    if length parts /= 2
      then error ("Invalid rule: " ++ line)
      else makeRule (head parts) (parts!!1)

splitStr :: String -> String -> [String]
splitStr delim str =
  let
    txt = Text.pack str
    parts = Text.splitOn (Text.pack delim) txt
  in map Text.unpack parts


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
