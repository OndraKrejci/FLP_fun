
module Input(readInput, parseGrammar) where

import qualified System.IO as IO
import qualified Data.Set as Set
import qualified Data.List as List

import Grammar (CFG(CFG), Rule(Rule))
import Util (splitStr)

-- Reads all input from a file if a file path is supplied, otherwise uses stdin
readInput :: Maybe String -> IO String
readInput = maybe IO.getContents IO.readFile

-- Creates a CFG representation from input strings
-- Expected format: nonterminals, terminals, start symbol, rule (one or more)
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

-- Creates a set of nonterminal symbols from a comma separated list
-- Nonterminal must be an upper case letter
parseNonterms :: String -> Set.Set String
parseNonterms line =
  let
    symbols = splitStr "," line
    validNonterms = Set.fromList (map (:[]) ['A'..'Z'])
    fn symbol acc =
      if Set.member symbol validNonterms
        then Set.insert symbol acc
        else error ("Invalid nonterminal symbol [" ++ symbol ++ "]")
  in foldr fn Set.empty symbols

-- Creates a set of terminal symbols from a comma separated list
-- Terminal must be a lower case letter,
-- also allows some additional symbols not specified in the requirements (used in tests)
parseTerms :: String -> Set.Set String
parseTerms line =
  let
    symbols = splitStr "," line
    validTerms = Set.union (Set.fromList (map (:[]) ['a'..'z'])) (Set.fromList ["+", "-", "*", "/", "(", ")", "[", "]"])
    fn symbol acc =
      if Set.member symbol validTerms
        then Set.insert symbol acc
        else error ("Invalid terminal symbol [" ++ symbol ++ "]")
  in foldr fn Set.empty symbols

-- Validates that the start symbol is a nonterminal
parseStartSymbol :: String -> Set.Set String -> String
parseStartSymbol start nonterms =
  if Set.member start nonterms
    then start
    else error ("Start symbol [" ++ start ++ "] is not a nonterminal symbol")

-- Parses the lines of input into a set of rules
parseRules :: [String] -> Set.Set String -> Set.Set String -> Set.Set Rule
parseRules ls nonterms terms =
  foldr (\line acc -> Set.insert (parseRule line nonterms terms) acc) Set.empty ls

-- Parses a string into a rule of a context-free grammar
-- Expects the rule to be separated with '->' with a nonterminal on the left side
-- and one or more symbols on the right side (each of them only one character)
-- Checks that the symbols in the rule belong to the existing sets of symbols
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
