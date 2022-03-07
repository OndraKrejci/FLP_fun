
import qualified System.IO as IO
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Text as Text

import Grammar

main :: IO ()
main = do
  contents <- IO.readFile "cfg-pr4_14.txt"
  let grammar = parseGrammar (lines contents)
  print grammar
  return ()

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
    validTerms = Set.union (Set.fromList (map (:[]) ['a'..'z'])) (Set.fromList ["+", "-", "*", "/", "(", ")"])
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
