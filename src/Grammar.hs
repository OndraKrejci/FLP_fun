-- FLP - Functional project - BKG-2-CNF
-- Author: Ondřej Krejčí (xkrejc69)
-- Year: 2022
-- Datatypes for a context-free grammar, constants for valid symbols

module Grammar (Rule(..), CFG(..), validNonterms, validTerms) where

import qualified Data.Set as Set
import qualified Data.List as List

-- Rule of a context-free grammar
data Rule = Rule {
  _left :: String,
  _right :: [String]
} deriving (Eq, Ord)

-- Context-free grammar
data CFG = CFG {
  _nonterms :: Set.Set String,
  _terms :: Set.Set String,
  _rules :: Set.Set Rule,
  _start :: String
} deriving Eq

instance Show Rule where
  -- String representation of a rule
  show (Rule left right) = left ++ "->" ++ List.intercalate "" right

instance Show CFG where
  -- String representation of a CFG
  show (CFG nonterms terms rules start) =
    List.intercalate "," (Set.toList nonterms) ++ "\n" ++
    List.intercalate "," (Set.toList terms) ++ "\n" ++
    start ++ "\n" ++
    List.intercalate "\n" (map show (Set.toList rules))

validNonterms :: Set.Set String
validNonterms = Set.fromList (map (:[]) ['A'..'Z'])

validTerms :: Set.Set String
validTerms = Set.fromList (map (:[]) (['a'..'z'] ++ ['+', '-', '*', '/', '(', ')', '[', ']']))
