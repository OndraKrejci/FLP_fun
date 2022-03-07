
module Grammar (Rule(..), CFG(..)) where

import qualified Data.Set as Set
import qualified Data.List as List

data Rule = Rule {
    _left :: String,
    _right :: [String]
} deriving (Eq, Ord)

data CFG = CFG {
    _nonterms :: Set.Set String,
    _terms :: Set.Set String,
    _rules :: Set.Set Rule,
    _start :: String
} deriving Eq

instance Show Rule where
    show (Rule left right) = left ++ "->" ++ List.intercalate "" right

instance Show CFG where
    show (CFG nonterms terms rules start) =
        List.intercalate "," (Set.toList nonterms) ++ "\n" ++
        List.intercalate "," (Set.toList terms) ++ "\n" ++
        start ++ "\n" ++
        List.intercalate "\n" (map show (Set.toList rules))
