
module Grammar (Rule(..), CFG(..)) where

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
