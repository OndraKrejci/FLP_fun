
module Algorithm (removeSimpleRules) where

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.List as List

import Grammar (CFG(CFG), Rule(..))

simpleRulesSet :: Set.Set String -> Set.Set Rule -> Set.Set String -> Set.Set String
simpleRulesSet ni rules nonterms
    | Set.null rules = Set.empty
    | otherwise =
        let nni = Set.union ni (Set.fromList [head ruleRes | rule <- Set.toList rules, let ruleRes = _right rule, Set.member (_left rule) ni && isSingleNonterm ruleRes nonterms])
        in if nni == ni then ni else simpleRulesSet nni rules nonterms

simpleRulesSet' :: Set.Set String -> Set.Set Rule -> Set.Set String -> Set.Set String
simpleRulesSet' ni rules nonterms =
    let
        fn rule acc =
            let ruleRes = _right rule in
                if Set.member (_left rule) ni && isSingleNonterm ruleRes nonterms
                    then Set.insert (head ruleRes) acc
                    else acc
        nni = Set.union ni (foldr fn Set.empty rules)
    in if nni == ni then ni else simpleRulesSet nni rules nonterms

generateSimpleRuleSets :: Set.Set String -> Set.Set Rule -> Map.Map String (Set.Set String)
generateSimpleRuleSets nonterms rules = foldr (\x acc -> Map.insert x (simpleRulesSet' (Set.singleton x) rules nonterms) acc) Map.empty nonterms

isSingleNonterm :: [String] -> Set.Set String -> Bool
isSingleNonterm symbols nonterms = length symbols == 1 && Set.member (head symbols) nonterms

isSimpleRule :: Rule -> Set.Set String -> Bool
isSimpleRule (Rule _ right) = isSingleNonterm right

getNonSimpleRules :: Set.Set Rule -> Set.Set String -> Set.Set Rule
getNonSimpleRules rules nonterms = Set.filter (\rule -> not $ isSimpleRule rule nonterms) rules

removeSimpleRules :: CFG -> CFG
removeSimpleRules (CFG nonterms terms rules start) =
    let
        rulesList = Set.toList (getNonSimpleRules rules nonterms)
        simpleRulesNontermsList = Map.toList (generateSimpleRuleSets nonterms rules)
        nrules = Set.fromList [Rule key right | (Rule left right) <- rulesList, (key, val) <- simpleRulesNontermsList, Set.member left val]
    in CFG nonterms terms nrules start
