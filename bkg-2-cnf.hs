
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Map as Map

data Rule = Rule {
    left :: String,
    right :: [String]
} deriving (Eq, Ord)

data CFG = CFG {
    nonterms :: Set.Set String,
    terms :: Set.Set String,
    rules :: Set.Set Rule,
    start :: String
} deriving Eq

instance Show Rule where
    show (Rule left right) = left ++ "->" ++ List.intercalate "" right

instance Show CFG where
    show (CFG nonterms terms rules start) =
        List.intercalate "," (Set.toList nonterms) ++ "\n" ++
        List.intercalate "," (Set.toList terms) ++ "\n" ++
        start ++ "\n" ++
        List.intercalate "\n" (map show (Set.toList rules))

simpleRulesSet :: Set.Set String -> Set.Set Rule -> Set.Set String -> Set.Set String
simpleRulesSet ni rules nonterms
    | Set.null rules = Set.empty
    | otherwise =
        let nni = Set.union ni (Set.fromList [head ruleRes | rule <- Set.toList rules, let ruleRes = right rule, Set.member (left rule) ni && isSingleNonterm ruleRes nonterms])
        in if nni == ni then ni else simpleRulesSet nni rules nonterms

simpleRulesSet' :: Set.Set String -> Set.Set Rule -> Set.Set String -> Set.Set String
simpleRulesSet' ni rules nonterms =
    let
        fn rule acc =
            let ruleRes = right rule in
                if Set.member (left rule) ni && isSingleNonterm ruleRes nonterms
                    then Set.insert (head ruleRes) acc
                    else acc
        nni = Set.union ni (foldr fn Set.empty rules)
    in if nni == ni then ni else simpleRulesSet nni rules nonterms

generateSimpleRuleSets :: Set.Set String -> Set.Set Rule -> Map.Map String (Set.Set String)
generateSimpleRuleSets nonterms rules = foldr (\x acc -> Map.insert x (simpleRulesSet' (Set.singleton x) rules nonterms) acc) Map.empty nonterms

isSingleNonterm :: [String] -> Set.Set String -> Bool
isSingleNonterm symbols nonterms = length symbols == 1 && Set.member (head symbols) nonterms

isSimpleRule :: Rule -> Set.Set String -> Bool
isSimpleRule (Rule left right) = isSingleNonterm right

getNonSimpleRules :: Set.Set Rule -> Set.Set String -> Set.Set Rule
getNonSimpleRules rules nonterms = Set.filter (\rule -> not $ isSimpleRule rule nonterms) rules

removeSimpleRules :: CFG -> CFG
removeSimpleRules grammar@(CFG nonterms terms rules start) =
    let
        rulesList = Set.toList (getNonSimpleRules rules nonterms)
        simpleRulesNontermsList = Map.toList (generateSimpleRuleSets nonterms rules)
        nrules = Set.fromList [Rule key right | (Rule left right) <- rulesList, (key, val) <- simpleRulesNontermsList, Set.member left val]
    in CFG nonterms terms nrules start

grammar45 :: CFG
grammar45 = CFG {
    nonterms = Set.fromList ["E", "T", "F"],
    terms = Set.fromList ["+", "*", "(", ")", "i"],
    rules = Set.fromList [Rule "E" ["E", "+", "T"], Rule "E" ["T"], Rule "T" ["T", "*", "F"], Rule "T" ["F"], Rule "F" ["(", "E", ")"], Rule "F" ["i"]],
    start = "E"
}
