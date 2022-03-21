
module Algorithm (removeSimpleRules, createCNF) where

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.List as List

import Grammar (CFG(CFG), Rule(..))

---- Algorithm 4.5 Transform a grammar without epsilon rules into a grammar without simple rules

simpleRulesSet :: Set.Set String -> Set.Set Rule -> Set.Set String -> Set.Set String
simpleRulesSet ni rules nonterms
    | Set.null rules = Set.empty
    | otherwise =
        let
            fn :: Rule -> Set.Set String -> Set.Set String
            fn rule@(Rule left right) acc =
                if Set.member left ni && isSimpleRule rule nonterms
                    then Set.insert (head right) acc
                    else acc
            nni = Set.union ni (foldr fn Set.empty rules)
        in if nni == ni then ni else simpleRulesSet nni rules nonterms

generateSimpleRuleSets :: Set.Set String -> Set.Set Rule -> Map.Map String (Set.Set String)
generateSimpleRuleSets nonterms rules = foldr (\x acc -> Map.insert x (simpleRulesSet (Set.singleton x) rules nonterms) acc) Map.empty nonterms

isSimpleRule :: Rule -> Set.Set String -> Bool
isSimpleRule (Rule _ right) nonterms = length right == 1 && Set.member (head right) nonterms

getNonSimpleRules :: Set.Set Rule -> Set.Set String -> Set.Set Rule
getNonSimpleRules rules nonterms = Set.filter (\rule -> not $ isSimpleRule rule nonterms) rules

removeSimpleRules :: CFG -> CFG
removeSimpleRules (CFG nonterms terms rules start) =
    let
        nonSimpleRules = getNonSimpleRules rules nonterms
        simpleRulesSets = generateSimpleRuleSets nonterms rules

        replaceRule :: Rule -> Set.Set Rule
        replaceRule (Rule left right) = Map.foldrWithKey (\key val acc -> if Set.member left val then Set.insert (Rule key right) acc else acc) Set.empty simpleRulesSets

        nrules = foldr (\rule acc -> Set.union acc (replaceRule rule)) Set.empty nonSimpleRules
    in CFG nonterms terms nrules start

---- Algorithm 4.7 Transform a proper grammar into grammar in Chomsky normal form

createCNF :: CFG -> CFG
createCNF (CFG nonterms terms rules start) =
    let
        initialP' = foldr (\rule@(Rule _ right) acc -> if isOneSymbol right terms || isTwoSymbols right nonterms then Set.insert rule acc else acc) Set.empty rules
        
        (replacedNontermRules, replacedMergedNonterms, replacedTermNonterms1) = replaceChomskyNontermRules nontermRules nonterms terms
            where nontermRules = foldr (\rule@(Rule _ right) acc -> if length right > 2 then Set.insert rule acc else acc) Set.empty rules
        (replacedTermRules, replacedTermNonterms2) = replaceChomskyTermRules termRules terms
            where termRules = foldr (\rule@(Rule _ right) acc -> if isOneOrTwoSymbols right terms then Set.insert rule acc else acc) Set.empty rules

        replacedTermNonterms = Set.union replacedTermNonterms1 replacedTermNonterms2
        nontermToTermRules = generateNontermToTermRules replacedTermNonterms

        rules' = foldr1 Set.union [initialP', replacedNontermRules, replacedTermRules, nontermToTermRules]
        nonterms' = foldr1 Set.union [nonterms, replacedMergedNonterms, replacedTermNonterms]
    in CFG nonterms' terms rules' start

isOneSymbol :: [String] -> Set.Set String -> Bool
isOneSymbol str symbols = length str == 1 && Set.member (head str) symbols

isTwoSymbols :: [String] -> Set.Set String -> Bool
isTwoSymbols str symbols = length str == 2 && all (`Set.member` symbols) str

isOneOrTwoSymbols :: [String] -> Set.Set String -> Bool
isOneOrTwoSymbols str symbols = length str == 2 && any (`Set.member` symbols) str

replaceChomskyNontermRules :: Set.Set Rule -> Set.Set String -> Set.Set String -> (Set.Set Rule, Set.Set String, Set.Set String)
replaceChomskyNontermRules rules nonterms terms =
    let
        fn :: Rule -> (Set.Set Rule, Set.Set String, Set.Set String) -> (Set.Set Rule, Set.Set String, Set.Set String)
        fn rule (rulesAcc, mergedNontermsAcc, termNontermsAcc) =
            let (newRules, mergedNonterms, termNonterms) = replaceChomskyNontermRule rule nonterms terms
            in (Set.union newRules rulesAcc, Set.union mergedNonterms mergedNontermsAcc, Set.union termNonterms termNontermsAcc)
    in foldr fn (Set.empty, Set.empty, Set.empty) rules

replaceChomskyNontermRule :: Rule -> Set.Set String -> Set.Set String -> (Set.Set Rule, Set.Set String, Set.Set String)
replaceChomskyNontermRule rule@(Rule left right) nonterms terms
    | length right < 2 = error ("Invalid usage: right side of the rule [" ++ show rule ++ "] has less than 2 symbols")
    | length right == 2 =
        if any (`Set.member` terms) right
            then
                let (finalRule, finalNonterms) = replaceChomskyTermRule rule terms
                in (Set.singleton finalRule, Set.empty, finalNonterms)
            else (Set.singleton rule, Set.empty, Set.empty)
    | otherwise = (Set.insert newRule rulesRec, Set.insert mergedNonterm mergedNontermsRec, Set.union termNonterm termNontermsRec)
    where
        (rightSide, mergedNonterm, termNonterm) =
            let
                first = head right
                nt2 = getMergedNontermSymbol (tail right)
            in
                if Set.member first nonterms
                    then ([first, nt2], nt2, Set.empty)
                    else
                        let nt1 = getNewSingleNontermSymbol first
                        in ([nt1, nt2], nt2, Set.singleton nt1)
        newRule = Rule left rightSide
        (rulesRec, mergedNontermsRec, termNontermsRec) = replaceChomskyNontermRule (Rule mergedNonterm (tail right)) nonterms terms

getNewSingleNontermSymbol :: String -> String
getNewSingleNontermSymbol nonterm = nonterm ++ "'"

getMergedNontermSymbol :: [String] -> String
getMergedNontermSymbol nonterms = "<" ++ List.intercalate "" nonterms ++ ">"

replaceChomskyTermRules :: Set.Set Rule -> Set.Set String -> (Set.Set Rule, Set.Set String)
replaceChomskyTermRules rules terms =
    let
        fn :: Rule -> (Set.Set Rule, Set.Set String) -> (Set.Set Rule, Set.Set String)
        fn rule (rulesAcc, nontermsAcc) =
            let (newRule, nonterms) = replaceChomskyTermRule rule terms
            in (Set.insert newRule rulesAcc, Set.union nonterms nontermsAcc)
    in foldr fn (Set.empty, Set.empty) rules

replaceChomskyTermRule :: Rule -> Set.Set String-> (Rule, Set.Set String)
replaceChomskyTermRule (Rule left right) terms =
    let
        fn :: String -> ([String], Set.Set String) -> ([String], Set.Set String)
        fn symbol (symbols, nts) =
            if Set.member symbol terms
                then
                    let replacement = getNewSingleNontermSymbol symbol
                    in (replacement:symbols, Set.insert replacement nts)
                else (symbol:symbols, nts)
        (rightSide, newNonterms) = foldr fn ([], Set.empty) right
    in (Rule left rightSide, newNonterms)

generateNontermToTermRules :: Set.Set String -> Set.Set Rule
generateNontermToTermRules = foldr (\x acc -> Set.insert (Rule x [init x]) acc) Set.empty
