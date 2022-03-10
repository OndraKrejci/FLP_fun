
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use head" #-}

module Algorithm (removeSimpleRules, createCNF) where

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Maybe as Maybe

import Grammar (CFG(CFG), Rule(..))

---- Algorithm 4.5 Transform a grammar without epsilon rules into a grammar without simple rules

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

---- Algorithm 4.7 Transform a proper grammar into grammar in Chomsky normal form

createCNF :: CFG -> CFG
createCNF (CFG nonterms terms rules start) =
    let
        initialP' = foldr (\rule acc -> if isSingleTerm (_right rule) terms || isTwoNonterms (_right rule) nonterms then Set.insert rule acc else acc) Set.empty rules
        nontermRules = foldr (\rule acc -> if length (_right rule) > 2 then Set.insert rule acc else acc) Set.empty rules
        (replacedNontermRules, replacedMergedNonterms, replacedTermNonterms1) = replaceChomskyNontermRules nontermRules nonterms terms
        termRules = foldr (\rule acc -> if isOneOrTwoTerms (_right rule) terms then Set.insert rule acc else acc) Set.empty rules
        (replacedTermRules, replacedTermNonterms2) = replaceChomskyTermRules termRules terms

        replacedTermNonterms = Set.union replacedTermNonterms1 replacedTermNonterms2

        nontermToTermRules = generateNontermToTermRules replacedTermNonterms

        remainingNonterms = foldr (\(Rule left _) acc -> Set.insert left acc) Set.empty initialP'

        rules' = foldr1 Set.union [initialP', replacedNontermRules, replacedTermRules, nontermToTermRules]
        nonterms' = foldr1 Set.union [replacedMergedNonterms, replacedTermNonterms, remainingNonterms]
    in CFG nonterms' terms rules' start

isSingleTerm :: [String] -> Set.Set String -> Bool
isSingleTerm symbols terms = length symbols == 1 && Set.member (head symbols) terms

isTwoNonterms :: [String] -> Set.Set String -> Bool
isTwoNonterms symbols nonterms = length symbols == 2 && all (`Set.member` nonterms) symbols

isOneOrTwoTerms :: [String] -> Set.Set String -> Bool
isOneOrTwoTerms symbols terms = length symbols == 2 && any (`Set.member` terms) symbols

replaceChomskyNontermRules :: Set.Set Rule -> Set.Set String -> Set.Set String -> (Set.Set Rule, Set.Set String, Set.Set String)
replaceChomskyNontermRules rules nonterms terms =
    let
        fn :: Rule -> (Set.Set Rule, Set.Set String, Set.Set String) -> (Set.Set Rule, Set.Set String, Set.Set String)
        fn rule (rulesAcc, mergedNontermsAcc, termNontermsAcc) =
            let (newRules, mergedNonterms, termNonterms) = replaceChomskyNontermRule rule nonterms terms
            in (Set.union newRules rulesAcc, Set.union mergedNonterms mergedNontermsAcc, Set.union termNonterms termNontermsAcc)
    in foldr fn (Set.empty, Set.empty, Set.empty) rules

replaceChomskyNontermRule :: Rule -> Set.Set String -> Set.Set String -> (Set.Set Rule, Set.Set String, Set.Set String)
replaceChomskyNontermRule rule@(Rule left right) nonterms terms =
    if length right <= 2
        then error ("Invalid usage: right side of the rule [" ++ show rule ++ "] has less than 3 symbols")
        else (Set.insert newRule rulesOut, Set.insert mergedNonterm mergedNontermsOut, newTermNonterms)
    where
        (newRightSide, mergedNonterm, termNonterm) = createRightSide right
        newRule = Rule left newRightSide
        (rulesOut, mergedNontermsOut, termNontermsOut) = replaceChomskyNontermRule' (tail right) (Set.empty, Set.empty, Set.empty)
        newTermNonterms =
            if Maybe.isJust termNonterm
                then Set.insert (Maybe.fromJust termNonterm) termNontermsOut
                else termNontermsOut

        createRightSide :: [String] -> ([String], String, Maybe String)
        createRightSide rightSide =
            let
                first = head rightSide
                nt2 = getMergedNontermSymbol (tail rightSide)
            in
                if Set.member first nonterms
                    then ([first, nt2], nt2, Nothing)
                    else
                        let nt1 = getNewSingleNontermSymbol first
                        in ([nt1, nt2], nt2, Just nt1)

        replaceChomskyNontermRule' :: [String] -> (Set.Set Rule, Set.Set String, Set.Set String) -> (Set.Set Rule, Set.Set String, Set.Set String)
        replaceChomskyNontermRule' right' (rulesAcc, mergedNontermsAcc, termNontermsAcc)
            | length right' < 2 = error ("Right side of the rule is too short [" ++ List.intercalate "" right' ++ "]")
            | length right' == 2 = (Set.insert finalRule rulesAcc, mergedNontermsAcc, Set.union finalNonterms termNontermsAcc)
            | otherwise = (Set.insert newRule' rulesAcc', Set.insert mergedNonterm' mergedNontermsAcc', newTermNonterms')
            where
                nt1 = getMergedNontermSymbol right'
                (newRightSide', mergedNonterm', termNonterm') = createRightSide right'
                newRule' = Rule nt1 newRightSide'
                newTermNonterms' =
                    if Maybe.isJust termNonterm'
                        then Set.insert (Maybe.fromJust termNonterm') termNontermsAcc'
                        else termNontermsAcc'

                (finalRule, finalNonterms) =
                    if any (`Set.member` terms) right'
                        then replaceChomskyTermRule tempRule terms
                        else (tempRule, Set.empty)
                    where
                        tempRule = Rule nt1 right'

                (rulesAcc', mergedNontermsAcc', termNontermsAcc') = replaceChomskyNontermRule' (tail right') (rulesAcc, mergedNontermsAcc, termNontermsAcc)

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

replaceChomskyTermRule :: Rule -> Set.Set String -> (Rule, Set.Set String)
replaceChomskyTermRule rule@(Rule left right) terms
    | all (`Set.member` terms) right = (Rule left [replacement1, replacement2], Set.fromList [replacement1, replacement2])
    | Set.member (right!!0) terms = (Rule left [replacement1, right!!1], Set.singleton replacement1)
    | Set.member (right!!1) terms = (Rule left [right!!0, replacement2], Set.singleton replacement2)
    | otherwise = error ("Supplied invalid rule: " ++ show rule)
    where
        replacement1 = getNewSingleNontermSymbol (right!!0)
        replacement2 = getNewSingleNontermSymbol (right!!1)

generateNontermToTermRules :: Set.Set String -> Set.Set Rule
generateNontermToTermRules = foldr (\x acc -> Set.insert (Rule x [init x]) acc) Set.empty
