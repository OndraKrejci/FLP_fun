
module Algorithm (removeSimpleRules, createCNF) where

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.List as List

import Grammar (CFG(..), Rule(..))

---- Algorithm 4.5 The elimination of simple rules

-- Removes simple rules from a context-free grammar
-- Implementation of Algorithm 4.5
-- Expects a grammar without epsilon rules
removeSimpleRules :: CFG -> CFG
removeSimpleRules (CFG nonterms terms rules start) =
  let
    nonSimpleRules = getNonSimpleRules rules nonterms
    simpleRulesSets = generateSimpleRulesSets nonterms rules

    -- Replaces the rule with nonterminals from the simple rules sets
    -- Part of Step 2 of algorithm 4.5
    replaceRule :: Rule -> Set.Set Rule
    replaceRule (Rule left right) = Map.foldrWithKey (\key val acc -> if Set.member left val then Set.insert (Rule key right) acc else acc) Set.empty simpleRulesSets

    nrules = foldr (\rule acc -> Set.union acc (replaceRule rule)) Set.empty nonSimpleRules
  in CFG nonterms terms nrules start

-- Constructs a set of nonterminals for each nonterminal in input set,
-- the sets contain all nonterminals B that the input nonterminal A can be rewritten into:
-- N_A = {B | A =>* B}
-- Step 1 of algorithm 4.5
generateSimpleRulesSets :: Set.Set String -> Set.Set Rule -> Map.Map String (Set.Set String)
generateSimpleRulesSets nonterms rules = foldr (\x acc -> Map.insert x (generateSimpleRulesSet (Set.singleton x) rules nonterms) acc) Map.empty nonterms

-- Constructs a set of nonterminals N_A
-- Expects the nonterminal A in the input set, i.e. ni = N_0 = {A}
generateSimpleRulesSet :: Set.Set String -> Set.Set Rule -> Set.Set String -> Set.Set String
generateSimpleRulesSet ni rules nonterms
  | Set.null rules = Set.empty
  | otherwise =
    let
      fn :: Rule -> Set.Set String -> Set.Set String
      fn rule@(Rule left right) acc =
        if Set.member left ni && isSimpleRule rule nonterms
          then Set.insert (head right) acc
          else acc
      nni = Set.union ni (foldr fn Set.empty rules)
    in if nni == ni then ni else generateSimpleRulesSet nni rules nonterms

-- Checks if a rule is simple
isSimpleRule :: Rule -> Set.Set String -> Bool
isSimpleRule (Rule _ right) nonterms = length right == 1 && Set.member (head right) nonterms

-- Returns all rules which are not simple
getNonSimpleRules :: Set.Set Rule -> Set.Set String -> Set.Set Rule
getNonSimpleRules rules nonterms = Set.filter (\rule -> not $ isSimpleRule rule nonterms) rules

---- Algorithm 4.7 The transformation into Chomsky normal form

-- Transforms a context-free grammar into Chomsky normal form
-- Implementation of Algorithm 4.5
-- Expects a proper grammar without simple rules
-- Assumes that the grammar does not generate empty words (i.e. does not contain S -> epsilon)
createCNF :: CFG -> CFG
createCNF (CFG nonterms terms rules start) =
  let
    -- Set from steps 1 and 2 of algorithm 4.7
    initialP' = foldr (\rule@(Rule _ right) acc -> if isOneSymbol right terms || isTwoSymbols right nonterms then Set.insert rule acc else acc) Set.empty rules
    
    (replacedLongRules, mergedNonterms, apostropheNonterms1) = replaceLongRules longRules nonterms terms
      where longRules = foldr (\rule@(Rule _ right) acc -> if length right > 2 then Set.insert rule acc else acc) Set.empty rules
    (replacedTermRules, apostropheNonterms2) = replaceChomskyTermRules termRules terms
      where termRules = foldr (\rule@(Rule _ right) acc -> if isOneOrTwoSymbols right terms then Set.insert rule acc else acc) Set.empty rules

    apostropheNonterms = Set.union apostropheNonterms1 apostropheNonterms2

    nontermToTermRules = generateNontermToTermRules apostropheNonterms
  in CFG {
    _nonterms = foldr1 Set.union [nonterms, mergedNonterms, apostropheNonterms],
    _terms = terms,
    _rules = foldr1 Set.union [initialP', replacedLongRules, replacedTermRules, nontermToTermRules],
    _start = start
  }

-- Checks if string is exactly one symbol from given set
isOneSymbol :: [String] -> Set.Set String -> Bool
isOneSymbol str symbols = length str == 1 && Set.member (head str) symbols

-- Checks if string is exactly two symbols from given set
isTwoSymbols :: [String] -> Set.Set String -> Bool
isTwoSymbols str symbols = length str == 2 && all (`Set.member` symbols) str

-- Checks if string is exactly one or two synbols from the given set
isOneOrTwoSymbols :: [String] -> Set.Set String -> Bool
isOneOrTwoSymbols str symbols = length str == 2 && any (`Set.member` symbols) str

-- Replaces all input long rules with CNF compliant rules
-- Step 4 of algorithm 4.7
-- Returns a set of new rules, merged nonterminals (e.g. <ABC>) and apostrophe nonterminals (e.g. a')
replaceLongRules :: Set.Set Rule -> Set.Set String -> Set.Set String -> (Set.Set Rule, Set.Set String, Set.Set String)
replaceLongRules rules nonterms terms =
  let
    fn :: Rule -> (Set.Set Rule, Set.Set String, Set.Set String) -> (Set.Set Rule, Set.Set String, Set.Set String)
    fn rule (rulesAcc, mergedNontermsAcc, termNontermsAcc) =
      let (newRules, mergedNonterms, termNonterms) = replaceLongRule rule nonterms terms
      in (Set.union newRules rulesAcc, Set.union mergedNonterms mergedNontermsAcc, Set.union termNonterms termNontermsAcc)
  in foldr fn (Set.empty, Set.empty, Set.empty) rules

-- Replaces a single long rule with CNF compliant rules
replaceLongRule :: Rule -> Set.Set String -> Set.Set String -> (Set.Set Rule, Set.Set String, Set.Set String)
replaceLongRule rule@(Rule left right) nonterms terms
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
            let nt1 = getApostropheNontermSymbol first
            in ([nt1, nt2], nt2, Set.singleton nt1)
    newRule = Rule left rightSide
    (rulesRec, mergedNontermsRec, termNontermsRec) = replaceLongRule (Rule mergedNonterm (tail right)) nonterms terms

-- Replaces a terminal symbol with a nonterminal symbol
getApostropheNontermSymbol :: String -> String
getApostropheNontermSymbol nonterm = nonterm ++ "'"

-- Replaces multiple symbols with a single merged nonterminal symbol
getMergedNontermSymbol :: [String] -> String
getMergedNontermSymbol nonterms = "<" ++ List.intercalate "" nonterms ++ ">"

-- Replaces rules whose right side contains two symbols at least one of which is a term
-- Step 5 of algorithm 4.7
replaceChomskyTermRules :: Set.Set Rule -> Set.Set String -> (Set.Set Rule, Set.Set String)
replaceChomskyTermRules rules terms =
  let
    fn :: Rule -> (Set.Set Rule, Set.Set String) -> (Set.Set Rule, Set.Set String)
    fn rule (rulesAcc, nontermsAcc) =
      let (newRule, nonterms) = replaceChomskyTermRule rule terms
      in (Set.insert newRule rulesAcc, Set.union nonterms nontermsAcc)
  in foldr fn (Set.empty, Set.empty) rules

-- Replaces a single rule whose right side contains two symbols at least one of which is a term
replaceChomskyTermRule :: Rule -> Set.Set String-> (Rule, Set.Set String)
replaceChomskyTermRule (Rule left right) terms =
  let
    fn :: String -> ([String], Set.Set String) -> ([String], Set.Set String)
    fn symbol (symbols, nts) =
      if Set.member symbol terms
        then
          let replacement = getApostropheNontermSymbol symbol
          in (replacement:symbols, Set.insert replacement nts)
        else (symbol:symbols, nts)
    (rightSide, newNonterms) = foldr fn ([], Set.empty) right
  in (Rule left rightSide, newNonterms)

-- Creates new rules transforming nonterminal representations of terminals back into terminal
-- Step 6 of algorithm 4.7
generateNontermToTermRules :: Set.Set String -> Set.Set Rule
generateNontermToTermRules = foldr (\x acc -> Set.insert (Rule x [init x]) acc) Set.empty
