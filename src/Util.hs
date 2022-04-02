-- FLP - Functional project - BKG-2-CNF
-- Author: Ondřej Krejčí (xkrejc69)
-- Year: 2022
-- Utility functions

module Util where

import qualified Data.Maybe as Maybe

-- Splits a string on a delimiter
splitStr :: String -> String -> [String]
splitStr _ "" = [""]
splitStr delim str =
  let
    delimLen = length delim

    findDelim :: String -> Int -> Maybe Int
    findDelim [] _ = Nothing
    findDelim str'@(_:rest) n
      | length str' < delimLen = Nothing
      | take delimLen str' == delim = Just n
      | otherwise = findDelim rest (n + 1)

    maybeIdx = findDelim str 0
  in
    if Maybe.isJust maybeIdx
      then
        let idx = Maybe.fromJust maybeIdx
        in take idx str : splitStr delim (drop (idx + delimLen) str)
      else [str]
