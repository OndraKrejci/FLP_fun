
module Util where

import qualified Data.Text as Text

-- Splits string on a delimiter string
splitStr :: String -> String -> [String]
splitStr delim str =
  let
    txt = Text.pack str
    parts = Text.splitOn (Text.pack delim) txt
  in map Text.unpack parts
