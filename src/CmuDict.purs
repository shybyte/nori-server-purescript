module CmuDict where

import Prelude hiding (apply)
import Control.Monad.Eff
import Control.Monad.Eff.Console (log)
import Data.Maybe
import Data.List (toList, mapMaybe, take, (..))
import Data.Tuple hiding (lookup)
import Data.StrMap hiding (toList)
import Data.String (drop, toUpper, length, split)
import Data.String.Regex (noFlags, replace, regex)
import Data.Foldable (foldl, Foldable)

import FileSystem

type Phonemes = Array String
type Word = String
type CmuDict = StrMap Phonemes

type DictEntry = Tuple Word Phonemes
showDictEntry :: DictEntry -> String
showDictEntry  (Tuple word phonemes )=
  "DictEntry  { word: "<> show word
  <> ", phonemes: " <> show phonemes
  <> " }"

lineToDictEntry :: String -> Maybe DictEntry
lineToDictEntry line =
    case split "  " line of
      [word, phonemesString] -> Just $ Tuple word (split " " (removeNumbers phonemesString))
      _ -> Nothing

removeNumbers :: String -> String
removeNumbers = replace replaceDigitsRegex ""
  where
    replaceDigitsRegex = regex "\\d+" (noFlags {global = true})

loadDict :: forall e. Eff (console :: Control.Monad.Eff.Console.CONSOLE, fs:: FS | e) CmuDict
loadDict = do
    dictFileContent <- readTextFile "data/cmudict-small.txt"
    let dictLines = toList $ split "\n" dictFileContent
    let entryPairs =  mapMaybe lineToDictEntry dictLines
    --log $ show $ map showDictEntry entryPairs
    let dict = fromList $ map (\i -> Tuple (show i) i) entryPairs
    return $ fromList $ toList entryPairs