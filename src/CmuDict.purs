module CmuDict where

import Prelude hiding (apply)
import Control.Monad.Eff
import Control.Monad.Eff.Console (log)
import Data.Maybe
import Data.List (toList, mapMaybe, take)
import Data.Tuple hiding (lookup)
import Data.StrMap hiding (toList)
import Data.String (drop, toUpper, length, split)

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
      [word, phonemesString] -> Just (Tuple word (split " " phonemesString))
      _ -> Nothing

loadDict :: forall e. Eff (console :: Control.Monad.Eff.Console.CONSOLE, fs:: FS | e) CmuDict
loadDict = do
    dictFileContent <- readTextFile "data/cmudict.txt"
    let dictLines = toList $ split "\n" dictFileContent
    let entryPairs =  mapMaybe lineToDictEntry dictLines
    --log $ show $ map showDictEntry entryPairs
    let ls =  toList (take 10000 entryPairs)
    return $ fromList $ toList []