-- Inspired by
-- Automatic Translation of English Text to Phonetics by Means of Letter-to-Sound Rules
-- www.dtic.mil/dtic/tr/fulltext/u2/a021929.pdf

module Reciter where

import Prelude
import ReciterTranslationTable
import Control.Monad.Eff.Console
import Data.Array.Unsafe (unsafeIndex)
import Data.Array (head, filter)
import Data.Maybe.Unsafe
import Data.Maybe
import Data.String (drop, take, toUpper, length, split)
import Data.String.Regex (replace, test, regex, noFlags, match)
import Data.Foldable (intercalate)

replaceS = Data.String.replace

type Rule =
  {
    headPattern :: String,
    body :: String,
    tailPattern :: String,
    replacement :: String
  }

showRule :: Rule -> String
showRule  x =
  "Rule  { headPattern: "<> show x.headPattern
  <> ", body: " <> show x.body
  <> ", tailPattern: " <> show x.tailPattern
  <> ", replacement: " <> show x.replacement
  <> " }"

arrayToRule :: Array String -> Rule
arrayToRule ruleAsArray =
  {
    headPattern: getPatternPart 1,
    body: getPatternPart 2,
    tailPattern: getPatternPart 3,
    replacement: replaceS "/H" "H" (unsafeIndex ruleAsArray 1)
  }
  where
    patternString = unsafeIndex ruleAsArray 0
    patternRegExp = regex "(.*)\\((.*)\\)(.*)" noFlags
    matchResult = fromJust (match patternRegExp patternString)
    getPatternPart index = fromJust (unsafeIndex matchResult index)

getRules :: Array Rule
getRules =
  map arrayToRule translationTable

rules = getRules


translateWord :: String -> String
translateWord text = translateTail " " (toUpper text)

translateTail :: String -> String -> String
translateTail head "" = ""
translateTail head tail = rule.replacement ++ translateTail head' tail'
  where
    rule = fromMaybe defaultRule $ findMatchingRule head tail
    head' = head ++ rule.body
    tail' = drop (length rule.body) tail

defaultRule = { headPattern: "", body: "*", tailPattern: "", replacement: ""}

findMatchingRule :: String -> String -> Maybe Rule
findMatchingRule sHead sTail = head matchingRules
  where
    matchingRules :: Array Rule
    matchingRules = filter (isMatching sHead (sTail ++ " ")) rules


isMatching :: String -> String -> Rule -> Boolean
isMatching sHead bodyAndTail rule =
  startsWith bodyAndTail rule.body
  && test headRegex sHead
  && test tailRegex sTail
  where
    headRegex = regex (toRegexString rule.headPattern ++ "$") noFlags
    tailRegex = regex ("^" ++ toRegexString rule.tailPattern) noFlags
    sTail = drop (length rule.body) bodyAndTail


startsWith :: String -> String -> Boolean
startsWith s needle =
  needle == take (length needle) s


toRegexString :: String -> String
toRegexString pattern =
  replaceSuffix $ replaceSibilant $ replaceConsonantInfluencingU $ replaceZeroOrMoreConsonantes
  $ replaceConsonant $ replaceOneOrMoreVowels $ replacePlus $ pattern
  where
    replacePlus = replaceS "+"  "[EIY]" -- frontVowel
    replaceConsonant = replaceS "^" "[BCDFGHJKLMNPQRSTVWXZ]"
    replaceOneOrMoreVowels = replaceS "#" "[AEIOUY]+"
    replaceZeroOrMoreConsonantes = replaceS ":" "[BCDFGHJKLMNPQRSTVWXZ]*"
    replaceConsonantInfluencingU = replaceS "@" "([TSRDLZNJ]|TH|CH|SH)"
    replaceSibilant = replaceS "&" "([SCGZXJ]|CH|SH)"
    replaceSuffix = replaceS "%" "(ER|E|ES|ED|ING|ELY)"


splitToArpabet :: String -> Array String
splitToArpabet arpabetWord =
  case matchingArpabetPhoneme of
    Just arpabetPhoneme -> [arpabetPhoneme] ++ splitToArpabet(drop (length arpabetPhoneme) arpabetWord)
    Nothing -> if (length arpabetWord < 2) then [] else splitToArpabet (drop 1 arpabetWord)
  where
    matchingArpabetPhoneme = head $ filter (startsWith arpabetWord) arpabetPhonemes

