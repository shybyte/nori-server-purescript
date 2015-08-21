module ReciterTest where

import Prelude
import Test.Unit
import Control.Monad.Eff.Console
import Reciter


assertTranslation english expectedPhonemes = do
  let phonemes = translateWord english
  assert (english ++ " -> " ++ expectedPhonemes ++ " but was " ++ phonemes) $ expectedPhonemes == phonemes

assertSplitToArpabet arpabetWord expectedArpabetPhonemes = do
  let arpabetPhonemes = splitToArpabet arpabetWord
  assert (arpabetWord ++ " -> " ++ (show expectedArpabetPhonemes) ++ " but was " ++ (show arpabetPhonemes)) $ expectedArpabetPhonemes == arpabetPhonemes


testReciter = do
  test "translateWord" do
      assertTranslation "I" "AY4"
      assertTranslation "II" "IHAY"
      assertTranslation "LOVE" "LAH4V"
      assertTranslation "RULE" "RUWL" -- testing @
      assertTranslation "STEN" "SEHN"
      assertTranslation "ICE" "AY5S" -- [" :(I)^%","AY5"],
      assertTranslation "ö" "" -- remove unknown characters
  test "splitToArpabet" do
        assertSplitToArpabet "N" ["N"]
        assertSplitToArpabet "AY" ["AY"]
        assertSplitToArpabet "AYS" ["AY", "S"]
        assertSplitToArpabet "ÖAYS" ["AY", "S"]
        assertSplitToArpabet "AY3S" ["AY", "S"] -- remove number for now

