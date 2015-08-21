module ReciterTest where

import Prelude
import Test.Unit
import Control.Monad.Eff.Console
import Reciter


assertTranslation english expectedPhonemes = do
  let phonemes = translateWord english
  assert (english ++ " -> " ++ expectedPhonemes ++ " but was " ++ phonemes) $ expectedPhonemes == phonemes


testReciter = test "translateWord" do
    assertTranslation "I" "AY4"
    assertTranslation "II" "IHAY"
    assertTranslation "LOVE" "LAH4V"
    assertTranslation "RULE" "RUWL" -- testing @
    assertTranslation "STEN" "SEHN"
    assertTranslation "ICE" "AY5S" -- [" :(I)^%","AY5"],
    assertTranslation "ö" "" -- remove unknown characters
