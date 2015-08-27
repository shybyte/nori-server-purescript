module Main where

import Prelude hiding (apply)
import Control.Monad.Eff
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Exception
import Data.Foreign.EasyFFI
import Data.Maybe
import Data.Array (head, index, mapMaybe)
import Data.Maybe.Unsafe
import Data.String (drop, take, toUpper, length, split)
import Data.StrMap (lookup)

import Node.Express.Types
import Node.Express.App
import Node.Express.Handler

import FileSystem
import Reciter (translateWord, splitToArpabet)
import CmuDict


type Phoneme = String
type PhonemeWord = Array Phoneme
type PhonemeLine = Array PhonemeWord

textToPhonemes :: CmuDict ->  String -> Array PhonemeLine
textToPhonemes cmuDict text =
  map  (\line -> map mapWord line) linesOfWords
  where
    linesOfWords :: Array (Array String)
    linesOfWords = map (split " ") (split "\n" (toUpper text))
    mapWordFallBack word = splitToArpabet $ translateWord word
    mapWord word =
      case (lookup word cmuDict) of
        Just phonmes -> phonmes
        Nothing -> mapWordFallBack word


handler :: CmuDict -> Handler
handler cmuDict = do
  textParam <- getRouteParam "text"
  setResponseHeader "Access-Control-Allow-Origin" "*"
  setResponseHeader "Access-Control-Allow-Headers" "Origin, X-Requested-With, Content-Type, Accept"
  case textParam of
      Nothing -> nextThrow $ error "Text is required"
      Just text -> do
          sendJson (textToPhonemes cmuDict text)


app :: CmuDict -> App
app cmuDict = get "/phonemes/:text" (handler cmuDict)

main :: forall e. Eff (express :: Express, console :: Control.Monad.Eff.Console.CONSOLE, fs:: FS | e) Unit
main = do
   cmuDict <- loadDict
   port <- unsafeForeignFunction [""] "process.env.PORT || 3000"
   listenHttp (app cmuDict) port \_ ->
       log $ "Listening on " ++ show port