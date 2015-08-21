module Main where

import Prelude hiding (apply)
import Control.Monad.Eff
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Exception
import Data.Foreign.EasyFFI
import Data.Maybe

import Node.Express.Types
import Node.Express.App
import Node.Express.Handler


type Phoneme = String
type PhonemeWord = Array Phoneme
type PhonemeLine = Array PhonemeWord

textToPhonemes :: String -> Array PhonemeLine
textToPhonemes text = [[[text]]]


handler :: Handler
handler = do
  textParam <- getRouteParam "text"
  case textParam of
      Nothing -> nextThrow $ error "Text is required"
      Just text -> do
          sendJson (textToPhonemes text)


app :: App
app = get "/phonemes/:text" handler

main :: forall e. Eff (express :: Express | e) Unit
main = do
    port <- unsafeForeignFunction [""] "process.env.PORT || 3000"
    listenHttp app port \_ ->
        log $ "Listening on " ++ show port