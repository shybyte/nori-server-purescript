module FileSystem where

import Prelude
import Control.Monad.Eff
import Data.Function

foreign import data FS :: !

type ErrorCode = String
type FilePath = String

foreign import readTextFile :: forall e. FilePath -> Eff (fs :: FS | e) String
