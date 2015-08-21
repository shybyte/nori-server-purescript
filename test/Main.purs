module Test.Main where

import Test.Unit
import ReciterTest
import Control.Monad.Eff.Console

main = runTest do
 testReciter
