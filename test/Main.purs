module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Test.Unit (suite, test, timeout)
import Test.Unit.Main (runTest)
import Test.Unit.Assert as Assert


import Main

--main :: forall e. Eff (console :: CONSOLE | e) Unit
main = runTest do
  suite "test1" do
    test "stuff" do
      Assert.assert "hello" $ 1 == 1
