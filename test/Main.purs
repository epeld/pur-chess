module Test.Main where

import Prelude
import Data.Maybe
import Data.Functor
import Data.String
import Control.Monad
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Test.Unit (suite, test, timeout)
import Test.Unit.Main (runTest)
import Test.Unit.Assert as Assert


import Main

decodeEncodeSquare = parseSquare >=> squareString

assertIdempotSquare sq =
  let isIdempot s = fromMaybe false (decodeEncodeSquare s <#> \s' -> s == s')          
  in if isIdempot sq
     then Assert.assert sq $ true
     else Assert.assert (joinWith " " [sq, "!=", (fromMaybe "??" (decodeEncodeSquare sq))]) $ false


--main :: forall e. Eff (console :: CONSOLE | e) Unit
main = runTest do
  suite "test1" do
    test "decoding, then encoding square is idempotent" do
      assertIdempotSquare "e4"
      assertIdempotSquare "g3"
      assertIdempotSquare "f2"
      assertIdempotSquare "a8"
