module Test.Main where

import Prelude
import Data.Maybe
import Data.Functor
import Data.Traversable
import Data.String
import Control.Monad
import Control.Monad.Aff
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Test.Unit --(suite, test, timeout)
import Test.Unit.Main (runTest)
import Test.Unit.Assert as Assert


import Main

decodeEncodeSquare :: String -> Maybe String
decodeEncodeSquare = parseSquare >=> squareString


assertIdempotSquare :: forall a. String -> Aff a Unit
assertIdempotSquare sq =
  let isIdempot s = fromMaybe false (decodeEncodeSquare s <#> \s' -> s == s')          
  in if isIdempot sq
     then Assert.assert sq $ true
     else Assert.assert (joinWith " " [sq, "!=", (fromMaybe "??" (decodeEncodeSquare sq))]) $ false


testSquareEncode :: forall a f. (Foldable f) => f String -> TestSuite a
testSquareEncode sqs = test "decoding, then encoding square is idempotent" do
  traverse_ assertIdempotSquare sqs


--main :: forall a.
--        (Partial) => Eff ( console :: CONSOLE, testOutput :: TESTOUTPUT, avar :: AVAR | a) Unit
main = runTest do
  suite "Tests" do
    test "Foo" do
      Assert.assert "Testing works" $ true
  
  suite "Basics" do
    testSquareEncode ["e4", "c3", "b2", "f4", "a8", "h1"]

  suite "Parsing" do
    withBoard "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR" \b -> do
      assertPieceType b "e4" Pawn
      assertPieceType b "g1" (Officer Knight)
      assertPieceType b "a1" (Officer Rook)

  suite "Move Logic" do
    withPosition "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1" \p -> do
      isValidMove p "e4" -- TODO
      

assertPieceType b s pt = withSquare s \sq ->
  Assert.assert (unwords [show pt, "at", s]) $ pieceTypeAt sq b == Just pt


isValidMove _ _ = Assert.assert "resulting position is legal" $ true


withPosition :: forall a. String -> (Position -> Test a) -> TestSuite a
withPosition s f = test (unwords ["Position", s])
                   (case parsePosition (FEN s) of
                     Nothing -> Assert.assert (unwords ["Parses", s]) false
                     Just pos -> f pos)


withBoard :: forall a. String -> (Board -> Test a) -> TestSuite a
withBoard s f = test (unwords ["Board", s])
                (case board (FENBoard s) of
                     Nothing -> Assert.assert (unwords ["Parses", s]) false
                     Just b -> f b)


withSquare s f = case parseSquare s of
  Nothing -> Assert.assert (unwords ["Parses", s]) false
  Just sq -> f sq


unwords :: Array String -> String
unwords = joinWith " "
