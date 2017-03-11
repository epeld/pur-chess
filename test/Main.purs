module Test.Main where

import Prelude
import Data.Maybe
import Data.Functor
import Data.Traversable
import Data.String hiding (length)
import Data.Map
import Data.Array
import Control.Monad
import Control.Monad.Aff
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Test.Unit --(suite, test, timeout)
import Test.Unit.Main (runTest)
import Test.Unit.Assert as Assert
import Test.Unit.Console


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


old = let 
           s = joinWith " " (map (show <<< squareString) squares)
       in consoleLog s


main = testIt

--main :: forall a.
--        (Partial) => Eff ( console :: CONSOLE, testOutput :: TESTOUTPUT, avar :: AVAR | a) Unit
testIt = runTest do
  suite "Tests" do
    test "Foo" do
      Assert.assert "Testing works" $ true
  
  suite "Basics" do
    testSquareEncode ["e4", "c3", "b2", "f4", "a8", "h1"]
    

  suite "Parsing" do
    testWith board "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR" \b -> do
      let pt = assertPieceType b
          pc = assertPieceColor b
      pt "e4" Pawn
      pc "e4" White
      
      pt "g1" (Officer Knight)
      pt "a1" (Officer Rook)
      pt "c8" (Officer Bishop)
      
      pt "c7" Pawn
      pc "c7" Black
      
      pc "c2" White

    test "Passant" do
      with' parsePassant "c3" \sq -> sq == parseSquare "c3"
      with' parsePassant "-" \sq -> sq == Nothing

      Assert.assert "garbage" $ parsePassant "wfwfs" == Nothing

    test "Castling Rights" do
      with parseRights "Kk" \r -> do
        Assert.assert "length == 2" $ length r == 2
        Assert.assert "All king side" $ all (\rt -> side rt == Kingside) r
        
      with parseRights "KQ" \r -> do
        Assert.assert "length == 2" $ length r == 2
        Assert.assert "All white" $ all (\rt -> clr rt == White) r

      with parseRights "-" \r -> do
        Assert.assert "length == 0" $ length r == 0

    test "Move Number" do
      with' parseInt "334" \x -> x == 334
      with' parseInt "0" \x -> x == 0

    test "Turn" do
      with' parseTurn "w" \c -> c == White
      with' parseTurn "b" \c -> c == Black

    test "FEN Properties" do
      with' props "b KQkq e3 0 1" \p -> true

    let fen = "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1"
        
    test "FEN Properties String" do
          
      with' propString fen \s ->
        s == "b KQkq e3 0 1"

    test "FEN Board String" do
      with' boardString fen \s ->
        s == "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR"

  suite "Move Logic" do
    test "??" do
      with parsePosition fen \p -> do
        isValidMove p "e4"
      
assertPieceType b s pt = let msg = (unwords [show pt, "at", s])
                         in assertPiece b s msg \pc -> pieceType pc == pt

assertPieceColor b s c = let msg = (unwords [show c, "piece at", s])
                         in assertPiece b s msg \pc -> pieceColor pc == c
                                                        
assertPiece b s msg pred = with parseSquare s \sq ->
  Assert.assert msg $ maybe false pred (pieceAt sq b)


isValidMove _ _ = Assert.assert "resulting position is legal" $ true


-- Helper for testing things that need to be parsed out first, e.g
-- Parsing a position AND THEN running tests on it
with p s f = case p s of
  Nothing -> Assert.assert s false
  Just sth -> f sth

-- Helper for running quick tests on things that need to be parsed
with' p s f = with p s (Assert.assert s <<< f)


testWith p s f = test s $ with p s f


unwords :: Array String -> String
unwords = joinWith " "
