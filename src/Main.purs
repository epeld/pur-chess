module Main where

import Prelude 
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
--import Control.Applicative

import Data.Array (index, length, replicate, singleton, concat, zip, mapMaybe, range, elem, takeWhile, catMaybes, take, concatMap, reverse)
import Data.String (Pattern(..), split, toCharArray, fromCharArray, joinWith)
import Data.Tuple (Tuple(..), fst, snd, uncurry)
import Data.Maybe (Maybe(..), isNothing, isJust)
import Data.Char (toUpper)
import Data.Traversable (traverse)
import Data.Functor (mapFlipped, (<#>))
import Data.Map
import Data.Enum (succ)

import Control.Apply

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "Hello sailor!"


--
-- Squares and Offsets
--

initialPawnRank :: Color -> Int
initialPawnRank White = 2
initialPawnRank Black = 7

-- In FEN coordinates:
pawnDirection :: Color -> Int
pawnDirection White = 1
pawnDirection Black = -1

validCoord :: Int -> Boolean
validCoord x = elem x (range 1 8)

offset :: Square -> Int -> Int -> Maybe Square
offset (Tuple x y) offx offy = let x2 = x + offx
                                   y2 = y + offy
                                   coords = range 1 8
                               in if validCoord x2 && validCoord y2
                                  then Just (Tuple x2 y2)
                                  else Nothing

offset' :: Square -> (Tuple Int Int) -> Maybe Square
offset' s = uncurry (offset s)


-- Assumes an array of consecutive offsets 'walking away' from 's'
walkOffsets :: Square -> Array (Tuple Int Int) -> Array Square
walkOffsets s = mapMaybe (offset' s) -- implementation walks over all squares, yes


forwardSquares :: Color -> Square -> Array Square
forwardSquares c sq =
  let x = pawnDirection c
      x2 = x * 8
  in mapMaybe (offset' sq) (zip (range x x2) (replicate 8 0))

pawnMoveSquares :: Color -> Square -> Array Square
pawnMoveSquares c sq = let n = if initialPawnRank c == rank sq
                               then 2
                               else 1
                       in take n (forwardSquares c sq)



kingSquares :: Square -> Array Square
kingSquares = concatMap (take 1) <<< queenSquares



knightSquares :: Square -> Array Square
knightSquares sq = let l = 2
                       l2 = -2
                       s = 1
                       s2 = -1

                       jmps = (zip [l2, l2, l, l, s, s, s2, s2] [s, s2, s, s2, l, l2, l, l2])
                   in mapMaybe (offset' sq) jmps


bishopSquares :: Square -> Array (Array Square)
bishopSquares s = let r = range 1 8
                      r2 = range (-8) (-1)
                      d1 = zip r r
                      d2 = zip r r2
                      d3 = zip r2 r
                      d4 = zip r2 r2
                  in map (walkOffsets s) [d1, d2, d3, d4]


rookSquares :: Square -> Array (Array Square)
rookSquares s = let r = range 1 8
                    r2 = range (-8) (-1)
                    z = replicate 8 0

                    h = zip r z
                    h2 = zip r2 z
                    v = zip z r
                    v2 = zip z r2

                    in map (walkOffsets s) [h, h2, v, v2]


queenSquares :: Square -> Array (Array Square)
queenSquares s = concat [rookSquares s, bishopSquares s]


--
-- FEN stuff
--

newtype FENString = FEN String
newtype FENBoardString = FENBoard String
newtype FENPropertyString = FENProps String
newtype FENRowString = FENRow String


fen :: String
fen = "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1"

data Piece = Piece PieceType Color
data PieceType = Pawn | King | Queen | Rook | Bishop | Knight 
data Color = White | Black

derive instance colorEq :: Eq Color
derive instance colorOrd :: Ord Color

derive instance pieceEq :: Eq Piece
derive instance pieceOrd :: Ord Piece

derive instance pieceTypeEq :: Eq PieceType
derive instance pieceTypeOrd :: Ord PieceType

instance showColor :: Show Color where
  show White = "White"
  show Black = "Black"

instance showPieceType :: Show PieceType where
  show Bishop = "Bishop"
  show Knight = "Knight"
  show Rook = "Rook"
  show King = "King"
  show Queen = "Queen"
  show Pawn = "Pawn"

instance showPiece :: Show Piece where
  show (Piece pt c) = joinWith "" ["(", joinWith " " ["Piece", show pt, show c], ")"]

decodeChar :: Char -> Maybe (Array (Maybe Piece))
decodeChar '1' = Just (replicate 1 Nothing)
decodeChar '2' = Just (replicate 2 Nothing)
decodeChar '3' = Just (replicate 3 Nothing)
decodeChar '4' = Just (replicate 4 Nothing)
decodeChar '5' = Just (replicate 5 Nothing)
decodeChar '6' = Just (replicate 6 Nothing)
decodeChar '7' = Just (replicate 7 Nothing)
decodeChar '8' = Just (replicate 8 Nothing)
decodeChar c = case decodePieceType (toUpper c) of
  Just pt -> Just (singleton (Just (Piece pt (color c))))
  Nothing -> Nothing

  
decodePieceType :: Char -> Maybe PieceType
decodePieceType 'K' = Just King
decodePieceType 'Q' = Just Queen
decodePieceType 'B' = Just Bishop
decodePieceType 'N' = Just Knight
decodePieceType 'R' = Just Rook
decodePieceType 'P' = Just Pawn
decodePieceType _ = Nothing

color :: Char -> Color
color c = if c == toUpper c then White else Black


type Square = Tuple Int Int

-- Cartesian coordinates (imo):
-- Bottom left is (0,0). The rest of squares go along positive x and y axes
squareToCartesian :: Square -> Square
squareToCartesian sq = Tuple (8 - fst sq) (8 - snd sq)

rank :: Square -> Int
rank = (add 1) <<< snd <<< squareToCartesian

fileNr :: Square -> Int
fileNr = (add 1) <<< fst <<< squareToCartesian

fileChar :: Square -> Maybe Char
fileChar sq = index (toCharArray "abcdefgh") (add (fileNr sq) (-1))


squareString :: Square -> Maybe String
squareString sq = fileChar sq <#> \c -> do
  appendStrings [fromCharArray [c], show (rank sq)]


appendStrings :: Array String -> String
appendStrings = fromCharArray <<< concatMap toCharArray


-- enumerate all the squares of the board (in FEN order)
squares :: Array Square
squares = let range = [1,2,3,4,5,6,7,8]
          in concat (reverse range <#> \n -> do
                        range <#> \m -> do
                          Tuple n m)


board :: FENBoardString -> Maybe (Map Square Piece)
board s = do
  ps <- pieces s
  let
    assocs :: Array (Tuple Square (Maybe Piece))
    assocs = zip squares ps

    check :: Tuple Square (Maybe Piece) -> Maybe (Tuple Square Piece)
    check (Tuple s Nothing) = Nothing
    check (Tuple s (Just p)) = Just (Tuple s p)
  Just (fromFoldable (mapMaybe check assocs))


pieces :: FENBoardString -> Maybe (Array (Maybe Piece))
pieces s = do
  rs <- rowStrings s
  rs' <- traverse unrle rs
  Just (concat rs')


unrle :: FENRowString -> Maybe (Array (Maybe Piece))
unrle (FENRow r) = do
  let
    parts :: Maybe (Array (Array (Maybe Piece)))
    parts = traverse decodeChar (toCharArray r)
  ps <- map concat parts
  case length ps of
    8 -> Just ps
    _ -> Nothing


rowStrings :: FENBoardString -> Maybe (Array FENRowString)
rowStrings (FENBoard s) = let parts = split (Pattern "/") s
                          in case length parts of
                            8 -> Just (map FENRow parts)
                            _ -> Nothing


boardString :: FENString -> Maybe FENBoardString
boardString = map fst <<< fenSplit


propString :: FENString -> Maybe FENPropertyString
propString = map snd <<< fenSplit


fenSplit :: FENString -> Maybe (Tuple FENBoardString FENPropertyString)
fenSplit (FEN s) = let parts = split (Pattern " ") s
                       
                       b :: Maybe String
                       b = index parts 0
                       
                       p :: Maybe String
                       p = index parts 1
                       
                   in map wrap (lift2 Tuple p b)

wrap :: Tuple String String -> Tuple FENBoardString FENPropertyString
wrap (Tuple p b) = Tuple (FENBoard b) (FENProps p)
