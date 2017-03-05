module Main where

import Prelude 
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
--import Control.Applicative

import Data.Array (index, length, replicate, singleton, concat, zip, mapMaybe, range, elem, takeWhile, catMaybes, take, concatMap, reverse, findIndex, union, filter)
import Data.String (Pattern(..), split, toCharArray, fromCharArray, joinWith)
import Data.Tuple (Tuple(..), fst, snd, uncurry)
import Data.Maybe (Maybe(..), isNothing, isJust, fromMaybe, maybe)
import Data.Char (toUpper)
import Data.Traversable (traverse, for)
import Data.Functor (mapFlipped, (<#>))
import Data.Map
import Data.Enum (succ)

import Control.Apply

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "Hello sailor!"

--
-- Moves
--


data MoveType = Captures | Moves
data SourceIndicator = SquareHint Square | RankHint Int | FileHint Int


type Move = forall e.
            { hint :: Maybe SourceIndicator
            , moveType :: MoveType
            , destination :: Square
            , promotion :: Maybe Square
            , pieceType :: PieceType | e }

type FullMove = { source :: Square
                , moveType :: MoveType
                , destination :: Square
                , promotion :: Maybe Square
                , pieceType :: PieceType }            


-- Resolve 'standard' moves into fully qualified moves
resolve :: Move -> Position -> Array FullMove
resolve mv@{moveType, destination, promotion, pieceType} p =
  candidates mv p <#> \source -> { moveType, destination, promotion, pieceType, source }


candidates :: Move -> Position -> Array Square
candidates mv p = let pc = Piece (mv.pieceType) (currentPlayer p)
                      b = positionBoard p
                  in filter (flip congruent mv.hint) (pieceSquares pc b)


-- Checks if a square 'works' with a given hint
congruent :: Square -> Maybe SourceIndicator -> Boolean
congruent sq Nothing = true
congruent sq (Just (SquareHint sq2)) = sq == sq2
congruent sq (Just (RankHint r)) = rank sq == r
congruent sq (Just (FileHint f)) = fileNr sq == f


pieceSquares :: Piece -> Board -> Array Square
pieceSquares pc b = mapMaybe (\x -> if snd x == pc
                                    then Just (fst x)
                                    else Nothing)
                    (toUnfoldable b)



--
-- Position
--
currentPlayer :: Position -> Color
currentPlayer _ = White -- TODO


findIndexFlipped :: forall a. Array a -> (a -> Boolean) -> Maybe Int  
findIndexFlipped a b = findIndex b a
  
firstPieceIndex :: Color -> Array Square -> Board -> Maybe Int
firstPieceIndex c sqs b = findIndexFlipped sqs \sq -> do
  pieceColorAt sq b == Just c


pieceAt :: Square -> Board -> Maybe Piece
pieceAt = lookup

pieceColor :: Piece -> Color
pieceColor (Piece _ c) = c

pieceType :: Piece -> PieceType
pieceType (Piece t _) = t

pieceTypeAt :: Square -> Board -> Maybe PieceType
pieceTypeAt sq b = map pieceType (pieceAt sq b)

pieceColorAt :: Square -> Board -> Maybe Color
pieceColorAt sq b = map pieceColor (pieceAt sq b)

opponent :: Color -> Color
opponent White = Black
opponent Black = White

data Position = Position Board Properties
data Properties = Properties -- TODO

passant :: Position -> Maybe Square
passant _ = Nothing -- TODO

positionBoard :: Position -> Board
positionBoard (Position b _) = b

-- Designates a connected sequence of squares
type SquareSequence = Array Square


pieceRange :: MoveType -> Piece -> Square -> Position -> Array SquareSequence
pieceRange Moves pc sq p = moveRange pc sq p
pieceRange Captures pc sq p = map singleton (attackRange pc sq p)


moveRange :: Piece -> Square -> Position -> Array SquareSequence
moveRange pc sq p = moveRange' pc sq (positionBoard p)


moveRange' :: Piece -> Square -> Board -> Array SquareSequence
moveRange' pc sq b = pieceMoveSquares pc sq <#> \sqs -> do
  let opp = firstPieceIndex (opponent (pieceColor pc)) sqs b
      own = firstPieceIndex (pieceColor pc) sqs b
  take (fromMaybe 8 (min opp own)) sqs


mapMaybeFlipped :: forall a b. Array a -> (a -> Maybe b) -> Array b
mapMaybeFlipped a b = mapMaybe b a


attackRange :: Piece -> Square -> Position -> Array Square
attackRange pc sq p =
  let rng = attackRange' pc sq (positionBoard p)
  in if pieceType pc == Pawn 
     then maybe rng (union rng <<< singleton) (passantAttackRange pc sq p)
     else rng


passantAttackRange :: Piece -> Square -> Position -> Maybe Square
passantAttackRange pc sq p = do
  pass <- passant p
  
  let sqs = concat (pieceAttackSquares pc sq)
  if elem pass sqs
    then passant p
    else Nothing


-- note: disregards passant
attackRange' :: Piece -> Square -> Board -> Array Square
attackRange' pc sq b = mapMaybeFlipped (pieceAttackSquares pc sq) \sqs -> do
  let opp = firstPieceIndex (opponent (pieceColor pc)) sqs b
      own = firstPieceIndex (pieceColor pc) sqs b

  ix <- opp
  if opp < own
    then index sqs ix
    else Nothing

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
walkOffsets :: Square -> Array (Tuple Int Int) -> SquareSequence
walkOffsets s = mapMaybe (offset' s) -- implementation walks over all squares, yes


forwardSquares :: Color -> Square -> SquareSequence
forwardSquares c sq =
  let x = pawnDirection c
      x2 = x * 8
  in mapMaybe (offset' sq) (zip (range x x2) (replicate 8 0))


pawnAttackSquares :: Color -> Square -> Array SquareSequence
pawnAttackSquares c sq = map pure (pawnAttackSquares' c sq)

pawnAttackSquares' :: Color -> Square -> Array Square
pawnAttackSquares' c sq = mapMaybeFlipped [1, -1] \x -> do
  offset' sq (Tuple x (pawnDirection c))


pawnMoveSquares' :: Color -> Square -> SquareSequence
pawnMoveSquares' c sq = let n = if initialPawnRank c == rank sq
                                then 2
                                else 1
                        in take n (forwardSquares c sq)

pawnMoveSquares :: Color -> Square -> Array SquareSequence
pawnMoveSquares c sq = map pure (pawnMoveSquares' c sq)


kingSquares' :: Square -> SquareSequence
kingSquares' = concatMap (take 1) <<< queenSquares



knightSquares' :: Square -> Array Square
knightSquares' sq = let l = 2
                        l2 = -2
                        s = 1
                        s2 = -1

                        jmps = (zip [l2, l2, l, l, s, s, s2, s2] [s, s2, s, s2, l, l2, l, l2])
                    in mapMaybe (offset' sq) jmps

knightSquares :: Square -> Array SquareSequence
knightSquares sq = map pure (knightSquares' sq)

kingSquares :: Square -> Array SquareSequence
kingSquares sq = map pure (kingSquares' sq)


bishopSquares :: Square -> Array SquareSequence
bishopSquares s = let r = range 1 8
                      r2 = range (-8) (-1)
                      d1 = zip r r
                      d2 = zip r r2
                      d3 = zip r2 r
                      d4 = zip r2 r2
                  in map (walkOffsets s) [d1, d2, d3, d4]


rookSquares :: Square -> Array SquareSequence
rookSquares s = let r = range 1 8
                    r2 = range (-8) (-1)
                    z = replicate 8 0

                    h = zip r z
                    h2 = zip r2 z
                    v = zip z r
                    v2 = zip z r2

                    in map (walkOffsets s) [h, h2, v, v2]


queenSquares :: Square -> Array SquareSequence
queenSquares s = concat [rookSquares s, bishopSquares s]


officerSquares :: OfficerType -> Square -> Array SquareSequence
officerSquares Knight = knightSquares
officerSquares Bishop = bishopSquares
officerSquares Queen  = queenSquares
officerSquares Rook   = rookSquares
officerSquares King   = kingSquares

pieceMoveSquares :: Piece -> Square -> Array SquareSequence
pieceMoveSquares pc sq = case pieceType pc of
  Pawn -> pawnMoveSquares (pieceColor pc) sq
  Officer ot -> officerSquares ot sq


pieceAttackSquares :: Piece -> Square -> Array SquareSequence
pieceAttackSquares pc sq = case pieceType pc of
  Pawn -> pawnAttackSquares (pieceColor pc) sq
  Officer ot -> officerSquares ot sq


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
data PieceType = Pawn | Officer OfficerType
data OfficerType = King | Queen | Rook | Bishop | Knight 
data Color = White | Black

derive instance colorEq :: Eq Color
derive instance colorOrd :: Ord Color

derive instance officerEq :: Eq OfficerType
derive instance officerOrd :: Ord OfficerType

derive instance pieceEq :: Eq Piece
derive instance pieceOrd :: Ord Piece

derive instance pieceTypeEq :: Eq PieceType
derive instance pieceTypeOrd :: Ord PieceType

instance showColor :: Show Color where
  show White = "White"
  show Black = "Black"

instance showOfficerType :: Show OfficerType where
  show Bishop = "Bishop"
  show Knight = "Knight"
  show Rook = "Rook"
  show King = "King"
  show Queen = "Queen"

instance showPieceType :: Show PieceType where
  show (Officer ot) = appendStrings ["(", "Officer ", show ot, ")"]
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


decodeOfficerType :: Char -> Maybe OfficerType
decodeOfficerType 'K' = Just King
decodeOfficerType 'Q' = Just Queen
decodeOfficerType 'B' = Just Bishop
decodeOfficerType 'N' = Just Knight
decodeOfficerType 'R' = Just Rook
decodeOfficerType _ = Nothing

decodePieceType :: Char -> Maybe PieceType
decodePieceType 'P' = Just Pawn
decodePieceType c = map Officer (decodeOfficerType c)

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


type Board = Map Square Piece

board :: FENBoardString -> Maybe Board
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
