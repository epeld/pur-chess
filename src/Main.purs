module Main where

import Prelude 
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Data.Array (index, length, replicate, singleton, concat, zip, mapMaybe, range, elem, takeWhile, catMaybes, take, concatMap, reverse, findIndex, union, filter, intersect, null, elemIndex)
import Data.Array as Array
import Data.String (Pattern(..), split, toCharArray, fromCharArray, joinWith)
import Data.Tuple (Tuple(..), fst, snd, uncurry)
import Data.Maybe (Maybe(..), isNothing, isJust, fromMaybe, maybe)
import Data.Char (toUpper)
import Data.Traversable (traverse, for)
import Data.Functor (mapFlipped, (<#>))
import Data.Map
import Data.Enum (succ)
import Data.Foldable
import Data.Int
import Data.Unfoldable
import Data.List as List

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
            , passant :: Maybe Square
            , promotion :: Maybe OfficerType
            , pieceType :: PieceType | e }

type FullMove = { source :: Square
                , moveType :: MoveType
                , destination :: Square
                , passant :: Maybe Square
                , promotion :: Maybe OfficerType
                , pieceType :: PieceType }            


resolveLegal :: Move -> Position -> Array FullMove
resolveLegal mv p = filter (flip isLegalMove p) (resolve mv p)


isLegalMove :: FullMove -> Position -> Boolean
isLegalMove mv p = map isLegal (perform mv p) == Just true


-- It's got to be called something..
huff :: forall d c f b a u.                                                      
        ( Ord c, Foldable f, Unfoldable u) =>
        (u (Tuple a b) -> f (Tuple c d)) -> Map a b -> Map c d
huff f = fromFoldable <<< f <<< toUnfoldable


filterMap :: forall a b. Ord a => (Tuple a b -> Boolean) -> Map a b -> Map a b
filterMap pred m = huff (filter pred) m

arrayKeys :: forall u a b. (Unfoldable u) => Map a b -> u a
arrayKeys = List.toUnfoldable <<< keys

-- A position is legal if the king cannot be attacked.
isLegal :: Position -> Boolean
isLegal p = let king = Piece (Officer King) (opponentPlayer p)
                ksq = arrayKeys (filterPieces p (\pc -> pc == king))
                
                pcs = toUnfoldable (filterPieces p \pc -> pieceColor pc == (currentPlayer p))
                asqs = concatMap (attacks p) pcs
            in null (intersect ksq asqs)


-- Helper, to simplify isLegal
attacks :: Position -> Tuple (Tuple Int Int) Piece -> Array (Tuple Int Int)
attacks p (Tuple sq pc) = attackRange pc sq p


filterPieces :: Position -> (Piece -> Boolean) -> Board
filterPieces p pred = filterMap (pred <<< snd) (positionBoard p)


perform :: FullMove -> Position -> Maybe Position
perform mv p = Position <$> newboard mv p <*> pure (newprops mv p)


newboard :: FullMove -> Position -> Maybe Board
newboard mv p = if isPawnMove mv p
                then newBoardAfterPawnMove mv p
                else move' mv.source mv.destination (positionBoard p)


newBoardAfterPawnMove :: FullMove -> Position -> Maybe Board
newBoardAfterPawnMove mv p =
  let b' = move' mv.source mv.destination (positionBoard p)
      down = -pawnDirection (currentPlayer p)
  in if isLastRankMove mv p
     then insert mv.destination <$> promotionPiece mv p <*> b'
     else if mv.moveType == Captures && Just mv.destination == positionPassant p
          then delete <$> offset mv.destination 0 down <*> b'
          else b'


positionPassant :: Position -> Maybe Square
positionPassant p = (positionProps p).passant


promotionPiece :: FullMove -> Position -> Maybe Piece
promotionPiece mv p = Piece <$> map Officer mv.promotion <*> pure (currentPlayer p)


isLastRankMove :: FullMove -> Position -> Boolean
isLastRankMove mv p = rank mv.destination == lastRank (currentPlayer p)


lastRank :: Color -> Int
lastRank White = 8
lastRank Black = 1


isPawnMove :: FullMove -> Position -> Boolean
isPawnMove mv p =
  let b = positionBoard p
  in mv.pieceType == Pawn && pieceTypeAt mv.source b == Just Pawn


newprops :: FullMove -> Position -> Properties
newprops mv p = let props = positionProps p
                    b = positionBoard p

                    hf = if pieceTypeAt mv.source b == Just Pawn
                         then 0
                         else props.halfMove + 1
                              
                    fl = props.fullMove + 1

                    passant = newPassant mv p
                    rights = newRights mv p
                    turn = opponentPlayer p
                in props { turn = turn
                         , rights = rights
                         , passant = passant
                         , halfMove = hf
                         , fullMove = fl }


newPassant :: FullMove -> Position -> Maybe Square
newPassant mv p =
  let up = pawnDirection (currentPlayer p)
  in if pieceTypeAt mv.source (positionBoard p) == Just Pawn &&
        rank mv.destination - rank mv.source == 2 * up
     then offset mv.source 0 up
     else Nothing


castlingSquares :: Map String (Array CastlingRight)
castlingSquares = fromFoldable [ Tuple "e1" [Castle Kingside White, Castle Queenside White]
                               , Tuple "a1" [Castle Queenside White]
                               , Tuple "h1" [Castle Kingside White]
                                 
                               , Tuple "e8" [Castle Kingside Black, Castle Queenside Black]
                               , Tuple "a8" [Castle Queenside Black]
                               , Tuple "h8" [Castle Kingside Black]]

newRights :: FullMove -> Position -> Array CastlingRight
newRights mv p = let r = (positionProps p).rights
                     s = squareString mv.source
                     x = case s of
                       Just s' -> lookup s' castlingSquares
                       Nothing -> Nothing
                 in maybe r (Array.difference r) x


move :: Square -> Square -> Board -> Board
move sq sq2 b = fromMaybe b (move' sq sq2 b)


-- Try to move the piece at 'sq' to 'sq2'
-- If no piece is found, return Nothing
move' :: Square -> Square -> Board -> Maybe Board
move' sq sq2 b = lookup sq b <#> \pc ->
  insert sq2 pc (delete sq b)


-- Resolve 'standard' moves into fully qualified moves
resolve :: Move -> Position -> Array FullMove
resolve mv@{moveType, destination, promotion, pieceType, passant} p =
  candidates mv p <#> \source -> { moveType
                                 , destination
                                 , promotion
                                 , pieceType
                                 , source
                                 , passant }


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
currentPlayer p = (positionProps p).turn


positionProps :: Position -> Properties
positionProps (Position _ p) = p

opponentPlayer :: Position -> Color
opponentPlayer = opponent <<< currentPlayer

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
type Properties = { turn :: Color
                  , rights :: Array CastlingRight
                  , passant :: Maybe Square
                  , halfMove :: Int
                  , fullMove :: Int }


parsePosition :: FENString -> Maybe Position
parsePosition s =
  let
    b :: Maybe Board
    b = bind (boardString s) board

    p :: Maybe Properties
    p = bind (propString s) props
  in Position <$> b <*> p

-- fen = "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1"

passant :: Position -> Maybe Square
passant p = (positionProps p).passant

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


props :: FENPropertyString -> Maybe Properties
props (FENProps s) = do
  let parts = split (Pattern " ") s

  st <- index parts 0
  sr <- index parts 1
  sp <- index parts 2
  shf <- index parts 3 
  sfl <- index parts 3 
  
  t <- parseTurn st
  r <- parseRights sr
  p <- parsePassant sp
  hf <- parseInt shf
  fl <- parseInt sfl

  Just { turn : t, rights : r, passant : p, halfMove : hf, fullMove : fl }


fen :: String
fen = "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1"

data Piece = Piece PieceType Color
data PieceType = Pawn | Officer OfficerType
data OfficerType = King | Queen | Rook | Bishop | Knight 
data Color = White | Black

data CastlingRight = Castle Side Color
data Side = Queenside | Kingside


derive instance moveTypeEq :: Eq MoveType

derive instance castlingRightEq :: Eq CastlingRight
derive instance castlingRightOrd :: Ord CastlingRight

derive instance sideEq :: Eq Side
derive instance sideOrd :: Ord Side


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

parseSquare :: String -> Maybe Square
parseSquare = parseSquare' <<< toCharArray

parseSquare' :: Array Char -> Maybe Square
parseSquare' [f,r] =
  let files = toCharArray "abcdefgh"
      ranks = toCharArray "12345678"
  in Tuple <$> elemIndex f files <*> elemIndex r ranks

parseSquare' _ = Nothing     


parseInt :: String -> Maybe Int
parseInt = fromString

parseTurn :: String -> Maybe Color
parseTurn "b" = Just Black
parseTurn "w" = Just White
parseTurn _ = Nothing

parseRights :: String -> Maybe (Array CastlingRight)
parseRights "-" = Just [] -- Valid but empty
parseRights s = traverse parseRight (toCharArray s)

parseRight :: Char -> Maybe CastlingRight
parseRight 'K' = Just (Castle Kingside White)
parseRight 'Q' = Just (Castle Kingside White)
parseRight 'k' = Just (Castle Kingside Black)
parseRight 'q' = Just (Castle Kingside Black)
parseRight _ = Nothing

parsePassant :: String -> Maybe (Maybe Square)
parsePassant "-" = Just Nothing
parsePassant sq = map Just (parseSquare sq)

fenSplit :: FENString -> Maybe (Tuple FENBoardString FENPropertyString)
fenSplit (FEN s) = let parts = split (Pattern " ") s
                       
                       b :: Maybe String
                       b = index parts 0
                       
                       p :: Maybe String
                       p = index parts 1
                       
                   in map wrap (lift2 Tuple p b)

wrap :: Tuple String String -> Tuple FENBoardString FENPropertyString
wrap (Tuple p b) = Tuple (FENBoard b) (FENProps p)
