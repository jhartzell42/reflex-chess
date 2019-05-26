{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE DataKinds             #-}

module Frontend.Chess (app) where

import Control.Monad
import Control.Monad.Fix (MonadFix)
import Data.Array.IArray as A
import Data.Maybe
import qualified Data.Text as T

import Reflex.Dom
import Obelisk.Generated.Static

type Point = (Int, Int)

data Color = White | Black deriving (Eq, Show)
data Piece = King | Queen | Rook | Bishop | Knight | Pawn deriving (Eq, Show)

data ColoredPiece = ColoredPiece
  { color :: Color
  , piece :: Piece
  } deriving (Eq)

instance Show ColoredPiece where
  show (ColoredPiece clr p) = show clr <> " " <> show p

type Board = Array Point (Maybe ColoredPiece)

data GameState = GameState 
  { board       :: Board
  , turn        :: Color
  , startOfMove :: Maybe Point
  }

initialBoard :: Board
initialBoard = A.array ((0, 0), (7,7))
               [ ((i, j), square i j) | i <- [0..7], j <- [0..7]]
  where
    initialColor n | n >= 6 = Just Black
                   | n <= 1 = Just White
                   | otherwise = Nothing
    mainRow n = n == 7 || n == 0
    pawnRow n = n == 1 || n == 6
    piece i j | pawnRow j = Just Pawn
              | mainRow j = case i of
                0 -> Just Rook
                1 -> Just Knight
                2 -> Just Bishop
                3 -> Just Queen
                4 -> Just King
                5 -> Just Bishop
                6 -> Just Knight
                7 -> Just Rook
              | otherwise = Nothing
    square i j = ColoredPiece <$> initialColor j <*> piece i j

initialState :: GameState
initialState = GameState initialBoard White Nothing

mkBoard
  :: ( DomBuilder t m
     , MonadFix m
     , PostBuild t m
     )
  => Dynamic t GameState -> m (Event t Point)
mkBoard gs =
  elAttr "table" ("style" =: "margin-left: auto; margin-right: auto") $ el "tbody" $ do
    rows <- mapM (row gs) [7, 6..0]
    return $ leftmost rows

row
  :: ( DomBuilder t m
     , MonadFix m
     , PostBuild t m
     )
  => Dynamic t GameState -> Int ->  m (Event t Point)
row gs j =
  el "tr" $ do
    cells <- mapM (cell gs) [(i, j) | i <- [0..7]]
    return $ leftmost cells


cell
  :: ( DomBuilder t m
     , MonadFix m
     , PostBuild t m
     )
  => Dynamic t GameState -> Point -> m (Event t Point)
cell gs p = el "td" $ do
    (e, _) <- elDynAttr' "img" (square p <$> gs) (return ())
    return $ p <$ domEvent Click e
    where
      square pos (GameState bd _ active) = "src" =: (translate $ bd A.! p)
        <> "style" =: ("display: block; width: 45px; height: 45px; background-color: " <> backgroundColor active p)
        <> "draggable" =: "false"
      backgroundColor (Just p) p' | p == p' = "yellow"
      backgroundColor _ p                   = defaultColor p
      defaultColor (i,j) | (i + j) `mod` 2 == 0 = "grey"
                         | otherwise            = "white"
      translate (Just (ColoredPiece clr p)) = translatePiece clr p
      translate _ = static @"chess/blank.svg"
      translatePiece White King   = static @"chess/kl.svg"
      translatePiece White Queen  = static @"chess/ql.svg"
      translatePiece White Rook   = static @"chess/rl.svg"
      translatePiece White Bishop = static @"chess/bl.svg"
      translatePiece White Knight = static @"chess/nl.svg"
      translatePiece White Pawn   = static @"chess/pl.svg"
      translatePiece Black King   = static @"chess/kd.svg"
      translatePiece Black Queen  = static @"chess/qd.svg"
      translatePiece Black Rook   = static @"chess/rd.svg"
      translatePiece Black Bishop = static @"chess/bd.svg"
      translatePiece Black Knight = static @"chess/nd.svg"
      translatePiece Black Pawn   = static @"chess/pd.svg"

move :: Point -> GameState -> GameState
move new gs@(GameState brd clr act) = fromMaybe (game clr brd) $ case act of
  Nothing -> do
    Just (ColoredPiece color piece) <- pure $ brd A.! new
    guard $ color == clr
    pure $ GameState brd clr $ Just new
  Just old -> do
    guard $ validBasicMove brd clr old new
    let newBoard = brd // [(old, Nothing), (new, brd A.! old)]
    guard $ not $ inCheck newBoard clr
    pure $ game (opponent clr) newBoard
  where
    game c b = GameState b c Nothing
    validGameMove old = validBasicMove brd clr old new
    validGrab = isJust $ do
      Just (ColoredPiece color piece) <- pure $ brd A.! new
      guard $ color == clr

inCheck :: Board -> Color -> Bool
inCheck board turn = not $ null $ do
  let points = [(i, j) | i <- [0..7], j <- [0..7]]

  new <- points
  Just (ColoredPiece newColor King) <- pure $ board A.! new
  guard $ newColor == turn

  old <- points
  let attacker = opponent turn
  guard $ validBasicMove board attacker old new

opponent :: Color -> Color
opponent = \case
  Black -> White
  White -> Black

validBasicMove :: Board -> Color -> Point -> Point -> Bool
validBasicMove brd turn old new = isJust $ do
  Just (ColoredPiece color piece) <- pure $ brd A.! old
  guard $ turn == color
  guard $ old /= new
  let dest = brd A.! new

  isCapture <- case dest of
    Just (ColoredPiece color _) -> do
      guard $ color /= turn
      pure True
    Nothing -> pure False

  let ((oldX, oldY), (newX, newY)) = (old, new)
      diffX = newX - oldX
      diffY = newY - oldY

      rookLike = diffX == 0 || diffY == 0
      bishopLike = abs diffX == abs diffY
      pawnLike = diffX == 0 && (diffY == pawnDirection || diffY == 2 * pawnDirection && oldY == pawnRow)
      pawnCaptureLike = diffY == pawnDirection && abs diffX == 1
      knightLike = (abs diffX == 1 && abs diffY == 2) || (abs diffX == 2 && abs diffY == 1)
      kingLike = abs diffX <= 1 && abs diffY <= 1

      -- There has got to be a better way to write this:
      stepX = signum diffX
      stepY = signum diffY
      steps (x, y) | x >= 8 || y >= 8       = []
                   | x == newX && y == newY = []
                   | otherwise              = (x, y):steps (x + stepX, y + stepY)
      clearPath = all isNothing $ (brd A.!) <$> steps (oldX + stepX, oldY + stepY)

  case piece of
    Pawn -> guard $ if isCapture then pawnCaptureLike else pawnLike && clearPath
    Bishop -> guard $ bishopLike && clearPath
    Rook -> guard $ rookLike && clearPath
    Queen -> guard $ (rookLike || bishopLike) && clearPath
    Knight -> guard knightLike
    King -> guard kingLike

  pure ()
  where
    pawnDirection = case turn of
      White -> 1
      Black -> -1
    pawnRow = case turn of
      White -> 1
      Black -> 6

app
  :: ( DomBuilder t m
     , MonadFix m
     , MonadHold t m
     , PostBuild t m
     )
  => m ()
app = divClass "container" $ do
  el "br" blank
  elAttr "h1" ("style" =: "text-align: center") $ text "Chess"
  el "div" $ do
    el "br" blank
    el "div" $ do
      rec
        gs  <- foldDyn move initialState pos
        pos <- mkBoard gs
        elAttr "h3" ("style" =: "text-align: center") $ do
          dynText $ T.pack . show . turn <$> gs
          text "'s Turn"
          dynText $ checkText . checkInCheck <$> gs
      pure ()
  where
  checkText True = " ... and they're in check"
  checkText False = ""
  checkInCheck (GameState board turn _) = inCheck board turn
