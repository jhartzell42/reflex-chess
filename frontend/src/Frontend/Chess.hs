{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE DataKinds             #-}

module Frontend.Chess (app) where

import Control.Monad.Fix (MonadFix)
import Data.Array.IArray as A

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
  { board :: Board
  , startOfMove :: Maybe Point
  }

initialBoard :: Board
initialBoard = A.array ((0, 0), (7,7))
               [ ((i, j), square i j) | i <- [0..7], j <- [0..7]]
  where
    color n | n >= 6 = Just Black
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
    square i j = ColoredPiece <$> color j <*> piece i j

initialState :: GameState
initialState = GameState initialBoard Nothing

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
    (e, _) <- elDynAttr' "img" (fmap (square p . board) gs) (return ())
    return $ p <$ domEvent Click e
    where
      square pos bd = "src" =: (translate $ bd A.! p)
        <> "style" =: ("display: block; width: 45px; height: 45px; background-color: " <> backgroundColor p)
        <> "draggable" =: "false"
      backgroundColor (i,j) | (i + j) `mod` 2 == 0 = "grey"
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
move _ gs = gs -- XXX for right now

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
      return ()
