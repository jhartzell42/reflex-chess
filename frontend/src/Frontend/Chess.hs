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

import Common.Chess

data GameState = GameState 
  { board       :: Board
  , turn        :: Color
  , startOfMove :: Maybe Point
  }

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

click :: Point -> GameState -> GameState
click new gs@(GameState brd clr act) = fromMaybe (game clr brd) $ case act of
  Nothing -> do
    Just (ColoredPiece color piece) <- pure $ brd A.! new
    guard $ color == clr
    pure $ GameState brd clr $ Just new
  Just old -> do
    newBoard <- basicMove brd clr old new
    guard $ not $ inCheck newBoard clr
    pure $ game (opponent clr) newBoard
  where
    game c b = GameState b c Nothing
    validGameMove old = validBasicMove brd clr old new
    validGrab = isJust $ do
      Just (ColoredPiece color piece) <- pure $ brd A.! new
      guard $ color == clr

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
        gs  <- foldDyn click initialState pos
        pos <- mkBoard gs
        elAttr "h3" ("style" =: "text-align: center") $ do
          dynText $ T.pack . scenarioText <$> gs
      pure ()

scenarioText :: GameState -> String
scenarioText (GameState board turn _) = show turn <> checkText where
  checkText | inCheckMate board turn = " has been checkmated"
            | inCheck board turn     = "'s turn -- to get out of check"
            | otherwise              = "'s turn"
