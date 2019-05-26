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
import Data.Maybe
import qualified Data.Text as T

import Reflex.Dom
import Obelisk.Generated.Static

import Common.Chess

data AppState = AppState
  { appStateState       :: GameState
  , appStateTurn        :: Color
  , appStateStartOfMove :: Maybe Point
  }

initialState :: AppState
initialState = AppState initialGameState White Nothing

mkBoard
  :: ( DomBuilder t m
     , MonadFix m
     , PostBuild t m
     )
  => Dynamic t AppState -> m (Event t Point)
mkBoard gs =
  elAttr "table" ("style" =: "margin-left: auto; margin-right: auto") $ el "tbody" $ do
    rows <- mapM (row gs) [7, 6..0]
    pure $ leftmost rows

row
  :: ( DomBuilder t m
     , MonadFix m
     , PostBuild t m
     )
  => Dynamic t AppState -> Int ->  m (Event t Point)
row gs j =
  el "tr" $ do
    cells <- mapM (cell gs) [(i, j) | i <- [0..7]]
    pure $ leftmost cells

cell
  :: ( DomBuilder t m
     , MonadFix m
     , PostBuild t m
     )
  => Dynamic t AppState -> Point -> m (Event t Point)
cell gs p = el "td" $ do
    (e, _) <- elDynAttr' "img" (square p <$> gs) $ pure ()
    pure $ p <$ domEvent Click e
    where
      square pos (AppState bd _ active) = "src" =: (translate $ bd <!> p)
        <> "style" =: ("display: block; width: 45px; height: 45px; background-color: " <> backgroundColor active p)
        <> "draggable" =: "false"
      backgroundColor (Just p) p' | p == p' = "yellow"
      backgroundColor _ p                   = defaultColor p
      defaultColor (i,j) | (i + j) `mod` 2 == 0 = "grey"
                         | otherwise            = "white"
      translate (Just (ColoredPiece clr p)) = translatePiece clr p
      translate _                           = static @"chess/blank.svg"
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

click :: Point -> AppState -> AppState
click new (AppState brd clr act) = fromMaybe (game clr brd) $ case act of
  Nothing -> case brd <!> new of
    Just (ColoredPiece color piece) -> do
      guard $ color == clr
      pure $ AppState brd clr $ Just new
    Nothing -> Nothing
  Just old -> game (opponent clr) <$> move brd clr old new
  where
    game c b = AppState b c Nothing
    validGrab = isJust $ do
      Just (ColoredPiece color piece) <- pure $ brd <!> new
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
        appState <- foldDyn click initialState pos
        pos <- mkBoard appState
        elAttr "h3" ("style" =: "text-align: center") $ do
          dynText $ T.pack . scenarioText <$> appState
        elAttr "h4" ("style" =: "text-align: center") $ do
          dynText $ T.pack . detailText <$> appState
      pure ()

scenarioText :: AppState -> String
scenarioText (AppState board turn _) = show turn <> checkText where
  checkText | inCheckmate board turn = " has been checkmated"
            | inStalemate board turn = " would be next, but it's a stalemate"
            | inCheck board turn     = "'s turn -- to get out of check"
            | otherwise              = "'s turn"

detailText :: AppState -> String
detailText (AppState board _ _) = show (gameStateCastle board) <> " " <> show (gameStatePhantomPawn board)
