{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
module Frontend where

import Common.Api
import Common.Route
import Control.Arrow     ((&&&))
import Control.Monad.Fix (MonadFix)
import Data.Array.IArray as A
import Data.Monoid       ((<>))
import qualified Data.Text         as T
import Obelisk.Frontend
import Obelisk.Generated.Static
import Obelisk.Route
import Reflex.Dom
import qualified Frontend.Chess as Ch


frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
    el "title" $ text "Obelisk Minimal Example"
    styleSheet $ static @"css/fontawesome.min.css"
    styleSheet $ static @"css/font.css"
    styleSheet $ static @"css/normalize.css"
    styleSheet $ static @"css/style.css"

  , _frontend_body = do
        el "main" $ el "article" $ Ch.app
  }

styleSheet :: DomBuilder t m => T.Text -> m ()
styleSheet myLink = elAttr "link" attrs blank
  where attrs = "rel" =: "stylesheet"
             <> "type" =: "text/css"
             <> "href" =: myLink
