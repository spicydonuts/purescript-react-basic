module ToggleButton where

import Prelude

import Effect.Console (log)
import React.Basic (CreateComponent, component, render, toKey, useEffect, useState, (/\))
import React.Basic.DOM as R
import React.Basic.DOM.Events (capture_)

mkToggleButton :: CreateComponent { label :: String }
mkToggleButton = do
  component "ToggleButton" \bind discard { label } -> do
    on /\ setOn <- useState false

    useEffect [toKey on] $ logState on

    render $ R.button
      { onClick: capture_ $ setOn not
      , children:
          [ R.text label
          , R.text if on then " On" else " Off"
          ]
      }
  where
    logState on = do
      log $ "State: " <> if on then "On" else "Off"
      pure (pure unit)
