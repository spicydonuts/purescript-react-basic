module Counter where

import Prelude

import Effect (Effect)
import React.Basic (CreateComponent, component, render, toKey, useEffect, useState, (/\))
import React.Basic.DOM as R
import React.Basic.DOM.Events (capture_)

mkCounter :: CreateComponent {}
mkCounter = do
  component "Counter" \bind discard props -> do
    counter /\ setCounter <- useState 0

    useEffect [toKey counter] $ docTitle counter

    render $ R.button
      { onClick: capture_ $ setCounter (_ + 1)
      , children: [ R.text $ "Increment: " <> show counter ]
      }

  where
    docTitle counter = do
      setDocumentTitle $ "Count: " <> show counter
      pure (pure unit)

foreign import setDocumentTitle :: String -> Effect Unit
