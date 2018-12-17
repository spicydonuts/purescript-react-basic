module Counter where

import Prelude

import Effect (Effect)
import React.Basic (CreateComponent, component, render, toKey, useEffect, useState, (/\))
import React.Basic as React
import React.Basic.DOM as R
import React.Basic.DOM.Events (capture_)

mkCounter :: CreateComponent {}
mkCounter = do
  component "Counter" \props -> React.do
    counter /\ setCounter <- useState 0

    useEffect [toKey counter] do
      setDocumentTitle $ "Count: " <> show counter
      pure mempty

    render $ R.button
      { onClick: capture_ $ setCounter (_ + 1)
      , children: [ R.text $ "Increment: " <> show counter ]
      }

foreign import setDocumentTitle :: String -> Effect Unit
