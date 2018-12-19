module ControlledInput where

import Prelude

import Control.Applicative.Indexed (ipure)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import React.Basic (CreateComponent, Render, RenderState, component, fragment, render, useState, (/\))
import React.Basic as React
import React.Basic.DOM as R
import React.Basic.DOM.Events (capture, targetValue, timeStamp)
import React.Basic.Events (EventHandler, merge)

mkControlledInput :: CreateComponent {} (RenderInput () (RenderInput () Unit))
mkControlledInput =
  component "ControlledInput" \props -> React.do
    firstName <- useInput "hello"
    lastName <- useInput "world"

    render $ R.form_
      [ renderInput firstName
      , renderInput lastName
      ]
  where
    renderInput input =
      fragment
        [ R.input { onChange: input.onChange, value: input.value }
        , R.p_ [ R.text ("Current value = " <> show input.value) ]
        , R.p_ [ R.text ("Changed at = " <> maybe "never" show input.lastChanged) ]
        ]

type InputState state = { value :: String, lastChanged :: Maybe Number | state }

type RenderInput state hooks = RenderState (InputState state) hooks
useInput
  :: forall hooks
   . String
  -> Render hooks
       (RenderInput () hooks)
       (InputState ( onChange :: EventHandler ))
useInput initialValue = React.do
  { value, lastChanged } /\ replaceState <- useState { value: initialValue, lastChanged: Nothing }
  ipure
    { onChange: capture
        (merge { targetValue, timeStamp })
        \{ timeStamp, targetValue } -> do
          replaceState \_ ->
            { value: fromMaybe "" targetValue
            , lastChanged: Just timeStamp
            }
    , value
    , lastChanged
    }
