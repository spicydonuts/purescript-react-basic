module ControlledInput where

import Prelude

import Control.Applicative.Indexed (ipure)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import React.Basic.Hooks (CreateComponent, Render, UseState, component, fragment, useState, (/\))
import React.Basic.Hooks as React
import React.Basic.Hooks.DOM as R
import React.Basic.Hooks.DOM.Events (capture, targetValue, timeStamp)
import React.Basic.Hooks.Events (EventHandler, merge)

mkControlledInput :: CreateComponent {} (UseInput () (UseInput () Unit))
mkControlledInput =
  component "ControlledInput" \props -> React.do
    firstName <- useInput "hello"
    lastName <- useInput "world"

    React.pure $ R.form_
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

type UseInput state hooks = UseState (InputState state) hooks
useInput
  :: forall hooks
   . String
  -> Render hooks
       (UseInput () hooks)
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
