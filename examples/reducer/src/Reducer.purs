module Reducer where

import Prelude

import Data.Maybe (Maybe(..))
import React.Basic (CreateComponent, component, fragment, render, useReducer, (/\))
import React.Basic as React
import React.Basic.DOM as R
import React.Basic.DOM.Events (capture_)

data Action
  = Increment
  | Decrement

mkReducer :: CreateComponent {}
mkReducer = do
  component "Reducer" \props -> React.do
    state /\ dispatch <-
      useReducer
        Nothing        -- initial action
        { counter: 0 } -- initial state
        \state -> case _ of
          Increment -> state { counter = state.counter + 1 }
          Decrement -> state { counter = state.counter - 1 }

    render $ fragment
      [ R.button
          { onClick: capture_ $ dispatch Increment
          , children: [ R.text $ "Increment" ]
          }
      , R.button
          { onClick: capture_ $ dispatch Decrement
          , children: [ R.text $ "Decrement" ]
          }
      , R.div_
          [ R.text $ show state.counter
          ]
      ]
