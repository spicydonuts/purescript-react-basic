module Reducer where

import Prelude

import React.Basic (CreateComponent, RenderReducer, component, fragment, render, useReducer, (/\))
import React.Basic as React
import React.Basic.DOM as R
import React.Basic.DOM.Events (capture_)

data Action
  = Increment
  | Decrement

mkReducer :: CreateComponent {} (RenderReducer { counter :: Int } Action Unit)
mkReducer = do
  component "Reducer" \props -> React.do

    state /\ dispatch <-
      useReducer { counter: 0 } \state -> case _ of
        Increment -> state { counter = state.counter + 1 }
        Decrement -> state { counter = state.counter - 1 }

    render $ fragment
      [ R.button
          { onClick: capture_ $ dispatch Decrement
          , children: [ R.text $ "Decrement" ]
          }
      , R.button
          { onClick: capture_ $ dispatch Increment
          , children: [ R.text $ "Increment" ]
          }
      , R.div_
          [ R.text $ show state.counter
          ]
      ]
