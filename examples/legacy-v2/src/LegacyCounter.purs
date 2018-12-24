module LegacyCounter where

import Prelude

import React.Basic.Hooks.Compat (Component, UseStatefulComponent, component, element, stateless)
import React.Basic.Hooks.DOM as R
import React.Basic.Hooks.Events as Events

type Props =
  { label :: String
  }

-- | checks `component`
legacyCounter :: Component Props (UseStatefulComponent { counter :: Int })
legacyCounter = component { displayName: "LegacyCounter", initialState, receiveProps, render }
  where
    initialState =
      { counter: 0
      }

    receiveProps self =
      pure unit

    render self =
      R.button
        { onClick: Events.handler_ do
            self.setState \s -> s { counter = s.counter + 1 }
        , children: [ element buttonLabel { label: self.props.label, counter: self.state.counter } ]
        }

-- | checks `stateless`
buttonLabel :: Component { label :: String, counter :: Int } Unit
buttonLabel = stateless { displayName: "ButtonLabel", render }
  where
    render props =
      R.text (props.label <> ": " <> show props.counter)
