module Container where

import Prelude

import React.Basic (CreateComponent, component, element)
import React.Basic as React
import React.Basic.DOM as R
import ToggleButton (mkToggleButton)

mkToggleButtonContainer :: CreateComponent {} Unit
mkToggleButtonContainer = do
  toggleButton <- mkToggleButton

  component "Container" \_ ->
    React.pure $ R.div
      { children:
          [ element toggleButton { label: "A" }
          , element toggleButton { label: "B" }
          ]
      }
