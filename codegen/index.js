const fs = require("fs");
const { props, voids, types, reserved } = require("./consts");
const genFile = "../src/React/Basic/DOM/Generated.purs";

const header = `-- | ----------------------------------------
-- | THIS FILE IS GENERATED -- DO NOT EDIT IT
-- | ----------------------------------------

module React.Basic.Hooks.DOM.Generated where

import Prim.Row (class Union)
import React.Basic.Hooks (JSX, element)
import React.Basic.Hooks.DOM.Internal (SharedProps, unsafeCreateDOMComponent)
import React.Basic.Hooks.Events (EventHandler)

`;

const printRecord = elProps =>
  elProps.length
    ? `
  ( ${elProps.map(p => `${p} :: ${types[p] || "String"}`).join("\n  , ")}
  )`
    : "()";

const domTypes = props.elements.html
  .map(e => {
    const noChildren = voids.includes(e);
    const symbol = reserved.includes(e) ? `${e}'` : e;
    return `
    type Props_${e} =${printRecord(
      (noChildren ? [] : ["children"]).concat(props[e] || []).sort()
    )}

    ${symbol}
      :: forall attrs attrs_
       . Union attrs attrs_ (SharedProps Props_${e})
      => Record attrs
      -> JSX
    ${symbol} = element (unsafeCreateDOMComponent "${e}")${
      noChildren
        ? ""
        : `

    ${e}_ :: Array JSX -> JSX
    ${e}_ children = ${symbol} { children }`
    }
`;
  })
  .map(x => x.replace(/^\n\ {4}/, "").replace(/\n\ {4}/g, "\n"))
  .join("\n");

console.log(`Writing "${genFile}" ...`);
fs.writeFileSync(genFile, header + domTypes);
console.log("Done.");
