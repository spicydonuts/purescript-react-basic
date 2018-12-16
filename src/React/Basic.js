"use strict";

var React = require("react");
var Fragment = React.Fragment || "div";

exports.useState_ = function(tuple, initialState) {
  var r = React.useState(initialState);
  var state = r[0];
  var setState = r[1];
  return tuple(state)(function(update) {
    return function() {
      return setState(update);
    };
  });
};

exports.useEffect_ = React.useEffect;

// exports.useReducer_ = function(reducer, initialState, initialAction) {
//   var state = React.useReducer(reducer, initialState, initialAction);
//   return { state: state[0], dispatch: state[1] };
// };

// exports.readRef_ = function(ref) {
//   return ref.current;
// };

// exports.writeRef_ = function(ref, a) {
//   ref.current = a;
// };

// exports.useRef_ = React.useRef;

exports.empty = null;

exports.keyed_ = function(key, child) {
  return React.createElement(Fragment, { key: key }, child);
};

exports.element_ = function(component, props) {
  return React.createElement.apply(
    null,
    [component, props].concat((props && props.children) || null)
  );
};

exports.elementKeyed_ = exports.element_;

exports.fragment = function(children) {
  return React.createElement.apply(null, [Fragment, {}].concat(children));
};

exports.displayName = function(component) {
  return typeof component === "string"
    ? component
    : component.displayName || "[unknown]";
};

exports.unsafeSetDisplayName = function(displayName, component) {
  component.displayName = displayName;
  component.toString = function() {
    return displayName;
  };
  return component;
};
