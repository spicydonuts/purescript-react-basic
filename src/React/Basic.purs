module React.Basic
  ( Component
  , Render
  , bind
  , discard
  , pure
  , RenderJSX
  , render
  , CreateComponent
  , JSX
  , component
  , RenderState
  , useState
  , RenderEffect
  , useEffect
  , RenderReducer
  , useReducer
  -- , Ref
  -- , readRef
  -- , renderRef
  -- , writeRef
  -- , useRef
  , Key
  , class ToKey
  , toKey
  , empty
  , keyed
  , fragment
  , element
  , elementKeyed
  , displayName
  , module Data.Tuple
  , module Data.Tuple.Nested
  ) where

import Prelude hiding (bind, discard, pure)

import Control.Applicative.Indexed (class IxApplicative, ipure)
import Control.Apply.Indexed (class IxApply)
import Control.Bind.Indexed (class IxBind, ibind)
import Data.Function.Uncurried (Fn2, mkFn2, runFn2)
import Data.Functor.Indexed (class IxFunctor)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toNullable)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (tuple2, (/\))
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn4, mkEffectFn1, runEffectFn2, runEffectFn4)
import Prelude (bind, pure) as Prelude
import Unsafe.Coerce (unsafeCoerce)

newtype Component props = Component (EffectFn1 props JSX)

foreign import data Render :: Type -> Type -> Type -> Type

instance ixFunctorRender :: IxFunctor Render where
  imap = unsafeCoerce (map :: forall a b. (a -> b) -> Effect a -> Effect b)

instance ixApplyRender :: IxApply Render where
  iapply = unsafeCoerce (apply :: forall a b. Effect (a -> b) -> Effect a -> Effect b)

instance ixBindRender :: IxBind Render where
  ibind = unsafeCoerce (Prelude.bind :: forall a b. Effect a -> (a -> Effect b) -> Effect b)

bind :: forall a b x y z m. IxBind m => m x y a -> (a -> m y z b) -> m x z b
bind = ibind

discard :: forall a b x y z m. IxBind m => m x y a -> (a -> m y z b) -> m x z b
discard = ibind

pure :: forall a x m. IxApplicative m => a -> m x x a
pure = ipure

instance ixApplicativeRender :: IxApplicative Render where
  ipure = unsafeCoerce (Prelude.pure :: forall a. a -> Effect a)

type CreateComponent props = Effect (Component props)

component
  :: forall props
   . String
  -> (forall hooks. props -> Render hooks RenderJSX JSX)
  -> CreateComponent props
component name renderFn =
  let c = Component (mkEffectFn1 (unsafeCoerce renderFn))
   in runEffectFn2 unsafeSetDisplayName name c

foreign import data RenderState :: Type -> Type -> Type

useState
  :: forall hooks state
   . state
  -> Render hooks (RenderState state hooks) (Tuple state ((state -> state) -> Effect Unit))
useState initialState = unsafeCoerce do
  runEffectFn2 useState_ (mkFn2 Tuple) initialState

foreign import data RenderEffect :: Type -> Type

useEffect
  :: forall hooks
   . Array Key
  -> Effect (Effect Unit)
  -> Render hooks (RenderEffect hooks) Unit
useEffect keys effect = unsafeCoerce (runEffectFn2 useEffect_ effect keys)

foreign import data RenderJSX :: Type

render :: forall hooks. JSX -> Render hooks RenderJSX JSX
render jsx = unsafeCoerce (ipure jsx :: forall a. Render a a JSX)

foreign import data RenderReducer :: Type -> Type -> Type -> Type

-- | useReducer
-- | TODO: add note about conditionally updating state
useReducer
  :: forall hooks state action
   . ToKey state
  => Maybe action
  -> state
  -> (state -> action -> state)
  -> Render hooks (RenderReducer state action hooks) (Tuple state (action -> Effect Unit))
useReducer initialAction initialState reducer = unsafeCoerce do
  runEffectFn4 useReducer_
    (mkFn2 Tuple)
    (mkFn2 reducer)
    initialState
    (toNullable initialAction)

-- data Ref a

-- readRef :: forall a. Ref a -> Effect a
-- readRef = runEffectFn1 readRef_

-- renderRef :: forall a. Ref a -> Render a
-- renderRef ref = Render (readRef ref)

-- writeRef :: forall a. Ref a -> a -> Effect Unit
-- writeRef = runEffectFn2 writeRef_

-- useRef
--   :: forall a
--    . a
--   -> Render (Ref a)
-- useRef initialValue = Render do
--   runEffectFn1 useRef_ initialValue

-- | Keys represent values React uses to check for changes.
-- | This is done using JavaScript's reference equality (`===`),
-- | so complicated types may want to implement `ToKey` so that
-- | it returns a primative like a `String`. A timestamp appended
-- | to a unique ID, for example. Less strict cases can implement
-- | `ToKey` using `unsafeCoerce`, while some extreme cases may
-- | need a hashing or stringifying mechanism.
data Key

class ToKey a where
  toKey :: a -> Key

instance trString :: ToKey String where
  toKey = unsafeCoerce

instance trInt :: ToKey Int where
  toKey = unsafeCoerce

instance trNumber :: ToKey Number where
  toKey = unsafeCoerce

instance trBoolean :: ToKey Boolean where
  toKey = unsafeCoerce

instance trRecord :: ToKey (Record a) where
  toKey = unsafeCoerce

instance trArray :: ToKey (Array a) where
  toKey = unsafeCoerce

instance trNullable :: ToKey (Nullable a) where
  toKey = unsafeCoerce

instance trMaybe :: ToKey (Maybe a) where
  toKey a = toKey (toNullable a)

-- | Represents rendered React VDOM (the result of calling `React.createElement`
-- | in JavaScript).
-- |
-- | `JSX` is a `Monoid`:
-- |
-- | - `append`
-- |   - Merge two `JSX` nodes using `React.Fragment`.
-- | - `mempty`
-- |   - The `empty` node; renders nothing.
-- |
-- | __*Hint:* Many useful utility functions already exist for Monoids. For example,
-- |   `guard` can be used to conditionally render a subtree of components.__
foreign import data JSX :: Type

instance semigroupJSX :: Semigroup JSX where
  append a b = fragment [ a, b ]

instance monoidJSX :: Monoid JSX where
  mempty = empty

-- | An empty `JSX` node. This is often useful when you would like to conditionally
-- | show something, but you don't want to (or can't) modify the `children` prop
-- | on the parent node.
-- |
-- | __*See also:* `JSX`, Monoid `guard`__
foreign import empty :: JSX

-- | Apply a React key to a subtree. React-Basic usually hides React's warning about
-- | using `key` props on components in an Array, but keys are still important for
-- | any dynamic lists of child components.
-- |
-- | __*See also:* React's documentation regarding the special `key` prop__
keyed :: String -> JSX -> JSX
keyed = runFn2 keyed_

-- | Render an Array of children without a wrapping component.
-- |
-- | __*See also:* `JSX`__
foreign import fragment :: Array JSX -> JSX

-- | Create a `JSX` node from a `Component`, by providing the props.
-- |
-- | This function is for non-React-Basic React components, such as those
-- | imported from FFI.
-- |
-- | __*See also:* `Component`, `elementKeyed`__
element
  :: forall props
   . Component {| props }
  -> {| props }
  -> JSX
element (Component c) props = runFn2 element_ c props

-- | Create a `JSX` node from a `Component`, by providing the props and a key.
-- |
-- | This function is for non-React-Basic React components, such as those
-- | imported from FFI.
-- |
-- | __*See also:* `Component`, `element`, React's documentation regarding the special `key` prop__
elementKeyed
  :: forall props
   . Component {| props }
  -> { key :: String | props }
  -> JSX
elementKeyed = runFn2 elementKeyed_

-- | Retrieve the Display Name from a `ComponentSpec`. Useful for debugging and improving
-- | error messages in logs.
-- |
-- | __*See also:* `displayNameFromSelf`, `createComponent`__
foreign import displayName
  :: forall props
   . Component props
  -> String



-- |
-- | Internal utility or FFI functions
-- |

foreign import unsafeSetDisplayName
  :: forall props
   . EffectFn2 String (Component props) (Component props)

foreign import useState_
  :: forall state
   . EffectFn2
       (forall a b. Fn2 a b (Tuple a b))
       state
       (Tuple state ((state -> state) -> Effect Unit))

foreign import useEffect_
  :: EffectFn2
       (Effect (Effect Unit))
       (Array Key)
       Unit

foreign import useReducer_
  :: forall state action
   . EffectFn4
       (forall a b. Fn2 a b (Tuple a b))
       (Fn2 state action state)
       state
       (Nullable action)
       (Tuple state (action -> Effect Unit))

-- foreign import readRef_
--   :: forall a
--    . EffectFn1
--        (Ref a)
--        a

-- foreign import writeRef_
--   :: forall a
--    . EffectFn2
--        (Ref a)
--        a
--        Unit

-- foreign import useRef_
--   :: forall a
--    . EffectFn1
--        a
--        (Ref a)

foreign import keyed_ :: Fn2 String JSX JSX

foreign import element_
  :: forall props
   . Fn2 (EffectFn1 { | props } JSX) { | props } JSX

foreign import elementKeyed_
  :: forall props
   . Fn2 (Component {| props }) { key :: String | props } JSX
