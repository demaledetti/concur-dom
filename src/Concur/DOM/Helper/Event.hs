module Concur.DOM.Helper.Event (
    click
  , dblclick
  , input
  , inputValue
  , key
  , keydown
  , MouseEvent
  ) where

import Concur.DOM.Run (DOMAttr(..), DOMAttrValue(..), DOMEventHandler)
import GHC.JS.Prim (fromJSString, JSVal, unsafeGetProp)


click, dblclick, input, keydown :: DOMEventHandler -> DOMAttr
click = _ev "click"
dblclick = _ev "dblclick"
input = _ev "input"
keydown = _ev "keydown"

_ev :: String -> DOMEventHandler -> DOMAttr
_ev name = DOMAttr ("on" <> name) . DOMAttrEvent

type MouseEvent = JSVal

inputValue :: JSVal -> JSVal
inputValue e = unsafeGetProp (unsafeGetProp e "target") "value"

key :: JSVal -> String
key e = fromJSString (unsafeGetProp e "key")
